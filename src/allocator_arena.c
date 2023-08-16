#include <stdlib.h> // malloc, free
#include <string.h> // memcpy

#include "allocator.h"
#include "thread.h"

#if defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef IN
#endif

typedef struct Arena Arena;
typedef struct ArenaRegion ArenaRegion;

static const Size ARENA_DEFAULT_CAPACITY = 1024 * 1024; // 1 MiB
static const Size ARENA_DEFAULT_ALIGNMENT = 16;

struct Arena {
	Mutex mutex;
	ArenaRegion *beg  THREAD_GUARDED(mutex);
	ArenaRegion *end  THREAD_GUARDED(mutex);
};

struct ArenaRegion {
	Mutex mutex;
	ArenaRegion *next THREAD_GUARDED(mutex);
	Size count        THREAD_GUARDED(mutex);
	Size capacity     THREAD_GUARDED(mutex);
	ALIGN(16) char data[]; // This should be 16 byte aligned
};

static void *raw_alloc(Size size) {
#if defined(OS_WINDOWS)
	// Because Windows is shit
	return HeapAlloc(GetProcessHeap(), 0, size);
#else
	return malloc(size);
#endif
}

static void raw_free(void *ptr) {
#if defined(OS_WINDOWS)
	// Because Windows is shit
	HeapFree(GetProcessHeap(), 0, ptr);
#else
	free(ptr);
#endif
}

static void *raw_aligned_alloc(Size bytes, Size alignment) {
	const Size offset = alignment - 1 + sizeof(void*);
	void *p1 = raw_alloc(bytes + offset);
	if (!p1) {
		return 0;
	}
	void **p2 = RCAST(void **, (RCAST(Size, p1) + offset) & ~(alignment - 1));
	p2[-1] = p1;
	return p2;
}

static void raw_aligned_free(void *p) {
	raw_free(RCAST(void **, p)[-1]);
}

static ArenaRegion *arena_allocator_new_region(Size capacity)
	THREAD_INTERNAL
{
	const Size bytes = sizeof(ArenaRegion) + capacity;
	ArenaRegion *region = CAST(ArenaRegion *, raw_aligned_alloc(bytes, ARENA_DEFAULT_ALIGNMENT));
	if (!region) {
		return 0;
	}
	mutex_init(&region->mutex);
	region->next = 0;
	region->count = 0;
	region->capacity = capacity;
	return region;
}

static Bool arena_allocator_init(Allocator *allocator)
	THREAD_INTERNAL
{
	Arena *arena = CAST(Arena *, raw_aligned_alloc(sizeof *arena, ARENA_DEFAULT_ALIGNMENT));
	if (!arena) {
		return false;
	}

	mutex_init(&arena->mutex);

	arena->beg = arena_allocator_new_region(ARENA_DEFAULT_CAPACITY);
	arena->end = arena->beg;

	allocator->user = arena;

	return true;
}

static void arena_allocator_fini(Allocator *allocator) {
	Arena *arena = CAST(Arena *, allocator->user);
	mutex_lock(&arena->mutex);
	ArenaRegion *region = arena->beg;
	while (region) {
		ArenaRegion *const self = region;
		mutex_lock(&self->mutex);
		region = self->next;
		mutex_unlock(&self->mutex);
		mutex_fini(&self->mutex);
		raw_aligned_free(self);
	}
	mutex_unlock(&arena->mutex);
	mutex_fini(&arena->mutex);
	raw_aligned_free(arena);
	allocator->user = 0;
}

static Ptr arena_allocator_allocate(Allocator *allocator, Size size) {
	size = (size + ARENA_DEFAULT_ALIGNMENT - 1) & CAST(Size, -ARENA_DEFAULT_ALIGNMENT);

	Arena *const arena = CAST(Arena *, allocator->user);

	mutex_lock(&arena->mutex);
	ArenaRegion *region = arena->end;
	for (;;) {
		mutex_lock(&region->mutex);
		if (!(region->count + size > region->capacity && region->next)) {
			arena->end = region;
			// Keep mutex locked throughout.
			break;
		}
		ArenaRegion *next = region->next;
		mutex_unlock(&region->mutex);
		region = next;
	}

	if (region->count + size > region->capacity) {
		const Size capacity = (size + ARENA_DEFAULT_CAPACITY - 1) & -ARENA_DEFAULT_CAPACITY;
		ArenaRegion *next = arena_allocator_new_region(capacity);
		if (!next) {
			// Out of memory.
			mutex_unlock(&region->mutex);
			mutex_unlock(&arena->mutex);
			return 0;
		}
		region->next = next;
		region = next;
	}

	void *result = &region->data[region->count];
	region->count += size;
	mutex_unlock(&region->mutex);

	arena->end = region;
	mutex_unlock(&arena->mutex);

	return result;
}

static Ptr arena_allocator_reallocate(Allocator *allocator, void *old_data, Size old_size, Size new_size) {
	if (new_size <= old_size) {
		return old_data;
	}

	Ptr new_data = arena_allocator_allocate(allocator, new_size);
	return memcpy(new_data, old_data, old_size);
}

static void arena_allocator_deallocate(Allocator *allocator, void *data) {
	(void)allocator;
	(void)data;
	// Does nothing.
}

const AllocatorOperations ALLOCATOR_ARENA = {
	SLIT("arena"),
	arena_allocator_init,
	arena_allocator_fini,
	arena_allocator_allocate,
	arena_allocator_reallocate,
	arena_allocator_deallocate,
};