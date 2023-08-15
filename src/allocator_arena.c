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

static const Size ARENA_DEFAULT_CAPACITY = 1024 * 1024 * 1024; // 1 MiB

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
	Uint8 data[];
};

void *my_alloc(Size size) {
#if defined(OS_WINDOWS)
	// Because Windows is shit
	return HeapAlloc(GetProcessHeap(), 0, size);
#else
	return malloc(size);
#endif
}

void my_free(void *ptr) {
#if defined(OS_WINDOWS)
	// Because Windows is shit
	HeapFree(GetProcessHeap(), 0, ptr);
#else
	free(ptr);
#endif
}

static ArenaRegion *arena_allocator_new_region(Size capacity) {
	const Size bytes = sizeof(ArenaRegion) + capacity;
	ArenaRegion *region = CAST(ArenaRegion *, my_alloc(bytes));
	if (!region) {
		return 0;
	}
	mutex_init(&region->mutex);
	mutex_lock(&region->mutex);
	region->next = 0;
	region->count = 0;
	region->capacity = capacity;
	mutex_unlock(&region->mutex);
	return region;
}

static Bool arena_allocator_init(Allocator *allocator) {
	Arena *arena = CAST(Arena *, my_alloc(sizeof *arena));
	if (!arena) {
		return false;
	}

	mutex_init(&arena->mutex);

	mutex_lock(&arena->mutex);
	arena->beg = arena_allocator_new_region(ARENA_DEFAULT_CAPACITY);
	arena->end = arena->beg;
	mutex_unlock(&arena->mutex);

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
		region = region->next;
		mutex_unlock(&self->mutex);
		mutex_fini(&self->mutex);
		my_free(self);
	}
	mutex_unlock(&arena->mutex);
	mutex_fini(&arena->mutex);
	my_free(arena);
	allocator->user = 0;
}

static Ptr arena_allocator_allocate(Allocator *allocator, Size size) {
	// Round to 16 byte alignment.
	size = (size + 16 - 1) & -16;

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