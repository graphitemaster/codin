#include <stdlib.h>
#include <stdatomic.h>

#include "allocator.h"
#include "thread.h"

static const Size DEFAULT_CAPACITY = 4096;
static const Float32 RESIZE_THRESHOLD = 0.75;
static const Uint32 PRIME1 = 73;
static const Uint32 PRIME2 = 5009;

#define TOMBSTONE RCAST(void *, 1)

typedef struct StandardAllocator StandardAllocator;

struct StandardAllocator {
	Size size     THREAD_GUARDED(mutex);
	Size deleted  THREAD_GUARDED(mutex);
	Size capacity THREAD_GUARDED(mutex);
	void **items  THREAD_GUARDED(mutex);
	Mutex mutex;
};

static Bool std_allocator_init(Allocator *ctx) {
	StandardAllocator *allocator = CAST(StandardAllocator*, calloc(1, sizeof *allocator));
	if (!allocator) {
		return false;
	}

	mutex_init(&allocator->mutex);
	mutex_lock(&allocator->mutex);
	allocator->size = 0;
	allocator->capacity = DEFAULT_CAPACITY;
	allocator->items = CAST(void**, calloc(allocator->capacity, sizeof *allocator->items));
	if (!allocator->items) {
		mutex_unlock(&allocator->mutex);
		free(allocator);
		return false;
	}
	mutex_unlock(&allocator->mutex);

	ctx->user = allocator;

	return true;
}

static void std_allocator_fini(Allocator *ctx) {
	StandardAllocator *allocator = CAST(StandardAllocator *, ctx->user);

	mutex_lock(&allocator->mutex);
	const Size capacity = allocator->capacity;
	for (Size i = 0; i < capacity; i++) {
		void *data = allocator->items[i];
		if (data == TOMBSTONE) {
			continue;
		}
		free(data);
		allocator->items[i] = TOMBSTONE;
		allocator->deleted++;
	}
	free(allocator->items);
	mutex_unlock(&allocator->mutex);

	mutex_fini(&allocator->mutex);
	free(allocator);
}

static Bool std_allocator_add_unlocked(StandardAllocator *allocator, void *item);
static Bool std_allocator_maybe_rehash_unlocked(StandardAllocator *allocator)
	THREAD_REQUIRES(allocator->mutex)
{
	if (allocator->size + allocator->deleted < CAST(Float32, allocator->capacity) * RESIZE_THRESHOLD) {
		return true;
	}

	Size capacity = allocator->capacity * 2;
	void **items = CAST(void**, calloc(capacity, sizeof *items));
	if (!items) {
		return false;
	}

	void **old_items = allocator->items;
	Size old_capacity = allocator->capacity;

	allocator->capacity = capacity;
	allocator->items = items;
	allocator->deleted = 0;
	allocator->size = 0;

	for (Size i = 0; i < old_capacity; i++) {
		// NOTE(dweiler): This cannot fail since the capacity is strictly greater.
		std_allocator_add_unlocked(allocator, old_items[i]);
	}

	free(old_items);

	return true;
}

static Bool std_allocator_add_unlocked(StandardAllocator *allocator, void *item)
	THREAD_REQUIRES(allocator->mutex)
{
	Uint64 hash = RCAST(Uint64, item); // TODO(dweiler): MixInt
	const Size mask = allocator->capacity - 1;

	Size index = (PRIME1 * hash) & mask;

	for (;;) {
		void *element = allocator->items[index];
		if (element && element != TOMBSTONE) {
			if (element == item) {
				return false;
			} else {
				index = (index + PRIME2) & mask;
			}
		} else {
			break;
		}
	}

	allocator->size++;
	allocator->items[index] = item;

	return std_allocator_maybe_rehash_unlocked(allocator);
}

static Bool std_allocator_add(StandardAllocator *allocator, void *item) {
	if (!item || item == TOMBSTONE) {
		return false;
	}

	mutex_lock(&allocator->mutex);
	const Bool result = std_allocator_add_unlocked(allocator, item);
	mutex_unlock(&allocator->mutex);
	return result;
}

static Bool std_allocator_remove(StandardAllocator *allocator, void *item) {
	Uint64 hash = RCAST(Uint64, item); // TODO(dweiler): MixInt

	mutex_lock(&allocator->mutex);

	const Size mask = allocator->capacity - 1;
	Size index = mask & (PRIME1 * hash);

	for (;;) {
		void *element = allocator->items[index];
		if (element) {
			if (element == item) {
				allocator->items[index] = TOMBSTONE;
				allocator->size--;
				allocator->deleted++;
				mutex_unlock(&allocator->mutex);
				return true;
			} else {
				index = mask & (index + PRIME2);
			}
		} else {
			break;
		}
	}

	mutex_unlock(&allocator->mutex);
	return false;
}

static Ptr std_allocator_allocate(Allocator *allocator, Size bytes) {
	void *data = malloc(bytes);
	if (!data) {
		return 0;
	}
	
	if (!std_allocator_add(CAST(StandardAllocator*, allocator->user), data)) {
		free(data);
		return 0;
	}
	
	return data;
}

static Ptr std_allocator_reallocate(Allocator *allocator, void *data, Size old_size, Size new_size) {
	if (!data) {
		return 0;
	}

	if (new_size <= old_size) {
		return data;
	}

	StandardAllocator *std_allocator = CAST(StandardAllocator*, allocator->user);
	std_allocator_remove(std_allocator, data);

	void *resize = realloc(data, new_size);
	if (!resize) {
		return 0;
	}

	if (!std_allocator_add(std_allocator, resize)) {
		free(resize);
		return 0;
	}

	return resize;
}

static void std_allocator_deallocate(Allocator *allocator, void *data) {
	if (!data) {
		return;
	}
	std_allocator_remove(CAST(StandardAllocator*, allocator->user), data);
	free(data);
}

const AllocatorOperations ALLOCATOR_STD = {
	SLIT("std"),
	std_allocator_init,
	std_allocator_fini,
	std_allocator_allocate,
	std_allocator_reallocate,
	std_allocator_deallocate,
};