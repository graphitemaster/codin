#include <stdlib.h>
#include <string.h>

#include "context.h"
#include "thread.h"

static const Size DEFAULT_CAPACITY = 4096;
static const Float32 RESIZE_THRESHOLD = 0.75;
static const Uint32 PRIME1 = 73;
static const Uint32 PRIME2 = 5009;
#define TOMBSTONE RCAST(void *, 1)

typedef struct DefaultAllocator DefaultAllocator;

struct DefaultAllocator {
	Size size;
	Size deleted;
	Size capacity;
	void **items;
	Mutex mutex;
};

static DefaultAllocator *create_default_allocator(Size capacity) {
	DefaultAllocator *allocator = CAST(DefaultAllocator*, calloc(1, sizeof *allocator));
	if (!allocator) {
		return 0;
	}

	allocator->size = 0;
	allocator->capacity = capacity;
	allocator->items = CAST(void**, calloc(capacity, sizeof *allocator->items));
	if (!allocator->items) {
		free(allocator);
		return 0;
	}

	mutex_init(&allocator->mutex);

	return allocator;
}

static void destroy_default_allocator(DefaultAllocator *allocator) {
	const Size capacity = allocator->capacity;
	for (Size i = 0; i < capacity; i++) {
		void *item = allocator->items[i];
		if (item == TOMBSTONE) {
			continue;
		}
		free(item);
		allocator->items[i] = TOMBSTONE;
		allocator->deleted++;
	}
	free(allocator->items);

	mutex_destroy(&allocator->mutex);

	free(allocator);
}

static Bool default_allocator_add_unlocked(DefaultAllocator *allocator, void *item);
static Bool default_allocator_maybe_rehash_unlocked(DefaultAllocator *allocator) {
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
		default_allocator_add_unlocked(allocator, old_items[i]);
	}

	return true;
}

static Bool default_allocator_add_unlocked(DefaultAllocator *allocator, void *item) {
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

	return default_allocator_maybe_rehash_unlocked(allocator);
}

static Bool default_allocator_add(DefaultAllocator *allocator, void *item) {
	if (!item || item == TOMBSTONE) {
		return false;
	}

	mutex_lock(&allocator->mutex);
	const Bool result = default_allocator_add_unlocked(allocator, item);
	mutex_unlock(&allocator->mutex);
	return result;
}

static Bool default_allocator_remove(DefaultAllocator *allocator, void *item) {
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

static Bool ensure_initialized(Allocator *allocator) {
	if (allocator->user) {
		return true;
	}

	allocator->user = create_default_allocator(DEFAULT_CAPACITY);
	if (allocator->user) {
		return true;
	}

	return false;
}

static void *default_allocator_allocate(Allocator *allocator, Size bytes) {
	(void)allocator;
	void *ptr = malloc(bytes);
	if (ptr) {
		if (!ensure_initialized(allocator) ||
		    !default_allocator_add(CAST(DefaultAllocator*, allocator->user), ptr))
		{
			free(ptr);
			return 0;
		}
		return ptr;
	}
	return 0;
}

static void *default_allocator_reallocate(Allocator *allocator, void *ptr, Size bytes) {
	if (!ptr) {
		return 0;
	}

	DefaultAllocator *default_allocator = CAST(DefaultAllocator*, allocator->user);

	default_allocator_remove(default_allocator, ptr);

	void *resize = realloc(ptr, bytes);
	if (!resize) {
		return 0;
	}

	if (!default_allocator_add(default_allocator, resize)) {
		free(resize);
		return 0;
	}

	return resize;
}

static void default_allocator_deallocate(Allocator *allocator, void *ptr) {
	if (ptr) {
		default_allocator_remove(CAST(DefaultAllocator*, allocator->user), ptr);
		free(ptr);
	}
}
static void default_allocator_finalize(Allocator *allocator) {
	destroy_default_allocator(CAST(DefaultAllocator*, allocator->user));
}

Allocator DEFAULT_ALLOCATOR = {
	&default_allocator_allocate,
	&default_allocator_reallocate,
	&default_allocator_deallocate,
	&default_allocator_finalize,
	0,
};