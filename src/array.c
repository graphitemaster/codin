#include <stdlib.h>

#include "array.h"
#include "context.h"

Ptr array_create(Context *context) {
	Allocator *allocator = context->allocator;
	void *data = allocator->allocate(allocator, sizeof(Array));
	if (!data) {
		return 0;
	}
	Array *array = CAST(Array *, data);
	array->allocator = allocator;
	array->size = 0;
	array->capacity = 0;
	return array + 1;
}

Bool array_grow(void **const array, Size elements, Size type_size) {
	ASSERT(*array);
	Array *meta = array_meta(*array);
	Allocator *const allocator = meta->allocator;
	const Size count = 2 * meta->capacity + elements;
	void *data = allocator->reallocate(allocator, meta, type_size * count + sizeof *meta);
	if (!data) {
		allocator->deallocate(allocator, meta);
		return false;
	}
	meta = CAST(Array *, data);
	meta->capacity = count;
	*array = meta + 1;
	return true;
}

void array_delete(void *const array) {
	ASSERT(array);
	Array *const meta = array_meta(array);
	Allocator *const allocator = meta->allocator;
	allocator->deallocate(allocator, meta);
}