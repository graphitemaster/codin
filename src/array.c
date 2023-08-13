#include <stdlib.h>

#include "array.h"
#include "context.h"

Ptr array_create(Context *context) {
	Allocator *allocator = context->allocator;
	void *data = allocator->allocate(allocator, sizeof(Array));
	if (!data) {
		THROW(ERROR_OOM);
	}
	Array *array = CAST(Array *, data);
	array->context = context;
	array->size = 0;
	array->capacity = 0;
	return array + 1;
}

void array_grow(void **const array, Size elements, Size type_size) {
	ASSERT(*array);
	Array *meta = array_meta(*array);
	Context *const context = meta->context;
	Allocator *const allocator = context->allocator;
	const Size count = 2 * meta->capacity + elements;
	void *data = allocator->reallocate(allocator, meta, type_size * count + sizeof *meta);
	if (!data) {
		THROW(ERROR_OOM);
	}
	meta = CAST(Array *, data);
	meta->capacity = count;
	*array = meta + 1;
}

void array_delete(void *const array) {
	ASSERT(array);
	Array *const meta = array_meta(array);
	Context *const context = meta->context;
	Allocator *const allocator = context->allocator;
	allocator->deallocate(allocator, meta);
}