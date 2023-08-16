#include <stdlib.h>

#include "array.h"
#include "context.h"
#include "allocator.h"

Ptr array_create(Context *context) {
	Allocator *allocator = &context->allocator;
	Array *array = allocator_allocate(allocator, sizeof *array);
	if (!array) {
		THROW(ERROR_OOM);
	}
	array->context = context;
	array->size = 0;
	array->capacity = 0;
	return array + 1;
}

void array_grow(void **const array, Size elements, Size type_size) {
	ASSERT(*array);
	Array *meta = array_meta(*array);
	Context *const context = meta->context;
	PROF_ENTER();
	Allocator *const allocator = &context->allocator;
	const Size old_capacity = meta->capacity;
	const Size new_capacity = ((old_capacity + elements) * 3) / 2;
	const Size old_size = old_capacity * type_size + sizeof *meta;
	const Size new_size = new_capacity * type_size + sizeof *meta;
	void *data = allocator_reallocate(allocator, meta, old_size, new_size);
	if (!data) {
		THROW(ERROR_OOM);
	}
	meta = CAST(Array *, data);
	meta->capacity = new_capacity;
	*array = meta + 1;
	PROF_LEAVE();
}

void array_delete(void *const array) {
	ASSERT(array);
	Array *const meta = array_meta(array);
	Context *const context = meta->context;
	Allocator *const allocator = &context->allocator;
	allocator_deallocate(allocator, meta);
}