#include <stdlib.h>

#include "array.h"
#include "context.h"

Bool array_grow(Context *context, void **const array, Size elements, Size type_size) {
	Allocator *allocator = context->allocator;
	Size count = 0;
	void *data = 0;
	if (*array) {
		Array *const meta = array_meta(*array);
		count = 2 * meta->capacity + elements;
		data = allocator->reallocate(allocator, meta, type_size * count + sizeof *meta);
		if (!data) {
			allocator->deallocate(allocator, meta);
			return false;
		}
	} else {
		count = elements + 1;
		data = allocator->allocate(allocator, type_size * count + sizeof(Array));
		if (!data) {
			return false;
		}
		CAST(Array *, data)->size = 0;
	}
	Array *meta = CAST(Array *, data);
	meta->capacity = count;
	*array = meta + 1;
	return true;
}

void array_delete(Context *context, void *const array) {
	Allocator *allocator = context->allocator;
	allocator->deallocate(allocator, array_meta(array));
}