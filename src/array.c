#include <stdlib.h>

#include "array.h"

Bool array_grow(void **const array, Uint64 elements, Uint64 type_size) {
	Uint64 count = 0;
	void *data = 0;
	if (*array) {
		Array *const meta = array_meta(*array);
		count = 2 * meta->capacity + elements;
		data = realloc(meta, type_size * count + sizeof *meta);
		if (!data) {
			free(meta);
			return false;
		}
	} else {
		count = elements + 1;
		data = malloc(type_size * count + sizeof(Array));
		if (!data) {
			return false;
		}
		((Array *)data)->size = 0;
	}
	Array *meta = (Array *)data;
	meta->capacity = count;
	*array = meta + 1;
	return true;
}

void array_delete(void *const array) {
	free(array_meta(array));
}