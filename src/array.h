#ifndef CODIN_ARRAY_H
#define CODIN_ARRAY_H
#include <stdalign.h>
#include <string.h> // memmove
#include "support.h"

typedef struct Array Array;

struct Array {
	alignas(16) Uint64 capacity;
	Uint64 size;
};
_Static_assert(alignof(Array) == 16, "not aligned");

#define Array(T) T*

#define array_meta(array) \
	(&((Array*)(array))[-1])

#define array_try_grow(array, size_) \
	(((array) && array_meta(array)->size + (size_) < array_meta(array)->capacity) \
		? true \
		: array_grow((void **)&(array), (size_), sizeof *(array)))

#define array_size(array) \
	((array) ? array_meta(array)->size : 0)

#define array_expand(array, size_) \
	(array_try_grow((array), (size_)) \
		? (array_meta(array)->size += (size_), true) \
		: false)

#define array_push(array, value) \
	(array_try_grow((array), 1) \
		? ((array)[array_meta(array)->size++] = (value), true) \
		: false)

#define array_free(array) \
	(void)((array) ? (array_delete(array), (array) = 0) : 0)

#define array_insert(array, index, value) \
	(array_expand(array, 1) \
		? (memmove(&(array)[index+1], &(array)[index], (array_size(array) - (index) - 1) * sizeof *(array)), (array)[index] = (value), true) \
		: false)

#define array_resize(array, size_) \
	((array) \
		? (array_meta(array)->size >= (size_) \
			? (array_meta(array)->size = (size_), true) \
			: array_expand((array), (size_) - array_meta(array)->size)) \
		: (array_grow((void **)&(array), (size_), sizeof *(array)) \
			? (array_meta(array)->size = (size_), true) \
			: false))

#define array_clear(array) \
	(void)((array) ? array_meta(array)->size = 0 : 0)

Bool array_grow(void **const array, Uint64 elements, Uint64 type_size);
void array_delete(void *const array);

#endif // CODIN_ARRAY_H