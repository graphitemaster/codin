#ifndef CODIN_ARRAY_H
#define CODIN_ARRAY_H
#include <string.h> // memmove

#include "support.h"

typedef struct Context Context;
typedef struct Array Array;

struct ALIGN(16) Array {
	Size capacity;
	Size size;
};

#define Array(T) T*

#define array_meta(array) \
	(&RCAST(Array*, (array))[-1])

#define array_try_grow(array, size_) \
	(((array) && array_meta(array)->size + (size_) < array_meta(array)->capacity) \
		? true \
		: array_grow(context, RCAST(void **, &(array)), (size_), sizeof *(array)))

#define array_size(array) \
	((array) ? array_meta(array)->size : 0)

#define array_capacity(array) \
	((arrray) ? array_meta(array)->capacity : 0)

#define array_expand(array, size_) \
	(array_try_grow((array), (size_)) \
		? (array_meta(array)->size += (size_), true) \
		: false)

#define array_push(array, value) \
	(array_try_grow((array), 1) \
		? ((array)[array_meta(array)->size++] = (value), true) \
		: false)

#define array_free(array) \
	(void)((array) ? (array_delete(context, array), (array) = 0) : 0)

#define array_insert(array, index, value) \
	(array_expand(array, 1) \
		? (memmove(&(array)[index+1], &(array)[index], (array_size(array) - (index) - 1) * sizeof *(array)), (array)[index] = (value), true) \
		: false)

#define array_resize(array, size_) \
	((array) \
		? (array_meta(array)->size >= (size_) \
			? (array_meta(array)->size = (size_), true) \
			: array_expand((array), (size_) - array_meta(array)->size)) \
		: (array_grow(context, RCAST(void **, &(array)), (size_), sizeof *(array)) \
			? (array_meta(array)->size = (size_), true) \
			: false))

#define array_last(array) \
	((array)[array_size(array) - 1])

#define array_clear(array) \
	(void)((array) ? array_meta(array)->size = 0 : 0)

Bool array_grow(Context *context, void **const array, Size elements, Size type_size);
void array_delete(Context *context, void *const array);

#endif // CODIN_ARRAY_H