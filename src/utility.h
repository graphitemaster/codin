#ifndef CODIN_UTILITY_H
#define CODIN_UTILITY_H
#include "array.h"
#include "string.h"

typedef struct Context Context;

Array(Uint8) _readfile(String filename, Context *context);

#define readfile(filename) \
	_readfile((filename), context)

Float16 f32_to_f16(Float32 x);

#endif // CODIN_UTILITY_H