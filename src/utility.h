#ifndef CODIN_UTILITY_H
#define CODIN_UTILITY_H
#include "array.h"
#include "string.h"

typedef Uint16 Float16;
typedef float Float32;
typedef double Float64;

Array(Uint8) readfile(String filename);

Float16 f32_to_f16(Float32 x);

#endif // CODIN_UTILITY_H