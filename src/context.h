#ifndef CODIN_CONTEXT_H
#define CODIN_CONTEXT_H
#include <setjmp.h>

#include "allocator.h"

typedef struct Context Context;

enum Error {
	ERROR_LEX,
	ERROR_PARSE,
	ERROR_OOM,
	ERROR_UNKNOWN,
};

typedef enum Error Error;

#define THROW(error) \
	longjmp((context)->jmp, CAST(int, (error)))

struct Context {
	Allocator allocator;
	jmp_buf jmp;
};

#endif // CODIN_CONTEXT_H