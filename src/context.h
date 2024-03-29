#ifndef CODIN_CONTEXT_H
#define CODIN_CONTEXT_H
#include <setjmp.h> // jmp_buf, setjmp, longjmp

#include "allocator.h"
#include "profiler.h"

typedef struct Context Context;

enum Error {
	ERROR_LEX,
	ERROR_PARSE,
	ERROR_BUILD,
	ERROR_OOM,
	ERROR_UNKNOWN,
};

typedef enum Error Error;

#define THROW(error) \
	longjmp((context)->jmp, CAST(int, (error)))

struct ALIGN(16) Context {
	Allocator allocator;
	Profiler profiler;
	jmp_buf jmp;
};

void context_init(Context *context, String allocator, String profiler);
void context_fini(Context *context);

#endif // CODIN_CONTEXT_H