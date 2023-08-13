#ifndef CODIN_CONTEXT_H
#define CODIN_CONTEXT_H
#include <setjmp.h>

#include "support.h"

typedef struct Context Context;
typedef struct Allocator Allocator;

struct Allocator {
	Ptr (*allocate)(Allocator *allocator, Size bytes);
	Ptr (*reallocate)(Allocator *allocator, void *ptr, Size bytes);
	void (*deallocate)(Allocator *allocator, void *ptr);
	void (*finalize)(Allocator *allocator);
	void *user;
};

enum Error {
	ERROR_LEX,
	ERROR_PARSE,
	ERROR_OOM,
	ERROR_UNKNOWN,
};

typedef enum Error Error;

#define THROW(error) \
	longjmp((context)->jmp, CAST(int, (error)))

extern Allocator DEFAULT_ALLOCATOR;

struct Context {
	Allocator *allocator;
	jmp_buf jmp;
};

#endif // CODIN_CONTEXT_H