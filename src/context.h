#include <setjmp.h>
#include "support.h"

typedef struct Context Context;
typedef struct Allocator Allocator;

struct Allocator {
	void *(*allocate)(Allocator *allocator, Size bytes);
	void *(*reallocate)(Allocator *allocator, void *ptr, Size bytes);
	void (*deallocate)(Allocator *allocator, void *ptr);
	void (*finalize)(Allocator *allocator);
	void *user;
};

#define THROW(error) \
	longjmp(context->jmp, (error))

extern Allocator DEFAULT_ALLOCATOR;

struct Context {
	Allocator *allocator;
	jmp_buf jmp;
};