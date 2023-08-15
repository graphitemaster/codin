#ifndef CODIN_ALLOCATOR_H
#define CODIN_ALLOCATOR_H
#include "string.h"

typedef struct Allocator Allocator;
typedef struct AllocatorOperations AllocatorOperations;

struct AllocatorOperations {
	String name;
	Bool (*init)(Allocator *);
	void (*fini)(Allocator *);
	Ptr (*allocate)(Allocator *allocator, Size bytes);
	Ptr (*reallocate)(Allocator *allocator, void *data, Size old_size, Size new_size);
	void (*deallocate)(Allocator *allocator, void *data);
};

struct Allocator {
	const AllocatorOperations *ops;
	void *user;
};

Bool allocator_init(Allocator *allocator, const String name);
void allocator_fini(Allocator *allocator);

Ptr allocator_allocate(Allocator *allocator, Size bytes);
Ptr allocator_reallocate(Allocator *allocator, void *data, Size old_size, Size new_size);
void allocator_deallocate(Allocator *allocator, void *data);

#endif // CODIN_ALLOCATOR_H