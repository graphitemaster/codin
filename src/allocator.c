#include "allocator.h"

extern const AllocatorOperations ALLOCATOR_STD;
extern const AllocatorOperations ALLOCATOR_ARENA;
extern const AllocatorOperations ALLOCATOR_NULL;

Bool allocator_init(Allocator *allocator, const String name) {
	if (string_compare(name, SCLIT("std"))) {
		allocator->ops = &ALLOCATOR_STD;
	} else if (string_compare(name, SCLIT("arena"))) {
		allocator->ops = &ALLOCATOR_ARENA;
	} else {
		allocator->ops = &ALLOCATOR_NULL;
	}

	allocator->user = 0;

	return allocator->ops->init(allocator);
}

void allocator_fini(Allocator *allocator) {
	allocator->ops->fini(allocator);
}

Ptr allocator_allocate(Allocator *allocator, Size bytes) {
	return allocator->ops->allocate(allocator, bytes);
}

Ptr allocator_reallocate(Allocator *allocator, void *data, Size old_size, Size new_size) {
	return allocator->ops->reallocate(allocator, data, old_size, new_size);
}

void allocator_deallocate(Allocator *allocator, void *data) {
	return allocator->ops->deallocate(allocator, data);
}
