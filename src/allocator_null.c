#include "allocator.h"

static Bool allocator_null_init(Allocator *allocator) {
	(void)allocator;
	// Does nothing.
	return true;
}

static void allocator_null_fini(Allocator *allocator) {
	(void)allocator;
	// Does nothing.
}

static Ptr allocator_null_allocate(Allocator *allocator, Size bytes) {
	(void)allocator;
	(void)bytes;
	// Does nothing.
	return 0;
}

static Ptr allocator_null_reallocate(Allocator *allocator, void *data, Size old_size, Size new_size) {
	(void)allocator;
	(void)data;
	(void)old_size;
	(void)new_size;
	// Does nothing.
	return 0;
}

static void allocator_null_deallocate(Allocator *allocator, void *data) {
	(void)allocator;
	(void)data;
	// Does nothing.
}

const AllocatorOperations ALLOCATOR_NULL = {
	SLIT("null"),
	allocator_null_init,
	allocator_null_fini,
	allocator_null_allocate,
	allocator_null_reallocate,
	allocator_null_deallocate,
};