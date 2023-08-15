#include "context.h"
#include "allocator.h"

void context_init(Context *context, String allocator) {
	allocator_init(&context->allocator, allocator);
	profiler_init(&context->profiler);
}

void context_fini(Context *context) {
	profiler_fini(&context->profiler);
	allocator_fini(&context->allocator);
}

void context_copy(Context *dst, const Context *src) {
	profiler_init(&dst->profiler);
	dst->allocator = src->allocator;
}