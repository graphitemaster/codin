#include "context.h"
#include "allocator.h"

void context_init(Context *context, String allocator, String profiler) {
	allocator_init(&context->allocator, allocator);
	profiler_init(&context->profiler, profiler, context);
}

void context_fini(Context *context) {
	profiler_fini(&context->profiler);
	allocator_fini(&context->allocator);
}