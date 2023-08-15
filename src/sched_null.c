#include "sched.h"

Bool sched_null_init(Context *context, void **instance) {
	(void)context;
	(void)instance;
	// Does nothing.
	return true;
}

void sched_null_fini(void *ctx) {
	(void)ctx;
	// Does nothing.
}

Bool sched_null_queue(void *ctx, void *data, void (*func)(void *data, Context *context), void (*dispose)(void *data, Context *context)) {
	(void)ctx;
	(void)data;
	(void)func;
	(void)dispose;
	// Does nothing.
	return true;
}

void sched_null_wait(void *ctx) {
	(void)ctx;
	// Does nothing.
}

const SchedOperations SCHED_NULL = {
	SLIT("null"),
	sched_null_init,
	sched_null_fini,
	sched_null_queue,
	sched_null_wait,
};