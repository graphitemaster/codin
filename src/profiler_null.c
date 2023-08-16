#include "profiler.h"

static void *profiler_null_init(Context *context) {
	(void)context;
	return 0;
}

static void profiler_null_fini(void *ctx) {
	(void)ctx;
}

static void profiler_null_enter(void *ctx, String file, int line, String function) {
	(void)ctx;
	(void)file;
	(void)line;
	(void)function;
}

static void profiler_null_leave(void *ctx) {
	(void)ctx;
}

const ProfilerOperations PROFILER_NULL = {
	SLIT("null"),
	profiler_null_init,
	profiler_null_fini,
	profiler_null_enter,
	profiler_null_leave,
};