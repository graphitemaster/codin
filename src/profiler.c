#include "profiler.h"

extern const ProfilerOperations PROFILER_NULL;
extern const ProfilerOperations PROFILER_SPALL;

void profiler_init(Profiler *profiler, String name, Context *context) {
	if (string_compare(name, PROFILER_SPALL.name)) {
		profiler->ops = &PROFILER_SPALL;
	} else {
		profiler->ops = &PROFILER_NULL;
	}
	profiler->instance = profiler->ops->init(context);
}

void profiler_fini(Profiler *profiler) {
	profiler->ops->fini(profiler->instance);
}

void profiler_enter(Profiler *profiler, String file, int line, String function) {
	profiler->ops->enter(profiler->instance, file, line, function);
}

void profiler_leave(Profiler *profiler) {
	profiler->ops->leave(profiler->instance);
}