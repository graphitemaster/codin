#ifndef CODIN_PROFILER_H
#define CODIN_PROFILER_H
#include "string.h"

typedef struct Context Context;

typedef struct Profiler Profiler;
typedef struct ProfilerOperations ProfilerOperations;

struct ProfilerOperations {
	String name;
	void *(*init)(Context *);
	void (*fini)(void*);
	void (*enter)(void *, String file, int line, String function);
	void (*leave)(void *);
};

struct Profiler {
	const ProfilerOperations *ops;
	void *instance;
};

void profiler_init(Profiler *profiler, String name, Context *context);
void profiler_fini(Profiler *profiler);
void profiler_enter(Profiler *profiler, String, int line, String function);
void profiler_leave(Profiler *profiler);

#define PROF_ENTER() profiler_enter(&(context)->profiler, SCLIT(__FILE__), __LINE__, SCLIT(__FUNCTION__))
#define PROF_LEAVE() profiler_leave(&(context)->profiler)

#endif