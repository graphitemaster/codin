#ifndef CODIN_PROFILER_H
#define CODIN_PROFILER_H

typedef struct Profiler Profiler;

struct Profiler {
	int tid;
};

void profiler_init(Profiler *profiler);
void profiler_fini(Profiler *profiler);
void profiler_enter(Profiler *profiler, const char *file, int line, const char *function);
void profiler_leave(Profiler *profiler);

#define PROF_ENTER() prof_enter(&(context)->profiler, __FILE__, __LINE__, __FUNCTION__)
#define PROF_LEAVE() prof_leave(&(context)->profiler)

#endif