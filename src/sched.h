#ifndef CODIN_SCHED_H
#define CODIN_SCHED_H
#include "string.h"

typedef struct Context Context;

typedef struct Sched Sched;
typedef struct SchedOperations SchedOperations;

struct Sched {
	const SchedOperations *operations;
	void *instance;
};

struct SchedOperations {
	String name;
	Bool (*init)(Context *context, void **instance);
	void (*fini)(void *ctx);
	Bool (*queue)(void *ctx, void *data, void (*func)(void *data, Context *context), void (*dispose)(void *data, Context *context));
	void (*wait)(void *ctx);
};

Bool sched_init(Sched *sched, String name, Context *context);
void sched_fini(Sched *sched);
void sched_queue(Sched *sched, void *data, void (*func)(void *data, Context *context), void (*dispose)(void *data, Context *context));
void sched_wait(Sched *sched);

#endif // CODIN_SCHEDULER_H