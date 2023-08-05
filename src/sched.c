#include "sched.h"

extern const SchedOperations SCHED_SYNC;
extern const SchedOperations SCHED_ASYNC;

Bool sched_init(Sched *sched, String name, Context *context) {
	const SchedOperations *operations = 0;
	if (string_compare(name, SCHED_SYNC.name)) {
		operations = &SCHED_SYNC;
	} else if (string_compare(name, SCHED_ASYNC.name)) {
		operations = &SCHED_ASYNC;
	}
	sched->operations = operations;
	return operations->init(context, &sched->instance);
}

void sched_fini(Sched *sched) {
	sched->operations->fini(sched->instance);
}

void sched_queue(Sched *sched, void *data, void (*work)(void *data, Context *context), void (*dispose)(void *data, Context *context)) {
	sched->operations->queue(sched->instance, data, work, dispose);
}

void sched_wait(Sched *sched) {
	sched->operations->wait(sched->instance);
}