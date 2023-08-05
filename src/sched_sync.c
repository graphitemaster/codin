
#include "sched.h"
#include "array.h"
#include "context.h"

typedef struct SchedSync SchedSync;
typedef struct SchedSyncWork SchedSyncWork;

struct SchedSyncWork {
	void *ctx;
	void *data;
	void (*func)(void *data, Context *context);
	void (*dispose)(void *data, Context *context);
};

struct SchedSync {
	Context context;
	Array(SchedSyncWork) work;
};

static Bool sched_sync_init(Context *context, void **instance) {
	Allocator *allocator = context->allocator;
	SchedSync *sched = RCAST(SchedSync *, allocator->allocate(allocator, sizeof *sched));
	if (!sched) {
		return false;
	}
	sched->context.allocator = context->allocator;
	sched->work = 0;
	*instance = RCAST(void *, sched);
	return true;
}

static void sched_sync_fini(void *ctx) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	Context *context = &sched->context;
	ASSERT(array_size(sched->work) == 0);
	array_free(sched->work);
	Allocator *allocator = context->allocator;
	allocator->deallocate(allocator, sched);
}

static Bool sched_sync_queue(void *ctx, void *data, void (*func)(void *data, Context *context), void (*dispose)(void *data, Context *context)) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	Context *context = &sched->context;
	return array_push(sched->work, LIT(SchedSyncWork, ctx, data, func, dispose));
}

#include <stdio.h>

static void sched_sync_wait(void *ctx) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	const Size n_work = array_size(sched->work);
	for (Size i = 0; i < n_work; i++) {
		const SchedSyncWork *work = &sched->work[i];
		if (!setjmp(sched->context.jmp)) {
			work->func(work->data, &sched->context);
		}
	}
	for (Size i = 0; i < n_work; i++) {
		const SchedSyncWork *work = &sched->work[i];
		if (work->dispose) {
			work->dispose(work->data, &sched->context);
		}
	}
	array_clear(sched->work);
}

const SchedOperations SCHED_SYNC = {
	SLIT("sync"),
	sched_sync_init,
	sched_sync_fini,
	sched_sync_queue,
	sched_sync_wait,
};