
#include "sched.h"
#include "array.h"
#include "context.h"

typedef struct SchedSync SchedSync;

struct SchedSync {
	Context *context;
	Array(SchedWork) work;
};

static Bool sched_sync_init(Context *context, void **instance) {
	Allocator *allocator = context->allocator;
	SchedSync *sched = RCAST(SchedSync *, allocator->allocate(allocator, sizeof *sched));
	if (!sched) {
		return false;
	}
	sched->context = context;
	sched->work = 0;
	*instance = RCAST(void *, sched);
	return true;
}

static void sched_sync_fini(void *ctx) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	ASSERT(array_size(sched->work) == 0);
	Context *context = sched->context;
	array_free(sched->work);
	Allocator *allocator = context->allocator;
	allocator->deallocate(allocator, sched);
}

static Bool sched_sync_queue(void *ctx, void *data, void (*func)(void*), void (*dispose)(void*)) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	Context *context = sched->context;
	return array_push(sched->work, LIT(SchedWork, ctx, data, func, dispose));
}

static void sched_sync_wait(void *ctx) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	const Size n_work = array_size(sched->work);
	for (Size i = 0; i < n_work; i++) {
		const SchedWork *work = &sched->work[i];
		work->func(work->data);
		if (work->dispose) {
			work->dispose(work->data);
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