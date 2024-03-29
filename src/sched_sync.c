
#include "sched.h"
#include "array.h"
#include "context.h"
#include "allocator.h"

typedef struct SchedSync SchedSync;
typedef struct SchedSyncWork SchedSyncWork;

struct SchedSyncWork {
	void *ctx;
	void *data;
	void (*func)(void *data, Context *context);
	void (*dispose)(void *data, Context *context);
};

struct SchedSync {
	Context *context;
	Array(SchedSyncWork) work;
};

static Bool sched_sync_init(Context *context, void **instance) {
	Allocator *const allocator = &context->allocator;
	SchedSync *const sched = allocator_allocate(allocator, sizeof *sched);
	if (!sched) {
		THROW(ERROR_OOM);
	}
	sched->context = context;
	sched->work = array_make(context);
	*instance = RCAST(void *, sched);
	return true;
}

static void sched_sync_fini(void *ctx) {
	SchedSync *const sched = CAST(SchedSync *, ctx);
	Context *const context = sched->context;
	ASSERT(array_size(sched->work) == 0);
	array_free(sched->work);
	Allocator *allocator = &context->allocator;
	allocator_deallocate(allocator, sched);
}

static Bool sched_sync_queue(void *ctx, void *data, void (*func)(void *data, Context *context), void (*dispose)(void *data, Context *context)) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	return array_push(sched->work, LIT(SchedSyncWork, ctx, data, func, dispose));
}

static void sched_sync_wait(void *ctx) {
	SchedSync *const sched = CAST(SchedSync *, ctx);
	Context *const context = sched->context;
	PROF_ENTER();
	for (volatile Size i = 0; i < array_size(sched->work); i++) {
		const SchedSyncWork *work = &sched->work[i];
		if (!setjmp(sched->context->jmp)) {
			work->func(work->data, context);
		}
	}
	const Size n_work = array_size(sched->work);
	for (Size i = 0; i < n_work; i++) {
		const SchedSyncWork *work = &sched->work[i];
		if (work->dispose) {
			work->dispose(work->data, context);
		}
	}
	array_clear(sched->work);
	PROF_LEAVE();
}

const SchedOperations SCHED_SYNC = {
	SLIT("sync"),
	sched_sync_init,
	sched_sync_fini,
	sched_sync_queue,
	sched_sync_wait,
};