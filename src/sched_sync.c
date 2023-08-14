
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
	Context context;
	Array(SchedSyncWork) work;
};

static Bool sched_sync_init(Context *context, void **instance) {
	Allocator *allocator = &context->allocator;
	SchedSync *sched = allocator_allocate(allocator, sizeof *sched);
	if (!sched) {
		THROW(ERROR_OOM);
	}
	sched->context.allocator = context->allocator;
	sched->work = array_make(context);
	*instance = RCAST(void *, sched);
	return true;
}

static void sched_sync_fini(void *ctx) {
	SchedSync *sched = CAST(SchedSync *, ctx);
	Context *context = &sched->context;
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
	SchedSync *sched = CAST(SchedSync *, ctx);
	Size n_work = array_size(sched->work);
	// NOTE(dweiler): Needs to be volatile because of setjmp
	volatile Size i = 0;
	for (i = 0; i < n_work; i++) {
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