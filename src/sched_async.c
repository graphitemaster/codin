#include "sched.h"
#include "context.h"
#include "threadpool.h"

typedef struct SchedAsync SchedAsync;

struct SchedAsync {
	Context *context;
	ThreadPool pool;
	Size count;
	Mutex mutex;
	Cond cond;
};

static void _sched_async_work_func(void *data) {
	SchedWork *work = RCAST(SchedWork *, data);
	SchedAsync *sched = RCAST(SchedAsync *, work->ctx);
	work->func(work->data);
	if (work->dispose) {
		work->dispose(work->data);
	}
	mutex_lock(&sched->mutex);
	sched->count--;
	cond_signal(&sched->cond);
	mutex_unlock(&sched->mutex);
}

static void _sched_async_work_dispose(void *data) {
	SchedWork *work = RCAST(SchedWork *, data);
	SchedAsync *sched = RCAST(SchedAsync *, work->ctx);
	Allocator *allocator = sched->context->allocator;
	allocator->deallocate(allocator, data);
}

Bool sched_async_init(Context *context, void **instance) {
	Allocator *allocator = context->allocator;
	SchedAsync *sched = RCAST(SchedAsync *, allocator->allocate(allocator, sizeof *sched));
	if (!sched) {
		return false;
	}
	if (!threadpool_init(&sched->pool, 4, context)) {
		allocator->deallocate(allocator, sched);
		return false;
	}
	sched->context = context;
	sched->count = 0;
	mutex_init(&sched->mutex);
	cond_init(&sched->cond);
	*instance = RCAST(void *, sched);
	return true;
}

static void sched_async_fini(void *ctx) {
	SchedAsync *sched = CAST(SchedAsync *, ctx);
	ASSERT(sched->count == 0);
	Context *context = sched->context;
	threadpool_free(&sched->pool);
	mutex_destroy(&sched->mutex);
	cond_destroy(&sched->cond);
	Allocator *allocator = context->allocator;
	allocator->deallocate(allocator, sched);
}

static Bool sched_async_queue(void *ctx, void *data, void (*func)(void*), void (*dispose)(void*)) {
	SchedAsync *sched = CAST(SchedAsync *, ctx);
	Context *context = sched->context;
	Allocator *allocator = context->allocator;
	SchedWork *work = allocator->allocate(allocator, sizeof *work);
	if (!work) {
		return false;
	}
	*work = LIT(SchedWork, ctx, data, func, dispose);
	mutex_lock(&sched->mutex);
	sched->count++;
	const Bool result = threadpool_queue(
		&sched->pool,
		_sched_async_work_func,
		work,
		_sched_async_work_dispose);
	mutex_unlock(&sched->mutex);
	return result;
}

static void sched_async_wait(void *ctx) {
	SchedAsync *sched = CAST(SchedAsync *, ctx);
	mutex_lock(&sched->mutex);
	while (sched->count) {
		cond_wait(&sched->cond, &sched->mutex);
	}
	mutex_unlock(&sched->mutex);
}

const SchedOperations SCHED_ASYNC = {
	SLIT("async"),
	sched_async_init,
	sched_async_fini,
	sched_async_queue,
	sched_async_wait,
};

