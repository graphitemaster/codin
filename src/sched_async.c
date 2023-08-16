#include "sched.h"
#include "context.h"
#include "threadpool.h"
#include "allocator.h"

typedef struct SchedAsync SchedAsync;
typedef struct SchedAsyncWork SchedAsyncWork;

struct SchedAsync {
	Context *context;
	Mutex mutex;
	ThreadPool pool THREAD_GUARDED(mutex);
	Cond cond       THREAD_GUARDED(mutex);
	Size count      THREAD_GUARDED(mutex);
};

struct SchedAsyncWork {
	SchedAsync *sched;
	void *data;
	void (*func)(void *data, Context *context);
	void (*dispose)(void *data, Context *context);
};

static void _sched_async_work_func(void *data, Context *context) {
	SchedAsyncWork *work = RCAST(SchedAsyncWork *, data);
	if (!setjmp(context->jmp)) {
		work->func(work->data, context);
	}
	if (work->dispose) {
		work->dispose(work->data, context);
	}
	SchedAsync *sched = work->sched;
	mutex_lock(&sched->mutex);
	sched->count--;
	cond_signal(&sched->cond);
	mutex_unlock(&sched->mutex);
}

static void _sched_async_work_dispose(void *data, Context *context) {
	Allocator *allocator = &context->allocator;
	allocator_deallocate(allocator, data);
}

Bool sched_async_init(Context *context, void **instance)
	THREAD_INTERNAL
{
	Allocator *allocator = &context->allocator;
	SchedAsync *sched = allocator_allocate(allocator, sizeof *sched);
	if (!sched) {
		THROW(ERROR_OOM);
	}
	if (!threadpool_init(&sched->pool, 4, context)) {
		allocator_deallocate(allocator, sched);
		return false;
	}
	sched->context = context;
	mutex_init(&sched->mutex);
	cond_init(&sched->cond);
	sched->count = 0;
	*instance = RCAST(void *, sched);
	return true;
}

static void sched_async_fini(void *ctx) {
	SchedAsync *sched = CAST(SchedAsync *, ctx);
	mutex_lock(&sched->mutex);
	ASSERT(sched->count == 0);
	mutex_unlock(&sched->mutex);
	Context *context = sched->context;
	threadpool_fini(&sched->pool);
	mutex_fini(&sched->mutex);
	cond_fini(&sched->cond);
	Allocator *allocator = &context->allocator;
	allocator_deallocate(allocator, sched);
}

static Bool sched_async_queue(void *ctx, void *data, void (*func)(void *data, Context *context), void (*dispose)(void *data, Context *context)) {
	SchedAsync *const sched = CAST(SchedAsync *, ctx);
	Context *const context = sched->context;
	Allocator *const allocator = &context->allocator;
	SchedAsyncWork *const work = allocator_allocate(allocator, sizeof *work);
	if (!work) {
		THROW(ERROR_OOM);
	}

	work->sched = sched;
	work->data = data;
	work->func = func;
	work->dispose = dispose;

	mutex_lock(&sched->mutex);
	sched->count++;
	mutex_unlock(&sched->mutex);

	threadpool_queue(
		&sched->pool,
		_sched_async_work_func,
		work,
		_sched_async_work_dispose);

	return true;
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

