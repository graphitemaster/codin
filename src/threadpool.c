#include "threadpool.h"
#include "context.h"

struct ThreadPoolWork {
	void (*function)(void *user, Context *context);
	void *user;
	void (*dispose)(void *user, Context *context);
};

static int threadpool_worker(void *user) {
	ThreadPool *pool = CAST(ThreadPool*, user);

	const String allocator = pool->context->allocator.ops->name;
	const String profiler = pool->context->profiler.ops->name;

	Context ctx;
	Context *context = &ctx;
	context_init(context, allocator, profiler);

	PROF_ENTER();

	for (;;) {
		mutex_lock(&pool->mutex);
		while (!pool->quit && array_size(pool->work) == 0) {
			cond_wait(&pool->cond, &pool->mutex);
		}

		if (pool->quit && array_size(pool->work) == 0) {
			mutex_unlock(&pool->mutex);
			break;
		}
	
		Array *meta = array_meta(pool->work);
		ThreadPoolWork work = pool->work[--meta->size];
		mutex_unlock(&pool->mutex);

		work.function(work.user, context);

		if (work.dispose) {
			work.dispose(work.user, context);
		}
	}

	PROF_LEAVE();

	context_fini(context);

	return 0;
}

Bool threadpool_init(ThreadPool *pool, Size n_threads, Context *context)
	THREAD_INTERNAL
{
	cond_init(&pool->cond);
	mutex_init(&pool->mutex);

	pool->context = context;
	pool->threads = array_make(context);
	pool->work = array_make(context);
	pool->quit = false;

	array_resize(pool->threads, n_threads);
	for (Size i = 0; i < n_threads; i++) {
		thread_create(&pool->threads[i], threadpool_worker, pool, context);
	}
	return true;
}

void threadpool_fini(ThreadPool *pool) {
	Context *const context = pool->context;

	PROF_ENTER();

	mutex_lock(&pool->mutex);
	pool->quit = true;
	cond_broadcast(&pool->cond);
	mutex_unlock(&pool->mutex);

	const Size n_threads = array_size(pool->threads);
	for (Size i = 0; i < n_threads; i++) {
		thread_join(&pool->threads[i], context);
	}

	array_free(pool->threads);

	cond_fini(&pool->cond);
	mutex_fini(&pool->mutex);

	PROF_LEAVE();
}

void threadpool_queue(ThreadPool *pool, void (*function)(void*, Context*), void *user, void (*dispose)(void*, Context*)) {
	mutex_lock(&pool->mutex);
	const ThreadPoolWork work = LIT(ThreadPoolWork, function, user, dispose);
	array_push(pool->work, work);
	cond_signal(&pool->cond);
	mutex_unlock(&pool->mutex);
}