#include "threadpool.h"

struct ThreadPoolWork {
	void (*function)(void *user);
	void *user;
	void (*dispose)(void *user);
};

static int threadpool_worker(void *user) {
	ThreadPool *pool = CAST(ThreadPool*, user);
	for (;;) {
		mutex_lock(&pool->mutex);
		while (!pool->quit && array_size(pool->work) == 0) {
			cond_wait(&pool->cond, &pool->mutex);
		}

		if (pool->quit && array_size(pool->work) == 0) {
			mutex_unlock(&pool->mutex);
			return 0;
		}
	
		Array *meta = array_meta(pool->work);
		ThreadPoolWork work = pool->work[--meta->size];
		mutex_unlock(&pool->mutex);

		work.function(work.user);

		if (work.dispose) {
			work.dispose(work.user);
		}
	}
}

Bool threadpool_init(ThreadPool *pool, Size n_threads, Context *context) {
	cond_init(&pool->cond);
	mutex_init(&pool->mutex);

	mutex_lock(&pool->mutex);
	pool->context = context;
	pool->threads = array_make(context);
	pool->work = array_make(context);
	pool->quit = false;

	array_resize(pool->threads, n_threads);
	for (Size i = 0; i < n_threads; i++) {
		thread_create(&pool->threads[i], threadpool_worker, pool);
	}
	mutex_unlock(&pool->mutex);

	return true;
}

void threadpool_fini(ThreadPool *pool) {
	mutex_lock(&pool->mutex);
	pool->quit = true;
	cond_broadcast(&pool->cond);
	mutex_unlock(&pool->mutex);

	const Size n_threads = array_size(pool->threads);
	for (Size i = 0; i < n_threads; i++) {
		thread_join(&pool->threads[i]);
	}

	array_free(pool->threads);

	mutex_lock(&pool->mutex);
	const Size n_work = array_size(pool->work);
	for (Size i = 0; i < n_work; i++) {
		ThreadPoolWork *work = &pool->work[i];
		if (work->dispose) {
			work->dispose(work->user);
		}
	}
	array_free(pool->work);
	mutex_unlock(&pool->mutex);

	cond_fini(&pool->cond);
	mutex_fini(&pool->mutex);
}

void threadpool_queue(ThreadPool *pool, void (*function)(void*), void *user, void (*dispose)(void*)) {
	mutex_lock(&pool->mutex);
	const ThreadPoolWork work = LIT(ThreadPoolWork, function, user, dispose);
	array_push(pool->work, work);
	cond_signal(&pool->cond);
	mutex_unlock(&pool->mutex);
}