#include "threadpool.h"

struct Work {
	void (*function)(void *user);
	void *user;
	void (*dispose)(void *user);
};

static int worker(void *user) {
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
		Work work = pool->work[--meta->size];
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

	pool->context = context;
	pool->threads = 0;
	pool->work = 0;

	if (!array_resize(pool->threads, n_threads)) {
		return false;
	}

	for (Size i = 0; i < n_threads; i++) {
		thread_create(&pool->threads[i], worker, pool);
	}

	return true;
}

Bool threadpool_free(ThreadPool *pool) {
	Context *context = pool->context;

	mutex_lock(&pool->mutex);
	pool->quit = true;
	cond_broadcast(&pool->cond);
	mutex_unlock(&pool->mutex);

	const Size n_threads = array_size(pool->threads);
	for (Size i = 0; i < n_threads; i++) {
		thread_join(&pool->threads[i]);
	}

	array_free(pool->threads);

	cond_destroy(&pool->cond);
	mutex_destroy(&pool->mutex);

	const Size n_work = array_size(pool->work);
	for (Size i = 0; i < n_work; i++) {
		Work *work = &pool->work[i];
		if (work->dispose) {
			work->dispose(work->user);
		}
	}
	array_free(pool->work);

	return true;
}

Bool threadpool_queue(ThreadPool *pool, void (*function)(void*), void *user, void (*dispose)(void*)) {
	Context *context = pool->context;

	mutex_lock(&pool->mutex);
	const Work work = LIT(Work, function, user, dispose);
	if (!array_push(pool->work, work)) {
		if (dispose) {
			dispose(user);
		}
		mutex_unlock(&pool->mutex);
		return false;
	}

	cond_signal(&pool->cond);
	mutex_unlock(&pool->mutex);

	return true;
}