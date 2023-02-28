#include "threadpool.h"

struct Work {
	void (*function)(void *user);
	void *user;
	void (*dispose)(void *user);
};

static int worker(void *user) {
	ThreadPool *pool = CAST(ThreadPool*, user);
	for (;;) {
		mtx_lock(&pool->mutex);
		while (!pool->quit && array_size(pool->work) == 0) {
			cnd_wait(&pool->cond, &pool->mutex);
		}

		if (pool->quit && array_size(pool->work) == 0) {
			mtx_unlock(&pool->mutex);
			return 0;
		}
	
		Array *meta = array_meta(pool->work);
		Work work = pool->work[--meta->size];
		mtx_unlock(&pool->mutex);

		work.function(work.user);

		if (work.dispose) {
			work.dispose(work.user);
		}
	}
}

Bool threadpool_init(ThreadPool *pool, Size n_threads, Context *context) {
	// TODO(pankkor): init can fail
	cnd_init(&pool->cond);
	mtx_init(&pool->mutex, mtx_plain);

	pool->context = context;
	pool->threads = 0;
	pool->work = 0;

	if (!array_resize(pool->threads, n_threads)) {
		return false;
	}

	for (Size i = 0; i < n_threads; i++) {
		thrd_create(&pool->threads[i], worker, pool);
	}

	return true;
}

Bool threadpool_free(ThreadPool *pool) {
	Context *context = pool->context;

	mtx_lock(&pool->mutex);
	pool->quit = true;
	cnd_broadcast(&pool->cond);
	mtx_unlock(&pool->mutex);

	const Size n_threads = array_size(pool->threads);
	for (Size i = 0; i < n_threads; i++) {
		thrd_join(pool->threads[i], 0);
	}

	array_free(pool->threads);

	cnd_destroy(&pool->cond);
	mtx_destroy(&pool->mutex);

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

	mtx_lock(&pool->mutex);
	if (!array_push(pool->work, ((Work){function, user, dispose}))) {
		if (dispose) {
			dispose(user);
		}
		mtx_unlock(&pool->mutex);
		return false;
	}
	cnd_signal(&pool->cond);
	mtx_unlock(&pool->mutex);
	return true;
}