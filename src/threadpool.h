#ifndef CODIN_THREADPOOL_H
#define CODIN_THREADPOOL_H

#include "cthreads.h" // thrd_t, cnd_t, mtx_t

#include "array.h"

typedef struct Context Context;
typedef struct ThreadPool ThreadPool;
typedef struct Work Work;

struct ThreadPool {
	Context *context;
	Array(thrd_t) threads;
	Array(Work) work;
	cnd_t cond;
	mtx_t mutex;
	Bool quit;
};

Bool threadpool_init(ThreadPool *pool, Size n_threads, Context *context);
Bool threadpool_free(ThreadPool *pool);
Bool threadpool_queue(ThreadPool *pool, void (*function)(void*), void *user, void (*dispose)(void*));

#endif // CODIN_THREADPOOL_H
