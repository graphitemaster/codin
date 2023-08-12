#ifndef CODIN_THREADPOOL_H
#define CODIN_THREADPOOL_H

#include "thread.h"
#include "array.h"

typedef struct Context Context;
typedef struct ThreadPool ThreadPool;
typedef struct ThreadPoolWork ThreadPoolWork;

struct ThreadPool {
	Context *context;
	Array(Thread) threads;
	Mutex mutex;
	Array(ThreadPoolWork) work THREAD_GUARDED(mutex);
	Cond cond                  THREAD_GUARDED(mutex);
	Bool quit                  THREAD_GUARDED(mutex);
};

Bool threadpool_init(ThreadPool *pool, Size n_threads, Context *context);
Bool threadpool_free(ThreadPool *pool);
Bool threadpool_queue(ThreadPool *pool, void (*function)(void*), void *user, void (*dispose)(void*));

#endif // CODIN_THREADPOOL_H