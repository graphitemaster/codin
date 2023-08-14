#ifndef CODIN_THREAD_H
#define CODIN_THREAD_H
#include "support.h"

typedef struct Context Context;

typedef struct Thread Thread;
typedef struct Mutex Mutex;
typedef struct Cond Cond;
typedef struct WaitGroup WaitGroup;

struct ALIGN(16) Thread {
	Uint8 storage[64];
};

struct ALIGN(16) Cond { 
	Uint8 storage[64];
};

struct ALIGN(16) THREAD_CAPABILITY("mutex") Mutex { 
	Uint8 storage[128];
};

struct WaitGroup {
	Mutex mutex;
	Cond cond  THREAD_GUARDED(mutex);
	Size count THREAD_GUARDED(mutex);
};

#define thread_create(thread, proc, data) \
	_thread_create((thread), (proc), (data), context)

void _thread_create(Thread *thread, int (*proc)(void*), void *data, Context *context);
Bool thread_join(Thread *thread);

// Mutex
void mutex_init(Mutex *mutex);
void mutex_fini(Mutex *mutex)
	THREAD_EXCLUDES(*mutex);
void mutex_lock(Mutex *mutex)
	THREAD_ACQUIRES(*mutex);
void mutex_unlock(Mutex *mutex)
	THREAD_REQUIRES(*mutex)
	THREAD_RELEASES(*mutex);

// Cond
void cond_init(Cond *cond);
void cond_fini(Cond *cond);
void cond_wait(Cond *cond, Mutex *mutex) THREAD_REQUIRES(*mutex);
void cond_signal(Cond *cond);
void cond_broadcast(Cond *cond);

// WaitGroup
void waitgroup_init(WaitGroup *wg, Size count);
void waitgroup_fini(WaitGroup *wg);
void waitgroup_signal(WaitGroup *wg);
void waitgroup_wait(WaitGroup *wg);

#endif // CODIN_THREAD_H