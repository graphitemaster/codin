#ifndef CODIN_THREAD_H
#define CODIN_THREAD_H
#include "support.h"

typedef struct Context Context;

typedef struct Thread Thread;
typedef struct Mutex Mutex;
typedef struct Cond Cond;

struct ALIGN(16) Thread {
	Uint8 storage[64];
};

struct ALIGN(16) Cond { 
	Uint8 storage[64];
};

struct ALIGN(16) Mutex { 
	Uint8 storage[64];
};

#define thread_create(thread, proc, data) \
	_thread_create((thread), (proc), (data), context)

Bool _thread_create(Thread *thread, int (*proc)(void*), void *data, Context *context);
Bool thread_join(Thread *thread);

void mutex_init(Mutex *mutex);
void mutex_destroy(Mutex *mutex);
void mutex_lock(Mutex *mutex);
void mutex_unlock(Mutex *mutex);

void cond_init(Cond *cond);
void cond_destroy(Cond *cond);
void cond_wait(Cond *cond, Mutex *mutex);
void cond_signal(Cond *cond);
void cond_broadcast(Cond *cond);

#endif // CODIN_THREAD_H