#include <stdlib.h> // abort

#include "thread.h"
#include "context.h"
#include "allocator.h"

#if defined(OS_POSIX)
#include <pthread.h>
#include <signal.h>
#elif defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h>
#else
#error Unknown OS
#endif

typedef struct State State;

struct State {
	Allocator *allocator;
	int (*proc)(void *data);
	void *data;
};

#if defined(OS_POSIX)
static void* thread_proc(void *data)
#elif defined(OS_WINDOWS)
static unsigned thread_proc(void *data)
#endif
{
	State *state = CAST(State*, data);
	Allocator *allocator = state->allocator;
#if defined(OS_POSIX)
	// Block all signal delivery to this thread.
	sigset_t mask;
	sigfillset(&mask);
	pthread_sigmask(SIG_BLOCK, &mask, 0);
#endif
	state->proc(state->data);
	allocator_deallocate(allocator, state);
	return 0;
}

void _thread_create(Thread *thread, int (*proc)(void*), void *data, Context *context) {
	Allocator *allocator = &context->allocator;
	State *state = allocator_allocate(allocator, sizeof *state);
	state->allocator = allocator;
	state->proc = proc;
	state->data = data;
#if defined(OS_POSIX)
	pthread_t *handle = RCAST(pthread_t*, thread->storage);
	if (pthread_create(handle, 0, thread_proc, state) != 0) {
		THROW(ERROR_UNKNOWN);
	}
#elif defined(OS_WINDOWS)
	uintptr_t tid = _beginthreadex(
		0,
		0,
		thread_proc,
		state,
		0,
		0);
	if (tid == 0) {
		THROW(ERROR_UNKNOWN);
	}
	HANDLE handle = RCAST(HANDLE, tid);
	*RCAST(HANDLE*, thread->storage) = handle;
#endif
}

Bool thread_join(Thread *thread) {
#if defined(OS_POSIX)
	pthread_t *handle = RCAST(pthread_t*, thread->storage);
	if (pthread_join(*handle, 0) != 0) {
		return false;
	}
#elif defined(OS_WINDOWS)
	HANDLE handle = *RCAST(HANDLE*, thread->storage);
	if (WaitForSingleObject(handle, INFINITE) != WAIT_OBJECT_0) {
		return false;
	}
	CloseHandle(handle);
#endif
	return true;
}

void mutex_init(Mutex *mutex) {
#if defined(OS_POSIX)
	pthread_mutex_t *handle = RCAST(pthread_mutex_t *, mutex->storage);
	pthread_mutexattr_t attributes;
	if (pthread_mutexattr_init(&attributes) != 0
	 || pthread_mutexattr_settype(&attributes, PTHREAD_MUTEX_NORMAL) != 0
	 || pthread_mutex_init(handle, &attributes) != 0
	 || pthread_mutexattr_destroy(&attributes) != 0)
	{
		abort();
	}
#elif defined(OS_WINDOWS)
	CRITICAL_SECTION *handle = RCAST(CRITICAL_SECTION*, mutex->storage);
	InitializeCriticalSection(handle);
#endif
}

void mutex_fini(Mutex *mutex) {
#if defined(OS_POSIX)
	pthread_mutex_t *handle = RCAST(pthread_mutex_t *, mutex->storage);
	if (pthread_mutex_destroy(handle) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	CRITICAL_SECTION *handle = RCAST(CRITICAL_SECTION*, mutex->storage);
	DeleteCriticalSection(handle);
#endif
}

void mutex_lock(Mutex *mutex)
	THREAD_INTERNAL
{
#if defined(OS_POSIX)
	pthread_mutex_t *handle = RCAST(pthread_mutex_t *, mutex->storage);
	if (pthread_mutex_lock(handle) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	CRITICAL_SECTION *handle = RCAST(CRITICAL_SECTION*, mutex->storage);
	EnterCriticalSection(handle);
#endif
}

void mutex_unlock(Mutex *mutex)
	THREAD_INTERNAL
{
#if defined(OS_POSIX)
	pthread_mutex_t *handle = RCAST(pthread_mutex_t *, mutex->storage);
	if (pthread_mutex_unlock(handle) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	CRITICAL_SECTION *handle = RCAST(CRITICAL_SECTION*, mutex->storage);
	LeaveCriticalSection(handle);
#endif
}

void cond_init(Cond *cond) {
#if defined(OS_POSIX)
	pthread_cond_t *handle = RCAST(pthread_cond_t *, cond->storage);
	if (pthread_cond_init(handle, 0) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	CONDITION_VARIABLE *handle = RCAST(CONDITION_VARIABLE*, cond->storage);
	InitializeConditionVariable(handle);
#endif
}

void cond_fini(Cond *cond) {
#if defined(OS_POSIX)
	pthread_cond_t *handle = RCAST(pthread_cond_t *, cond->storage);
	if (pthread_cond_destroy(handle) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	(void)cond;
	// There is no destruction for CONDITION_VARIABLE on Windows.
#endif
}

void cond_wait(Cond *cond, Mutex *mutex) {
#if defined(OS_POSIX)
	pthread_cond_t *cond_handle = RCAST(pthread_cond_t *, cond->storage);
	pthread_mutex_t *mutex_handle = RCAST(pthread_mutex_t *, mutex->storage);
	if (pthread_cond_wait(cond_handle, mutex_handle) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	CONDITION_VARIABLE *cond_handle = RCAST(CONDITION_VARIABLE*, cond->storage);
	CRITICAL_SECTION *mutex_handle = RCAST(CRITICAL_SECTION*, mutex->storage);
	if (!SleepConditionVariableCS(cond_handle, mutex_handle, INFINITE)) {
		abort();
	}
#endif
}

void cond_signal(Cond *cond) {
#if defined(OS_POSIX)
	pthread_cond_t *handle = RCAST(pthread_cond_t*, cond->storage);
	if (pthread_cond_signal(handle) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	CONDITION_VARIABLE *handle = RCAST(CONDITION_VARIABLE*, cond->storage);
	WakeConditionVariable(handle);
#endif
}

void cond_broadcast(Cond *cond) {
#if defined(OS_POSIX)
	pthread_cond_t *handle = RCAST(pthread_cond_t*, cond->storage);
	if (pthread_cond_broadcast(handle) != 0) {
		abort();
	}
#elif defined(OS_WINDOWS)
	CONDITION_VARIABLE *handle = RCAST(CONDITION_VARIABLE*, cond->storage);
	WakeAllConditionVariable(handle);
#endif
}

void waitgroup_init(WaitGroup *wg, Size count)
	THREAD_INTERNAL
{
	mutex_init(&wg->mutex);
	cond_init(&wg->cond);
	wg->count = count;
}

void waitgroup_fini(WaitGroup *wg) {
	cond_fini(&wg->cond);
	mutex_fini(&wg->mutex);
}

void waitgroup_signal(WaitGroup *wg) {
	mutex_lock(&wg->mutex);
	wg->count--;
	cond_signal(&wg->cond);
	mutex_unlock(&wg->mutex);
}

void waitgroup_wait(WaitGroup *wg) {
	mutex_lock(&wg->mutex);
	while (wg->count) {
		cond_wait(&wg->cond, &wg->mutex);
	}
	mutex_unlock(&wg->mutex);
}