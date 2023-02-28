#ifndef CODIN_CTHREADS_PTHREADS_H
#define CODIN_CTHREADS_PTHREADS_H

#ifndef __STDC_NO_THREADS__
#error "C11 threads are available"
#endif

#include <errno.h> // ENOMEM
#include <pthread.h>

#include "support.h" // FORCE_INLINE ASSERT NULLABLE NONNULL

typedef int (*thrd_start_t)(void * NULLABLE);
typedef void (*tss_dtor_t)(void * NULLABLE);

enum {
	mtx_plain = 0,
	mtx_recursive = 1,
	mtx_timed = 2,
};

enum {
	thrd_success,
	thrd_timedout,
	thrd_busy,
	thrd_error,
	thrd_nomem
};

typedef pthread_t thrd_t;
typedef pthread_mutex_t mtx_t;
typedef pthread_cond_t cnd_t;

static FORCE_INLINE int thrd_create(
		thrd_t NULLABLE * restrict NONNULL thr,
		thrd_start_t NONNULL func,
		void * restrict NULLABLE arg) {
	int res = pthread_create(thr, 0, (void*(*)(void*))func, arg);
	return res == 0
		? thrd_success
		: res == ENOMEM ? thrd_nomem : thrd_error;
}

static FORCE_INLINE void thrd_exit(int res) {
	pthread_exit((void *)(intptr_t)res);
}

static FORCE_INLINE int thrd_join(thrd_t NULLABLE thr, int * NULLABLE res) {
	void *retval;
	if (pthread_join(thr, &retval) == 0) {
		return thrd_error;
	}

	if (res) {
		*res = (int)(intptr_t)retval;
	}
	return thrd_success;
}

static FORCE_INLINE int mtx_init(mtx_t * NONNULL mtx, int type) {
	if (type == mtx_plain) {
		return pthread_mutex_init(mtx, 0) == 0 ? thrd_success : thrd_error;
	}

	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);

#ifdef OS_APPLE
	if (type & mtx_timed) {
		ASSERT(0 && "Timed mutex is not supported by Apple platforms");
		return thrd_error;
	}
#endif

	// NOTE: glibc PTHREAD_MUTEX_NORMAL == PTHREAD_MUTEX_RECURSIVE
	int pthread_mtx_type = type & mtx_recursive
		? PTHREAD_MUTEX_RECURSIVE
		: PTHREAD_MUTEX_NORMAL;

	pthread_mutexattr_settype(&attr, pthread_mtx_type);

	int ret = pthread_mutex_init(mtx, &attr) == 0 ? thrd_success : thrd_error;
	pthread_mutexattr_destroy(&attr);
	return ret;
}

static FORCE_INLINE void mtx_destroy(mtx_t * NONNULL mtx) {
	pthread_mutex_destroy(mtx);
}

static FORCE_INLINE int mtx_lock(mtx_t * NONNULL mtx)
{
	return pthread_mutex_lock(mtx) == 0 ? thrd_success : thrd_error;
}

static FORCE_INLINE int mtx_unlock(mtx_t * NONNULL mtx)
{
	return pthread_mutex_unlock(mtx) == 0 ? thrd_success : thrd_error;
}

static FORCE_INLINE int cnd_init(cnd_t * NONNULL cond)
{
	return pthread_cond_init(cond, 0) == 0 ? thrd_success : thrd_error;
}

static FORCE_INLINE void cnd_destroy(cnd_t * NONNULL cond)
{
	pthread_cond_destroy(cond);
}

static FORCE_INLINE int cnd_signal(cnd_t * NONNULL cond)
{
	return pthread_cond_signal(cond) == 0 ? thrd_success : thrd_error;
}

static FORCE_INLINE int cnd_broadcast(cnd_t * NONNULL cond)
{
	return pthread_cond_broadcast(cond) == 0 ? thrd_success : thrd_error;
}

static FORCE_INLINE int cnd_wait(
		cnd_t * restrict NONNULL cond,
		mtx_t * restrict NONNULL mtx)
{
	return pthread_cond_wait(cond, mtx) == 0 ? thrd_success : thrd_error;
}

#endif // #ifndef CODIN_CTHREADS_PTHREADS_H
