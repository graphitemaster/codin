#ifndef CODIN_CTHREADS_WIN32_H
#define CODIN_CTHREADS_WIN32_H

#ifndef __STDC_NO_THREADS__
#error "C11 threads are available"
#endif

#ifndef _WIN32
#error "Platform is not _WIN32"
#endif

#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <process.h> // _beginthread, _endthread

#include "support.h" // CAST FORCE_INLINE ASSERT NULLABLE NONNULL

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

typedef HANDLE thrd_t;

typedef struct {
	CRITICAL_SECTION cs;
} mtx_t;

typedef struct {
	CONDITION_VARIABLE cv;
} cnd_t;

// TODO(pankkor): add asserts for NONNULL

typedef struct {
	thrd_start_t func;
	void *arg;
} FuncArg;

static unsigned __stdcall internal_trampoline(void *p) {
	FuncArg fa = *CAST(FuncArg *, p);
	free(p);
	return (unsigned)fa.func(fa.arg);
}

static FORCE_INLINE int thrd_create(
		thrd_t NULLABLE * restrict NONNULL thr,
		thrd_start_t NONNULL func,
		void * restrict NULLABLE arg) {
	FuncArg	*fa = (FuncArg *)malloc(sizeof(FuncArg));
	if (!fa) {
		return thrd_nomem;
	}
	fa->func = func;
	fa->arg = arg;
	uintptr_t handle = _beginthreadex(NULL, 0, internal_trampoline, fa, 0, NULL);
	if (handle == 0) {
		return errno == EAGAIN || errno == EACCES ? thrd_nomem : thrd_error;
	}
	*thr = (thrd_t)handle;
	return thrd_success;
}

static FORCE_INLINE void thrd_exit(int res) {
	_endthreadex((unsigned)res);
}

static FORCE_INLINE int thrd_join(thrd_t NULLABLE thr, int * NULLABLE res) {
	if (WaitForSingleObject(thr, INFINITE) != WAIT_OBJECT_0) {
		return thrd_error;
	}

	int ret = thrd_success;

    if (res) {
		DWORD code;
		if (GetExitCodeThread(thr, &code)) {
			*res = (int)code;
		} else {
			ret = thrd_error;
		}
    }

    CloseHandle(thr);
    return ret;
}

static FORCE_INLINE int mtx_init(mtx_t * NONNULL mtx, int type) {
	InitializeCriticalSection(&mtx->cs);
	return thrd_success;
}

static FORCE_INLINE void mtx_destroy(mtx_t * NONNULL mtx) {
	DeleteCriticalSection(&mtx->cs);
}

static FORCE_INLINE int mtx_lock(mtx_t * NONNULL mtx) {
	EnterCriticalSection(&mtx->cs);
	return thrd_success;
}

static FORCE_INLINE int mtx_unlock(mtx_t * NONNULL mtx) {
	LeaveCriticalSection(&mtx->cs);
	return thrd_success;
}

static FORCE_INLINE int cnd_init(cnd_t * NONNULL cond) {
	InitializeConditionVariable(&cond->cv);
	return thrd_success;
}

static FORCE_INLINE void cnd_destroy(cnd_t * NONNULL cond) {
	(void)cond;
}

static FORCE_INLINE int cnd_signal(cnd_t * NONNULL cond) {
	WakeConditionVariable(&cond->cv);
	return thrd_success;
}

static FORCE_INLINE int cnd_broadcast(cnd_t * NONNULL cond) {
	WakeAllConditionVariable(&cond->cv);
	return thrd_success;
}

static FORCE_INLINE int cnd_wait(
		cnd_t * restrict NONNULL cond,
		mtx_t * restrict NONNULL mtx) {
	SleepConditionVariableCS(&cond->cv, &mtx->cs, INFINITE);
	return thrd_success;
}

#endif // #ifndef CODIN_CTHREADS_WIN32_H
