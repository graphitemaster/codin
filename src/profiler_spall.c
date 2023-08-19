#include <stdatomic.h>

#include "profiler.h"
#include "allocator.h"
#include "context.h"
#include "utility.h"
#include "thread.h"

// Disable some warnings in the Spall header.
#if defined(COMPILER_GCC)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#pragma GCC diagnostic ignored "-Wsign-compare"
#elif defined(COMPILER_CLANG)
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmissing-field-initializers"
#pragma clang diagnostic ignored "-Wsign-compare"
#endif
#include "vendor/spall.h"
#if defined(COMPILER_GCC)
#pragma GCC diagnostic pop
#elif defined(COMPILER_CLANG)
#pragma clang diagnostic pop
#endif

#if defined(OS_LINUX)
#include <linux/perf_event.h> // struct perf_event_attr, PERF_TYPE_HARDWARE, PERF_COUNT_HW_INSTRUCTIONS
#include <sys/mman.h> // mmap, PROT_READ, MAP_SHARED, MAP_FAILED
#include <x86intrin.h> // __rdtsc
#include <time.h> // clock_gettime, usleep, CLOCK_MONOTONIC_RAW
#include <unistd.h> // close, usleep, syscall

static FORCE_INLINE Uint64 rdtsc(void) {
	return __rdtsc();
}

static inline Uint64 rdtsc_fallback_freq(void) {
	// Estimate TSC frequency with the monotonic clock as a fallback.
	struct timespec clock;
	clock_gettime(CLOCK_MONOTONIC_RAW, &clock);
	Sint64 c_beg = clock.tv_sec * 1e9 + clock.tv_nsec;
	Uint64 t_beg = rdtsc();
	usleep(2000);
	clock_gettime(CLOCK_MONOTONIC_RAW, &clock);
	Sint64 c_end = clock.tv_sec * 1e9 + clock.tv_nsec;
	Uint64 t_end = rdtsc();
	return (t_end - t_beg) * 1000000000 / (c_end - c_beg);
}

static inline Uint64 rdtsc_freq(void) {
	struct perf_event_attr pe;
	pe.type           = PERF_TYPE_HARDWARE;
	pe.size           = sizeof pe;
	pe.config         = PERF_COUNT_HW_INSTRUCTIONS;
	pe.disabled       = 1;
	pe.exclude_kernel = 1;
	pe.exclude_hv     = 1;

	#define PERF_EVENT_OPEN_SYSCALL 298 // x86_64

	int fd = -1;
	struct perf_event_mmap_page *page = 0;
	Uint64 tsc_freq = 0;

	const int page_size = sysconf(_SC_PAGESIZE);

	fd = syscall(PERF_EVENT_OPEN_SYSCALL, &pe, 0, -1, -1, 0);
	if (fd == -1) {
		goto L_fallback;
	}

	page = RCAST(struct perf_event_mmap_page *, mmap(0, page_size, PROT_READ, MAP_SHARED, fd, 0));
	if (page == MAP_FAILED) {
		goto L_fallback;
	}

	if (page->cap_user_time != -1) {
		// User time not supported.
		goto L_fallback;
	}

	tsc_freq = 1000000000ull << (page->time_shift / 2);
	tsc_freq /= page->time_mult >> (page->time_shift - page->time_shift / 2);

	munmap(page, page_size);
	close(fd);

	return tsc_freq;

L_fallback:
	munmap(page, page_size);
	close(fd);

	return rdtsc_fallback_freq();
}

#elif defined(OS_WINDOWS)

static FORCE_INLINE Uint64 rdtsc(void) {
	return __rdtsc();
}

static inline Uint64 rdtsc_fallback_freq(void) {
	LARGE_INTEGER freq;
	QueryPerformanceFrequency(&freq);
	LARGE_INTEGER q_beg;
	QueryPerformanceCounter(&q_beg);
	Uint64 t_beg = rdtsc();
	Sleep(2);
	LARGE_INTEGER q_end;
	QueryPerformanceCounter(&q_end);
	Uint64 t_end = rdtsc();
	return (t_end - t_beg) * freq.QuadPart / (q_end.QuadPart - q_beg.QuadPart);
}

static inline Uint64 rdtsc_freq(void) {
	HMODULE ntdll = LoadLibraryA("ntdll.dll");
	if (!ntdll) {
		goto L_fallback;
	}

	int (*NtQuerySystemInformation)(int, void *, unsigned int, unsigned int *) = 0;
	*RCAST(void **, &NtQuerySystemInformation) = GetProcAddress(ntdll, "NtQuerySystemInformation");
	if (!NtQuerySystemInformation) {
		goto L_fallback;
	}

	volatile Uint64 *HSUV = 0;
	Uint32 size = 0;
	Sint32 result = NtQuerySystemInformation(0xc5, RCAST(void **, &HSUV), sizeof HSUV, &size);
	if (size != sizeof HSUV || result < 0) {
		goto L_fallback;
	}

	FreeLibrary(ntdll);
	return (10000000ull << 32) / (HSUV[1] >> 32);

L_fallback:
	FreeLibrary(ntdll);
	return rdtsc_fallback_freq();
}

#endif

typedef struct SpallShared SpallShared;
typedef struct SpallThread SpallThread;

struct THREAD_CAPABILITY("mutex") SpallShared {
	SpallProfile ctx;
	_Atomic(Bool) lock;
	Size count;
};

static SpallShared g_spall;

struct SpallThread {
	Context *context;
	SpallBuffer buffer THREAD_GUARDED(g_spall);
	_Atomic(Uint32) tid;
};

static void spall_shared_lock(SpallShared *shared)
	THREAD_ACQUIRES(shared)
	THREAD_INTERNAL
{
	for (;;) {
		if (!atomic_exchange_explicit(&shared->lock, true, memory_order_acquire)) {
			return;
		}
		while (atomic_load_explicit(&shared->lock, memory_order_relaxed)) {
			// RELAX
		}
	}
}

static void spall_shared_unlock(SpallShared *shared)
	THREAD_REQUIRES(shared)
	THREAD_RELEASES(shared)
	THREAD_INTERNAL
{
	atomic_store_explicit(&shared->lock, false, memory_order_release);
}

static void spall_shared_init(SpallShared *shared) {
	spall_shared_lock(shared);
	if (shared->count == 0) {
		shared->ctx = spall_init_file("profile.spall", 1000000.0 / rdtsc_freq());
	}
	shared->count++;
	spall_shared_unlock(shared);
}

static void spall_shared_fini(SpallShared *shared)
	THREAD_EXCLUDES(g_spall)
{
	spall_shared_lock(shared);
	if (shared->count == 1) {
		spall_quit(&shared->ctx);
	}
	shared->count--;
	spall_shared_unlock(shared);
}

static void *profiler_spall_init(Context *context)
	THREAD_INTERNAL
{
	spall_shared_init(&g_spall);
	Allocator *const allocator = &context->allocator;
	SpallThread *thread = allocator_allocate(allocator, sizeof *thread);
	const Size capacity = 1024 * 1024 * 1024; // 1 GiB
	thread->context = context;
	thread->tid = thread_id();
	thread->buffer.length = capacity;
	thread->buffer.data = allocator_allocate(allocator, capacity);
	spall_buffer_init(&g_spall.ctx, &thread->buffer);
	return thread;
}

static void profiler_spall_fini(void *ctx) {
	SpallThread *const thread = CAST(SpallThread *, ctx);
	Allocator *const allocator = &thread->context->allocator;
	spall_shared_lock(&g_spall);
	spall_buffer_quit(&g_spall.ctx, &thread->buffer);
	allocator_deallocate(allocator, thread->buffer.data);
	allocator_deallocate(allocator, thread);
	spall_shared_unlock(&g_spall);
	spall_shared_fini(&g_spall);
}

static void profiler_spall_enter(void *ctx, String file, int line, String function) {
	(void)file;
	(void)line;
	SpallThread *const thread = CAST(SpallThread *, ctx);
	spall_buffer_begin_ex(
		&g_spall.ctx,
		&thread->buffer,
		RCAST(const char *, function.contents),
		function.length,
		rdtsc(),
		thread->tid,
		0);
}

static void profiler_spall_leave(void *ctx) {
	SpallThread *const thread = CAST(SpallThread *, ctx);
	spall_buffer_end_ex(
		&g_spall.ctx,
		&thread->buffer,
		rdtsc(),
		thread->tid,
		0);
}

const ProfilerOperations PROFILER_SPALL = {
	SLIT("spall"),
	profiler_spall_init,
	profiler_spall_fini,
	profiler_spall_enter,
	profiler_spall_leave,
};