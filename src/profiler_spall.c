#include <stdatomic.h>
#include <time.h>

#include "profiler.h"
#include "allocator.h"
#include "context.h"
#include "utility.h"
#include "thread.h"

// Disable some warnings in the Spall header.
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#pragma GCC diagnostic ignored "-Wsign-compare"
#include "vendor/spall.h"
#pragma GCC diagnostic pop

typedef struct SpallThread SpallThread;
typedef struct SpallShared SpallShared;

struct SpallThread {
	Context *context;
	SpallBuffer buffer;
	Uint32 tid;
};

struct SpallShared {
	SpallProfile ctx;
	_Atomic(Bool) lock;
	Size count;
};

double us() {
	struct timespec spec;
	clock_gettime(CLOCK_MONOTONIC, &spec);
	return (((double)spec.tv_sec) * 1000000) + (((double)spec.tv_nsec) / 1000);
}

static SpallShared g_spall;

static void spall_shared_lock(SpallShared *shared) {
	for (;;) {
		if (!atomic_exchange_explicit(&shared->lock, true, memory_order_acquire)) {
			return;
		}
		while (atomic_load_explicit(&shared->lock, memory_order_relaxed)) {
			// RELAX
		}
	}
}

static void spall_shared_unlock(SpallShared *shared) {
	atomic_store_explicit(&shared->lock, false, memory_order_release);
}

static void spall_shared_init(SpallShared *shared) {
	spall_shared_lock(shared);
	if (shared->count == 0) {
		shared->ctx = spall_init_file("profile.spall", 1.0);
	}
	shared->count++;
	spall_shared_unlock(shared);
}

static void spall_shared_fini(SpallShared *shared) {
	spall_shared_lock(shared);
	if (shared->count == 1) {
		spall_quit(&shared->ctx);
	}
	shared->count--;
	spall_shared_unlock(shared);
}

static void *profiler_spall_init(Context *context) {
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
	spall_buffer_quit(&g_spall.ctx, &thread->buffer);
	allocator_deallocate(allocator, thread->buffer.data);
	allocator_deallocate(allocator, thread);
	spall_shared_fini(&g_spall);
}

static void profiler_spall_enter(void *ctx, String file, int line, String function) {
	(void)file;
	(void)line;
	SpallThread *const thread = CAST(SpallThread *, ctx);
	spall_buffer_begin_ex(
		&g_spall.ctx,
		&thread->buffer,
		CAST(const char *, function.contents),
		function.length,
		us(),
		thread->tid,
		0);
}

static void profiler_spall_leave(void *ctx) {
	SpallThread *const thread = CAST(SpallThread *, ctx);
	spall_buffer_end_ex(
		&g_spall.ctx,
		&thread->buffer,
		us(),
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