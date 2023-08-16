#ifndef CODIN_BUILD_H
#define CODIN_BUILD_H
#include "context.h"
#include "sched.h"
#include "project.h"

typedef struct Tree Tree;

typedef struct BuildWork BuildWork;
typedef struct BuildContext BuildContext;
typedef struct BuildSettings BuildSettings;

enum Arch {
	ARCH_AMD64,
	ARCH_AARCH64,
};

enum Platform {
	PLATFORM_LINUX,
	PLATFORM_FREEBSD,
	PLATFORM_DARWIN,
	PLATFORM_WINDOWS,
};

typedef enum Arch Arch;
typedef enum Platform Platform;

struct BuildWork {
	BuildContext *build;
	const Package *package;
	Tree *tree;
	Bool error;
};

struct BuildSettings {
	Arch host_arch;
	Arch target_arch;
	Platform host_platform;
	Platform target_platform;
};

struct BuildContext {
	Context context;
	Sched sched;
	Project project;
	Array(Collection) collections;
	BuildSettings settings;

	Mutex mutex;
	Array(BuildWork*) work THREAD_GUARDED(mutex);
};

void build_init(BuildContext *ctx, String allocator, String scheduler, String profiler);
void build_fini(BuildContext *build);
void build_wait(BuildContext *build);
Bool build_add_collection(BuildContext *build, String name, String path);
void build_add_package(BuildContext *build, String pathname);

#endif // CODIN_BUILD_H