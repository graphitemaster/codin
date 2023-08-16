#include "build.h"
#include "tree.h"
#include "path.h"
#include "parser.h"
#include "report.h"

#define BUILD_ERROR(location, fmt, ...) \
	do { \
		const Tree *const tree = (work)->tree; \
		report_error(&tree->source, (location), tree->context, (fmt), ## __VA_ARGS__); \
		(work)->error = true; \
	} while (0)

static BuildWork *build_add_work(BuildContext *build, Package *package, String filename, Context *context);
static void build_worker(void *data, Context *context);

void build_init(BuildContext *ctx, String allocator, String scheduler, String profiler)
	THREAD_INTERNAL
{
	// Needs to be initialized first.
	Context *const context = &ctx->context;
	context_init(context, allocator, profiler);

	PROF_ENTER();

	sched_init(&ctx->sched, scheduler, context);
	project_init(&ctx->project, SCLIT("test"), context);
	mutex_init(&ctx->mutex);
	ctx->collections = array_create(context);
	ctx->work = array_create(context);

	// Set host platform and architecture.
#if defined(OS_WINDOWS)
	ctx->settings.host_platform = PLATFORM_WINDOWS;
#elif defined(OS_LINUX)
	ctx->settings.host_platform = PLATFORM_LINUX;
#endif

#if defined(ISA_AMD64)
	ctx->settings.host_arch = ARCH_AMD64;
#elif defined(ISA_AARCH64)
	ctx->settings.host_arch = ARCH_AARCH64;
#endif

	// TODO(dweiler): Support cross compilation.
	ctx->settings.target_platform = ctx->settings.host_platform;
	ctx->settings.target_arch = ctx->settings.host_arch;

	PROF_LEAVE();
}

void build_fini(BuildContext *build) {
	Context *const context = &build->context;

	PROF_ENTER();

	Allocator *const allocator = &context->allocator;
	mutex_lock(&build->mutex);
	const Size n_work = array_size(build->work);
	for (Size i = 0; i < n_work; i++) {
		BuildWork *const work = build->work[i];
		tree_fini(work->tree);
		allocator_deallocate(allocator, work);
	}
	array_free(build->work);

	array_free(build->collections);
	mutex_unlock(&build->mutex);
	mutex_fini(&build->mutex);
	project_fini(&build->project);
	sched_fini(&build->sched);

	PROF_LEAVE();

	context_fini(context);
}

void build_wait(BuildContext *build) {
	Context *const context = &build->context;
	PROF_ENTER();
	sched_wait(&build->sched);
	PROF_LEAVE();
}

Bool build_add_collection(BuildContext *build, String name, String path) {
	Context *const context = &build->context;

	PROF_ENTER();

	if (!path_directory_exists(path, &build->context)) {
		PROF_LEAVE();
		return false;
	}

	const Size n_collections = array_size(build->collections);
	for (Size i = 0; i < n_collections; i++) {
		const Collection *const collection = &build->collections[i];
		if (string_compare(collection->name, name)) {
			PROF_LEAVE();
			return false;
		}
	}

	array_push(build->collections, LIT(Collection, name, path));

	PROF_LEAVE();

	return true;
}

static void build_add_package_internal(BuildContext *build, String pathname, Context *context);

void build_add_package(BuildContext *build, String pathname) {
	build_add_package_internal(build, pathname, &build->context);
}

static Bool accepted_file(BuildContext *build, String filename) {
	const Platform target_platform = build->settings.target_platform;

	static const struct {
		String   suffix;
		Platform platform;
	} PLATFORMS[] = {
		{ SLIT("_linux.odin"),   PLATFORM_LINUX   },
		{ SLIT("_freebsd.odin"), PLATFORM_FREEBSD },
		{ SLIT("_darwin.odin"),  PLATFORM_DARWIN  },
		{ SLIT("_windows.odin"), PLATFORM_WINDOWS },
	};

	for (Size i = 0; i < sizeof PLATFORMS / sizeof *PLATFORMS; i++) {
		const String suffix = PLATFORMS[i].suffix;
		if (string_ends_with(filename, suffix)) {
			return target_platform == PLATFORMS[i].platform;
		}
	}

	if (string_ends_with(filename, SCLIT("_unix.odin"))) {
		return target_platform != PLATFORM_WINDOWS;
	}

	return true;
}

Package *project_add_package_internal(Project *project, String pathname, Context *context);

static void build_add_package_internal(BuildContext *build, String pathname, Context *context) {
	PROF_ENTER();

	Project *const project = &build->project;
	Package *package = project_add_package_internal(project, pathname, context);
	if (!package){
		PROF_LEAVE();
		return; // Package already exists
	}

	Array(String) filenames = path_list(pathname, context);
	const Size n_filenames = array_size(filenames);
	for (Size i = 0; i < n_filenames; i++) {
		const String filename = filenames[i];
		if (!string_ends_with(filename, SCLIT(".odin"))) {
			continue;
		}

		if (!accepted_file(build, filename)) {
			continue;
		}
		
		const String path = path_cat(pathname, filename, context);
		sched_queue(&build->sched, build_add_work(build, package, path, context), build_worker, 0);
	}

	PROF_LEAVE();
}

static String build_find_collection(BuildContext *build, String name, Context *context) {
	PROF_ENTER();

	const Size n_collections = array_size(build->collections);
	for (Size i = 0; i < n_collections; i++) {
		const Collection *const collection = &build->collections[i];
		if (string_compare(collection->name, name)) {
			PROF_LEAVE();
			return collection->path;
		}
	}

	PROF_LEAVE();

	return STRING_NIL;
}

static BuildWork *build_add_work(BuildContext *build, Package *package, String filename, Context *context) {
	PROF_ENTER();
	Allocator *const allocator = &context->allocator;
	BuildWork *work = allocator_allocate(allocator, sizeof *work);
	work->build = build;
	work->package = package;
	work->tree = package_add_tree(package, filename);
	work->error = false;
	mutex_lock(&build->mutex);
	array_push(build->work, work);
	mutex_unlock(&build->mutex);
	PROF_LEAVE();
	return work;
}

static void build_worker(void *data, Context *context) {
	BuildWork *const work = CAST(BuildWork *, data);
	
	PROF_ENTER();

	Tree *const tree = work->tree;
	if (!parse(tree, context)) {
		BUILD_ERROR(0, "Could not open file");
	}

	// The first statement should be a package declaration.
	const Statement *const first = tree->statements[0];
	if (first->kind != STATEMENT_PACKAGE) {
		BUILD_ERROR(0, "Expected a package declaration at the beginning of the file");
	}

	const PackageStatement *const package = RCAST(const PackageStatement *, first);
	if (string_compare(package->name, SCLIT("_"))) {
		BUILD_ERROR(&package->location, "Cannot name package '_'");
	} else if (string_compare(package->name, SCLIT("intrinsics"))
	        || string_compare(package->name, SCLIT("builtin")))
	{
		BUILD_ERROR(&package->location, "Use of reserved package name: '%.*s'", SFMT(package->name));
	}

	// Queue all the imports into the build.
	const Size n_imports = array_size(tree->imports);
	for (Size i = 0; i < n_imports; i++) {
		const ImportStatement *import = tree->imports[i];

		// Ignore "builtin" and "intrinsics" imports.
		if (string_compare(import->collection, SCLIT("core"))) {
			if (string_compare(import->pathname, SCLIT("./intrinsics"))
			 || string_compare(import->pathname, SCLIT("./builtin")))
			{
				continue;
			}
		}
		String pathname = STRING_NIL;
		if (import->collection.length == 0) {
			// The import will be relative to the current package.
			pathname = work->package->pathname;
		} else {
			// Otherwise it will be relative to the collection.
			pathname = build_find_collection(work->build, import->collection, context);
			if (pathname.length == 0) {
				BUILD_ERROR(&import->location, "Cannot find package collection: '%.*s'", SFMT(import->collection));
			}
		}

		const String splice = path_cat(pathname, import->pathname, context);
		const String result = path_canonicalize(splice, context);

		build_add_package_internal(work->build, result, context);
	}

	PROF_LEAVE();
}