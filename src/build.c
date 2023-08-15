#include "build.h"
#include "tree.h"
#include "path.h"
#include "parser.h"
#include "report.h"

#define BUILD_ERROR(location, fmt, ...) \
	do { \
		const Source *const source = &(work)->tree->source; \
		report_error(source, (location), (fmt), ## __VA_ARGS__); \
		(work)->error = true; \
	} while (0)

static BuildWork *build_add_work(BuildContext *build, Package *package, String filename);
static void build_worker(void *data, Context *context);

void build_init(BuildContext *ctx, String allocator, String scheduler) {
	context_init(&ctx->context, allocator);
	sched_init(&ctx->sched, scheduler, &ctx->context);
	project_init(&ctx->project, SCLIT("test"), &ctx->context);
	ctx->collections = array_create(&ctx->context);
	mutex_init(&ctx->mutex);
	ctx->work = array_create(&ctx->context);
}

void build_fini(BuildContext *build) {
	Allocator *const allocator = &build->context.allocator;
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
	context_fini(&build->context);
}

void build_wait(BuildContext *build) {
	sched_wait(&build->sched);
}

Bool build_add_collection(BuildContext *build, String name, String path) {
	const Size n_collections = array_size(build->collections);
	for (Size i = 0; i < n_collections; i++) {
		const Collection *const collection = &build->collections[i];
		if (string_compare(collection->name, name)) {
			return false;
		}
	}
	array_push(build->collections, LIT(Collection, name, path));
	return true;
}

void build_add_package(BuildContext *build, String pathname) {
	Context *const context = &build->context;
	Project *const project = &build->project;

	Package *package = project_add_package(project, pathname);
	if (!package) return; // Package already exists

	Array(String) filenames = path_list(pathname, context);
	const Size n_filenames = array_size(filenames);
	for (Size i = 0; i < n_filenames; i++) {
		const String filename = filenames[i];
		if (!string_ends_with(filename, SCLIT(".odin"))) {
			continue;
		}
		const String path = path_cat(pathname, filename, context);
		sched_queue(&build->sched, build_add_work(build, package, path), build_worker, 0);
	}
}

static String build_find_collection(BuildContext *build, String name) {
	const Size n_collections = array_size(build->collections);
	for (Size i = 0; i < n_collections; i++) {
		const Collection *const collection = &build->collections[i];
		if (string_compare(collection->name, name)) {
			return collection->path;
		}
	}
	return STRING_NIL;
}

static BuildWork *build_add_work(BuildContext *build, Package *package, String filename) {
	Context *const context = &build->context;
	Allocator *const allocator = &context->allocator;
	BuildWork *work = allocator_allocate(allocator, sizeof *work);
	work->build = build;
	work->package = package;
	work->tree = package_add_tree(package, filename);
	work->error = false;
	mutex_lock(&build->mutex);
	array_push(build->work, work);
	mutex_unlock(&build->mutex);
	return work;
}

static void build_worker(void *data, Context *context) {
	BuildWork *const work = CAST(BuildWork *, data);
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
			pathname = build_find_collection(work->build, import->collection);
			if (pathname.length == 0) {
				BUILD_ERROR(&import->location, "Cannot find package collection: '%.*s'", SFMT(import->collection));
			}
		}

		const String splice = path_cat(pathname, import->pathname, context);
		const String result = path_canonicalize(splice, context);

		build_add_package(work->build, result);
	}
}