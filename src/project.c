#include "project.h"
#include "allocator.h"
#include "context.h"

void project_init(Project *project, String name, Context *context)
	THREAD_INTERNAL
{
	mutex_init(&project->mutex);
	project->context = context;
	project->name = name;
	project->packages = array_make(context);
}

void project_fini(Project *project) {
	Context *const context = project->context;

	PROF_ENTER();

	mutex_lock(&project->mutex);

	const Size n_packages = array_size(project->packages);
	for (Size i = 0; i < n_packages; i++) {
		Package *const package = project->packages[i];
		package_fini(package);
		allocator_deallocate(&context->allocator, package);
	}

	array_free(project->packages);

	mutex_unlock(&project->mutex);
	mutex_fini(&project->mutex);

	PROF_LEAVE();
}

Package *project_add_package_internal(Project *project, String pathname, Context *context) {
	PROF_ENTER();

	mutex_lock(&project->mutex);

	const Size n_packages = array_size(project->packages);
	for (Size i = 0; i < n_packages; i++) {
		Package *const package = project->packages[i];
		if (string_compare(package->pathname, pathname)) {
			mutex_unlock(&project->mutex);
			PROF_LEAVE();
			return 0;
		}
	}

	Package *package = allocator_allocate(&context->allocator, sizeof *package);
	package_init(package, pathname, context);

	array_push(project->packages, package);

	mutex_unlock(&project->mutex);

	PROF_LEAVE();

	return package;
}

Package *project_add_package(Project *project, String pathname) {
	return project_add_package_internal(project, pathname, project->context);
}

void package_init(Package *package, String pathname, Context *context)
	THREAD_INTERNAL
{
	package->context = context;
	mutex_init(&package->mutex);
	package->pathname = pathname;
	package->trees = array_make(context);
}

void package_fini(Package *package) {
	Context *const context = package->context;

	PROF_ENTER();

	mutex_lock(&package->mutex);
	const Size n_trees = array_size(package->trees);
	for (Size i = 0; i < n_trees; i++) {
		Tree *const tree = package->trees[i];
		tree_fini(tree);
		allocator_deallocate(&context->allocator, tree);
	}
	array_free(package->trees);
	mutex_unlock(&package->mutex);

	mutex_fini(&package->mutex);

	PROF_LEAVE();
}

Tree *package_add_tree(Package *package, String filename) {
	Context *const context = package->context;

	PROF_ENTER();

	mutex_lock(&package->mutex);

	Tree *tree = allocator_allocate(&context->allocator, sizeof *tree);
	tree->source.name = filename;

	array_push(package->trees, tree);
	mutex_unlock(&package->mutex);

	PROF_LEAVE();

	return tree;
}