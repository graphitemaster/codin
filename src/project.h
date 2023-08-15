#ifndef CODIN_PROJECT_H
#define CODIN_PROJECT_H
#include "tree.h"
#include "thread.h"

typedef struct Project Project;
typedef struct Package Package;
typedef struct Collection Collection;

struct Collection {
	String name;
	String path;
};

struct Project {
	Context *context;
	Mutex mutex;
	String name              THREAD_GUARDED(mutex);
	Array(Package*) packages THREAD_GUARDED(mutex);
};

void project_init(Project *project, String name, Context *context);
void project_fini(Project *project);
Package *project_add_package(Project *project, String pathname);

struct Package {
	Context *context;
	Mutex mutex;
	String pathname;
	Array(Tree*) trees THREAD_GUARDED(mutex);
};

void package_init(Package *package, String pathname, Context *context);
void package_fini(Package *package);
Tree *package_add_tree(Package *package, String filename);

#endif