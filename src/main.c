#include <stdio.h> // fprintf
#include <stdlib.h> // system
#include <string.h> // strcmp

#include "string.h" // String, SCLIT, SFMT
#include "project.h"
#include "context.h"
#include "sched.h"
#include "path.h"
#include "parser.h"
#include "dump.h"

typedef struct Command Command;

struct Command {
	String name;
	String description;
};

static const Command COMMANDS[] = {
	{ SLIT("dump-ast"), SLIT("dump the generated syntax tree to stdout.") },
};

String project_name(String path) {
	(void)path;
	return SCLIT("main");
}

static int usage(const char *app) {
	printf("Usage:\n");
	printf("\t%s command [arguments]\n", app);
	printf("Commands:\n");
	Size max = 0;
	for (Size i = 0; i < sizeof(COMMANDS)/sizeof *COMMANDS; i++) {
		const Command *command = &COMMANDS[i];
		if (command->name.length > max) max = command->name.length;
	}
	for (Size i = 0; i < sizeof(COMMANDS)/sizeof *COMMANDS; i++) {
		const Command *command = &COMMANDS[i];
		printf("\t%.*s%*c\t%.*s\n",
			SFMT(command->name),
			CAST(Sint32, max - command->name.length), ' ',
			SFMT(command->description));
	}
	printf("\n");
	printf("For further details on a command, invoke command help:\n");
	printf("\te.g. `%s build -help` or `%s help build\n", app, app);
	return 1;
}

typedef struct Work Work;
struct Work {
	Sched *sched;
	Project *project;
	Tree *tree;
};

static void worker(void *data, Context *context) {
	Work *work = CAST(Work *, data);
	if (!parse(work->tree, context)) {
		return;
	}

	// Search for all import statements to add the packages
	const Tree *const tree = work->tree;
	const Size n_statements = array_size(tree->statements);
	for (Size i = 0; i < n_statements; i++) {
		const Statement *const statement = tree->statements[i];
		if (statement->kind != STATEMENT_IMPORT) {
			continue;
		}

		const ImportStatement *const import = 
			RCAST(const ImportStatement *const, statement);

		// TODO(dweiler): Handle the import by putting it on the scheduler.
	}
}

static Bool dump_ast(String pathname) {
	Context context;
	allocator_init(&context.allocator, SCLIT("arena"));

	Project project;
	project_init(&project, SCLIT("test"), &context);

	Sched sched;
	sched_init(&sched, SCLIT("sync"), &context);

	Array(Work*) pending = array_make(&context);

	Package *package = project_add_package(&project, pathname);
	Array(String) files = path_list(pathname, &context);
	const Size n_files = array_size(files);
	for (Size i = 0; i < n_files; i++) {
		const String filename = files[i];
		if (string_ends_with(filename, SCLIT(".odin"))) {
			Work *work = allocator_allocate(&context.allocator, sizeof *work);
			work->sched = &sched;
			work->project = &project;
			work->tree = package_add_tree(package, path_cat(pathname, filename, &context));
			array_push(pending, work);
			sched_queue(&sched, work, worker, 0);
		}
	}

	// Wait for all the work to be complete.
	sched_wait(&sched);

	const Size n_pending = array_size(pending);
	for (Size i = 0; i < n_pending; i++) {
		Work *const work = pending[i];
		dump(work->tree);
		allocator_deallocate(&context.allocator, work);
	}

	sched_fini(&sched);
	project_fini(&project);
	allocator_fini(&context.allocator);

	return true;
}

int main(int argc, char **argv) {
	const char *app = argv[0];

	argc--;
	argv++;
	if (argc <= 1) {
		return usage(app);
	}

	Bool result = false;
	if (!strcmp(argv[0], "dump-ast")) {
		result = dump_ast(string_from_null(argv[1]));
	} else {
		return usage(app);
	}

	return result ? 1 : 0;
}
