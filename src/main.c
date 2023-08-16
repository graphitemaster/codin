#include <stdio.h>

#include "string.h" // String, SCLIT, SFMT
#include "build.h"
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


static Bool dump_ast(String pathname) {
	Bool result = false;
	BuildContext build;
	build_init(&build, SCLIT("arena"), SCLIT("async"), SCLIT("spall"));
	// build_init(&build, SCLIT("arena"), SCLIT("sync"));
	build_add_collection(&build, SCLIT("core"), SCLIT("/home/graphitemaster/work/EmberGen/Odin/core"));
	build_add_collection(&build, SCLIT("vendor"), SCLIT("/home/graphitemaster/work/EmberGen/Odin/vendor"));
	build_add_package(&build, pathname);
	build_wait(&build);

	mutex_lock(&build.mutex);
	const Size n_work = array_size(build.work);
	for (Size i = 0; i < n_work; i++) {
		const BuildWork *const work = build.work[i];
		if (work->error) {
			goto L_error;
		}
		// dump(work->tree);
	}

	result = true;

L_error:
	mutex_unlock(&build.mutex);
	build_fini(&build);
	return result;
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

	return result ? 0 : 1;
}
