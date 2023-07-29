#include <stdio.h> // fprintf
#include <stdlib.h> // system
#include <string.h> // strcmp

#include "string.h" // String, SCLIT, SFMT
#include "path.h"
#include "tree.h"
#include "parser.h"
#include "context.h"
#include "dump.h"

typedef struct Command Command;

struct Command {
	String name;
	String description;
};

String project_name(String path) {
	(void)path;
	return SCLIT("main");
}

static int usage(const char *app) {
	static const Command COMMANDS[] = {
		{ SLIT("dump-ast"), SLIT("dump the generated syntax tree to stdout.") },
	};
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

static Bool dump_ast(Context *context, String file) {
	Tree *tree = parse(file, context);
	if (!tree) {
		return false;
	}

	dump(tree);

	return true;
}

int main(int argc, char **argv) {
	const char *app = argv[0];

	argc--;
	argv++;

	// Hack for now
	argc = 2;
	argv[0] = CCAST(char *, "dump-ast");
	argv[1] = CCAST(char *, "tests/main.odin");

	if (argc <= 1) {
		return usage(app);
	}

	Allocator *allocator = &DEFAULT_ALLOCATOR;

	Context context;
	context.allocator = allocator;

	if (setjmp(context.jmp) != 0) {
		allocator->finalize(allocator);
		return 1;
	}

	Bool result = false;
	if (!strcmp(argv[0], "dump-ast")) {
		result = dump_ast(&context, string_from_null(argv[1]));
	} else {
		return usage(app);
	}

	allocator->finalize(allocator);

	return result ? 1 : 0;
}