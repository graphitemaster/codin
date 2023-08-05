#include <stdio.h> // fprintf
#include <stdlib.h> // system
#include <string.h> // strcmp

#include "string.h" // String, SCLIT, SFMT
#include "path.h"
#include "tree.h"
#include "parser.h"
#include "context.h"
#include "dump.h"
#include "strbuf.h"
#include "utility.h"
#include "sched.h"

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

typedef struct Work Work;

struct Work {
	Context *ctx;
	String file;
	Tree *tree;
};

static void worker(void *data) {
	Work *work = RCAST(Work *, data);
	work->tree = parse(work->file, work->ctx);
}

static Bool dump_ast(String path) {
	Context ctx;
	ctx.allocator = &DEFAULT_ALLOCATOR;

	Context *context = &ctx;

	Array(String) files = path_list(path);
	const Size n_files = array_size(files);

	Sched sched;
	sched_init(&sched, SCLIT("async"), &ctx);

	Array(Work) work = 0;

	StrBuf buf;
	strbuf_init(&buf, context);
	for (Size i = 0; i < n_files; i++) {
		strbuf_clear(&buf);
		strbuf_put_string(&buf, path);
		strbuf_put_rune(&buf, '/');
		strbuf_put_string(&buf, files[i]);
		const String file = strbuf_result(&buf);
		array_push(work, LIT(Work, &ctx, file, 0));
	}

	for (Size i = 0; i < n_files; i++) {
		sched_queue(&sched, &work[i], worker, 0);
	}

	sched_wait(&sched);

	for (Size i = 0; i < n_files; i++) {
		Tree *tree = work[i].tree;
		if (tree) {
			dump(tree);
		}
	}

	sched_fini(&sched);

	return true;
}

int main(int argc, char **argv) {
	const char *app = argv[0];

	argc--;
	argv++;

	// Hack for now
	argc = 2;
	argv[0] = CCAST(char *, "dump-ast");
	argv[1] = CCAST(char *, "tests");

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