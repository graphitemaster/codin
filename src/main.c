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
#include "allocator.h"

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
	Context *ctx;
	String file;
	Tree *tree;
};

static void worker(void *data, Context *context) {
	Work *work = RCAST(Work *, data);
	work->tree = parse(work->file, context);
}

static Bool dump_ast(String path) {
	Context ctx;
	allocator_init(&ctx.allocator, SCLIT("arena"));

	Context *context = &ctx;

	Array(String) files = path_list(path, context);
	const Size n_files = array_size(files);

	Sched sched;
	sched_init(&sched, SCLIT("async"), &ctx);

	Array(Work) work = array_make(context);

	StrBuf buf;
	strbuf_init(&buf, context);
	for (Size i = 0; i < n_files; i++) {
		const String name = files[i];
		if (!string_ends_with(name, SCLIT(".odin"))) {
			continue;
		}
		strbuf_clear(&buf);
		strbuf_put_string(&buf, path);
		if (!string_ends_with(path, SCLIT("/"))) {
			strbuf_put_rune(&buf, '/');
		}
		strbuf_put_string(&buf, name);
		const String file = strbuf_result(&buf);
		array_push(work, LIT(Work, &ctx, string_copy(file), 0));
	}

	const Size n_work = array_size(work);
	for (Size i = 0; i < n_work; i++) {
		sched_queue(&sched, &work[i], worker, 0);
	}

	sched_wait(&sched);

	for (Size i = 0; i < n_work; i++) {
		Tree *tree = work[i].tree;
		if (tree) {
			// dump(tree);
		}
	}

	sched_fini(&sched);

	allocator_fini(&ctx.allocator);

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