#include <stdio.h> // fprintf
#include <stdlib.h> // system
#include <string.h> // strcmp

#include "string.h" // String, SCLIT, SFMT
#include "path.h"
#include "tree.h"
#include "parser.h"
#include "context.h"
#include "dump.h"
#include "threadpool.h"
#include "strbuf.h"
#include "utility.h"

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
	Allocator *allocator;
	WaitGroup *wg;
	String file;
	Tree *tree;
	Float64 beg, end;
};

Work *work_create(String file, WaitGroup *wg, Context *context) {
	Allocator *allocator = context->allocator;
	Work *work = RCAST(Work *, allocator->allocate(allocator, sizeof *work));
	work->allocator = allocator;
	work->wg = wg;
	work->file = string_copy(file);
	work->tree = 0;
	return work;
}

void work_destroy(void *data) {
	Work *work = CAST(Work *, data);
	Allocator *allocator = work->allocator;
	allocator->deallocate(allocator, work);
}

static void worker(void *data) {
	Work *work = RCAST(Work *, data);

	Context context;
	context.allocator = work->allocator;
	if (setjmp(context.jmp) != 0) {
		work->tree = 0;
		waitgroup_signal(work->wg);
		return;
	}

	work->beg = qpc();
	work->tree = parse(work->file, &context);
	work->end = qpc();

	waitgroup_signal(work->wg);
}

static Bool dump_ast(String path) {
	Context ctx;
	ctx.allocator = &DEFAULT_ALLOCATOR;

	Context *context = &ctx;

	Array(String) files = path_list(path);

	ThreadPool pool;
	if (!threadpool_init(&pool, 4, context)) {
		return false;
	}

	const Size n_files = array_size(files);

	WaitGroup wg;
	waitgroup_init(&wg, n_files);

	Array(Work*) works = 0;

	StrBuf buf;
	strbuf_init(&buf, context);
	for (Size i = 0; i < n_files; i++) {
		strbuf_clear(&buf);

		strbuf_put_string(&buf, path);
		strbuf_put_rune(&buf, '/');
		strbuf_put_string(&buf, files[i]);
	
		const String file = strbuf_result(&buf);
		Work *work = work_create(file, &wg, context);
		array_push(works, work);
		threadpool_queue(&pool, worker, work, 0);
	}

	// Wait for everything to parse.
	waitgroup_wait(&wg);
	waitgroup_destroy(&wg);

	for (Size i = 0; i < n_files; i++) {
		Work *work = works[i];
		if (work->tree) {
			printf("Took %.54g sec\n", work->end - work->beg);
			dump(work->tree);
			work->tree->context = context;
			// infer(work->tree);
		}
		work_destroy(work);
	}

	threadpool_free(&pool);

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