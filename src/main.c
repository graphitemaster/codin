#include <stdio.h> // fprintf
#include <stdlib.h> // system
#include <string.h> // strcmp

#include "string.h" // String, SCLIT, SFMT
#include "gen.h"
#include "path.h"
#include "parser.h"
#include "threadpool.h"

typedef struct Command Command;

struct Command {
	String name;
	String description;
};

static const Command COMMANDS[] = {
	{ SCLIT("build"),    SCLIT("compile directory of .odin files, as an executable\n\t\t\tone must contain the program's entry point, all must be the same package.") },
	{ SCLIT("run"),      SCLIT("same as 'build', but also runs the newly compiled executable.") },
	{ SCLIT("dump-ast"), SCLIT("dump the generated syntax tree to stdout.") },
	{ SCLIT("dump-c"),   SCLIT("dump the generated c to stdout.") },
};

String project_name(String path) {
	(void)path;
	return SCLIT("main");
}

static int usage(const char *app) {
	printf("Usage:\n");
	printf("\t%s command [arguments]\n", app);
	printf("Commands:\n");
	Uint64 max = 0;
	for (Uint32 i = 0; i < sizeof(COMMANDS)/sizeof(*COMMANDS); i++) {
		const Command *command = &COMMANDS[i];
		if (command->name.length > max) max = command->name.length;
	}
	for (Uint32 i = 0; i < sizeof(COMMANDS)/sizeof(*COMMANDS); i++) {
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

static Bool generate(const Tree *tree, StrBuf *strbuf) {
	Generator gen;
	if (!gen_init(&gen, tree)) {
		return false;
	}
	strbuf_init(strbuf);
	const Bool result = gen_run(&gen, strbuf, true);
	strbuf_free(strbuf);
	gen_free(&gen);
	return result;
}

static Bool transpile(String path) {
	Tree *tree = parse(path);
	if (!tree) {
		return false;
	}

	StrBuf strbuf;
	if (!generate(tree, &strbuf)) {
		fprintf(stderr, "Failed to generate C0\n");
		tree_free(tree);
		return false;
	}

	tree_free(tree);

	Uint64 slash = 0;
	if (!string_find_last_byte(path, '/', &slash)) {
		if (!string_find_last_byte(path, '\\', &slash)) {
			return false;
		}
	}
	Uint64 dot = 0;
	if (!string_find_last_byte(path, '.', &dot)) {
		return false;
	}
	
	const String name = string_slice(path, slash + 1, dot);
	StrBuf file;
	strbuf_init(&file);
	strbuf_put_formatted(&file, ".build/%.*s.c", SFMT(name));
	strbuf_put_rune(&file, '\0');

	// Ensure a build directory exists to shove the generated C0.
	path_mkdir(".build");

	const String filename = strbuf_result(&file);
	FILE *fp = fopen(CAST(const char *, filename.contents), "wb");
	if (!fp) {
		fprintf(stderr, "Failed to write C0\n");
		strbuf_free(&file);
		strbuf_free(&strbuf);
		return false;
	}
	strbuf_free(&file);

	const String source = strbuf_result(&strbuf);
	fwrite(source.contents, source.length, 1, fp);
	fclose(fp);

	return true;
}

static void transpile_worker(void *user) {
	const String *path = CAST(const String *, user);
	transpile(*path);
	string_free(*path);
}

static Bool build(const char *app, String path, Uint64 n_threads, Bool is_file) {
	StrBuf strbuf;
	strbuf_init(&strbuf);

	if (is_file) {
		// Transpile a single file.
		if (!transpile(path)) {
			return false;
		}
	} else {
		// Transpile many files.
		Array(String) files = path_list(path);
		const Uint64 n_files = array_size(files);
		if (n_files == 0) {
			fprintf(stderr, "ERROR: `%s build` takes a package as its first argument.\n", app);
			fprintf(stderr, "Did you mean `%s build %.*s -file`?\n", app, SFMT(path));
			fprintf(stderr, "The `-file` flag tells it to treat a file as a self-contained package.\n");
			return false;
		}
		ThreadPool pool;
		threadpool_init(&pool, n_threads);
		for (Uint64 i = 0; i < n_files; i++) {
			const String name = files[i];
			if (string_ends_with(name, SCLIT(".odin"))) {
				strbuf_put_formatted(&strbuf, "%.*s/%.*s", SFMT(path), SFMT(name));
				String *result = malloc(sizeof *result);
				*result = string_copy(strbuf_result(&strbuf));
				strbuf_clear(&strbuf);
				threadpool_queue(&pool, transpile_worker, result, free);
			}
		}
		threadpool_free(&pool);

		for (Uint64 i = 0; i < n_files; i++) {
			string_free(files[i]);
		}
		array_free(files);
	}

	// Generate one of the following commands to build the C0.
	//
	//	"gcc .build/*.c -O1 -o %s" or
	//	"cl.exe .build/*.c /O1 /OUT:%s"
	const String project = project_name(path);

#if defined(OS_WINDOWS)
	strbuf_put_formatted(&strbuf, "cl.exe .build/*.c /O1 /OUT:%.*s.exe >nul 2>nul", SFMT(project));
#elif defined(OS_LINUX)
	strbuf_put_formatted(&strbuf, "gcc .build/*.c -O1 -o %.*s.bin >/dev/null 2>/dev/null", SFMT(project));
#endif
	strbuf_put_rune(&strbuf, '\0');

	// Compile it.
	const String compile = strbuf_result(&strbuf);
	const Bool status = system(CAST(const char *, compile.contents)) == 0;
	strbuf_free(&strbuf);

	return status;
}

static Bool run(String path) {
	String project = project_name(path);
	StrBuf strbuf;
	strbuf_init(&strbuf);
#if defined(OS_WINDOWS)
	strbuf_put_formatted(&strbuf, ",\\%.*s.exe", SFMT(project));
#elif defined(OS_LINUX)
	strbuf_put_formatted(&strbuf, "./%.*s.bin", SFMT(project));
#endif
	strbuf_put_rune(&strbuf, '\0');
	const Bool result = system(CAST(const char *, strbuf.contents)) == 0;
	strbuf_free(&strbuf);
	return result;
}

static Bool dump_ast(String file) {
	Tree *tree = parse(file);
	if (tree) {
		tree_dump(tree);
		tree_free(tree);
		return true;
	}
	return false;
}

static Bool dump_c(String file) {
	Tree *tree = parse(file);
	if (!tree) {
		return false;
	}
	StrBuf strbuf;
	if (generate(tree, &strbuf)) {
		const Uint64 size = array_size(strbuf.contents);
		fwrite(strbuf.contents, size, 1, stdout);
		fflush(stdout);
		strbuf_free(&strbuf);
		tree_free(tree);
		return true;
	}
	tree_free(tree);
	return false;
}

int main(int argc, char **argv) {
	const char *app = argv[0];

	argc--;
	argv++;

	if (argc <= 1) {
		return usage(app);
	}

	const Bool file = argc >= 3 && !strcmp(argv[2], "-file");

	if (!strcmp(argv[0], "build")) {
		return build(app, string_from_null(argv[1]), 8, file) ? 0 : 1;
	} else if (!strcmp(argv[0], "run")) {
		return (build(app, string_from_null(argv[1]), 8, file) && run(string_from_null(argv[1]))) ? 0 : 1;
	} else if (!strcmp(argv[0], "dump-ast")) {
		return dump_ast(string_from_null(argv[1])) ? 0 : 1;
	} else if (!strcmp(argv[0], "dump-c")) {
		return dump_c(string_from_null(argv[1])) ? 0 : 1;
	}

	return usage(app);
}