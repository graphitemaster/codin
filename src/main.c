#include <stdio.h>

#include "parser.h"
#include "tree.h"
#include "gen.h"

int main(int argc, char **argv) {
	(void)argc;
	(void)argv;
	Tree *tree = parse("tests/main.odin");
	
	StrBuf strbuf;
	strbuf.contents = 0;

	// tree_dump(tree);

	Generator gen;
	gen_init(&gen, tree);
	gen_run(&gen, &strbuf);

	const String string = strbuf_result(&strbuf);
	printf("%.*s\n",
		CAST(Sint32,       string.size),
		CAST(const char *, string.data));

	strbuf_free(&strbuf);
	tree_free(tree);

	return 0;
}