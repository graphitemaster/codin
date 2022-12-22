#include <stdio.h>
#include <stdlib.h>

#include "parser.h"
#include "dump.h"

int usage(const char *app) {
	printf("Usage:\n");
	printf("\t%s command [arguments]\n", app);
	printf("Commands:\n");
	printf("\tbuild\n");
	printf("\trun\n");
	return 1;
}

int main(int argc, char **argv) {
	if (argc == 1) {
		return usage(argv[0]);
	}

	Tree *tree = parse("tests/main.odin");
	tree_dump(tree);
	tree_free(tree);

	return 0;
}