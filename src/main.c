#include "parser.h"
#include "tree.h"

int main(int argc, char **argv) {
	(void)argc;
	(void)argv;
	Tree *tree = parse("tests/main.odin");
	tree_dump(tree);
	tree_free(tree);
	return 0;
}