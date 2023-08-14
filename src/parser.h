#ifndef CODIN_PARSER_H
#define CODIN_PARSER_H
#include "string.h"

typedef struct Context Context;
typedef struct Tree Tree;

Bool parse(Tree *tree, Context *context);

#endif // CODIN_PARSER_H