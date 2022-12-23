#ifndef CODIN_GEN_H
#define CODIN_GEN_H
#include "strbuf.h"

typedef struct Tree Tree;
Bool gen(const Tree *tree, StrBuf *strbuf);

#endif // CODIN_GEN_H