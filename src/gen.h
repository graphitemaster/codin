#ifndef CODIN_GEN_H
#define CODIN_GEN_H
#include "strbuf.h"

typedef struct Tree Tree;

typedef struct Generator Generator;

struct Generator {
  StrBuf buffer;
};

#endif // CODIN_GEN_H