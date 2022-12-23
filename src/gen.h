#ifndef CODIN_GEN_H
#define CODIN_GEN_H
#include "strbuf.h"

typedef struct Tree Tree;

typedef struct Generator Generator;

typedef enum Instruction Instruction;

#define INSTRUCTION(enumerator, name) INSTRUCTION_ ## enumerator,
enum Instruction {
	#include "instructions.h"
	INSTRUCTION_COUNT,
};

struct Generator {
	const Tree *tree;
	Uint64 used[(INSTRUCTION_COUNT + 63) / 64];
};

Bool gen_init(Generator *generator, const Tree *tree);
Bool gen_run(Generator *generator, StrBuf *strbuf);

#endif // CODIN_GEN_H