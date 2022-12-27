#ifndef CODIN_GEN_H
#define CODIN_GEN_H
#include "strbuf.h"
#include "tree.h"

typedef struct Generator Generator;

typedef enum IntInstruction IntInstruction;
typedef enum FltInstruction FltInstruction;
typedef enum CmpInstruction CmpInstruction;
typedef enum RelInstruction RelInstruction;
typedef enum BitInstruction BitInstruction;

#define INT(name, ...) INSTR_ ## name,
enum IntInstruction {
	#include "instructions.h"
	INSTR_INT_COUNT,
};

#define FLT(name, ...) INSTR_ ## name,
enum FltInstruction {
	#include "instructions.h"
	INSTR_FLT_COUNT,
};

#define CMP(name, ...) INSTR_ ## name,
enum CmpInstruction {
	#include "instructions.h"
	INSTR_CMP_COUNT,
};

#define REL(name, ...) INSTR_ ## name,
enum RelInstruction {
	#include "instructions.h"
	INSTR_REL_COUNT,
};

#define BIT(name, ...) INSTR_ ## name,
enum BitInstruction {
	#include "instructions.h"
	INSTR_BIT_COUNT,
};

_Static_assert(INSTR_INT_COUNT <= 64, "Too many instructions");
_Static_assert(INSTR_FLT_COUNT <= 64, "Too many instructions");
_Static_assert(INSTR_CMP_COUNT <= 64, "Too many instructions");
_Static_assert(INSTR_REL_COUNT <= 64, "Too many instructions");
_Static_assert(INSTR_BIT_COUNT <= 64, "Too many instructions");

typedef struct Scope Scope;

struct Scope {
	Array(const DeferStatement*) defers;
};

struct Generator {
	const Tree *tree;
	Uint64 load_directive_id;

	// Bits indicating which instructions are used.
	Uint64 used_int;
	Uint64 used_flt;
	Uint64 used_cmp;
	Uint64 used_rel;
	Uint64 used_bit;

	Array(Scope*) scopes;
};

Bool gen_init(Generator *generator, const Tree *tree);
void gen_free(Generator *generator);
Bool gen_run(Generator *generator, StrBuf *strbuf);

#endif // CODIN_GEN_H