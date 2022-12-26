#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen.h"
#include "tree.h"
#include "utility.h"
#include "string.h"

static Bool gen_value(Generator *generator, const Node *type, const Node *node, StrBuf *strbuf);
static Bool gen_node(Generator *generator, const Node *node, StrBuf *strbuf, Sint32 depth);

static Bool gen_padding(Generator *generator, Sint32 depth, StrBuf *strbuf) {
	(void)generator;
	for (Sint32 i = 0; i < depth; i++) {
		strbuf_put_rune(strbuf, '\t');
	}
	return true;
}

static Bool gen_identifier(Generator *generator, const Identifier *identifier, StrBuf *strbuf) {
	(void)generator;
	const String contents = identifier->contents;
	return strbuf_put_formatted(strbuf, "%.*s", SFMT(contents));
}

static Bool gen_type(Generator *generator, const Node *node, StrBuf *strbuf) {
	if (node->kind == NODE_PROCEDURE_TYPE) {
		const ProcedureType *procedure_type = &node->procedure_type;
		// TODO(dweiler): Return structure!?
		return gen_type(generator, procedure_type->results, strbuf);
	} else if (node->kind == NODE_FIELD_LIST) {
		const FieldList *field_list = &node->field_list;
		return gen_type(generator, field_list->fields[0], strbuf);
	} else if (node->kind == NODE_IDENTIFIER) {
		const Identifier *identifier = &node->identifier;
		return gen_identifier(generator, identifier, strbuf);
	}
	return false;
}

static Bool gen_name(Generator *generator, const Node *node, StrBuf *strbuf) {
	(void)generator;
	ASSERT(node->kind == NODE_IDENTIFIER);
	const String string = node->identifier.contents;
	strbuf_put_formatted(strbuf, "%.*s", SFMT(string));
	return true;
}

static Bool gen_expression(Generator *generator, const Expression *expression, StrBuf *strbuf, Sint32 depth);
static Bool gen_directive(Generator *generator, const Directive *directive, StrBuf *strbuf);

static Bool gen_call_expression(Generator *generator, const CallExpression *expression, StrBuf *strbuf, Sint32 depth) {
	(void)depth;

	const Node *operand = expression->operand;
	if (operand->kind == NODE_IDENTIFIER) {
		// Could be a type cast.
		if (string_compare(operand->identifier.contents, SCLIT("f32"))) {
			strbuf_put_formatted(strbuf, "CODIN_f16_to_f32");
		} else if (!gen_name(generator, operand, strbuf)) {
			return false;
		}
	} else if (operand->kind == NODE_EXPRESSION) {
		const Expression *expression = &operand->expression;
		if (expression->kind == EXPRESSION_SELECTOR) {
			// Can be one of:
			// 	package.procedure()
			//	object.procedure()
			//	object->procedure(object, ...)
			const SelectorExpression *selector = &expression->selector;
			ASSERT(selector->identifier->kind == NODE_IDENTIFIER);
			const Identifier *identifier = &selector->identifier->identifier;
			if (!gen_identifier(generator, identifier, strbuf)) {
				return false;
			}
		}
	} else if (operand->kind == NODE_DIRECTIVE) {
		return gen_directive(generator, &operand->directive, strbuf);
	} else {
		return false;
	}

	strbuf_put_rune(strbuf, '(');
	const Uint64 n_arguments = array_size(expression->arguments);
	for (Uint64 i = 0; i < n_arguments; i++) {
		const Node *node = expression->arguments[i];
		if (!gen_node(generator, node, strbuf, 0)) {
			return false;
		}
		if (i != n_arguments - 1) {
			strbuf_put_string(strbuf, SCLIT(", "));
		}
	}

	strbuf_put_rune(strbuf, ')');

	return true;
}

static Bool gen_unary_expression(Generator *generator, const UnaryExpression *expression, StrBuf *strbuf, Sint32 depth) {
	(void)depth;
	switch (expression->operation) {
	case OPERATOR_SUB:
		strbuf_put_string(strbuf, SCLIT("negi32("));
		if (!gen_node(generator, expression->operand, strbuf, 0)) {
			return false;
		}
		strbuf_put_rune(strbuf, ')');
		return true;
	default:
		return false;
	}
	UNREACHABLE();
}

static Bool gen_binary_instruction(Generator *generator, const BinaryExpression *expression, StrBuf *strbuf) {
	strbuf_put_formatted(strbuf, "addf16(");
	if (!gen_node(generator, expression->lhs, strbuf, 0)) return false;
	strbuf_put_string(strbuf, SCLIT(", "));
	if (!gen_node(generator, expression->rhs, strbuf, 0)) return false;
	strbuf_put_formatted(strbuf, ")");
	return true;
}

static Bool gen_binary_expression(Generator *generator, const BinaryExpression *expression, StrBuf *strbuf, Sint32 depth) {
	(void)depth;
	return gen_binary_instruction(generator, expression, strbuf);
}

static Bool gen_expression(Generator *generator, const Expression *expression, StrBuf *strbuf, Sint32 depth) {
	switch (expression->kind) {
	case EXPRESSION_UNARY:
		return gen_unary_expression(generator, &expression->unary, strbuf, depth);
	case EXPRESSION_BINARY:
		return gen_binary_expression(generator, &expression->binary, strbuf, depth);
	case EXPRESSION_CALL:
		return gen_call_expression(generator, &expression->call, strbuf, depth);
	default:
		printf("Unimplemented expression\n");
		return false;
	}
	UNREACHABLE();
}

static Bool gen_expression_statement(Generator *generator, const ExpressionStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(generator, depth, strbuf);
	const Node *node = statement->expression;
	ASSERT(node->kind == NODE_EXPRESSION);
	if (!gen_expression(generator, &node->expression, strbuf, depth)) return false;
	strbuf_put_rune(strbuf, ';');
	return true;
}

static Bool gen_if_statement(Generator *generator, const IfStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(generator, depth, strbuf);
	strbuf_put_string(strbuf, SCLIT("if ("));
	if (!gen_node(generator, statement->condition, strbuf, 0)) {
		return false;
	}
	strbuf_put_string(strbuf, SCLIT(") "));
	if (!gen_node(generator, statement->body, strbuf, depth)) {
		return false;
	}
	if (statement->elif) {
		strbuf_put_string(strbuf, SCLIT(" else "));
		if (!gen_node(generator, statement->elif, strbuf, depth)) {
			return false;
		}
	}
	return true;
}

static Bool gen_return_statement(Generator *generator, const ReturnStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(generator, depth, strbuf);
	strbuf_put_string(strbuf, SCLIT("return "));
	if (!gen_node(generator, statement->results[0], strbuf, 0)) {
		return false;
	}
	strbuf_put_rune(strbuf, ';');
	strbuf_put_rune(strbuf, '\n');
	return true;
}

static Bool gen_declaration_statement(Generator *generator, const DeclarationStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(generator, depth, strbuf);
	const Uint64 n_decls = array_size(statement->names);
	const Uint64 n_values = array_size(statement->values);
	const Node *type = statement->type;

	for (Uint64 i = 0; i < n_decls; i++) {
		const Node *name = statement->names[i];
		if (type) {
			if (!gen_type(generator, type, strbuf)) {
				return false;
			}
			strbuf_put_rune(strbuf, ' ');
		}

		// Generate return type for procedure.
		const Node *value = i < n_values ? statement->values[i] : 0;
		if (value && value->kind == NODE_PROCEDURE) {
			const Procedure *procedure = &value->procedure;
			if (!gen_type(generator, procedure->type, strbuf)) {
				return false;
			}
			strbuf_put_rune(strbuf, ' ');
		}

		// Generate name.
		if (!gen_name(generator, name, strbuf)) {
			return false;
		}

		if (value) {
			// Generate procedure parameter list.
			if (value->kind == NODE_PROCEDURE) {
				strbuf_put_rune(strbuf, '(');
				strbuf_put_rune(strbuf, ')');
				strbuf_put_rune(strbuf, ' ');
			} else {
				strbuf_put_string(strbuf, SCLIT(" = "));
			}

		
			if (!gen_value(generator, type, value, strbuf)) {
				return false;
			}

			if (value->kind != NODE_PROCEDURE) {
				strbuf_put_rune(strbuf, ';');
			}
		} else {
			strbuf_put_rune(strbuf, ';');
		}

		strbuf_put_rune(strbuf, '\n');

		if (i != n_decls - 1) {
			gen_padding(generator, depth, strbuf);
		}
	}

	return true;
}

static Bool gen_statement(Generator *generator, const Statement *statement, StrBuf *strbuf, Sint32 depth);
static Bool gen_block_statement(Generator *generator, const BlockStatement *statement, StrBuf *strbuf, Sint32 depth) {
	strbuf_put_rune(strbuf, '{');
	strbuf_put_rune(strbuf, '\n');
	const Uint64 n_statements = array_size(statement->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *node = statement->statements[i];
		ASSERT(node->kind == NODE_STATEMENT);
		if (!gen_statement(generator, &node->statement, strbuf, depth + 1)) {
			return false;
		}
		if (i != n_statements - 1) {
			strbuf_put_rune(strbuf, '\n');
		}
	}
	gen_padding(generator, depth, strbuf);
	strbuf_put_rune(strbuf, '}');
	return true;
}

static Bool gen_statement(Generator *generator, const Statement *statement, StrBuf *strbuf, Sint32 depth) {
	switch (statement->kind) {
	case STATEMENT_EXPRESSION:
		return gen_expression_statement(generator, &statement->expression, strbuf, depth);
	case STATEMENT_DECLARATION:
		return gen_declaration_statement(generator, &statement->declaration, strbuf, depth);
	case STATEMENT_IF:
		return gen_if_statement(generator, &statement->if_, strbuf, depth);
	case STATEMENT_RETURN:
		return gen_return_statement(generator, &statement->return_, strbuf, depth);
	case STATEMENT_BLOCK:
		return gen_block_statement(generator, &statement->block, strbuf, depth);
	default:
		printf("Unimplemented statement\n");
		return false;
	}
	UNREACHABLE();
}

static Bool gen_literal_value(Generator *generator, const Node *type, const LiteralValue *literal, StrBuf *strbuf) {
	(void)generator;
	switch (literal->literal) {
	case LITERAL_INTEGER:
		FALLTHROUGH();
	case LITERAL_FLOAT:
		// printf("TYPE = %p\n", type);
		if (type && type->kind == NODE_IDENTIFIER) {
			const String contents = type->identifier.contents;
			if (string_compare(contents, SCLIT("f16"))) {
				char *copy = string_to_null(literal->value);
				char *end;
				const Float32 value = strtof(copy, &end);
				free(copy);
				const Uint16 pattern = f32_to_f16(value);
				return strbuf_put_formatted(strbuf, "0x%04x /* %f */", pattern, value);
			}
		}
		return strbuf_put_string(strbuf, literal->value);
		break;
	case LITERAL_IMAGINARY:
		FALLTHROUGH();
	case LITERAL_RUNE:
		FALLTHROUGH();
	case LITERAL_STRING:
		return strbuf_put_string(strbuf, literal->value);
	default:
		printf("Unimplemented literal value\n");
		return false;
	}
	UNREACHABLE();
}

static Bool gen_directive(Generator *generator, const Directive *directive, StrBuf *strbuf) {
	switch (directive->kind) {
	case DIRECTIVE_LOAD:
		{
			// Find the call expression matching the load directive
			const Tree *tree = generator->tree;
			const Uint64 n_nodes = array_size(tree->nodes);
			for (Uint64 j = 0; j < n_nodes; j++) {
				const Node *find = tree->nodes[j];
				if (find->kind != NODE_EXPRESSION) {
					continue;
				}
				const Expression *expression = &find->expression;
				if (expression->kind != EXPRESSION_CALL) {
					continue;
				}
				const CallExpression *call = &expression->call;
				const Node *operand = call->operand;
				if (operand->kind == NODE_DIRECTIVE && &operand->directive == directive) {
					Array(Node *) arguments = call->arguments;
					if (array_size(arguments) == 2) {
					const Node *type = arguments[1];
					ASSERT(type->kind == NODE_IDENTIFIER);
					const String ident = type->identifier.contents;
					strbuf_put_rune(strbuf, '(');
					strbuf_put_string(strbuf, ident);
					strbuf_put_rune(strbuf, ')');
					} else {
						strbuf_put_string(strbuf, SCLIT("(void*)"));
					}
					strbuf_put_string(strbuf, SCLIT("(&CODIN_load_0[0])"));
					return true;
				}
			}
			return false;
		}
	default:
		printf("Unimplemented directive\n");
		return false;
	}
	UNREACHABLE();
}

static Bool gen_value(Generator *generator, const Node *type, const Node *node, StrBuf *strbuf) {
	if (node->kind == NODE_PROCEDURE) {
		const Procedure *procedure = &node->procedure;
		const Node *node = procedure->body;
		ASSERT(node->kind == NODE_STATEMENT);
		const Statement *statement = &node->statement;
		ASSERT(statement->kind == STATEMENT_BLOCK);
		return gen_block_statement(generator, &statement->block, strbuf, 0);
	} else if (node->kind == NODE_LITERAL_VALUE) {
		return gen_literal_value(generator, type, &node->literal_value, strbuf);
	} else if (node->kind == NODE_EXPRESSION) {
		return gen_expression(generator, &node->expression, strbuf, 0);
	}
	return false;
}

static Bool gen_node(Generator *generator, const Node *node, StrBuf *strbuf, Sint32 depth) {
	switch (node->kind) {
	case NODE_EXPRESSION:
		return gen_expression(generator, &node->expression, strbuf, depth);
	case NODE_STATEMENT:
		return gen_statement(generator, &node->statement, strbuf, depth);
	case NODE_IDENTIFIER:
		return gen_identifier(generator, &node->identifier, strbuf);
	case NODE_LITERAL_VALUE:
		return gen_literal_value(generator, 0, &node->literal_value, strbuf);
	case NODE_DIRECTIVE:
		return gen_directive(generator, &node->directive, strbuf);
	default:
		printf("Unimplemented node\n");
		return false;
	}
	UNREACHABLE();
}

typedef struct InstructionInfo InstructionInfo;

struct InstructionInfo {
	const char *name;
	const char *c_op;
	Sint32 bits;
};

static Bool gen_c0_int(IntInstruction instr, StrBuf *strbuf) {
	#define INT(ignore, name, c_op, bits) { (name), (c_op), (bits) },
	static const InstructionInfo INFOS[] = {
		#include "instructions.h"
	};

	const InstructionInfo info = INFOS[instr];
	if (info.bits == 128) {
		// TODO(dweiler): Implement 128-bit integer emulation.
		printf("128-bit integers not supported yet\n");
		return true;
	}

	strbuf_put_formatted(strbuf, "static FORCE_INLINE u%d %su%d(u%d lhs, u%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	strbuf_put_formatted(strbuf, "\treturn lhs %s rhs;\n", info.c_op);
	strbuf_put_formatted(strbuf, "}\n\n");

	// We can get wrapping signed integer arithmetic simply by casting to unsigned
	// and back since C and C++ now both requires 2s complement representation of
	// signed integers.
	strbuf_put_formatted(strbuf, "static FORCE_INLINE i%d %si%d(i%d lhs, i%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	strbuf_put_formatted(strbuf, "\treturn %su%d(lhs, rhs);\n", info.name, info.bits);
	strbuf_put_formatted(strbuf, "}\n\n");

	return true;
}

static Bool gen_c0_flt(FltInstruction instr, StrBuf *strbuf) {
	#define FLT(ignore, name, c_op, bits) { (name), (c_op), (bits) },
	static const InstructionInfo INFOS[] = {
		#include "instructions.h"
	};

	const InstructionInfo info = INFOS[instr];
	if (info.bits == 16) {
		// Special behavior needed to emulate half-float
		strbuf_put_formatted(strbuf, "static FORCE_INLINE f%d %sf%d(f%d lhs, f%d rhs) {\n",
			info.bits, info.name, info.bits, info.bits, info.bits);
		strbuf_put_string(strbuf, SCLIT("\tconst f32 a = CODIN_f16_to_f32(lhs);\n"));
		strbuf_put_string(strbuf, SCLIT("\tconst f32 b = CODIN_f16_to_f32(rhs);\n"));
		strbuf_put_formatted(strbuf, "\treturn CODIN_f32_to_f16(a %s b);\n", info.c_op);
		strbuf_put_string(strbuf, SCLIT("}\n\n"));
	} else {
		strbuf_put_formatted(strbuf, "static FORCE_INLINE f%d %sf%d(f%d lhs, f%d rhs) {\n",
			info.bits, info.name, info.bits, info.bits, info.bits);
		strbuf_put_formatted(strbuf, "\treturn lhs %s rhs;\n", info.c_op);
		strbuf_put_string(strbuf, SCLIT("}\n\n"));
	}

	return true;
}

static Bool gen_c0_cmp(CmpInstruction instr, StrBuf *strbuf) {
	#define CMP(ignore, name, c_op, bits) { (name), (c_op), (bits) },
	static const InstructionInfo INFOS[] = {
		#include "instructions.h"
	};

	const InstructionInfo info = INFOS[instr];

	strbuf_put_formatted(strbuf, "static FORCE_INLINE u%d %su%d(u%d lhs, u%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	strbuf_put_formatted(strbuf, "\treturn lhs %s rhs;\n", info.c_op);
	strbuf_put_formatted(strbuf, "}\n\n");

	// In 2s complement representation comparisons are the same instruction.
	strbuf_put_formatted(strbuf, "static FORCE_INLINE i%d %si%d(i%d lhs, i%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	strbuf_put_formatted(strbuf, "\treturn %su%d(lhs, rhs);\n", info.name, info.bits);
	strbuf_put_formatted(strbuf, "}\n\n");

	return true;
}

static Bool gen_c0_int_prelude(Uint64 used, StrBuf *strbuf) {
	for (Uint64 i = 0; i < INSTR_INT_COUNT; i++) {
		if (used & (CAST(Uint64, 1) << i)) {
			if (!gen_c0_int(CAST(IntInstruction, i), strbuf)) {
				return false;
			}
		}
	}
	return true;
}

static Bool gen_c0_flt_prelude(Uint64 used, StrBuf *strbuf) {
	for (Uint64 i = 0; i < INSTR_FLT_COUNT; i++) {
		if (used & (CAST(Uint64, 1) << i)) {
			if (!gen_c0_flt(CAST(FltInstruction, i), strbuf)) {
				return false;
			}
		}
	}
	return true;
}

// Checks if any f16 instructions are used.
static Bool uses_f16_instructions(Generator *generator) {
	#define FLT(ignore0, ignore1, ignore2, bits) bits,
	static const Uint8 FLT_INSTR_SIZES[] = {
		#include "instructions.h"
	};
	for (Uint64 i = 0; i < INSTR_FLT_COUNT; i++) {
		if (generator->used_flt & (CAST(Uint64, 1) << i)) {
			if (FLT_INSTR_SIZES[i] == 16) {
				return true;
			}
		}
	}
	return false;
}

static Bool gen_c0_f16_prelude(StrBuf *strbuf) {
	// Generate the fp16 conversion routines.
	Uint32 base[512];
	Uint8 shift[512];
	for (Sint32 i = 0, e = 0; i < 256; ++i) {
		e = i - 127;
		if (e < -24) {
			base[i|0x000] = 0x0000;
			base[i|0x100] = 0x8000;
			shift[i|0x000] = 24;
			shift[i|0x100] = 24;
		} else if (e < -14) {
			base[i|0x000] = (0x0400 >> (-e - 14));
			base[i|0x100] = (0x0400 >> (-e - 14)) | 0x8000;
			shift[i|0x000] = -e - 1;
			shift[i|0x100] = -e - 1;
		} else if (e <= 15) {
			base[i|0x000] = ((e + 15) << 10);
			base[i|0x100] = ((e + 15) << 10) | 0x8000;
			shift[i|0x000] = 13;
			shift[i|0x100] = 13;
		} else if (e < 128) {
			base[i|0x000] = 0x7c00;
			base[i|0x100] = 0xfc00;
			shift[i|0x000] = 24;
			shift[i|0x100] = 24;
		} else {
			base[i|0x000] = 0x7c00;
			base[i|0x100] = 0xfc00;
			shift[i|0x000] = 13;
			shift[i|0x100] = 13;
		}
	}

	enum {
		MAX_BASE_COLUMNS  = 5,
		MAX_SHIFT_COLUMNS = 13
	};

	// Write out the base table.
	strbuf_put_string(strbuf, SCLIT("static const u32 CODIN_f16_base[] = {\n"));
	strbuf_put_rune(strbuf, '\t');
	for (Uint64 i = 0;; i++) {
		const Uint64 word = base[i];
		strbuf_put_formatted(strbuf, "0x%08x", word);
		const Bool nl = (i + 1) % MAX_BASE_COLUMNS == 0;
		const Bool lb = i == 512 - 1;
		if (lb) {
			strbuf_put_rune(strbuf, '\n');
			break;
		}
		strbuf_put_rune(strbuf, ',');
		if (!nl) {
			strbuf_put_rune(strbuf, ' ');
		} else {
			strbuf_put_rune(strbuf, '\n');
			strbuf_put_rune(strbuf, '\t');
		}
	}
	strbuf_put_string(strbuf, SCLIT("};\n\n"));

	// Write out the shift table.
	strbuf_put_string(strbuf, SCLIT("static const u8 CODIN_f16_shift[] = {\n"));
	strbuf_put_rune(strbuf, '\t');
	for (Uint64 i = 0;; i++) {
		const Uint8 byte = shift[i];
		strbuf_put_formatted(strbuf, "0x%02x", byte);
		const Bool nl = (i + 1) % MAX_SHIFT_COLUMNS == 0;
		const Bool lb = i == 512 - 1;
		if (lb) {
			strbuf_put_rune(strbuf, '\n');
			break;
		}
		strbuf_put_rune(strbuf, ',');
		if (!nl) {
			strbuf_put_rune(strbuf, ' ');
		} else {
			strbuf_put_rune(strbuf, '\n');
			strbuf_put_rune(strbuf, '\t');
		}
	}
	strbuf_put_string(strbuf, SCLIT("};\n\n"));

	strbuf_put_string(strbuf, SCLIT("static FORCE_INLINE f16 CODIN_f32_to_f16(f32 f) {\n"));
	strbuf_put_string(strbuf, SCLIT("\tconst union { f32 f; u32 u; } s = { f };\n"));
	strbuf_put_string(strbuf, SCLIT("\tconst u32 e = s.u >> 23;\n"));
	strbuf_put_string(strbuf, SCLIT("\tconst u32 base = CODIN_f16_base[e & 0x1ff];\n"));
	strbuf_put_string(strbuf, SCLIT("\tconst u8 shift = CODIN_f16_shift[e & 0x1ff];\n"));
	strbuf_put_string(strbuf, SCLIT("\treturn base + ((s.u & 0x007fffff) >> shift);\n"));
	strbuf_put_string(strbuf, SCLIT("}\n\n"));

	static const union { Uint32 u; float f; } MAGIC = { 113 << 23 };
	strbuf_put_string(strbuf, SCLIT("static FORCE_INLINE f32 CODIN_f16_to_f32(f16 f) {\n"));
	strbuf_put_string(strbuf, SCLIT("\tunion { u32 u; f32 f; } o = { (u32)(f & 0x7fff) << 13 };\n"));
	strbuf_put_formatted(strbuf, "\tconst u32 exp = 0x%08x & o.u;\n", 0x7c00 << 13);
	strbuf_put_formatted(strbuf, "\to.u += 0x%08x;\n", (127 - 15) << 23);
	strbuf_put_formatted(strbuf, "\tif (exp == 0x%08x) o.u += 0x%08x;\n", 0x7c00 << 13, (128 - 16) << 23);
	strbuf_put_formatted(strbuf, "\tif (exp == 0x%08x) o.u += 0x%08x, o.f -= %a;\n", MAGIC.f);
	strbuf_put_formatted(strbuf, "\to.u |= (f & 0x8000) << 16;\n");
	strbuf_put_formatted(strbuf, "\treturn o.f;\n");
	strbuf_put_string(strbuf, SCLIT("}\n\n"));

	return true;
}

static Bool gen_c0_prelude(Generator *generator, StrBuf *strbuf) {
	// Some types in Odin are easily representable with C types.
	static const struct {
		const char *odin;
		const char *c;
	} TYPES[] = {
		{ "b8",      "unsigned char"      },
		{ "b16",     "unsigned short"     },
		{ "b32",     "unsigned int"       },
		{ "b64",     "unsigned long long" },

		{ "i8",      "signed char"        },
		{ "i16",     "signed short"       },
		{ "i32",     "signed int"         },
		{ "i64",     "signed long long"   },

		{ "u8",      "unsigned char"      },
		{ "u16",     "unsigned short"     },
		{ "u32",     "unsigned int"       },
		{ "u64",     "unsigned long long" },

		{ "f16",     "unsigned short"     },
		{ "f32",     "float"              },
		{ "f64",     "double"             },
	
		{ "cstring", "const char*"        },

		{ "rawptr",  "void*"              },
	};

	// Hack for now
	strbuf_put_string(strbuf, SCLIT("#include <stdio.h>\n\n"));

	// Emit the typedefs for mapping Odin types to C ones.
	for (Uint64 i = 0; i < sizeof(TYPES)/sizeof(*TYPES); i++) {
		strbuf_put_formatted(strbuf, "typedef %s %s;\n",
			TYPES[i].c, TYPES[i].odin);
	}

	strbuf_put_rune(strbuf, '\n');
	strbuf_put_string(strbuf, SCLIT("#if defined(_MSC_VER)\n"));
	strbuf_put_string(strbuf, SCLIT("	#define FORCE_INLINE __forceinline\n"));
	strbuf_put_string(strbuf, SCLIT("#else\n"));
	strbuf_put_string(strbuf, SCLIT("	#define FORCE_INLINE __attribute__((always_inline)) inline\n"));
	strbuf_put_string(strbuf, SCLIT("#endif\n"));
	strbuf_put_rune(strbuf, '\n');

	if (uses_f16_instructions(generator)) {
		gen_c0_f16_prelude(strbuf);
	}

	gen_c0_int_prelude(generator->used_int, strbuf);
	gen_c0_flt_prelude(generator->used_flt, strbuf);
	// gen_c0_cmp_prelude(generator->used_cmp, strbuf);
	// gen_c0_rel_prelude(generator->used_rel, strbuf);
	// gen_c0_bit_prelude(generator->used_bit, strbuf);

	return true;
}

static Bool gen_load_directive_prelude(Generator *generator, const CallExpression *call, StrBuf *strbuf) {
	// Should have at least one argument.
	Array(Node*) args = call->arguments;
	ASSERT(array_size(args) >= 1);

	const Node *file = args[0];

	// The other optional argument is a type like
	// 	string
	//	[]T
	//
	// This cast is done on the generative side, but if we encounter a cstring
	// type we need to add a NUL terminator to the data.
	Bool is_cstring = false;
	if (array_size(args) > 1) {
		const Node *type = args[1];
		ASSERT(type->kind == NODE_IDENTIFIER);
		const String ident = type->identifier.contents;
		is_cstring = string_compare(ident, SCLIT("cstring"));
	}

	// The file argument should be a string literal.
	ASSERT(file->kind == NODE_LITERAL_VALUE);
	const LiteralValue *literal = &file->literal_value;
	ASSERT(literal->literal == LITERAL_STRING);

	Array(Uint8) contents = readfile(literal->value);
	if (!contents) {
		return false;
	}

	// Ensure we have a NUL byte for cstring.
	if (is_cstring) {
		array_push(contents, 0);
	}

	Uint64 id = generator->load_directive_id++;
	strbuf_put_string(strbuf, SCLIT("// Loaded from "));
	strbuf_put_string(strbuf, literal->value);
	strbuf_put_rune(strbuf, '\n');
	strbuf_put_formatted(strbuf, "static const unsigned char CODIN_load_%d[] = {\n", id);

	// Limit the generated output to 80 columns by limiting the number of byte
	// columns to 13 max.
	enum { MAX_COLUMNS = 13 };
	strbuf_put_rune(strbuf, '\t');
	const Uint64 n_bytes = array_size(contents);
	for (Uint64 i = 0;; i++) {
		const Uint8 byte = contents[i];
		strbuf_put_formatted(strbuf, "0x%02x", byte);
		const Bool nl = (i + 1) % MAX_COLUMNS == 0;
		const Bool lb = i == n_bytes - 1;
		if (lb) {
			strbuf_put_rune(strbuf, '\n');
			break;
		}
		strbuf_put_rune(strbuf, ',');
		if (!nl) {
			strbuf_put_rune(strbuf, ' ');
		} else {
			strbuf_put_rune(strbuf, '\n');
			strbuf_put_rune(strbuf, '\t');
		}
	}

	strbuf_put_string(strbuf, SCLIT("};\n\n"));

	array_free(contents);

	return true;
}

static Bool gen_load_directives_prelude(Generator *generator, StrBuf *strbuf) {
	const Tree *tree = generator->tree;
	const Uint64 n_nodes = array_size(tree->nodes);
	for (Uint64 i = 0; i < n_nodes; i++) {
		const Node *node = tree->nodes[i];
		if (node->kind != NODE_DIRECTIVE || node->directive.kind != DIRECTIVE_LOAD) {
			continue;
		}
		// Search all nodes to find the call expression which references this load directive.
		for (Uint64 j = 0; j < n_nodes; j++) {
			const Node *find = tree->nodes[j];
			if (find->kind != NODE_EXPRESSION) {
				continue;
			}
			const Expression *expression = &find->expression;
			if (expression->kind != EXPRESSION_CALL) {
				continue;
			}
			if (expression->call.operand == node) {
				gen_load_directive_prelude(generator, &expression->call, strbuf);
				break;
			}
		}
	}
	return true;
}

Bool gen_init(Generator *generator, const Tree *tree) {
	generator->tree = tree;
	// HACK(dweiler): Use all instructions for now.
	generator->used_int = 0;
	generator->used_flt = 0;
	generator->used_cmp = 0;
	generator->used_rel = 0;
	generator->used_bit = 0;
	generator->used_flt |= (1 << INSTR_ADDF16);
	return true;
}

Bool gen_run(Generator *generator, StrBuf *strbuf) {
	gen_c0_prelude(generator, strbuf);

	generator->load_directive_id = 0;
	if (!gen_load_directives_prelude(generator, strbuf)) {
		return false;
	}

	const Tree *tree = generator->tree;
	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		if (!gen_node(generator, tree->statements[i], strbuf, 0)) {
			return false;
		}
	}

	return true;
}