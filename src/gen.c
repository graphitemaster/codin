#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "gen.h"
#include "tree.h"

// Mark an instruction for use to know what prelude to generate.
static void use(Generator *generator, Instruction instruction) {
	generator->used[instruction >> 6] |= 1 << (instruction & 63);
}

static Bool uses(const Generator *generator, Instruction instruction) {
	return generator->used[instruction >> 6] & (1 << (instruction & 63));
}

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
	return strbuf_put_formatted(strbuf, "%.*s",
		CAST(Sint32,       contents.size),
		CAST(const char *, contents.data));
}

static Bool gen_type(Generator *generator, const Node *node, StrBuf *strbuf) {
	if (node->kind == NODE_FIELD_LIST) {
		const FieldList *field_list = &node->field_list;
		gen_type(generator, field_list->fields[0], strbuf);
	} else if (node->kind == NODE_IDENTIFIER) {
		const Identifier *identifier = &node->identifier;
		gen_identifier(generator, identifier, strbuf);
	}
	return false;
}

static Bool gen_name(Generator *generator, const Node *node, StrBuf *strbuf) {
	(void)generator;
	ASSERT(node->kind == NODE_IDENTIFIER);
	const String string = node->identifier.contents;
	strbuf_put_formatted(strbuf, "%.*s",
		CAST(Sint32, string.size),
		CAST(const char*, string.data));
	return false;
}

static Bool gen_expression(Generator *generator, const Expression *expression, StrBuf *strbuf, Sint32 depth);

static Bool gen_call_expression(Generator *generator, const CallExpression *expression, StrBuf *strbuf, Sint32 depth) {
	(void)depth;

	const Node *operand = expression->operand;
	if (operand->kind == NODE_IDENTIFIER) {
		gen_name(generator, operand, strbuf);
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
			gen_identifier(generator, identifier, strbuf);
		}
	} else {
		abort();
	}

	strbuf_put_rune(strbuf, '(');
	const Uint64 n_arguments = array_size(expression->arguments);
	for (Uint64 i = 0; i < n_arguments; i++) {
		const Node *node = expression->arguments[i];
		gen_node(generator, node, strbuf, 0);
		if (i != n_arguments - 1) {
			strbuf_put_string(strbuf, SLIT(", "));
		}
	}
	strbuf_put_rune(strbuf, ')');

	return false;
}

static Bool gen_unary_expression(Generator *generator, const UnaryExpression *expression, StrBuf *strbuf, Sint32 depth) {
	switch (expression->operation) {
	case OPERATOR_SUB:
		use(generator, INSTRUCTION_NEGI32);
		strbuf_put_string(strbuf, SLIT("negi32("));
		gen_node(generator, expression->operand, strbuf, 0);
		strbuf_put_rune(strbuf, ')');
		break;
	default:
		break;
	}
	(void)depth;
	return true;
}

static String instruction_to_string(Instruction instruction) {
	#define INSTRUCTION(enumerator, name) SLIT(name),
	const String INSTRUCTIONS[] = {
		#include "instructions.h"
	};
	return INSTRUCTIONS[instruction];
}

static Instruction operator_to_instruction(Operator operation) {
	switch (operation) {
	case OPERATOR_ADD:
		return INSTRUCTION_ADDI32;
	case OPERATOR_SUB:
		return INSTRUCTION_SUBI32;
	case OPERATOR_MUL:
		return INSTRUCTION_MULI32;
	default:
		return INSTRUCTION_UNDEF;
	}
	UNREACHABLE();
}

static Bool gen_binary_instruction(Generator *generator, const BinaryExpression *expression, StrBuf *strbuf) {
	const Instruction instruction = operator_to_instruction(expression->operation);
	use(generator, instruction);
	const String string = instruction_to_string(instruction);
	strbuf_put_formatted(strbuf, "%.*s(",
		CAST(Sint32,      string.size),
		CAST(const char*, string.data));
	gen_node(generator, expression->lhs, strbuf, 0);
	strbuf_put_string(strbuf, SLIT(", "));
	gen_node(generator, expression->rhs, strbuf, 0);
	strbuf_put_rune(strbuf, ')');
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
	case EXPRESSION_CAST:
		// return gen_cast_expression(generator, &expression->cast, strbuf, depth);
		break;
	case EXPRESSION_SELECTOR:
		// return gen_selector_expression(generator, &expression->selector, strbuf, depth);
		break;
	case EXPRESSION_CALL:
		return gen_call_expression(generator, &expression->call, strbuf, depth);
	case EXPRESSION_ASSERTION:
		// TODO(dweiler): Generate type assertions,
		//
		// Need to yield ok boolean for
		//	x.(T)
		//	x.?
		break;
	}
	return true;
}

static Bool gen_expression_statement(Generator *generator, const ExpressionStatement *statement, StrBuf *strbuf, Sint32 depth) {
	const Node *node = statement->expression;
	ASSERT(node->kind == NODE_EXPRESSION);
	gen_expression(generator, &node->expression, strbuf, depth);
	strbuf_put_rune(strbuf, ';');
	return true;
}

static Bool gen_declaration_statement(Generator *generator, const DeclarationStatement *statement, StrBuf *strbuf, Sint32 depth);

static Bool gen_statement(Generator *generator, const Statement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(generator, depth, strbuf);
	switch (statement->kind) {
	case STATEMENT_EXPRESSION:
		gen_expression_statement(generator, &statement->expression, strbuf, depth);
		break;
	case STATEMENT_DECLARATION:
		gen_declaration_statement(generator, &statement->declaration, strbuf, depth);
		break;
	default:
		break;
	}
	return true;
}

static Bool gen_block(Generator *generator, const BlockStatement *statement, StrBuf *strbuf, Sint32 depth) {
	strbuf_put_rune(strbuf, '{');
	strbuf_put_rune(strbuf, '\n');
	const Uint64 n_statements = array_size(statement->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *node = statement->statements[i];
		ASSERT(node->kind == NODE_STATEMENT);
		gen_statement(generator, &node->statement, strbuf, depth + 1);
		strbuf_put_rune(strbuf, '\n');
	}
	strbuf_put_rune(strbuf, '}');
	return true;
}

static Bool gen_literal_value(Generator *generator, const LiteralValue *literal, StrBuf *strbuf) {
	(void)generator;
	switch (literal->literal) {
	case LITERAL_INTEGER:
	case LITERAL_FLOAT:
	case LITERAL_IMAGINARY:
	case LITERAL_RUNE:
	case LITERAL_STRING:
		strbuf_put_string(strbuf, literal->value);
		break;
	case LITERAL_COUNT:
		return false;
	}
	return true;
}

static Bool gen_value(Generator *generator, const Node *node, StrBuf *strbuf) {
	// tree_dump_node(node, 0, false);
	if (node->kind == NODE_PROCEDURE) {
		const Procedure *procedure = &node->procedure;
		const Node *node = procedure->body;
		ASSERT(node->kind == NODE_STATEMENT);
		const Statement *statement = &node->statement;
		ASSERT(statement->kind == STATEMENT_BLOCK);
		gen_block(generator, &statement->block, strbuf, 0);
	} else if (node->kind == NODE_LITERAL_VALUE) {
		gen_literal_value(generator, &node->literal_value, strbuf);
	}
	return false;
}

static Bool gen_declaration_statement(Generator *generator, const DeclarationStatement *statement, StrBuf *strbuf, Sint32 depth) {
	const Uint64 n_decls = array_size(statement->names);
	const Node *type = statement->type;
	for (Uint64 i = 0; i < n_decls; i++) {
		const Node *name = statement->names[i];
		const Node *value = statement->values[i];
		if (type) {
			gen_type(generator, type, strbuf);
			strbuf_put_rune(strbuf, ' ');
		}

		// Generate return type for procedure.
		if (value->kind == NODE_PROCEDURE) {
			const Procedure *procedure = &value->procedure;
			gen_type(generator, procedure->type, strbuf);
			strbuf_put_rune(strbuf, ' ');
		}
		gen_name(generator, name, strbuf);

		// Generate procedure parameter list.
		if (value->kind == NODE_PROCEDURE) {
			strbuf_put_rune(strbuf, '(');
			strbuf_put_rune(strbuf, ')');
			strbuf_put_rune(strbuf, ' ');
		} else {
			strbuf_put_string(strbuf, SLIT(" = "));
		}
	
		gen_value(generator, value, strbuf);

		if (value->kind != NODE_PROCEDURE) {
			strbuf_put_rune(strbuf, ';');
		}

		if (i != n_decls - 1) {
			strbuf_put_rune(strbuf, '\n');
			gen_padding(generator, depth, strbuf);
		}
	}
	return true;
}

static Bool gen_node(Generator *generator, const Node *node, StrBuf *strbuf, Sint32 depth) {
	switch (node->kind) {
	case NODE_EXPRESSION:
		return gen_expression(generator, &node->expression, strbuf, depth);
	case NODE_STATEMENT:
		return gen_statement(generator, &node->statement, strbuf, depth);
	case NODE_IDENTIFIER:
		return gen_identifier(generator, &node->identifier, strbuf);
	case NODE_VALUE:
	case NODE_LITERAL_VALUE:
		return gen_literal_value(generator, &node->literal_value, strbuf);
	case NODE_COMPOUND_LITERAL:
	case NODE_FIELD_LIST:
	case NODE_PROCEDURE:
		break;
	}
	return false;
}

static Bool gen_c0_unary_prelude(Generator *generator, const char *name, const char *op, const char *type, StrBuf *strbuf) {
	(void)generator;
	strbuf_put_formatted(strbuf, "FORCE_INLINE %s %s(%s value) {\n", type, name, type);
	strbuf_put_formatted(strbuf, "\treturn %svalue;\n", op);
	strbuf_put_string(strbuf, SLIT("}\n"));
	return true;
}

static Bool gen_c0_binary_prelude(Generator *generator, const char *name, const char *op, const char *type, StrBuf *strbuf) {
	(void)generator;
	strbuf_put_formatted(strbuf, "FORCE_INLINE %s %s(%s lhs, %s rhs) {\n", type, name, type, type);
	strbuf_put_formatted(strbuf, "\treturn lhs %s rhs;\n", op);
	strbuf_put_string(strbuf, SLIT("}\n"));
	return true;
}

static Bool gen_c0_prelude(Generator *generator, StrBuf *strbuf) {
	strbuf_put_string(strbuf, SLIT("typedef int i32;\n"));
	strbuf_put_string(strbuf, SLIT("typedef const char *string;\n"));
	strbuf_put_rune(strbuf, '\n');
	strbuf_put_string(strbuf, SLIT("#if defined(_MSC_VER)\n"));
	strbuf_put_string(strbuf, SLIT("	#define FORCE_INLINE __forceinline\n"));
	strbuf_put_string(strbuf, SLIT("#else\n"));
	strbuf_put_string(strbuf, SLIT("	#define FORCE_INLINE __attribute__((always_inline)) inline\n"));
	strbuf_put_string(strbuf, SLIT("#endif\n"));
	strbuf_put_rune(strbuf, '\n');

	if (uses(generator, INSTRUCTION_NEGI32)) {
		gen_c0_unary_prelude(generator, "negi32", "-", "i32", strbuf);
	}
	if (uses(generator, INSTRUCTION_ADDI32)) {
		gen_c0_binary_prelude(generator, "addi32", "+", "i32", strbuf);
	}
	if (uses(generator, INSTRUCTION_SUBI32)) {
		gen_c0_binary_prelude(generator, "subi32", "-", "i32", strbuf);
	}
	if (uses(generator, INSTRUCTION_MULI32)) {
		gen_c0_binary_prelude(generator, "muli32", "*", "i32", strbuf);
	}

	return true;
}

Bool gen_init(Generator *generator, const Tree *tree) {
	generator->tree = tree;
	memset(generator->used, 0, sizeof(generator->used));
	return true;
}

Bool gen_run(Generator *generator, StrBuf *strbuf) {
	// HACK(dweiler): Manually mark the ones needed until we do two passes.
	use(generator, INSTRUCTION_NEGI32);
	use(generator, INSTRUCTION_ADDI32);
	use(generator, INSTRUCTION_SUBI32);
	use(generator, INSTRUCTION_MULI32);

	gen_c0_prelude(generator, strbuf);

	const Tree *tree = generator->tree;
	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		gen_node(generator, tree->statements[i], strbuf, 0);
	}
	return false;
}