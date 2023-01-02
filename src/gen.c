#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "gen.h"
#include "tree.h"
#include "utility.h"
#include "string.h"

static Bool gen_value(Generator *generator, const Node *type, const Node *node, StrBuf *strbuf);
static Bool gen_node(Generator *generator, const Node *node, StrBuf *strbuf, Sint32 depth);

static Bool gen_padding(Sint32 depth, StrBuf *strbuf) {
	for (Sint32 i = 0; i < depth * 2; i++) {
		strbuf_put_rune(strbuf, ' ');
	}
	return true;
}

static Bool gen_identifier(Generator *generator, const Identifier *identifier, StrBuf *strbuf) {
	(void)generator;
	const String contents = identifier->contents;
	return strbuf_put_formatted(strbuf, "%.*s", SFMT(contents));
}

static Bool gen_type(Generator *generator, const Type *type, StrBuf *strbuf) {
	switch (type->kind) {
	case TYPE_PROCEDURE:
		{
			const ProcedureType *procedure_type = &type->procedure;
			if (procedure_type->results) {
				return gen_node(generator, procedure_type->results, strbuf, 0);
			} else {
				strbuf_put_string(strbuf, SCLIT("void"));
			}
		}
		break;
	case TYPE_SLICE:
		if (!gen_node(generator, type->slice.type, strbuf, 0)) {
			return false;
		}
		strbuf_put_rune(strbuf, '*');
		break;
	case TYPE_ARRAY:
		if (!gen_node(generator, type->array.type, strbuf, 0)) {
			return false;
		}
		break;
	case TYPE_POINTER:
		if (!gen_node(generator, type->pointer.type, strbuf, 0)) {
			return false;
		}
		strbuf_put_rune(strbuf, '*');
		break;
	case TYPE_MULTI_POINTER:
		if (!gen_node(generator, type->multi_pointer.type, strbuf, 0)) {
			return false;
		}
		strbuf_put_rune(strbuf, '*');
		break;
	default:
		printf("Unimplemented type\n");
		return false;
	}
	return true;
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
	switch (operand->kind) {
	case NODE_IDENTIFIER:
		// Could be a type cast.
		if (string_compare(operand->identifier.contents, SCLIT("f32"))) {
			strbuf_put_formatted(strbuf, "CODIN_f16_to_f32");
		} else if (!gen_name(generator, operand, strbuf)) {
			return false;
		}
		break;
	case NODE_EXPRESSION:
		{
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
		}
		break;
	case NODE_DIRECTIVE:
		return gen_directive(generator, &operand->directive, strbuf);
	default:
		printf("Unimplemented call expression\n");
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

static Bool gen_dereference_expression(Generator *generator, const DereferenceExpression *expression, StrBuf *strbuf, Sint32 depth) {
	(void)depth;
	strbuf_put_rune(strbuf, '*');
	return gen_node(generator, expression->operand, strbuf, 0);
}

static Bool gen_unary_expression(Generator *generator, const UnaryExpression *expression, StrBuf *strbuf, Sint32 depth) {
	(void)depth;
	const char *what = "";
	if (expression->operation == OPERATOR_SUB) what = "negi32";
	if (expression->operation == OPERATOR_NOT) what = "noti32";
	if (expression->operation == OPERATOR_AND) {
		strbuf_put_rune(strbuf, '&');
	} else {
		strbuf_put_formatted(strbuf, "%s(", what);
	}
	if (!gen_node(generator, expression->operand, strbuf, 0)) {
		return false;
	}
	if (expression->operation != OPERATOR_AND) {
		strbuf_put_rune(strbuf, ')');
	}
	return true;
}

static Bool gen_binary_instruction(Generator *generator, const BinaryExpression *expression, StrBuf *strbuf) {
	const char *what = "unknown";
	switch (expression->operation) {
	case OPERATOR_LT:    what = "lti64";  break;
	case OPERATOR_GT:    what = "gti64";  break;
	case OPERATOR_ADD:   what = "addi64"; break;
	case OPERATOR_SUB:   what = "subi64"; break;
	case OPERATOR_MUL:   what = "muli64"; break;
	case OPERATOR_CMPEQ: what = "eqi64";  break;
	case OPERATOR_NOTEQ: what = "nei64";  break;
	case OPERATOR_GTEQ:  what = "gtei64"; break;
	case OPERATOR_LTEQ:  what = "ltei64"; break;
	case OPERATOR_AND:   what = "andi64"; break;
	default:
		printf("Unimplemented binary instructon\n");
	}
	strbuf_put_formatted(strbuf, "%s(", what);
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
	case EXPRESSION_DEREFERENCE:
		return gen_dereference_expression(generator, &expression->dereference, strbuf, depth);
	default:
		printf("Unimplemented expression\n");
		return false;
	}
	UNREACHABLE();
}

static Bool gen_expression_statement(Generator *generator, const ExpressionStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(depth, strbuf);
	const Node *node = statement->expression;
	ASSERT(node->kind == NODE_EXPRESSION);
	if (!gen_expression(generator, &node->expression, strbuf, depth)) return false;
	strbuf_put_rune(strbuf, ';');
	strbuf_put_rune(strbuf, '\n');
	return true;
}

static Bool gen_if_statement(Generator *generator, const IfStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(depth, strbuf);
	if (statement->init) {
		strbuf_put_rune(strbuf, '{');
		strbuf_put_rune(strbuf, '\n');
		gen_node(generator, statement->init, strbuf, depth + 1);
		depth++;
		gen_padding(depth, strbuf);
	}
	strbuf_put_string(strbuf, SCLIT("if ("));
	if (!gen_node(generator, statement->cond, strbuf, 0)) {
		return false;
	}
	strbuf_put_string(strbuf, SCLIT(")\n"));
	if (!gen_node(generator, statement->body, strbuf, depth)) {
		return false;
	}
	if (statement->elif) {
		strbuf_put_string(strbuf, SCLIT(" else "));
		if (statement->elif->kind == NODE_STATEMENT && statement->elif->statement.kind == STATEMENT_IF) {
			strbuf_put_string(strbuf, SCLIT("{\n"));
			if (!gen_node(generator, statement->elif, strbuf, depth + 1)) {
				return false;
			}
			gen_padding(depth, strbuf);
			strbuf_put_rune(strbuf, '}');
		} else {
			if (!gen_node(generator, statement->elif, strbuf, depth)) {
				return false;
			}
		}
	}
	if (statement->init) {
		depth--;
		gen_padding(depth, strbuf);
		strbuf_put_rune(strbuf, '}');
		strbuf_put_rune(strbuf, '\n');
	}
	return true;
}

static Bool gen_statement(Generator *generator, const Statement *statement, StrBuf *strbuf, Sint32 depth);

static Bool gen_return_statement(Generator *generator, const ReturnStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(depth, strbuf);
	strbuf_put_string(strbuf, SCLIT("return"));
	if (array_size(statement->results) != 0) {
		strbuf_put_rune(strbuf, ' ');
		if (!gen_node(generator, statement->results[0], strbuf, 0)) {
			return false;
		}
	}
	strbuf_put_rune(strbuf, ';');
	strbuf_put_rune(strbuf, '\n');
	return true;
}

static Bool gen_for_statement(Generator *generator, const ForStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(depth, strbuf);

	// We convert the following Odin
	//
	//	for init; cond; post {
	//		body
	//	}
	//
	// Into the following C
	//
	//	{
	//		init;
	//		while cond {
	//			body
	//			post
	//		}
	//	}
	strbuf_put_rune(strbuf, '{');
	strbuf_put_rune(strbuf, '\n');

	if (statement->init) {
		gen_node(generator, statement->init, strbuf, depth + 1);
	}

	gen_padding(depth + 1, strbuf);

	strbuf_put_string(strbuf, SCLIT("while ("));
	if (statement->cond) {
		if (!gen_node(generator, statement->cond, strbuf, 0)) {
			return false;
		}
	} else {
		strbuf_put_rune(strbuf, '1');
	}
	strbuf_put_string(strbuf, SCLIT(")\n"));

	// Inject the post statement as the last statement in the block.
	if (statement->post) {
		array_push(statement->body->statement.block.statements, statement->post);
	}

	gen_node(generator, statement->body, strbuf, depth + 1);

	gen_padding(depth, strbuf);
	strbuf_put_rune(strbuf, '}');
	strbuf_put_rune(strbuf, '\n');

	return true;
}

static Bool gen_assignment_statement(Generator *generator, const AssignmentStatement *statement, StrBuf *strbuf, Sint32 depth) {
	// NOTE(dweiler): Need a type system, assumes i32 for now.

	// Assignments in Odin behave more like memcpy unlike C which will not allow
	// you to copy structure types with regular assignment. Here we emit memcpy
	// instead.
	if (statement->assignment == ASSIGNMENT_EQ) {
		gen_padding(depth, strbuf);
		strbuf_put_string(strbuf, SCLIT("memcpy("));
		if (node_is_expression(statement->lhs[0], EXPRESSION_DEREFERENCE)) {
			// Expressions like ptr^ = value should become memcpy(ptr, value, ...)
			const DereferenceExpression *expression = &statement->lhs[0]->expression.dereference;
			gen_node(generator, expression->operand, strbuf, depth);
		} else {
			// Expressions like obj = value should become memcpy(&obj, value, ...)
			gen_padding(depth, strbuf);
			strbuf_put_rune(strbuf, '&');
			gen_node(generator, statement->lhs[0], strbuf, 0);
		}
		strbuf_put_string(strbuf, SCLIT(", "));
		// The value be assigned might be a literal. Construct a compound literal
		// on the C side to pass to memcpy.
		if (node_is_kind(statement->rhs[0], NODE_LITERAL_VALUE)) {
			strbuf_put_string(strbuf, SCLIT("&(i64){"));
			gen_node(generator, statement->rhs[0], strbuf, 0);
			strbuf_put_rune(strbuf, '}');
		} else {
			gen_node(generator, statement->rhs[0], strbuf, 0);
		}
		// NOTE(dweiler): We should probably evaluate the type size once we have a
		// type system so we can pass a literal into here.
		strbuf_put_string(strbuf, SCLIT(", sizeof("));
		gen_node(generator, statement->lhs[0], strbuf, 0);
		strbuf_put_string(strbuf, SCLIT("));\n"));
		return true;
	}

	// Compound assignment.
	const char *what = "";
	switch (statement->assignment) {
	case ASSIGNMENT_ADDEQ: what = "addi64"; break;
	case ASSIGNMENT_SUBEQ: what = "subi64"; break;
	default:
		printf("Unimplemented assignment\n");
		return false;
	}

	// generate lhs = op(lhs, rhs)
	gen_padding(depth, strbuf);
	gen_node(generator, statement->lhs[0], strbuf, depth);
	strbuf_put_formatted(strbuf, " = %s(", what);
	gen_node(generator, statement->lhs[0], strbuf, 0);
	strbuf_put_formatted(strbuf, ", ");
	gen_node(generator, statement->rhs[0], strbuf, 0);
	strbuf_put_string(strbuf, SCLIT(");\n"));

	return true;
}

static Bool gen_declaration_statement(Generator *generator, const DeclarationStatement *statement, StrBuf *strbuf, Bool prototype, Sint32 depth) {
	gen_padding(depth, strbuf);
	const Uint64 n_decls = array_size(statement->names);
	const Node *type = statement->type;

	for (Uint64 i = 0; i < n_decls; i++) {
		const Node *name = statement->names[i];
		if (prototype) {
			strbuf_put_string(strbuf, SCLIT("extern "));
		}
		if (type) {
			ASSERT(node_is_kind(type, NODE_TYPE) || node_is_kind(type, NODE_IDENTIFIER));
			if (!gen_node(generator, type, strbuf, 0)) {
				return false;
			}
			strbuf_put_rune(strbuf, ' ');
		} else {
			// When no type, emit "void".
			strbuf_put_string(strbuf, SCLIT("void "));
		}
		// Generate return type for procedure.
		const Node *value = statement->values[i];
		String rename = STRING_NIL;
		if (value && value->kind == NODE_PROCEDURE) {
			const Procedure *procedure = &value->procedure;
			if (procedure->flags & PROC_FLAG_FORCE_INLINE) {
				strbuf_put_string(strbuf, SCLIT("FORCE_INLINE "));
			}
			// When encountering "main" rename it to "CODIN_main".
			if (string_compare(name->identifier.contents, SCLIT("main"))) {
				rename = SCLIT("CODIN_main");
			}
		}

		if (!string_compare(rename, STRING_NIL)) {
			strbuf_put_string(strbuf, rename);
		} else {
			gen_name(generator, name, strbuf);
			// Special behavior needed for array types since C puts [N] after the identifier.
			if (type && node_is_type(type, TYPE_ARRAY)) {
				const Node *base = type;
				while (node_is_type(base, TYPE_ARRAY)) {
					strbuf_put_rune(strbuf, '[');
					gen_node(generator, base->type.array.count, strbuf, 0);
					strbuf_put_rune(strbuf, ']');
					base = base->type.array.type;
				}
			}
		}

		if (value) {
			// Generate procedure parameter list.
			if (value->kind == NODE_PROCEDURE) {
				strbuf_put_rune(strbuf, '(');
				Node *node = value->procedure.type;
				ASSERT(node_is_type(node, TYPE_PROCEDURE));
				const Node *params = node->type.procedure.params;
				ASSERT(params->kind == NODE_FIELD_LIST);
				const Uint64 n_params = array_size(params->field_list.fields);
				if (n_params == 0) {
					strbuf_put_string(strbuf, SCLIT("void"));
				}
				for (Uint64 i = 0; i < n_params; i++) {
					const Node *node = params->field_list.fields[i];
					ASSERT(node);
					const Field *field = &node->field;
					gen_node(generator, field->type, strbuf, 0);
					strbuf_put_rune(strbuf, ' ');
					gen_node(generator, field->name, strbuf, 0);
					if (i != n_params - 1) {
						strbuf_put_rune(strbuf, ',');
						strbuf_put_rune(strbuf, ' ');
					}
				}
				strbuf_put_rune(strbuf, ')');
				if (!prototype) {
					strbuf_put_rune(strbuf, ' ');
				}
			} else {
				strbuf_put_string(strbuf, SCLIT(" = "));
			}

			if (!prototype && !gen_value(generator, type, value, strbuf)) {
				return false;
			}

			if (value->kind != NODE_PROCEDURE || prototype) {
				strbuf_put_rune(strbuf, ';');
			}
		} else {
			strbuf_put_rune(strbuf, ';');
		}

		strbuf_put_rune(strbuf, '\n');

		if (i != n_decls - 1) {
			gen_padding(depth, strbuf);
		}
	}

	return true;
}

static Bool gen_block_statement(Generator *generator, const BlockStatement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(depth, strbuf);
	strbuf_put_rune(strbuf, '{');
	strbuf_put_rune(strbuf, '\n');
	const Uint64 n_statements = array_size(statement->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *node = statement->statements[i];
		ASSERT(node->kind == NODE_STATEMENT);
		if (!gen_statement(generator, &node->statement, strbuf, depth + 1)) {
			return false;
		}
	}
	gen_padding(depth, strbuf);
	strbuf_put_rune(strbuf, '}');
	strbuf_put_rune(strbuf, '\n');
	return true;
}

static Bool gen_statement(Generator *generator, const Statement *statement, StrBuf *strbuf, Sint32 depth) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:
		return true;
	case STATEMENT_EXPRESSION:
		return gen_expression_statement(generator, &statement->expression, strbuf, depth);
	case STATEMENT_DECLARATION:
		return gen_declaration_statement(generator, &statement->declaration, strbuf, false, depth);
	case STATEMENT_IF:
		return gen_if_statement(generator, &statement->if_, strbuf, depth);
	case STATEMENT_RETURN:
		return gen_return_statement(generator, &statement->return_, strbuf, depth);
	case STATEMENT_BLOCK:
		return gen_block_statement(generator, &statement->block, strbuf, depth);
	case STATEMENT_FOR:
		return gen_for_statement(generator, &statement->for_, strbuf, depth);
	case STATEMENT_ASSIGNMENT:
		return gen_assignment_statement(generator, &statement->assignment, strbuf, depth);
	case STATEMENT_IMPORT:
		// Handled else-where.
		return true;
	case STATEMENT_DEFER:
		fprintf(stderr, "Lowering pass failed to remove defer statement\n");
		return false;
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
			printf("Malformed load directive\n");
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
	} else if (node->kind == NODE_IDENTIFIER) {
		return strbuf_put_string(strbuf, node->identifier.contents);
	}
	printf("Unimplemented value\n");
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
	case NODE_TYPE:
		return gen_type(generator, &node->type, strbuf);
	case NODE_FIELD_LIST:
		{
			const FieldList *field_list = &node->field_list;
			const Uint64 n_fields = array_size(field_list->fields);
			for (Uint64 i = 0; i < n_fields; i++) {
				gen_node(generator, field_list->fields[i], strbuf, depth);
			}
			return true;
		}
		break;
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
		return true;
	}

	strbuf_put_formatted(strbuf, "static FORCE_INLINE u%d %su%d(u%d lhs, u%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "return lhs %s rhs;\n", info.c_op);
	strbuf_put_formatted(strbuf, "}\n\n");

	// We can get wrapping signed integer arithmetic simply by casting to unsigned
	// and back since C and C++ now both requires 2s complement representation of
	// signed integers.
	strbuf_put_formatted(strbuf, "static FORCE_INLINE i%d %si%d(i%d lhs, i%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "return %su%d(lhs, rhs);\n", info.name, info.bits);
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
		gen_padding(1, strbuf);
		strbuf_put_string(strbuf, SCLIT("const f32 a = CODIN_f16_to_f32(lhs);\n"));
		gen_padding(1, strbuf);
		strbuf_put_string(strbuf, SCLIT("const f32 b = CODIN_f16_to_f32(rhs);\n"));
		gen_padding(1, strbuf);
		strbuf_put_formatted(strbuf, "return CODIN_f32_to_f16(a %s b);\n", info.c_op);
		strbuf_put_string(strbuf, SCLIT("}\n\n"));
	} else {
		strbuf_put_formatted(strbuf, "static FORCE_INLINE f%d %sf%d(f%d lhs, f%d rhs) {\n",
			info.bits, info.name, info.bits, info.bits, info.bits);
		gen_padding(1, strbuf);
		strbuf_put_formatted(strbuf, "return lhs %s rhs;\n", info.c_op);
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
	if (info.bits == 128) {
		// TODO(dweiler): Implement 128-bit integer emulation.
		return true;
	}

	strbuf_put_formatted(strbuf, "static FORCE_INLINE u%d %su%d(u%d lhs, u%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "return lhs %s rhs;\n", info.c_op);
	strbuf_put_formatted(strbuf, "}\n\n");

	// In 2s complement representation comparisons are the same instruction.
	strbuf_put_formatted(strbuf, "static FORCE_INLINE i%d %si%d(i%d lhs, i%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "return %su%d(lhs, rhs);\n", info.name, info.bits);
	strbuf_put_formatted(strbuf, "}\n\n");

	return true;
}

static Bool gen_c0_rel(RelInstruction instr, StrBuf *strbuf) {
	#define REL(ignore, name, c_op, bits) { (name), (c_op), (bits) },
	static const InstructionInfo INFOS[] = {
		#include "instructions.h"
	};

	const InstructionInfo info = INFOS[instr];
	if (info.bits == 128) {
		// TODO(dweiler): Implement 128-bit integer emulation.
		return true;
	}

	// TODO(dweiler): Mark relational instructions as signed/unsigned to avoid
	// this hack.
	#define REL(enumerator, ...) #enumerator,
	static const char *ENUMS[] = {
		#include "instructions.h"
	};

	char bits[4]; // 128\0
	snprintf(bits, sizeof bits, "%d", info.bits);
	const Rune ts = tolower(strchr(ENUMS[instr], bits[0])[-1]);

	strbuf_put_formatted(strbuf, "static FORCE_INLINE %c%d %s%c%d(%c%d lhs, %c%d rhs) {\n",
		ts, info.bits, info.name, ts, info.bits, ts, info.bits, ts, info.bits);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "return lhs %s rhs;\n", info.c_op);
	strbuf_put_formatted(strbuf, "}\n\n");

	return true;
}

static Bool gen_c0_bit(BitInstruction instr, StrBuf *strbuf) {
	#define BIT(ignore, name, c_op, bits) { (name), (c_op), (bits) },
	static const InstructionInfo INFOS[] = {
		#include "instructions.h"
	};

	const InstructionInfo info = INFOS[instr];
	if (info.bits == 128) {
		// TODO(dweiler): Implement 128-bit integer emulation.
		return true;
	}

	strbuf_put_formatted(strbuf, "static FORCE_INLINE i%d %si%d(i%d lhs, i%d rhs) {\n",
		info.bits, info.name, info.bits, info.bits, info.bits);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "return lhs %s rhs;\n", info.c_op);
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

static Bool gen_c0_cmp_prelude(Uint64 used, StrBuf *strbuf) {
	for (Uint64 i = 0; i < INSTR_CMP_COUNT; i++) {
		if (used & (CAST(Uint64, 1) << i)) {
			if (!gen_c0_cmp(CAST(CmpInstruction, i), strbuf)) {
				return false;
			}
		}
	}
	return true;
}

static Bool gen_c0_rel_prelude(Uint64 used, StrBuf *strbuf) {
	for (Uint64 i = 0; i < INSTR_REL_COUNT; i++) {
		if (used & (CAST(Uint64, 1) << i)) {
			if (!gen_c0_rel(CAST(RelInstruction, i), strbuf)) {
				return false;
			}
		}
	}
	return true;
}

static Bool gen_c0_bit_prelude(Uint64 used, StrBuf *strbuf) {
	for (Uint64 i = 0; i < INSTR_BIT_COUNT; i++) {
		if (used & (CAST(Uint64, 1) << i)) {
			if (!gen_c0_bit(CAST(BitInstruction, i), strbuf)) {
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
	gen_padding(1, strbuf);
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
			gen_padding(1, strbuf);
		}
	}
	strbuf_put_string(strbuf, SCLIT("};\n\n"));

	// Write out the shift table.
	strbuf_put_string(strbuf, SCLIT("static const u8 CODIN_f16_shift[] = {\n"));
	gen_padding(1, strbuf);
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
			gen_padding(1, strbuf);
		}
	}
	strbuf_put_string(strbuf, SCLIT("};\n\n"));

	strbuf_put_string(strbuf, SCLIT("static FORCE_INLINE f16 CODIN_f32_to_f16(f32 f) {\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("const union { f32 f; u32 u; } s = { f };\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("const u32 e = s.u >> 23;\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("const u32 base = CODIN_f16_base[e & 0x1ff];\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("const u8 shift = CODIN_f16_shift[e & 0x1ff];\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("return base + ((s.u & 0x007fffff) >> shift);\n"));
	strbuf_put_string(strbuf, SCLIT("}\n\n"));

	static const union { Uint32 u; float f; } MAGIC = { 113 << 23 };
	strbuf_put_string(strbuf, SCLIT("static FORCE_INLINE f32 CODIN_f16_to_f32(f16 f) {\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("union { u32 u; f32 f; } o = { (u32)(f & 0x7fff) << 13 };\n"));
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "const u32 exp = 0x%08x & o.u;\n", 0x7c00 << 13);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "o.u += 0x%08x;\n", (127 - 15) << 23);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "if (exp == 0x%08x) o.u += 0x%08x;\n", 0x7c00 << 13, (128 - 16) << 23);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "if (exp == 0x%08x) o.u += 0x%08x, o.f -= %a;\n", MAGIC.f);
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "o.u |= (f & 0x8000) << 16;\n");
	gen_padding(1, strbuf);
	strbuf_put_formatted(strbuf, "return o.f;\n");
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

	// Emit the typedefs for mapping Odin types to C ones.
	for (Uint64 i = 0; i < sizeof(TYPES)/sizeof(*TYPES); i++) {
		strbuf_put_formatted(strbuf, "typedef %s %s;\n",
			TYPES[i].c, TYPES[i].odin);
	}

	strbuf_put_rune(strbuf, '\n');
	strbuf_put_string(strbuf, SCLIT("#if defined(_MSC_VER)\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("#define FORCE_INLINE __forceinline\n"));
	strbuf_put_string(strbuf, SCLIT("#else\n"));
	gen_padding(1, strbuf);
	strbuf_put_string(strbuf, SCLIT("#define FORCE_INLINE __attribute__((always_inline)) inline\n"));
	strbuf_put_string(strbuf, SCLIT("#endif\n"));
	strbuf_put_rune(strbuf, '\n');

	if (uses_f16_instructions(generator)) {
		gen_c0_f16_prelude(strbuf);
	}

	gen_c0_int_prelude(generator->used_int, strbuf);
	gen_c0_flt_prelude(generator->used_flt, strbuf);
	gen_c0_cmp_prelude(generator->used_cmp, strbuf);
	gen_c0_rel_prelude(generator->used_rel, strbuf);
	gen_c0_bit_prelude(generator->used_bit, strbuf);

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
	gen_padding(1, strbuf);
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
			gen_padding(1, strbuf);
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

static Bool gen_import_prelude(Generator *generator, StrBuf *strbuf) {
	const Tree *tree = generator->tree;
	const Uint64 n_nodes = array_size(tree->nodes);
	for (Uint64 i = 0; i < n_nodes; i++) {
		const Node *node = tree->nodes[i];
		if (node->kind != NODE_STATEMENT || node->statement.kind != STATEMENT_IMPORT) {
			continue;
		}
		// HACK(dweiler): When we see "core:fmt" just import <stdio.h> for now.
		const ImportStatement *import = &node->statement.import;
		if (string_compare(import->package, SCLIT("core:fmt"))) {
			strbuf_put_string(strbuf, SCLIT("#include <stdio.h>\n"));
			strbuf_put_string(strbuf, SCLIT("#include <string.h>\n"));
		}
	}
	return true;
}

Bool gen_init(Generator *generator, const Tree *tree) {
	generator->tree = tree;
	// TODO(dweiler): Only mark the ones we actually use.
	generator->used_int = (CAST(Uint64, 1) << INSTR_INT_COUNT) - 1;
	generator->used_flt = (CAST(Uint64, 1) << INSTR_FLT_COUNT) - 1;
	generator->used_cmp = (CAST(Uint64, 1) << INSTR_CMP_COUNT) - 1;
	generator->used_rel = (CAST(Uint64, 1) << INSTR_REL_COUNT) - 1;
	generator->used_bit = (CAST(Uint64, 1) << INSTR_BIT_COUNT) - 1;
	return true;
}

Bool gen_run(Generator *generator, StrBuf *strbuf, Bool generate_main) {
	gen_c0_prelude(generator, strbuf);
	gen_import_prelude(generator, strbuf);

	generator->load_directive_id = 0;
	if (!gen_load_directives_prelude(generator, strbuf)) {
		return false;
	}

	const Tree *tree = generator->tree;
	const Uint64 n_statements = array_size(tree->statements);

	// Odin allows use before declaration, C does not. Emit declarations before
	// everything else.
	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *node = tree->statements[i];
		const Statement *statement = &node->statement;
		if (statement->kind != STATEMENT_DECLARATION) {
			continue;
		}
		if (!gen_declaration_statement(generator, &statement->declaration, strbuf, true, 0)) {
			return false;
		}
	}
	strbuf_put_rune(strbuf, '\n');

	// Check for a main procedure.
	const Procedure *main = 0;
	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *node = tree->statements[i];
		const Statement *statement = &node->statement;
		if (statement->kind != STATEMENT_DECLARATION) {
			continue;
		}
		const DeclarationStatement *declaration = &statement->declaration;
		const Uint64 n_names = array_size(declaration->names);
		const Uint64 n_values = array_size(declaration->values);
		for (Uint64 i = 0; i < n_names; i++) {
			const Node *name = declaration->names[i];
			const Node *value = i < n_values ? declaration->values[i] : 0;
			if (value && value->kind == NODE_PROCEDURE && string_compare(name->identifier.contents, SCLIT("main"))) {
				main = &value->procedure;
				break;
			}
		}
	}

	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *statement = tree->statements[i];
		ASSERT(statement->kind == NODE_STATEMENT);
		if (!gen_node(generator, tree->statements[i], strbuf, 0)) {
			return false;
		}
	}

	// Emit an int main() which calls our CODIN_main.
	if (generate_main && main) {
		strbuf_put_string(strbuf, SCLIT("int main(int argc, char **argv) {\n"));
		gen_padding(1, strbuf);
		strbuf_put_string(strbuf, SCLIT("CODIN_main();\n"));
		gen_padding(1, strbuf);
		strbuf_put_string(strbuf, SCLIT("return 0;\n"));
		strbuf_put_string(strbuf, SCLIT("}\n"));
	}

	return true;
}