#include <stdio.h>
#include <stdlib.h>

#include "gen.h"
#include "tree.h"

static Bool gen_node(const Node *node, StrBuf *strbuf, Sint32 depth);

static Bool gen_padding(Sint32 depth, StrBuf *strbuf) {
	for (Sint32 i = 0; i < depth; i++) {
		strbuf_put_rune(strbuf, '\t');
	}
	return true;
}

static Bool gen_identifier(const Identifier *identifier, StrBuf *strbuf) {
	const String contents = identifier->contents;
	return strbuf_put_formatted(strbuf, "%.*s",
		CAST(Sint32,       contents.size),
		CAST(const char *, contents.data));
}

static Bool gen_type(const Node *node, StrBuf *strbuf) {
	// ASSERT(node->kind == NODE_IDENTIFIER);
	if (node->kind == NODE_FIELD_LIST) {
		const FieldList *field_list = &node->field_list;
		gen_type(field_list->fields[0], strbuf);
	} else if (node->kind == NODE_IDENTIFIER) {
		const Identifier *identifier = &node->identifier;
		gen_identifier(identifier, strbuf);
	}
	// tree_dump_node(node, 0, false);
	return false;
}

static Bool gen_name(const Node *node, StrBuf *strbuf) {
	ASSERT(node->kind == NODE_IDENTIFIER);
	const String string = node->identifier.contents;
	strbuf_put_formatted(strbuf, "%.*s",
		CAST(Sint32, string.size),
		CAST(const char*, string.data));
	return false;
}

static Bool gen_expression(const Expression *expression, StrBuf *strbuf, Sint32 depth);

static Bool gen_call_expression(const CallExpression *expression, StrBuf *strbuf, Sint32 depth) {
	(void)depth;

	const Node *operand = expression->operand;
	if (operand->kind == NODE_IDENTIFIER) {
		gen_name(operand, strbuf);
	} else if (operand->kind == NODE_EXPRESSION) {
		const Expression *expression = &operand->expression;
		if (expression->kind == EXPRESSION_SELECTOR) {
			// Can be one of:
			// 	package.procedure()
			//	object.procedure()
			//	object->procedure(object, ...)
			// TODO(dweiler): Implement.
		}
	} else {
		abort();
	}

	strbuf_put_rune(strbuf, '(');
	const Uint64 n_arguments = array_size(expression->arguments);
	for (Uint64 i = 0; i < n_arguments; i++) {
		const Node *node = expression->arguments[i];
		gen_node(node, strbuf, 0);
		if (i != n_arguments - 1) {
			strbuf_put_string(strbuf, SLIT(", "));
		}
	}
	strbuf_put_rune(strbuf, ')');

	return false;
}

static Bool gen_expression(const Expression *expression, StrBuf *strbuf, Sint32 depth) {
	if (expression->kind == EXPRESSION_CALL) {
		gen_call_expression(&expression->call, strbuf, depth);
	}
	return true;
}

static Bool gen_expression_statement(const ExpressionStatement *statement, StrBuf *strbuf, Sint32 depth) {
	const Node *node = statement->expression;
	ASSERT(node->kind == NODE_EXPRESSION);
	return gen_expression(&node->expression, strbuf, depth);
}

static Bool gen_declaration_statement(const DeclarationStatement *statement, StrBuf *strbuf);

static Bool gen_statement(const Statement *statement, StrBuf *strbuf, Sint32 depth) {
	gen_padding(depth, strbuf);
	switch (statement->kind) {
	case STATEMENT_EXPRESSION:
		gen_expression_statement(&statement->expression, strbuf, depth);
		break;
	case STATEMENT_DECLARATION:
		gen_declaration_statement(&statement->declaration, strbuf);
		break;
	default:
		break;
	}

	// NOTE(dweiler): Don't terminate procedure statements with a semicolon.
	if (statement->kind != STATEMENT_DECLARATION || statement->declaration.values[0]->kind != NODE_PROCEDURE) {
		strbuf_put_rune(strbuf, ';');
	}

	return true;
}

static Bool gen_block(const BlockStatement *statement, StrBuf *strbuf, Sint32 depth) {
	strbuf_put_rune(strbuf, '{');
	strbuf_put_rune(strbuf, '\n');
	const Uint64 n_statements = array_size(statement->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *node = statement->statements[i];
		ASSERT(node->kind == NODE_STATEMENT);
		gen_statement(&node->statement, strbuf, depth + 1);
		strbuf_put_rune(strbuf, '\n');
	}
	strbuf_put_rune(strbuf, '}');
	return true;
}

static Bool gen_literal_value(const LiteralValue *literal, StrBuf *strbuf) {
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

static Bool gen_value(const Node *node, StrBuf *strbuf) {
	// tree_dump_node(node, 0, false);
	if (node->kind == NODE_PROCEDURE) {
		const Procedure *procedure = &node->procedure;
		const Node *node = procedure->body;
		ASSERT(node->kind == NODE_STATEMENT);
		const Statement *statement = &node->statement;
		ASSERT(statement->kind == STATEMENT_BLOCK);
		gen_block(&statement->block, strbuf, 0);
	} else if (node->kind == NODE_LITERAL_VALUE) {
		gen_literal_value(&node->literal_value, strbuf);
	}
	return false;
}

static Bool gen_declaration_statement(const DeclarationStatement *statement, StrBuf *strbuf) {
	const Uint64 n_decls = array_size(statement->names);
	const Node *type = statement->type;
	for (Uint64 i = 0; i < n_decls; i++) {
		const Node *name = statement->names[i];
		const Node *value = statement->values[i];
		if (type) {
			gen_type(type, strbuf);
			strbuf_put_rune(strbuf, ' ');
		}

		// Generate return type for procedure.
		if (value->kind == NODE_PROCEDURE) {
			const Procedure *procedure = &value->procedure;
			gen_type(procedure->type, strbuf);
			strbuf_put_rune(strbuf, ' ');
		}
		gen_name(name, strbuf);

		// Generate procedure parameter list.
		if (value->kind == NODE_PROCEDURE) {
			strbuf_put_rune(strbuf, '(');
			strbuf_put_rune(strbuf, ')');
			strbuf_put_rune(strbuf, ' ');
		} else {
			strbuf_put_string(strbuf, SLIT(" = "));
		}
	
		gen_value(value, strbuf);
	}
	return true;
}

static Bool gen_node(const Node *node, StrBuf *strbuf, Sint32 depth) {
	switch (node->kind) {
	case NODE_EXPRESSION:
		return gen_expression(&node->expression, strbuf, depth);
	case NODE_STATEMENT:
		return gen_statement(&node->statement, strbuf, depth);
	case NODE_IDENTIFIER:
		return gen_identifier(&node->identifier, strbuf);
	case NODE_VALUE:
	case NODE_LITERAL_VALUE:
		return gen_literal_value(&node->literal_value, strbuf);
	case NODE_COMPOUND_LITERAL:
	case NODE_FIELD_LIST:
	case NODE_PROCEDURE:
		break;
	}
	return false;
}

Bool gen(const Tree *tree, StrBuf *strbuf) {
	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		gen_node(tree->statements[i], strbuf, 0);
	}
	return false;
}