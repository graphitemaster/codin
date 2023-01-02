#include <stdlib.h>
#include <stdio.h>

#include "lower.h"
#include "tree.h"
#include "strbuf.h"

// Lowers the AST into a simpler AST.
//
// * Removes STATEMENT_DEFER statements. Expands their contents at the end of
//   each scope and before return statements.
//
// * Removes EXPRESSION_IN expressions inside for loop, turning all for loops
//   into the simple form of:
//       for init; cond; post.
//
// * Generates zero initializers for all variables.
//
// * Hoists nested porcedure calls out and stores their results to unique
//   variables. This is necessary to ensure left-to-right evaluation and to
//   allow for massaging procedure calls.

typedef struct Lower Lower;
typedef struct Block Block;

struct Block {
	Block *prev;
	Array(const Node*) defers;
	Array(Node*) statements;
};

struct Lower {
	Tree *tree;
	Block *block;
	Sint32 unique_id;
};

static Node *infer(const Lower *lower, Node *node) {
	// if (!node) return 0;
	switch (node->kind) {
	case NODE_IDENTIFIER:
		return tree_clone_node(lower->tree, node);
	case NODE_PROCEDURE:
		return infer(lower, node->procedure.type);
	case NODE_EXPRESSION:
		switch (node->expression.kind) {
		case EXPRESSION_UNARY:
			return infer(lower, node->expression.unary.operand);
		case EXPRESSION_BINARY:
			return infer(lower, node->expression.binary.lhs);
		case EXPRESSION_CALL:
			return infer(lower, node->expression.call.operand);
		case EXPRESSION_CAST:
			return infer(lower, node->expression.cast.type ? node->expression.cast.type : node->expression.cast.expression);
		default:
			break;
		}
		break;
	case NODE_LITERAL_VALUE:
		switch (node->literal_value.literal) {
		case LITERAL_FLOAT:
			return tree_new_identifier(lower->tree, SCLIT("f64"));
		case LITERAL_INTEGER:
			return tree_new_identifier(lower->tree, SCLIT("i64"));
		case LITERAL_STRING:
			return tree_new_identifier(lower->tree, SCLIT("cstring"));
		default:
			break;
		}
		break;
	case NODE_FIELD:
		return infer(lower, node->field.type);
	case NODE_FIELD_LIST:
		// TODO(dweiler): Structure.
		return infer(lower, node->field_list.fields[0]);
	case NODE_TYPE:
		switch (node->type.kind) {
		case TYPE_PROCEDURE:
			return node->type.procedure.results ? infer(lower, node->type.procedure.results) : 0;
		default:
			break;
		}
		break;
	default:
		break;
	}
	return 0;
}

static Node *unique_identifier(Lower *lower) {
	StrBuf strbuf;
	strbuf_init(&strbuf);
	if (!strbuf_put_formatted(&strbuf, "_CODIN_%d", lower->unique_id)) {
		strbuf_free(&strbuf);
		return 0;
	}
	Node *node = tree_new_identifier(lower->tree, strbuf_result(&strbuf));
	strbuf_free(&strbuf);
	lower->unique_id++;
	return node;
}

static Node *lower_statement(Lower *lower, const Statement *statement);
static Node *lower_block_statement(Lower *lower, const BlockStatement *block_statement);
static Node *lower_node(Lower *lower, const Node *node);

static Node *lower_procedure(Lower *lower, const Procedure *procedure) {
	Tree *tree = lower->tree;
	Node *type = 0;
	if (procedure->type && !(type = tree_clone_node(tree, procedure->type))) {
		return 0;
	}
	Node *body = lower_block_statement(lower, &procedure->body->statement.block);
	return body ? tree_new_procedure(tree, procedure->flags, type, body) : 0;
}

static Node *lower_block_statement(Lower *lower, const BlockStatement *block_statement) {
	Block *block = malloc(sizeof *block);
	if (!block) {
		return 0;
	}

	block->prev = lower->block;
	block->defers = 0;
	block->statements = 0;

	lower->block = block;

	const Uint64 n_statements = array_size(block_statement->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		const Node *statement = block_statement->statements[i];
		if (node_is_statement(statement, STATEMENT_DEFER)) {
			if (!array_push(block->defers, statement)) {
				goto L_error;
			}
		} else {
			Node *node = lower_statement(lower, &statement->statement);
			if (!node || !array_push(block->statements, node)) {
				goto L_error;
			}
		}
	}

	// Expand defers in reverse order at the end of the block.
	if (n_statements && !node_is_statement(array_last(block_statement->statements), STATEMENT_RETURN)) {
		Array(const Node*) defers = block->defers;
		const Uint64 n_defers = array_size(defers);
		for (Uint64 i = n_defers - 1; i < n_defers; i--) {
			Node *node = lower_node(lower, defers[i]->statement.defer.statement);
			if (!node || !array_push(block->statements, node)) {
				goto L_error;
			}
		}
	}

	Node *result = tree_new_block_statement(lower->tree, block_statement->flags, block->statements);
	if (result) {
		lower->block = block->prev;
		array_free(block->defers);
		free(block);
		return result;
	}

L_error:
	lower->block = block->prev;
	array_free(block->statements);
	array_free(block->defers);
	free(block);
	return result;
}

static Node *lower_declaration_statement(Lower *lower, const DeclarationStatement *statement) {
	Tree *tree = lower->tree;

	Node *type = 0;
	if (statement->type && !(type = tree_clone_node(tree, statement->type))) {
		return 0;
	}

	Array(Node*) names = 0;
	Array(Node*) values = 0;
	const Uint64 n_names = array_size(statement->names);
	const Uint64 n_values = array_size(statement->values);

	if (!type && n_values == 1) {
		type = infer(lower, statement->values[0]);
	}

	for (Uint64 i = 0; i < n_names; i++) {
		Node *name = tree_clone_node(tree, statement->names[i]);
		if (!name || !array_push(names, name)) {
			goto L_error;
		}
	}
	for (Uint64 i = 0; i < n_values; i++) {
		Node *value = lower_node(lower, statement->values[i]);
		if (!value || !array_push(values, value)) {
			goto L_error;
		}
	}

	// Ensure n_values == n_names by explicitly initializing with = {}.
	for (Uint64 i = n_values; i < n_names; i++) {
		Node *value = tree_new_compound_literal(tree, 0, 0);
		if (!value || !array_push(values, value)) {
			goto L_error;
		}
	}

	Node *result = tree_new_declaration_statement(tree, type, names, values);
	if (result) {
		return result;
	}

L_error:
	array_free(values);
	array_free(names);

	return 0;
}

static Node *lower_return_statement(Lower *lower, const ReturnStatement *statement) {
	Array(Node*) statements = 0;
	for (Block *block = lower->block; block; block = block->prev) {
		const Uint64 n_defers = array_size(block->defers);
		for (Uint64 i = n_defers - 1; i < n_defers; i--) {
			Node *node = lower_node(lower, block->defers[i]->statement.defer.statement);
			if (!node || !array_push(statements, node)) {
				goto L_error;
			}
		}
	}
	Node *ret = tree_clone_return_statement(lower->tree, statement);
	if (!ret || !array_push(statements, ret)) {
		goto L_error;
	}
	// TODO(dweiler): Inherit the block flags here!
	Node *result = tree_new_block_statement(lower->tree, 0, statements);
	if (result) {
		return result;
	}
L_error:
	array_free(statements);
	return 0;
}

static Node *lower_if_statement(Lower *lower, const IfStatement *statement) {
	Tree *tree = lower->tree;

	Node *init = 0;
	Node *cond = 0;
	if ((statement->init && !(init = tree_clone_node(tree, statement->init))) ||
			(statement->cond && !(cond = tree_clone_node(tree, statement->cond))))
	{
		return 0;
	}

	// Optional else block.
	Node *elif = 0;
	if (statement->elif) {
		elif = lower_block_statement(lower, &statement->elif->statement.block);
		if (!elif) {
			return 0;
		}
	}

	Node *body = lower_block_statement(lower, &statement->body->statement.block);

	return body ? tree_new_if_statement(lower->tree, init, cond, body, elif) : 0;
}

static Node *lower_for_statement(Lower *lower, const ForStatement *statement) {
	Tree *tree = lower->tree;

	Node *body = lower_block_statement(lower, &statement->body->statement.block);
	if (!body) {
		return 0;
	}

	Array(Node*) names = 0;
	Array(Node*) values = 0;

	Array(Node*) assign_lhs = 0;
	Array(Node*) assign_rhs = 0;

	Node *init = 0;
	Node *cond = 0;
	Node *post = 0;

	// Lower "for in" into regular for init; cond; post.
	if (node_is_expression(statement->cond, EXPRESSION_IN)) {
		const InExpression *in = &statement->cond->expression.in;
		if (!node_is_expression(in->rhs, EXPRESSION_BINARY)) {
			return 0;
		}
		const BinaryExpression *bin = &in->rhs->expression.binary;
		const Uint64 n_names = array_size(in->lhs);
		if (n_names == 0) {
			Node *ident = unique_identifier(lower);
			if (!ident || !array_push(names, ident)) {
				goto L_error;
			}
		} else for (Uint64 i = 0; i < n_names; i++) {
			Node *name = lower_node(lower, in->lhs[i]);
			if (!name || !array_push(names, name)) {
				goto L_error;
			}
		}

		Node *lhs = lower_node(lower, bin->lhs);
		Node *rhs = lower_node(lower, bin->rhs);
		if (!lhs || !rhs || !array_push(values, lhs)) {
			goto L_error;
		}

		if (n_names == 2) {
			// Generate = {}.
			Node *value = tree_new_compound_literal(tree, 0, 0);
			if (!value || !array_push(values, value)) {
				goto L_error;
			}
		}

		Node *pred = array_last(names);
		init = tree_new_declaration_statement(tree, infer(lower, lhs), names, values);
		switch (bin->operation) {
		case OPERATOR_ELLIPSIS:
			FALLTHROUGH();
		case OPERATOR_RANGEFULL:
			cond = tree_new_binary_expression(tree, OPERATOR_LTEQ, pred, rhs);
			break;
		case OPERATOR_RANGEHALF:
			cond = tree_new_binary_expression(tree, OPERATOR_LT, pred, rhs);
			break;
		default:
			goto L_error;
		}

		if (!cond) {
			goto L_error;
		}

		// Generate the += 1 for the post statement.
		Node *literal_1 = tree_new_literal_value(tree, LITERAL_INTEGER, SCLIT("1"));
		if (!literal_1) {
			goto L_error;
		}
	
		if (!array_push(assign_lhs, pred)) {
			goto L_error;
		}
		if (!array_push(assign_rhs, literal_1)) {
			goto L_error;
		}

		post = tree_new_assignment_statement(tree, ASSIGNMENT_ADDEQ, assign_lhs, assign_rhs);
	} else if ((statement->init && !(init = lower_node(lower, statement->init))) ||
		         (statement->cond && !(cond = lower_node(lower, statement->cond))) ||
		         (statement->post && !(post = lower_node(lower, statement->post))))
	{
		goto L_error;
	}

	Node *result = tree_new_for_statement(tree, init, cond, body, post);
	if (result) {
		return result;
	}

L_error:
	array_free(assign_rhs);
	array_free(assign_lhs);

	array_free(values);
	array_free(names);

	return 0;
}

static Node *lower_expression_statement(Lower *lower, const ExpressionStatement *statement) {
	Node *expression = lower_node(lower, statement->expression);
	return expression ? tree_new_expression_statement(lower->tree, expression) : 0;
}

static Node *lower_statement(Lower *lower, const Statement *statement) {
	switch (statement->kind) {
	case STATEMENT_BLOCK:
		return lower_block_statement(lower, &statement->block);
	case STATEMENT_DECLARATION:
		return lower_declaration_statement(lower, &statement->declaration);
	case STATEMENT_RETURN:
		return lower_return_statement(lower, &statement->return_);
	case STATEMENT_IF:
		return lower_if_statement(lower, &statement->if_);
	case STATEMENT_FOR:
		return lower_for_statement(lower, &statement->for_);
	case STATEMENT_EXPRESSION:
		return lower_expression_statement(lower, &statement->expression);
	case STATEMENT_DEFER:
		// Should've been removed
		return false;
	default:
		return tree_clone_statement(lower->tree, statement);
	}
	UNREACHABLE();
}

static Node *lower_call_expression(Lower *lower, const CallExpression *expression) {
	Tree *tree = lower->tree;

	Node *operand = lower_node(lower, expression->operand);
	if (!operand) {
		return 0;
	}

	// We're going to generate a bunch of <ident> = <argument> in the block just
	// before the call so that the arguments are evaluated left to right.
	Array(Node*) arguments = 0;
	const Uint64 n_arguments = array_size(expression->arguments);
	for (Uint64 i = 0; i < n_arguments; i++) {
		Array(Node*) names = 0;
		Array(Node*) values = 0;

		Node *name = unique_identifier(lower);
		if (!name || !array_push(names, name)) {
			goto L_inner_error;
		}

		if (!array_push(arguments, name)) {
			goto L_inner_error;
		}

		Node *value = lower_node(lower, expression->arguments[i]);
		if (!value || !array_push(values, value)) {
			goto L_inner_error;
		}

		Node *statement = tree_new_declaration_statement(tree, infer(lower, value), names, values);
		if (!statement) {
			goto L_inner_error;
		}

		if (!array_push(lower->block->statements, statement)) {
			goto L_inner_error;
		}

		continue;

L_inner_error:
		array_free(values);
		array_free(names);
		goto L_error;
	}

	Node *result = tree_new_call_expression(tree, operand, arguments);
	if (result) {
		return result;
	}

L_error:
	array_free(arguments);
	return 0;
}

static Node *lower_expression(Lower *lower, const Expression *expression) {
	switch (expression->kind) {
	case EXPRESSION_CALL:
		return lower_call_expression(lower, &expression->call);
	default:
		return tree_clone_expression(lower->tree, expression);
	}
	UNREACHABLE();
}

static Node *lower_node(Lower *lower, const Node *node) {
	switch (node->kind) {
	case NODE_PROCEDURE:
		return lower_procedure(lower, &node->procedure);
	case NODE_STATEMENT:
		return lower_statement(lower, &node->statement);
	case NODE_EXPRESSION:
		return lower_expression(lower, &node->expression);
	default:
		return tree_clone_node(lower->tree, node);
	}
	UNREACHABLE();
}

Tree *lower(const Tree *tree) {
	Lower lower;
	if (!(lower.tree = malloc(sizeof *tree))) {
		return 0;
	}

	lower.block = 0;
	lower.unique_id = 0;
	tree_init(lower.tree);

	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		Node *statement = lower_statement(&lower, &tree->statements[i]->statement);
		if (!statement || !array_push(lower.tree->statements, statement)) {
			goto L_error;
		}
	}

	ASSERT(!lower.block);

	return lower.tree;

L_error:
	tree_free(lower.tree);
	return 0;
}