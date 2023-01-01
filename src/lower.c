#include <stdlib.h>
#include <stdio.h>

#include "lower.h"
#include "tree.h"

// Lowers the AST into a simpler AST.
//
// * Removes STATEMENT_DEFER statements.
// * Removes EXPRESSION_IN expressions.
// * Generates zero initializers for all variables.

typedef struct Lower Lower;
typedef struct Block Block;

struct Block {
	Array(Node*) defers;
	Array(Node*) *statements;
};

struct Lower {
	Tree *tree;
	Array(Block*) blocks;
};

static Bool lower_statement(Lower *lower, Statement *statement);

// Rudimentary type-inference
Node *infer(Tree *tree, Node *node) {
	switch (node->kind) {
	case NODE_EXPRESSION:
		switch (node->expression.kind) {
		case EXPRESSION_CALL:
			// TODO(dweiler): Need symbol table.
			printf("Unimplemented inference for call\n");
			return 0;
		case EXPRESSION_BINARY:
			return tree_new_identifier(tree, SCLIT("bool"));
		case EXPRESSION_UNARY:
			return infer(tree, node->expression.unary.operand);
		case EXPRESSION_CAST:
			if (node->expression.cast.type) {
				return infer(tree, node->expression.cast.type);
			} else {
				return infer(tree, node->expression.cast.expression);
			}
		case EXPRESSION_DEREFERENCE:
			printf("Unimplemented type inference for dereference\n");
			// TODO(dweiler): Need type system.
			return 0;
		case EXPRESSION_SELECTOR:
			printf("Unimplemented type inference for selector\n");
			// TODO(dweiler): Neeed symbol table.
			return 0;
		case EXPRESSION_IN:
			return tree_new_identifier(tree, SCLIT("bool"));
		case EXPRESSION_ASSERTION:
			return infer(tree, node->expression.assertion.type);
		}
		break;
	case NODE_STATEMENT:
		printf("Cannot infer type in a statement\n");
		return 0;
	case NODE_IDENTIFIER:
		return node;
	case NODE_VALUE:
		return infer(tree, node->value.value);
	case NODE_LITERAL_VALUE:
		switch (node->literal_value.literal) {
		case LITERAL_FLOAT:
			return tree_new_identifier(tree, SCLIT("f64"));
		case LITERAL_INTEGER:
			return tree_new_identifier(tree, SCLIT("i64"));
		case LITERAL_IMAGINARY:
			return 0;
		case LITERAL_RUNE:
			return tree_new_identifier(tree, SCLIT("rune"));
		case LITERAL_STRING:
			return tree_new_identifier(tree, SCLIT("cstring"));
		default:
			return 0;
		}
		break;
	case NODE_COMPOUND_LITERAL:
		return infer(tree, node->compound_literal.type);
	case NODE_FIELD:
		return infer(tree, node->field.type);
	case NODE_FIELD_LIST:
		if (array_size(node->field_list.fields)) {
			return infer(tree, node->field_list.fields[0]);
		} else {
			return 0;
		}
	case NODE_PROCEDURE:
		return 0;
	case NODE_PROCEDURE_GROUP:
		return 0;
	case NODE_DIRECTIVE:
		return 0;
	case NODE_TYPE:
		// TODO(dweiler): Need type system.
		switch (node->type.kind) {
		case TYPE_PROCEDURE:
		case TYPE_ARRAY:
		case TYPE_DYNAMIC_ARRAY:
		case TYPE_MULTI_POINTER:
		case TYPE_POINTER:
		case TYPE_SLICE:
			return 0;
		}
	}
	return 0;
}

static Bool lower_block_statement(Lower *lower, BlockStatement *statement) {
	Block *block = malloc(sizeof *block);
	if (!block) {
		return false;
	}

	Array(Node*) *statements = &statement->statements;
	block->statements = statements;
	block->defers = 0;
	if (!array_push(lower->blocks, block)) {
		free(block);
		return false;
	}

	Uint64 n_statements = array_size(statement->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		Node *node = statement->statements[i];
		ASSERT(node_is_kind(node, NODE_STATEMENT));
		if (node_is_statement(node, STATEMENT_DEFER)) {
			array_push(block->defers, node);
			statement->statements[i] = tree_new_empty_statement(lower->tree);
		} else if (!lower_statement(lower, &node->statement)) {
			return false;
		}
	}

	// Lowering can change the # of statements.
	n_statements = array_size(statement->statements);

	// Expand defer blocks in reverse order at the end of this block.
	if (n_statements && !node_is_statement(statement->statements[n_statements - 1], STATEMENT_RETURN)) {
		Array(Node*) defers = block->defers;
		const Uint64 n_defers = array_size(defers);
		for (Uint64 j = n_defers - 1; j < n_defers; j--) {
			array_push(*statements, defers[j]->statement.defer.statement);
		}
	}

	array_free(block->defers);
	free(block);
	array_meta(lower->blocks)->size--;

	return true;
}

static Bool lower_declaration_statement(Lower *lower, DeclarationStatement *statement) {
	const Uint64 n_names = array_size(statement->names);
	const Uint64 n_values = array_size(statement->values);
	if (n_values && !statement->type) {
		statement->type = infer(lower->tree, statement->values[0]);
	}
	// Generate zero initialization.
	for (Uint64 i = n_values; i < n_names; i++) {
		// TODO(dweiler): Need a type system.
		Node *init = tree_new_literal_value(lower->tree, LITERAL_INTEGER, SCLIT("0"));
		array_push(statement->values, init);
	}
	for (Uint64 i = 0; i < n_values; i++) {
		Node *value = statement->values[i];
		if (node_is_kind(value, NODE_PROCEDURE)) {
			Node *block = value->procedure.body;
			ASSERT(node_is_statement(block, STATEMENT_BLOCK));
			if (!lower_block_statement(lower, &block->statement.block)) {
				return false;
			}
		}
	}
	return true;
}

static Bool lower_if_statement(Lower *lower, IfStatement *statement) {
	if (!lower_block_statement(lower, &statement->body->statement.block)) {
		return false;
	}
	if (statement->elif && !lower_block_statement(lower, &statement->elif->statement.block)) {
		return false;
	}
	return true;
}

static Bool lower_return_statement(Lower *lower, ReturnStatement *statement) {
	// Search the current block for the return statement index.
	const Block *block = lower->blocks[array_size(lower->blocks) - 1];
	const Uint64 n_statements = array_size(*block->statements);
	Uint64 index = 0;
	for (Uint64 i = n_statements - 1; i < n_statements; i--) {
		Node *node = (*block->statements)[i];
		if (node_is_statement(node, STATEMENT_RETURN) && &node->statement.return_ == statement) {
			index = i;
			break;
		}
	}

	// Expand defer blocks in reverse order before the return statement.
	Array(Block*) blocks = lower->blocks;
	const Uint64 n_blocks = array_size(blocks);
	for (Uint64 i = n_blocks - 1; i < n_blocks; i--) {
		Array(Node*) defers = blocks[i]->defers;
		const Uint64 n_defers = array_size(defers);
		for (Uint64 j = n_defers - 1; j < n_defers; j--) {
			array_insert(*block->statements, index, defers[j]->statement.defer.statement);
			index++;
		}
	}

	return true;
}

static Bool lower_for_statement(Lower *lower, ForStatement *statement) {
	// Lowers if in into regular if init; cond; post
	if (!lower_block_statement(lower, &statement->body->statement.block)) {
		return false;
	}

	if (!node_is_expression(statement->cond, EXPRESSION_IN)) {
		return true;
	}

	InExpression *in = &statement->cond->expression.in;
	if (!node_is_expression(in->rhs, EXPRESSION_BINARY)) {
		return true;
	}

	BinaryExpression *binary = &in->rhs->expression.binary;

	Array(Node*) names = 0;
	Array(Node*) values = 0;
	switch (array_size(in->lhs)) {
	case 0:
		array_push(names, tree_new_identifier(lower->tree, SCLIT("_CODIN_for")));
		array_push(values, binary->lhs);
		break;
	case 1:
		array_push(names, in->lhs[0]);
		array_push(values, binary->lhs);
		break;
	case 2:
		array_push(names, in->lhs[0]);
		array_push(names, in->lhs[1]);
		array_push(values, binary->lhs);
		array_push(values, tree_new_compound_literal(lower->tree, 0, 0));
		break;
	}

	array_free(in->lhs);

	Node *pred = names[array_size(names) - 1];
	Node *init = tree_new_declaration_statement(lower->tree, infer(lower->tree, values[0]), names, values);
	Node *cond = 0;
	switch (binary->operation) {
	case OPERATOR_ELLIPSIS:
		FALLTHROUGH();
	case OPERATOR_RANGEFULL:
		cond = tree_new_binary_expression(lower->tree, OPERATOR_LTEQ, pred, binary->rhs);
		break;
	case OPERATOR_RANGEHALF:
		cond = tree_new_binary_expression(lower->tree, OPERATOR_LT, pred, binary->rhs);
		break;
	default:
		break;
	}

	Array(Node*) lhs = 0;
	Array(Node*) rhs = 0;
	array_push(lhs, pred);
	array_push(rhs, tree_new_literal_value(lower->tree, LITERAL_INTEGER, SCLIT("1")));

	Node *post = tree_new_assignment_statement(lower->tree, ASSIGNMENT_ADDEQ, lhs, rhs);

	statement->init = init;
	statement->cond = cond;
	statement->post = post;

	return true;
}

static Bool lower_statement(Lower *lower, Statement *statement) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:
		return true;
	case STATEMENT_BLOCK:
		return lower_block_statement(lower, &statement->block);
	case STATEMENT_IMPORT:
		return true;
	case STATEMENT_EXPRESSION:
		return true;
	case STATEMENT_ASSIGNMENT:
		return true;
	case STATEMENT_DECLARATION:
		return lower_declaration_statement(lower, &statement->declaration);
	case STATEMENT_IF:
		return lower_if_statement(lower, &statement->if_);
	case STATEMENT_RETURN:
		return lower_return_statement(lower, &statement->return_);
	case STATEMENT_FOR:
		return lower_for_statement(lower, &statement->for_);
	case STATEMENT_DEFER:
		return false;
	}
	return false;
}

Bool lower(Tree *tree) {
	Lower lower;
	lower.tree = tree;
	lower.blocks = 0;

	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		Node *node = tree->statements[i];
		ASSERT(node_is_kind(node, NODE_STATEMENT));
		if (!lower_statement(&lower, &node->statement)) {
			return false;
		}
	}

	const Uint64 n_nodes = array_size(tree->nodes);
	for (Uint64 i = 0; i < n_nodes; i++) {
		Node *node = tree->nodes[i];
		if (node_is_statement(node, STATEMENT_DECLARATION)) {
			if (!lower_declaration_statement(&lower, &node->statement.declaration)) {
				return false;
			}
		}
	}

	ASSERT(array_size(lower.blocks) == 0);
	array_free(lower.blocks);

	return true;
}