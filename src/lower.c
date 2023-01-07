#include <stdlib.h>
#include <stdio.h>

#include "lower.h"
#include "tree.h"
#include "strbuf.h"
#include "context.h"

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
// * Hoists nested procedure calls out and stores their results to unique
//   variables. This is necessary to ensure left-to-right evaluation and to
//   allow for massaging procedure calls.
typedef struct Lower Lower;
typedef struct Block Block;
typedef struct Symbol Symbol;

struct Symbol {
	String name;
	Node *type;
};

struct Block {
	Block *prev;
	Array(const Node*) defers;
	Array(Node*) statements;
	Array(Symbol) symbols;
};

struct Lower {
	Tree *tree;
	Context *context;
	Block *block;
	Sint32 unique_id;
	Array(Symbol) symbols;
};

static Bool define(Lower *lower, String name, Node *type) {
	Context *context = lower->context;
	Block *block = lower->block;
	Array(Symbol) *symbols = block ? &block->symbols : &lower->symbols;
	return array_push(*symbols, ((Symbol) { name, type }));
}

static const Symbol *symbol(const Lower *lower, String name) {
	for (Block *block = lower->block; block; block = block->prev) {
		const Array(Symbol) symbols = block->symbols;
		const Size n_symbols = array_size(symbols);
		for (Size i = 0; i < n_symbols; i++) {
			const Symbol *sym = &symbols[i];
			if (string_compare(sym->name, name)) {
				return sym;
			}
		}
	}

	const Array(Symbol) symbols = lower->symbols;
	const Size n_symbols = array_size(symbols);
	for (Size i = 0; i < n_symbols; i++) {
		const Symbol *sym = &symbols[i];
		if (string_compare(sym->name, name)) {
			return sym;
		}
	}

	return 0;
}

static Node *infer(const Lower *lower, Node *node) {
	switch (node->kind) {
	case NODE_IDENTIFIER:
		{
			const Symbol *sym = symbol(lower, node->identifier.contents);
			return sym ? sym->type : 0;
		}
	case NODE_PROCEDURE:
		return infer(lower, node->procedure.type);
	case NODE_EXPRESSION:
		switch (node->expression.kind) {
		case EXPRESSION_UNARY:
			{
				const UnaryExpression *unary = &node->expression.unary;
				if (unary->operation == OPERATOR_AND) {
					return tree_new_pointer_type(lower->tree, infer(lower, unary->operand));
				} else {
					return infer(lower, unary->operand);
				}
			}
			break;
		case EXPRESSION_DEREFERENCE:
			{
				const Node *operand = infer(lower, node->expression.dereference.operand);
				if (node_is_type(operand, TYPE_POINTER)) {
					return infer(lower, operand->type.pointer.type);
				} else if (node_is_type(operand, TYPE_MULTI_POINTER)) {
					return infer(lower, operand->type.multi_pointer.type);
				} else {
					return 0;
				}
			}
			break;
		case EXPRESSION_BINARY:
			return infer(lower, node->expression.binary.rhs);
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
		case TYPE_POINTER:
			return infer(lower, node->type.pointer.type);
		case TYPE_MULTI_POINTER:
			return infer(lower, node->type.multi_pointer.type);
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
	strbuf_init(&strbuf, lower->context);
	if (!strbuf_put_formatted(&strbuf, "_CODIN_%d", lower->unique_id)) {
		return 0;
	}
	Node *node = tree_new_identifier(lower->tree, strbuf_result(&strbuf));
	lower->unique_id++;
	return node;
}

static Node *lower_statement(Lower *lower, const Statement *statement);
static Node *lower_block_statement(Lower *lower, const BlockStatement *block_statement, Bool new_block);
static Node *lower_node(Lower *lower, const Node *node);

static Bool push_block(Lower *lower) {
	Context *context = lower->context;
	Allocator *allocator = context->allocator;
	Block *block = allocator->allocate(allocator, sizeof *block);
	if (!block) {
		return false;
	}

	block->prev = lower->block;
	block->defers = 0;
	block->statements = 0;
	block->symbols = 0;

	lower->block = block;

	return true;
}

static void pop_block(Lower *lower) {
	Context *context = lower->context;
	Allocator *allocator = context->allocator;
	Block *block = lower->block;
	lower->block = block->prev;
	array_free(block->symbols);
	array_free(block->defers);
	allocator->deallocate(allocator, block);
}

static Node *lower_procedure(Lower *lower, const Procedure *procedure) {
	Tree *tree = lower->tree;
	Node *type = 0;
	if (procedure->type && !(type = lower_node(lower, procedure->type))) {
		return 0;
	}

	if (!push_block(lower)) {
		return 0;
	}

	// Define the local variables of the procedure after we lower block to enter
	// that block scope.
	const FieldList *arguments = &type->type.procedure.params->field_list;
	const Size n_arguments = array_size(arguments->fields);
	for (Size i = 0; i < n_arguments; i++) {
		const Field *field = &arguments->fields[i]->field;
		define(lower, field->name->identifier.contents, infer(lower, field->type));
	}

	Node *body = lower_block_statement(lower, &procedure->body->statement.block, false);

	pop_block(lower);

	return body ? tree_new_procedure(tree, procedure->flags, type, body) : 0;
}

static Node *lower_block_statement(Lower *lower, const BlockStatement *block_statement, Bool new_block) {
	Context *context = lower->context;

	if (new_block && !push_block(lower)) {
		return 0;
	}

	Block *block = lower->block;

	const Size n_statements = array_size(block_statement->statements);
	for (Size i = 0; i < n_statements; i++) {
		const Node *statement = block_statement->statements[i];
		if (node_is_statement(statement, STATEMENT_DEFER)) {
			if (!array_push(block->defers, statement)) {
				return 0;
			}
		} else {
			Node *node = lower_statement(lower, &statement->statement);
			if (!node || !array_push(block->statements, node)) {
				return 0;
			}
		}
	}

	// Expand defers in reverse order at the end of the block.
	if (n_statements && !node_is_statement(array_last(block_statement->statements), STATEMENT_RETURN)) {
		Array(const Node*) defers = block->defers;
		const Size n_defers = array_size(defers);
		for (Size i = n_defers - 1; i < n_defers; i--) {
			Node *node = lower_node(lower, defers[i]->statement.defer.statement);
			if (!node || !array_push(block->statements, node)) {
				return 0;
			}
		}
	}

	Node *result = tree_new_block_statement(lower->tree, block_statement->flags, block->statements);
	if (!result) {
		return 0;
	}

	if (new_block) {
		pop_block(lower);
	}

	return result;
}

static Node *lower_declaration_statement(Lower *lower, const DeclarationStatement *statement) {
	Context *context = lower->context;

	Tree *tree = lower->tree;

	Node *type = 0;
	if (statement->type && !(type = lower_node(lower, statement->type))) {
		return 0;
	}

	Array(Node*) names = 0;
	Array(Node*) values = 0;
	const Size n_names = array_size(statement->names);
	const Size n_values = array_size(statement->values);

	if (!type && n_values == 1) {
		type = infer(lower, statement->values[0]);
	}

	// Define early as possible.
	for (Size i = 0; i < n_names; i++) {
		define(lower, statement->names[i]->identifier.contents, type);
	}

	for (Size i = 0; i < n_names; i++) {
		Node *name = tree_clone_node(tree, statement->names[i]);
		if (!name || !array_push(names, name)) {
			return 0;
		}
	}
	for (Size i = 0; i < n_values; i++) {
		Node *value = lower_node(lower, statement->values[i]);
		if (!value || !array_push(values, value)) {
			return 0;
		}
	}

	// Ensure n_values == n_names by explicitly initializing with = {}.
	for (Size i = n_values; i < n_names; i++) {
		Node *value = tree_new_compound_literal(tree, 0, 0);
		if (!value || !array_push(values, value)) {
			return 0;
		}
	}

	return tree_new_declaration_statement(tree, type, names, values);
}

static Node *lower_return_statement(Lower *lower, const ReturnStatement *statement) {
	Context *context = lower->context;

	Array(Node*) statements = 0;
	for (Block *block = lower->block; block; block = block->prev) {
		const Size n_defers = array_size(block->defers);
		for (Size i = n_defers - 1; i < n_defers; i--) {
			Node *node = lower_node(lower, block->defers[i]->statement.defer.statement);
			if (!node || !array_push(statements, node)) {
				return 0;
			}
		}
	}

	Array(Node*) results = 0;
	const Size n_results = array_size(statement->results);
	for (Size i = 0; i < n_results; i++) {
		Node *result = lower_node(lower, statement->results[i]);
		if (!result || !array_push(results, result)) {
			return 0;
		}
	}

	Node *return_ = tree_new_return_statement(lower->tree, results);
	if (!return_) {
		return 0;
	}

	if (!statements) {
		return return_;
	}

	// We have more than one statement generated at the return.
	if (!array_push(statements, return_)) {
		return 0;
	}

	// TODO(dweiler): Inherit the block flags here!
	return tree_new_block_statement(lower->tree, 0, statements);
}

static Node *lower_if_statement(Lower *lower, const IfStatement *statement) {
	// Tree *tree = lower->tree;

	Node *init = 0;
	Node *cond = 0;
	if ((statement->init && !(init = lower_node(lower, statement->init))) ||
			(statement->cond && !(cond = lower_node(lower, statement->cond))))
	{
		return 0;
	}

	// Optional else block.
	Node *elif = 0;
	if (statement->elif) {
		elif = lower_block_statement(lower, &statement->elif->statement.block, true);
		if (!elif) {
			return 0;
		}
	}

	Node *body = lower_block_statement(lower, &statement->body->statement.block, true);

	return body ? tree_new_if_statement(lower->tree, init, cond, body, elif) : 0;
}

static Node *lower_for_statement(Lower *lower, const ForStatement *statement) {
	Context *context = lower->context;

	Tree *tree = lower->tree;

	Array(Node*) names = 0;
	Array(Node*) values = 0;

	Array(Node*) assign_lhs = 0;
	Array(Node*) assign_rhs = 0;

	Node *init = 0;
	Node *cond = 0;
	Node *post = 0;

	// Lowers
	//
	//	for in <expression>
	//	for <ident:a> in <expression>
	//	for <ident:a>, <ident:a> in <expression>
	//
	// Into
	//
	//	<statement:init>; <expression:cond>; <statement:post>
	if (node_is_expression(statement->cond, EXPRESSION_IN)) {
		const InExpression *in = &statement->cond->expression.in;
		if (!node_is_expression(in->rhs, EXPRESSION_BINARY)) {
			return 0;
		}
		const BinaryExpression *bin = &in->rhs->expression.binary;
		const Size n_names = array_size(in->lhs);
		if (n_names == 0) {
			Node *ident = unique_identifier(lower);
			if (!ident || !array_push(names, ident)) {
				return 0;
			}
		} else for (Size i = 0; i < n_names; i++) {
			Node *name = lower_node(lower, in->lhs[i]);
			if (!name || !array_push(names, name)) {
				return 0;
			}
		}

		Node *lhs = lower_node(lower, bin->lhs);
		Node *rhs = lower_node(lower, bin->rhs);
		if (!lhs || !rhs || !array_push(values, lhs)) {
			return 0;
		}

		Node *type = infer(lower, lhs);

		if (n_names == 2) {
			// Generate = {}.
			Node *value = tree_new_compound_literal(tree, type, 0);
			if (!value || !array_push(values, value)) {
				return 0;
			}
		}

		Node *pred = array_last(names);
		define(lower, pred->identifier.contents, type);
		init = tree_new_declaration_statement(tree, type, names, values);
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
			return 0;
		}

		if (!cond) {
			return 0;
		}

		// Generate the += 1 for the post statement.
		Node *literal_1 = tree_new_literal_value(tree, LITERAL_INTEGER, SCLIT("1"));
		if (!literal_1) {
			return 0;
		}
	
		if (!array_push(assign_lhs, pred)) {
			return 0;
		}
		if (!array_push(assign_rhs, literal_1)) {
			return 0;
		}

		post = tree_new_assignment_statement(tree, ASSIGNMENT_ADDEQ, assign_lhs, assign_rhs);
	}

	// Lowers
	//
	//	for <statetement:init>; <expression:cond>; <statement:post>
	//	{
	//		<statement:block>
	//	}
	//
	// Into
	//	{
	//		<statement:init>
	//		for
	//		{
	//			if !<expression:cond> do break;
	//			<statement:block>;
	//			<statement:post>;
	//		}
	//	}
	push_block(lower);
	if (statement->init && !(init = lower_node(lower, statement->init))) {
		return 0;
	}

	push_block(lower);
	if (statement->cond && !(cond = lower_node(lower, statement->cond))) {
		return 0;
	}

	// TODO(dweiler): Clean this up.
	Uint64 n = array_size(lower->block->statements);
	Node *body = lower_block_statement(lower, &statement->body->statement.block, false);
	Node *not_cond = tree_new_unary_expression(tree, OPERATOR_NOT, cond);
	Array(Node*) break_ = 0;
	array_push(break_, tree_new_break_statement(tree));
	Node *do_break = tree_new_block_statement(tree, 0, break_);
	Node *cond_if = tree_new_if_statement(tree, 0, not_cond, do_break, 0);
	array_insert(body->statement.block.statements, n, cond_if);
	if (statement->post) post = lower_node(lower, statement->post);
	array_push(body->statement.block.statements, post);
	pop_block(lower);
	Node *for_ = tree_new_for_statement(tree, 0, 0, body, 0);
	pop_block(lower);

	Array(Node*) statements = 0;
	array_push(statements, init);
	array_push(statements, for_);
	return tree_new_block_statement(tree, 0, statements);
}

static Node *lower_expression_statement(Lower *lower, const ExpressionStatement *statement) {
	Node *expression = lower_node(lower, statement->expression);
	return expression ? tree_new_expression_statement(lower->tree, expression) : 0;
}

static Node *lower_statement(Lower *lower, const Statement *statement) {
	switch (statement->kind) {
	case STATEMENT_BLOCK:
		return lower_block_statement(lower, &statement->block, true);
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
	Context *context = lower->context;

	Tree *tree = lower->tree;

	Node *operand = lower_node(lower, expression->operand);
	if (!operand) {
		return 0;
	}

	// We're going to generate a bunch of <ident> = <argument> in the block just
	// before the call so that the arguments are evaluated left to right.
	Array(Node*) arguments = 0;
	const Size n_arguments = array_size(expression->arguments);
	for (Size i = 0; i < n_arguments; i++) {
		Array(Node*) names = 0;
		Array(Node*) values = 0;

		Node *name = unique_identifier(lower);
		if (!name || !array_push(names, name)) {
			return 0;
		}

		if (!array_push(arguments, name)) {
			return 0;
		}

		Node *value = lower_node(lower, expression->arguments[i]);
		if (!value || !array_push(values, value)) {
			return 0;
		}

		Node *type = infer(lower, value);
		define(lower, name->identifier.contents, type);

		Node *statement = tree_new_declaration_statement(tree, type, names, values);
		if (!statement) {
			return 0;
		}

		if (!array_push(lower->block->statements, statement)) {
			return 0;
		}
	}

	return tree_new_call_expression(tree, operand, arguments);
}

static Node *lower_unary_expression(Lower *lower, const UnaryExpression *expression) {
	Node *operand = lower_node(lower, expression->operand);
	return operand ? tree_new_unary_expression(lower->tree, expression->operation, operand) : 0;
}

static Node *lower_binary_expression(Lower *lower, const BinaryExpression *expression) {
	Context *context = lower->context;

	Node *lhs = lower_node(lower, expression->lhs);
	Node *rhs = lower_node(lower, expression->rhs);

	Array(Node*) *statements = &lower->block->statements;

	Node *lhs_name = unique_identifier(lower);
	Node *rhs_name = unique_identifier(lower);
	Node *lhs_type = infer(lower, lhs);
	Node *rhs_type = infer(lower, rhs);
	define(lower, lhs_name->identifier.contents, lhs_type);
	define(lower, rhs_name->identifier.contents, rhs_type);

	Array(Node*) lhs_names = 0;
	Array(Node*) rhs_names = 0;
	array_push(lhs_names, lhs_name);
	array_push(rhs_names, rhs_name);
	Array(Node*) lhs_values = 0;
	Array(Node*) rhs_values = 0;
	array_push(lhs_values, lhs);
	array_push(rhs_values, rhs);

	Node *lhs_decl = tree_new_declaration_statement(lower->tree, lhs_type, lhs_names, lhs_values);
	Node *rhs_decl = tree_new_declaration_statement(lower->tree, rhs_type, rhs_names, rhs_values);

	array_push(*statements, lhs_decl);
	array_push(*statements, rhs_decl);

	return lhs && rhs ? tree_new_binary_expression(lower->tree, expression->operation, lhs_name, rhs_name) : 0;
}

static Node *lower_expression(Lower *lower, const Expression *expression) {
	switch (expression->kind) {
	case EXPRESSION_CALL:
		return lower_call_expression(lower, &expression->call);
	case EXPRESSION_UNARY:
		return lower_unary_expression(lower, &expression->unary);
	case EXPRESSION_BINARY:
		return lower_binary_expression(lower, &expression->binary);
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
	Context *context = tree->context;
	Allocator *allocator = context->allocator;

	Lower lower;
	lower.context = context;
	lower.tree = allocator->allocate(allocator, sizeof *tree);
	if (!lower.tree) {
		return 0;
	}

	lower.block = 0;
	lower.unique_id = 0;
	lower.symbols = 0;
	lower.context = context;
	tree_init(lower.tree, context);

	static const String BUILTIN_TYPES[] = {
		SLIT("i8"),  SLIT("i16"), SLIT("i32"), SLIT("i64"), SLIT("i128"),
		SLIT("u8"),  SLIT("u16"), SLIT("u32"), SLIT("u64"), SLIT("u128"),
		SLIT("b8"),  SLIT("b16"), SLIT("b32"), SLIT("b64"),
		SLIT("f16"), SLIT("f32"), SLIT("f64"),
	};

	for (Size i = 0; i < sizeof(BUILTIN_TYPES) / sizeof(BUILTIN_TYPES[0]); i++) {
		Node *type = tree_new_identifier(lower.tree, BUILTIN_TYPES[i]);
		define(&lower, BUILTIN_TYPES[i], type);
	}

	const Size n_statements = array_size(tree->statements);
	for (Size i = 0; i < n_statements; i++) {
		Node *statement = lower_statement(&lower, &tree->statements[i]->statement);
		if (!statement || !array_push(lower.tree->statements, statement)) {
			return 0;
		}
	}

	ASSERT(!lower.block);

	array_free(lower.symbols);
	return lower.tree;
}