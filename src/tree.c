#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "tree.h"

static Node *new_node(Tree *tree, NodeKind kind) {
	Node *node = calloc(sizeof *node, 1);
	node->kind = kind;
	array_push(tree->nodes, node);
	return node;
}

static Node *new_expression(Tree *tree, ExpressionKind kind) {
	Node *node = new_node(tree, NODE_EXPRESSION);
	node->expression.kind = kind;
	return node;
}

static Node *new_statement(Tree *tree, StatementKind kind) {
	Node *node = new_node(tree, NODE_STATEMENT);
	node->statement.kind = kind;
	return node;
}

Node *tree_new_unary_expression(Tree *tree, Operator operation, Node *operand) {
	Node *node = new_expression(tree, EXPRESSION_UNARY);
	UnaryExpression *expression = &node->expression.unary;
	expression->operation = operation;
	expression->operand = operand;
	return node;
}

Node *tree_new_binary_expression(Tree *tree, Operator operation, Node *lhs, Node *rhs) {
	Node *node = new_expression(tree, EXPRESSION_BINARY);
	BinaryExpression *expression = &node->expression.binary;
	expression->operation = operation;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return node;
}

Node *tree_new_cast_expression(Tree *tree, Node *type, Node *expr) {
	Node *node = new_expression(tree, EXPRESSION_CAST);
	CastExpression *expression = &node->expression.cast;
	expression->expression = expr;
	expression->type = type;
	return node;
}

Node *tree_new_selector_expression(Tree *tree, Node *operand, Node *identifier) {
	Node *node = new_expression(tree, EXPRESSION_SELECTOR);
	SelectorExpression *expression = &node->expression.selector;
	ASSERT(identifier->kind == NODE_IDENTIFIER);
	expression->operand = operand;
	expression->identifier = identifier;
	return node;
}

Node *tree_new_call_expression(Tree *tree, Node *operand, Array(Node*) arguments) {
	Node *node = new_expression(tree, EXPRESSION_CALL);
	CallExpression *expression = &node->expression.call;
	expression->operand = operand;
	expression->arguments = arguments;
	return node;
}

Node *tree_new_assertion_expression(Tree *tree, Node *operand, Node *type) {
	Node *node = new_expression(tree, EXPRESSION_ASSERTION);
	AssertionExpression *expression = &node->expression.assertion;
	expression->operand = operand;
	expression->type = type;
	return node;
}

Node *tree_new_empty_statement(Tree *tree) {
	Node *node = new_statement(tree, STATEMENT_EMPTY);
	return node;
}

Node *tree_new_import_statement(Tree *tree, String package) {
	Node *node = new_statement(tree, STATEMENT_IMPORT);
	ImportStatement *statement = &node->statement.import;
	statement->package = package;
	return node;
}

Node *tree_new_expression_statement(Tree *tree, Node *expression) {
	Node *node = new_statement(tree, STATEMENT_EXPRESSION);
	ExpressionStatement *statement = &node->statement.expression;
	statement->expression = expression;
	return node;
}

Node *tree_new_block_statement(Tree *tree, Array(Node*) statements) {
	Node *node = new_statement(tree, STATEMENT_BLOCK);
	BlockStatement *statement = &node->statement.block;
	statement->statements = statements;
	return node;
}

Node *tree_new_assignment_statement(Tree *tree, Assignment assignment, Array(Node*) lhs, Array(Node*) rhs) {
	Node *node = new_statement(tree, STATEMENT_ASSIGNMENT);
	AssignmentStatement *statement = &node->statement.assignment;
	statement->assignment = assignment;
	statement->lhs = lhs;
	statement->rhs = rhs;
	return node;
}

Node *tree_new_identifier(Tree *tree, String contents) {
	Node *node = new_node(tree, NODE_IDENTIFIER);
	Identifier *identifier = &node->identifier;
	identifier->contents = contents;
	return node;
}

Node *tree_new_declaration_statement(Tree *tree, Node *type, Array(Node*) names, Array(Node*) values) {
	Node *node = new_statement(tree, STATEMENT_DECLARATION);
	DeclarationStatement *statement = &node->statement.declaration;
	statement->type = type;
	statement->names = names;
	statement->values = values;
	return node;
}

Node *tree_new_value(Tree *tree, Node *field, Node *val) {
	Node *node = new_node(tree, NODE_VALUE);
	Value *value = &node->value;
	value->field = field;
	value->value = val;
	return node;
}

Node *tree_new_literal_value(Tree *tree, Literal literal, String value) {
	Node *node = new_node(tree, NODE_LITERAL_VALUE);
	LiteralValue *literal_value = &node->literal_value;
	literal_value->literal = literal;
	literal_value->value = value;
	return node;
}

Node *tree_new_compound_literal(Tree *tree, Node *type, Array(Node*) elements) {
	Node *node = new_node(tree, NODE_COMPOUND_LITERAL);
	CompoundLiteral *compound_literal = &node->compound_literal;
	compound_literal->type = type;
	compound_literal->elements = elements;
	return node;
}

Node *tree_new_field_list(Tree *tree, Array(Node*) fields) {
	Node *node = new_node(tree, NODE_FIELD_LIST);
	FieldList *field_list = &node->field_list;
	field_list->fields = fields;
	return node;
}

Node *tree_new_procedure(Tree *tree, Node *type, Node *body) {
	Node *node = new_node(tree, NODE_PROCEDURE);
	Procedure *procedure = &node->procedure;
	procedure->type = type;
	procedure->body = body;
	return node;
}

void tree_init(Tree *tree) {
	tree->nodes = 0;
	tree->statements = 0;
}

void tree_free(Tree *tree) {
	const Uint64 n_nodes = array_size(tree->nodes);
	for (Uint64 i = 0; i < n_nodes; i++) {
		free(tree->nodes[i]);
	}
	array_free(tree->nodes);
}

void tree_dump_node(const Node *node, Sint32 depth, Bool nl);

static void tree_dump_pad(Sint32 depth) {
	for (Sint32 i = 0; i < depth; i++) {
		printf("..");
	}
}

static void tree_dump_unary_expression(const UnaryExpression *expression, Sint32 depth) {
	(void)depth;
	const String operation = operator_to_string(expression->operation);
	printf("(unary '%.*s'",
		CAST(Sint32,       operation.size),
		CAST(const char *, operation.data));
	if (expression->operand) {
		putchar(' ');
		tree_dump_node(expression->operand, 0, false);
	}
	printf(")");
}

static void tree_dump_binary_expression(const BinaryExpression *expression, Sint32 depth) {
	(void)depth;
	const String operation = operator_to_string(expression->operation);
	printf("(binary '%.*s' ",
		CAST(Sint32,       operation.size),
		CAST(const char *, operation.data));
	tree_dump_node(expression->lhs, 0, false);
	putchar(' ');
	tree_dump_node(expression->rhs, 0, false);
	putchar(')');
}

static void tree_dump_cast_expression(const CastExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(cast ");
	if (expression->type) {
		tree_dump_node(expression->type, 0, false);
		putchar(' ');
	}
	tree_dump_node(expression->expression, 0, false);
	putchar(')');
}

static void tree_dump_selector_expression(const SelectorExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(selector ");
	if (expression->operand) {
		tree_dump_node(expression->operand, 0, false);
		putchar(' ');
	}
	tree_dump_node(expression->identifier, 0, false);
	putchar(')');
}

static void tree_dump_call_expression(const CallExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(call ");
	tree_dump_node(expression->operand, 0, false);
	putchar('\n');
	const Uint64 n_arguments = array_size(expression->arguments);
	for (Uint64 i = 0; i < n_arguments; i++) {
		tree_dump_node(expression->arguments[i], depth + 1, i != n_arguments - 1);
	}
	putchar(')');
}

static void tree_dump_assertion_expression(const AssertionExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(assert ");
	tree_dump_node(expression->operand, 0, false);
	putchar(' ');
	tree_dump_node(expression->type, 0, false);
	putchar(')');
}

// expressions
static void tree_dump_expression(const Expression *expression, Sint32 depth) {
	switch (expression->kind) {
	case EXPRESSION_UNARY:
		return tree_dump_unary_expression(&expression->unary, depth);
	case EXPRESSION_BINARY:
		return tree_dump_binary_expression(&expression->binary, depth);
	case EXPRESSION_CAST:
		return tree_dump_cast_expression(&expression->cast, depth);
	case EXPRESSION_SELECTOR:
		return tree_dump_selector_expression(&expression->selector, depth);
	case EXPRESSION_CALL:
		return tree_dump_call_expression(&expression->call, depth);
	case EXPRESSION_ASSERTION:
		return tree_dump_assertion_expression(&expression->assertion, depth);
	}
}

// statements
static void tree_dump_empty_statement(const EmptyStatement *statement, Sint32 depth) {
	(void)statement;
	(void)depth;
	printf("(empty)");
}

static void tree_dump_block_statement(const BlockStatement *statement, Sint32 depth) {
	const Uint64 n_statements = array_size(statement->statements);
	printf("(block");
	if (n_statements == 0) {
		printf(" <empty>");
	} else {
		putchar('\n');
	}
	for (Uint64 i = 0; i < n_statements; i++) {
		tree_dump_node(statement->statements[i], depth + 1, i != n_statements - 1);
	}
	printf(")");
}

static void tree_dump_import_statement(const ImportStatement *statement, Sint32 depth) {
	(void)depth;
	const String package = statement->package;
	printf("(import '%.*s')",
		CAST(Sint32,       package.size),
		CAST(const char *, package.data));
}

static void tree_dump_expression_statement(const ExpressionStatement *statement, Sint32 depth) {
	(void)depth;
	printf("(expression ");
	tree_dump_node(statement->expression, 0, false);
	putchar(')');
}

static void tree_dump_assignment_statement(const AssignmentStatement *statement, Sint32 depth) {
	(void)depth;
	const String assignment = assignment_to_string(statement->assignment);
	printf("(assignment '%.*s'\n",
		CAST(Sint32,       assignment.size),
		CAST(const char *, assignment.data));
	const Uint64 n_assignments = array_size(statement->lhs);
	for (Uint64 i = 0; i < n_assignments; i++) {
		const Node *const lhs = statement->lhs[i];
		const Node *const rhs = statement->rhs[i];
		tree_dump_node(lhs, depth + 1, 0);
		putchar(' ');
		tree_dump_node(rhs, 0, i != n_assignments - 1);
	}
	putchar(')');
}

static void tree_dump_declaration_statement(const DeclarationStatement *statement, Sint32 depth) {
	printf("(decl ");
	if (statement->type) {
		tree_dump_node(statement->type, 0, false);
	}
	putchar('\n');
	const Uint64 n_decls = array_size(statement->names);
	for (Uint64 i = 0; i < n_decls; i++) {
		const Node *name = statement->names[i];
		const Node *value = statement->values[i];
		tree_dump_node(name, depth + 1, false);
		putchar(' ');
		tree_dump_node(value, 0, i != n_decls - 1);
	}
	putchar(')');
}

static void tree_dump_statement(const Statement *statement, Sint32 depth) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:
		return tree_dump_empty_statement(&statement->empty, depth);
	case STATEMENT_BLOCK:
		return tree_dump_block_statement(&statement->block, depth);
	case STATEMENT_IMPORT:
		return tree_dump_import_statement(&statement->import, depth);
	case STATEMENT_EXPRESSION:
		return tree_dump_expression_statement(&statement->expression, depth);
	case STATEMENT_ASSIGNMENT:
		return tree_dump_assignment_statement(&statement->assignment, depth);
	case STATEMENT_DECLARATION:
		return tree_dump_declaration_statement(&statement->declaration, depth);
	}
}

static void tree_dump_identifier(const Identifier *identifier, Sint32 depth) {
	(void)depth;
	const String contents = identifier->contents;
	printf("(ident '%.*s')",
		CAST(Sint32,       contents.size),
		CAST(const char *, contents.data));
}

static void tree_dump_value(const Value *value, Sint32 depth) {
	(void)value;
	(void)depth;
	// TODO(dweiler): Implement.
	printf("(value)");
}

static void tree_dump_literal_value(const LiteralValue *literal_value, Sint32 depth) {
	(void)depth;
	const String literal = literal_to_string(literal_value->literal);
	const String value = literal_value->value;
	printf("(literal %.*s '%.*s')",
		CAST(Sint32,       literal.size),
		CAST(const char *, literal.data),
		CAST(Sint32,       value.size),
		CAST(const char *, value.data));
}

static void tree_dump_compound_literal(const CompoundLiteral *compound_literal, Sint32 depth) {
	printf("(compound ");
	if (compound_literal->type) {
		tree_dump_node(compound_literal->type, 0, false);
		putchar(' ');
	} else {
		printf("<unknown>");
	}
	const Uint64 n_elements = array_size(compound_literal->elements);
	if (n_elements != 0) {
			putchar('\n');
	}
	for (Uint64 i = 0; i < n_elements; i++) {
		const Node *const element = compound_literal->elements[i];
		tree_dump_node(element, depth + 1, i != n_elements - 1);
	}
	putchar(')');
}

static void tree_dump_field_list(const FieldList* field_list, Sint32 depth) {
	const Uint64 n_fields = array_size(field_list->fields);
	printf("(fields");
	if (n_fields == 0) {
		printf(" <empty>");
	} else {
		putchar('\n');
	}
	for (Uint64 i = 0; i < n_fields; i++) {
		const Node *field = field_list->fields[i];
		tree_dump_node(field, depth + 1, i != n_fields - 1);
	}
	putchar(')');
}

static void tree_dump_procedure(const Procedure *procedure, Sint32 depth) {
	(void)depth;
	printf("(proc\n");
	tree_dump_node(procedure->type, depth + 1, false);
	putchar(' ');
	tree_dump_node(procedure->body, depth + 1, false);
	putchar(')');
}

void tree_dump_node(const Node *node, Sint32 depth, Bool nl) {
	tree_dump_pad(depth);
	switch (node->kind) {
	case NODE_EXPRESSION:
		tree_dump_expression(&node->expression, depth);
		break;
	case NODE_STATEMENT:
		tree_dump_statement(&node->statement, depth);
		break;
	case NODE_IDENTIFIER:
		tree_dump_identifier(&node->identifier, depth);
		break;
	case NODE_VALUE:
		tree_dump_value(&node->value, depth);
		break;
	case NODE_LITERAL_VALUE:
		tree_dump_literal_value(&node->literal_value, depth);
		break;
	case NODE_COMPOUND_LITERAL:
		tree_dump_compound_literal(&node->compound_literal, depth);
		break;
	case NODE_FIELD_LIST:
		tree_dump_field_list(&node->field_list, depth);
		break;
	case NODE_PROCEDURE:
		tree_dump_procedure(&node->procedure, depth);
		break;
	}
	if (nl) {
		putchar('\n');
	}
}

void tree_dump(Tree *tree) {
	printf("(tree\n");
	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		tree_dump_node(tree->statements[i], 1, i != n_statements - 1);
	}
	printf(")\n");
}