#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "tree.h"

// parser.c
void source_free(Source *source);

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

Bool tree_is_node_literal(const Node *node) {
	switch (node->kind) {
	case NODE_IDENTIFIER:
		return true;
	case NODE_EXPRESSION:
		switch (node->expression.kind) {
		case EXPRESSION_SELECTOR:
			return true;
		case EXPRESSION_CALL:
			return true;
		default:
			return false;
		}
	default:
		return false;
	}
	UNREACHABLE();
}

Node *tree_new_unary_expression(Tree *tree, OperatorKind operation, Node *operand) {
	Node *node = new_expression(tree, EXPRESSION_UNARY);
	UnaryExpression *expression = &node->expression.unary;
	expression->operation = operation;
	expression->operand = operand;
	return node;
}

Node *tree_new_binary_expression(Tree *tree, OperatorKind operation, Node *lhs, Node *rhs) {
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

Node *tree_new_import_statement(Tree *tree, String name, String package) {
	Node *node = new_statement(tree, STATEMENT_IMPORT);
	ImportStatement *statement = &node->statement.import;
	statement->name = name;
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

Node *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, Array(Node*) lhs, Array(Node*) rhs) {
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

Node *tree_new_if_statement(Tree *tree, Node *init, Node *condition, Node *body, Node *elif) {
	Node *node = new_statement(tree, STATEMENT_IF);
	IfStatement *statement = &node->statement.if_;
	statement->condition = condition;
	statement->init = init;
	statement->body = body;
	statement->elif = elif;
	return node;
}

Node *tree_new_for_statement(Tree *tree, Node *init, Node *cond, Node *body, Node *post) {
	Node *node = new_statement(tree, STATEMENT_FOR);
	ForStatement *statement = &node->statement.for_;
	statement->init = init;
	statement->cond = cond;
	statement->body = body;
	statement->post = post;
	return node;
}

Node *tree_new_return_statement(Tree *tree, Array(Node*) results) {
	Node *node = new_statement(tree, STATEMENT_RETURN);
	ReturnStatement *statement = &node->statement.return_;
	statement->results = results;
	return node;
}

Node *tree_new_defer_statement(Tree *tree, Node *stmt) {
	Node *node = new_statement(tree, STATEMENT_DEFER);
	DeferStatement *statement = &node->statement.defer;
	statement->statement = stmt;
	return node;
}

Node *tree_new_value(Tree *tree, Node *field, Node *val) {
	Node *node = new_node(tree, NODE_VALUE);
	Value *value = &node->value;
	value->field = field;
	value->value = val;
	return node;
}

Node *tree_new_literal_value(Tree *tree, LiteralKind literal, String value) {
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

Node *tree_new_procedure_type(Tree *tree, Node* params, Node* results, Uint64 flags, CallingConvention convention) {
	Node *node = new_node(tree, NODE_PROCEDURE_TYPE);
	ProcedureType *procedure_type = &node->procedure_type;
	procedure_type->params = params;
	procedure_type->results = results;
	procedure_type->flags = flags;
	procedure_type->convention = convention;
	return node;
}

Node *tree_new_procedure_group(Tree *tree, Array(Node*) procedures) {
	Node *node = new_node(tree, NODE_PROCEDURE_GROUP);
	ProcedureGroup *procedure_group = &node->procedure_group;
	procedure_group->procedures = procedures;
	return node;
}

Node *tree_new_directive(Tree *tree, DirectiveKind kind) {
	Node *node = new_node(tree, NODE_DIRECTIVE);
	node->directive.kind = kind;
	return node;
}

void tree_init(Tree *tree) {
	tree->nodes = 0;
	tree->statements = 0;
}

void tree_free(Tree *tree) {
	const Uint64 n_nodes = array_size(tree->nodes);
	for (Uint64 i = 0; i < n_nodes; i++) {
		Node *node = tree->nodes[i];
		switch (node->kind) {
		case NODE_STATEMENT:
			{
				Statement *statement = &node->statement;
				switch (statement->kind) {
				case STATEMENT_BLOCK:
					array_free(statement->block.statements);
					break;
				case STATEMENT_ASSIGNMENT:
					array_free(statement->assignment.lhs);
					array_free(statement->assignment.rhs);
					break;
				case STATEMENT_DECLARATION:
					array_free(statement->declaration.names);
					array_free(statement->declaration.values);
					break;
				case STATEMENT_RETURN:
					array_free(statement->return_.results);
					break;
				default:
					break;
				}
			}
			break;
		case NODE_COMPOUND_LITERAL:
			{
				CompoundLiteral *compound_literal = &node->compound_literal;
				array_free(compound_literal->elements);
			}
			break;
		case NODE_FIELD_LIST:
			{
				FieldList *field_list = &node->field_list;
				array_free(field_list->fields);
			}
			break;
		case NODE_EXPRESSION:
			{
				Expression *expression = &node->expression;
				switch (expression->kind) {
				case EXPRESSION_CALL:
					{
						CallExpression *call_expression = &expression->call;
						array_free(call_expression->arguments);
					}
					break;
				default:
					break;
				}
			}
			break;
		default:
			break;
		}
		free(tree->nodes[i]);
	}
	array_free(tree->nodes);
	array_free(tree->statements);

	source_free(&tree->source);

	free(tree);
}

void tree_dump_node(const Node *node, Sint32 depth);

static void tree_dump_pad(Sint32 depth) {
	for (Sint32 i = 0; i < depth; i++) {
		printf("..");
	}
}

static void tree_dump_unary_expression(const UnaryExpression *expression, Sint32 depth) {
	(void)depth;
	const String operation = operator_to_string(expression->operation);
	printf("(unary\n");
	tree_dump_pad(depth + 1);
	printf("'%.*s'", SFMT(operation));
	if (expression->operand) {
		putchar('\n');
		tree_dump_node(expression->operand, depth + 1);
	}
	putchar(')');
}

static void tree_dump_binary_expression(const BinaryExpression *expression, Sint32 depth) {
	(void)depth;
	const String operation = operator_to_string(expression->operation);
	printf("(binary\n");
	tree_dump_pad(depth + 1);
	printf("'%.*s'\n", SFMT(operation));
	tree_dump_node(expression->lhs, depth + 1);
	putchar('\n');
	tree_dump_node(expression->rhs, depth + 1);
	putchar(')');
}

static void tree_dump_cast_expression(const CastExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(cast\n");
	if (expression->type) {
		tree_dump_node(expression->type, depth + 1);
		putchar('\n');
	}
	tree_dump_node(expression->expression, depth + 1);
	putchar(')');
}

static void tree_dump_selector_expression(const SelectorExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(selector\n");
	if (expression->operand) {
		tree_dump_node(expression->operand, depth + 1);
		putchar('\n');
	}
	tree_dump_node(expression->identifier, depth + 1);
	putchar(')');
}

static void tree_dump_call_expression(const CallExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(call\n");
	if (expression->operand) {
		tree_dump_node(expression->operand, depth + 1);
	} else {
		tree_dump_pad(depth + 1);
		printf("<builtin>");
	}
	putchar('\n');
	const Uint64 n_arguments = array_size(expression->arguments);
	for (Uint64 i = 0; i < n_arguments; i++) {
		tree_dump_node(expression->arguments[i], depth + 1);
		if (i != n_arguments - 1) {
			putchar('\n');
		}
	}
	if (n_arguments == 0) {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	putchar(')');
}

static void tree_dump_assertion_expression(const AssertionExpression *expression, Sint32 depth) {
	(void)depth;
	printf("(assert\n");
	tree_dump_node(expression->operand, depth + 1);
	putchar('\n');
	tree_dump_node(expression->type, depth + 1);
	putchar(')');
}

// expressions
static void tree_dump_expression(const Expression *expression, Sint32 depth) {
	tree_dump_pad(depth);
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
	printf("(block\n");
	for (Uint64 i = 0; i < n_statements; i++) {
		tree_dump_node(statement->statements[i], depth + 1);
		if (i != n_statements - 1) {
			putchar('\n');
		}
	}
	if (n_statements == 0) {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	printf(")");
}

static void tree_dump_import_statement(const ImportStatement *statement, Sint32 depth) {
	(void)depth;
	const String package = statement->package;
	printf("(import '%.*s')", SFMT(package));
}

static void tree_dump_expression_statement(const ExpressionStatement *statement, Sint32 depth) {
	(void)depth;
	printf("(expression\n");
	tree_dump_node(statement->expression, depth + 1);
	putchar(')');
}

static void tree_dump_assignment_statement(const AssignmentStatement *statement, Sint32 depth) {
	(void)depth;
	const String assignment = assignment_to_string(statement->assignment);
	printf("(assignment\n");
	tree_dump_pad(depth + 1);
	printf("'%.*s'\n", SFMT(assignment));
	const Uint64 n_assignments = array_size(statement->lhs);
	for (Uint64 i = 0; i < n_assignments; i++) {
		const Node *const lhs = statement->lhs[i];
		const Node *const rhs = statement->rhs[i];
		tree_dump_node(lhs, depth + 1);
		putchar('\n');
		tree_dump_node(rhs, depth + 1);
		if (i != n_assignments) {
			putchar('\n');
		}
	}
	putchar(')');
}

static void tree_dump_declaration_statement(const DeclarationStatement *statement, Sint32 depth) {
	printf("(decl\n");
	if (statement->type) {
		tree_dump_pad(depth + 1);
		printf("(type\n");
		tree_dump_node(statement->type, depth + 2);
		printf(")\n");
	}
	const Uint64 n_decls = array_size(statement->names);
	const Uint64 n_values = array_size(statement->values);
	for (Uint64 i = 0; i < n_decls; i++) {
		const Node *name = statement->names[i];
		tree_dump_node(name, depth + 1);
		if (i < n_values) {
			const Node *value = statement->values[i];
			putchar('\n');
			tree_dump_node(value, depth + 1);
		}
		if (i != n_decls - 1) {
			putchar('\n');
		}
	}
	putchar(')');
}

static void tree_dump_if_statement(const IfStatement *statement, Sint32 depth) {
	printf("(if\n");
	if (statement->init) {
		tree_dump_pad(depth + 1);
		printf("(init\n");
		tree_dump_node(statement->init, depth + 2);
		printf(")\n");
	}
	tree_dump_node(statement->condition, depth + 1);
	putchar('\n');
	tree_dump_node(statement->body, depth + 1);
	if (statement->elif) {
		putchar('\n');
		tree_dump_node(statement->elif, depth + 1);
	}
	putchar(')');
}

static void tree_dump_return_statement(const ReturnStatement *statement, Sint32 depth) {
	printf("(return\n");
	const Uint64 n_results = array_size(statement->results);
	if (n_results == 0) {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	for (Uint64 i = 0; i < n_results; i++) {
		tree_dump_node(statement->results[i], depth + 1);
		if (i != n_results - 1) {
			putchar(',');
			putchar('\n');
		}
	}
	putchar(')');
}

static void tree_dump_for_statement(const ForStatement *statement, Sint32 depth) {
	printf("(for\n");
	if (statement->init) {
		tree_dump_pad(depth + 1);
		printf("(init\n");
		tree_dump_node(statement->init, depth + 2);
		putchar(')');
		putchar('\n');
	}
	if (statement->cond) {
		tree_dump_pad(depth + 1);
		printf("(cond\n");
		tree_dump_node(statement->cond, depth + 2);
		putchar(')');
		putchar('\n');
	}
	if (statement->body) {
		tree_dump_node(statement->body, depth + 1);
	}
	putchar(')');
}

static void tree_dump_defer_statement(const DeferStatement *statement, Sint32 depth) {
	printf("(defer\n");
	tree_dump_node(statement->statement, depth + 1);
	putchar(')');
}

static void tree_dump_statement(const Statement *statement, Sint32 depth) {
	tree_dump_pad(depth);
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
	case STATEMENT_IF:
		return tree_dump_if_statement(&statement->if_, depth);
	case STATEMENT_RETURN:
		return tree_dump_return_statement(&statement->return_, depth);
	case STATEMENT_FOR:
		return tree_dump_for_statement(&statement->for_, depth);
	case STATEMENT_DEFER:
		return tree_dump_defer_statement(&statement->defer, depth);
	}
}

static void tree_dump_identifier(const Identifier *identifier, Sint32 depth) {
	tree_dump_pad(depth);
	(void)depth;
	const String contents = identifier->contents;
	printf("(ident '%.*s')", SFMT(contents));
}

static void tree_dump_value(const Value *value, Sint32 depth) {
	tree_dump_pad(depth);
	(void)value;
	(void)depth;
	// TODO(dweiler): Implement.
	printf("(value)");
}

static void tree_dump_literal_value(const LiteralValue *literal_value, Sint32 depth) {
	tree_dump_pad(depth);
	(void)depth;
	const String literal = literal_to_string(literal_value->literal);
	const String value = literal_value->value;
	printf("(literal %.*s '%.*s')", SFMT(literal), SFMT(value));
}

static void tree_dump_compound_literal(const CompoundLiteral *compound_literal, Sint32 depth) {
	const Uint64 n_elements = array_size(compound_literal->elements);
	tree_dump_pad(depth);
	printf("(compound\n");
	if (compound_literal->type) {
		tree_dump_node(compound_literal->type, depth + 1);
	} else {
		printf("<unknown>");
	}
	if (n_elements != 0) {
		putchar('\n');
	}
	for (Uint64 i = 0; i < n_elements; i++) {
		const Node *const element = compound_literal->elements[i];
		tree_dump_node(element, depth + 1);
	}
	putchar(')');
}

static void tree_dump_field_list(const FieldList* field_list, Sint32 depth) {
	const Uint64 n_fields = array_size(field_list->fields);
	tree_dump_pad(depth);
	printf("(fields\n");
	if (n_fields == 0) {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	for (Uint64 i = 0; i < n_fields; i++) {
		const Node *field = field_list->fields[i];
		tree_dump_node(field, depth + 1);
	}
	putchar(')');
}

static void tree_dump_procedure(const Procedure *procedure, Sint32 depth) {
	tree_dump_pad(depth);
	(void)depth;
	printf("(proc\n");
	tree_dump_node(procedure->type, depth + 1);
	putchar('\n');
	tree_dump_node(procedure->body, depth + 1);
	putchar(')');
}

static const char *calling_convention_to_string(CallingConvention convention) {
	static const char *CALLING_CONVENTIONS[] = {
		[CCONV_INVALID]     = "invalid",
		[CCONV_ODIN]        = "odin",
		[CCONV_CONTEXTLESS] = "contextless",
		[CCONV_CDECL]       = "cdecl",
		[CCONV_STDCALL]     = "stdcall",
		[CCONV_FASTCALL]    = "fastcall",
		[CCONV_NAKED]       = "naked",
		[CCONV_NONE]        = "none",
	};
	return CALLING_CONVENTIONS[convention];
}

static void tree_dump_procedure_type(const ProcedureType *procedure, Sint32 depth) {
	tree_dump_pad(depth);
	const Uint64 n_parameters = array_size(procedure->params);
	const Uint64 n_results = array_size(procedure->results);
	printf("(cconv \"%s\")\n", calling_convention_to_string(procedure->convention));
	tree_dump_pad(depth);
	printf("(parameters ");
	if (n_parameters == 0) {
		printf("<empty>");
	} else {
		putchar('\n');
		tree_dump_node(procedure->params, depth + 1);
	}
	printf(")\n");
	tree_dump_pad(depth);
	printf("(results ");
	if (n_results == 0) {
		printf("<empty>");
	} else {
		putchar('\n');
		tree_dump_field_list(&procedure->results->field_list, depth + 1);
	}
	putchar(')');
}

static void tree_dump_procedure_group(const ProcedureGroup *group, Sint32 depth) {
	tree_dump_pad(depth);
	const Uint64 n_procedures = array_size(group->procedures);
	printf("(procgroup\n");
	for (Uint64 i = 0; i < n_procedures; i++) {
		const Node *procedure = group->procedures[i];
		tree_dump_node(procedure, depth + 1);
		if (i != n_procedures - 1) {
			putchar('\n');
		}
	}
	putchar(')');
}

static void tree_dump_directive(const Directive *directive, Sint32 depth) {
	tree_dump_pad(depth);
	const String string = directive_to_string(directive->kind);
	printf("(directive '%.*s\')", SFMT(string));
}

void tree_dump_node(const Node *node, Sint32 depth) {
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
	case NODE_PROCEDURE_TYPE:
		tree_dump_procedure_type(&node->procedure_type, depth);
		break;
	case NODE_PROCEDURE_GROUP:
		tree_dump_procedure_group(&node->procedure_group, depth);
		break;
	case NODE_DIRECTIVE:
		tree_dump_directive(&node->directive, depth);
		break;
	}
}

void tree_dump(Tree *tree) {
	printf("(tree\n");
	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		tree_dump_node(tree->statements[i], 1);
		if (i != n_statements - 1) {
			putchar('\n');
		}
	}
	printf(")\n");
}