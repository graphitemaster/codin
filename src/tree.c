#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "tree.h"
#include "context.h"

static String new_string(Tree *tree, String string) {
	Context *context = tree->context;
	String copy = string_copy(string);
	array_push(tree->strings, copy);
	return copy;
}

static Node *new_node(Tree *tree, NodeKind kind) {
	Context *context = tree->context;
	Allocator *allocator = context->allocator;
	Node *node = allocator->allocate(allocator, sizeof *node);
	memset(node, 0, sizeof *node);
	node->kind = kind;
	return array_push(tree->nodes, node) ? node : 0;
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

static Node *new_type(Tree *tree, TypeKind kind) {
	Node *node = new_node(tree, NODE_TYPE);
	node->type.kind = kind;
	return node;
}

Bool node_is_literal(const Node *node) {
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

Node *tree_new_in_expression(Tree *tree, Array(Node*) lhs, Node *rhs) {
	Node *node = new_expression(tree, EXPRESSION_IN);
	InExpression *expression = &node->expression.in;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return node;
}

Node *tree_new_dereference_expression(Tree *tree, Node *operand) {
	Node *node = new_expression(tree, EXPRESSION_DEREFERENCE);
	DereferenceExpression *expression = &node->expression.dereference;
	expression->operand = operand;
	return node;
}

Node *tree_new_empty_statement(Tree *tree) {
	Node *node = new_statement(tree, STATEMENT_EMPTY);
	return node;
}

Node *tree_new_import_statement(Tree *tree, String name, String package) {
	Node *node = new_statement(tree, STATEMENT_IMPORT);
	ImportStatement *statement = &node->statement.import;
	statement->name = new_string(tree, name);
	statement->package = new_string(tree, package);
	return node;
}

Node *tree_new_expression_statement(Tree *tree, Node *expression) {
	Node *node = new_statement(tree, STATEMENT_EXPRESSION);
	ExpressionStatement *statement = &node->statement.expression;
	statement->expression = expression;
	return node;
}

Node *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Node*) statements) {
	Node *node = new_statement(tree, STATEMENT_BLOCK);
	BlockStatement *statement = &node->statement.block;
	statement->flags = flags;
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
	identifier->contents = new_string(tree, contents);
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

Node *tree_new_if_statement(Tree *tree, Node *init, Node *cond, Node *body, Node *elif) {
	Node *node = new_statement(tree, STATEMENT_IF);
	IfStatement *statement = &node->statement.if_;
	statement->cond = cond;
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

Node *tree_new_break_statement(Tree *tree) {
	return new_statement(tree, STATEMENT_BREAK);
}

Node *tree_new_defer_statement(Tree *tree, Node *stmt) {
	Node *node = new_statement(tree, STATEMENT_DEFER);
	DeferStatement *statement = &node->statement.defer;
	statement->statement = stmt;
	return node;
}

Node *tree_new_procedure_type(Tree *tree, Node *params, Node *results, Uint64 flags, CallingConvention convention) {
	Node *node = new_type(tree, TYPE_PROCEDURE);
	ProcedureType *procedure_type = &node->type.procedure;
	procedure_type->params = params;
	procedure_type->results = results;
	procedure_type->flags = flags;
	procedure_type->convention = convention;
	return node;
}

Node *tree_new_slice_type(Tree *tree, Node *type) {
	Node *node = new_type(tree, TYPE_SLICE);
	SliceType *slice = &node->type.slice;
	slice->type = type;
	return node;
}

Node *tree_new_array_type(Tree *tree, Node *count, Node *type) {
	Node *node = new_type(tree, TYPE_ARRAY);
	ArrayType *array = &node->type.array;
	array->count = count;
	array->type = type;
	return node;
}

Node *tree_new_dynamic_array_type(Tree *tree, Node *type) {
	Node *node = new_type(tree, TYPE_DYNAMIC_ARRAY);
	DynamicArrayType *dynamic_array = &node->type.dynamic_array;
	dynamic_array->type = type;
	return node;
}

Node *tree_new_pointer_type(Tree *tree, Node *type) {
	Node *node = new_type(tree, TYPE_POINTER);
	PointerType *pointer = &node->type.pointer;
	pointer->type = type;
	return node;
}

Node *tree_new_typeid_type(Tree *tree) {
	return new_type(tree, TYPE_TYPEID);
}

Node *tree_new_multi_pointer_type(Tree *tree, Node *type) {
	Node *node = new_type(tree, TYPE_MULTI_POINTER);
	MultiPointerType *multi_pointer = &node->type.multi_pointer;
	multi_pointer->type = type;
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
	literal_value->value = new_string(tree, value);
	return node;
}

Node *tree_new_compound_literal(Tree *tree, Node *type, Array(Node*) elements) {
	Node *node = new_node(tree, NODE_COMPOUND_LITERAL);
	CompoundLiteral *compound_literal = &node->compound_literal;
	compound_literal->type = type;
	compound_literal->elements = elements;
	return node;
}

Node *tree_new_field(Tree *tree, Node* name, Node *type) {
	Node *node = new_node(tree, NODE_FIELD);
	Field *field = &node->field;
	field->name = name;
	field->type = type;
	return node;
}

Node *tree_new_field_list(Tree *tree, Array(Node*) fields) {
	Node *node = new_node(tree, NODE_FIELD_LIST);
	FieldList *field_list = &node->field_list;
	field_list->fields = fields;
	return node;
}

Node *tree_new_procedure(Tree *tree, ProcedureFlag flags, Node *type, Node *body) {
	Node *node = new_node(tree, NODE_PROCEDURE);
	Procedure *procedure = &node->procedure;
	procedure->flags = flags;
	procedure->type = type;
	procedure->body = body;
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

void tree_init(Tree *tree, Context *context) {
	tree->context = context;
	tree->package = STRING_NIL;
	tree->nodes = 0;
	tree->statements = 0;
	tree->strings = 0;
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
	const Size n_arguments = array_size(expression->arguments);
	for (Size i = 0; i < n_arguments; i++) {
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

static void tree_dump_in_expression(const InExpression *expression, Sint32 depth) {
	printf("(in\n");
	tree_dump_pad(depth + 1);
	printf("(lhs\n");
	const Size n_lhs = array_size(expression->lhs);
	for (Size i = 0; i < n_lhs; i++) {
		const Node *node = expression->lhs[i];
		tree_dump_node(node, depth + 2);
		putchar('\n');
	}
	tree_dump_pad(depth + 1);
	printf("(rhs\n");
	tree_dump_node(expression->rhs, depth + 2);
	putchar(')');
}

static void tree_dump_dereference_expression(const DereferenceExpression *expression, Sint32 depth) {;
	printf("(dereference\n");
	tree_dump_node(expression->operand, depth + 1);
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
	case EXPRESSION_IN:
		return tree_dump_in_expression(&expression->in, depth);
	case EXPRESSION_DEREFERENCE:
		return tree_dump_dereference_expression(&expression->dereference, depth);
	}
}

// statements
static void tree_dump_empty_statement(const EmptyStatement *statement, Sint32 depth) {
	(void)statement;
	(void)depth;
	printf("(empty)");
}

static void tree_dump_block_statement(const BlockStatement *statement, Sint32 depth) {
	const Size n_statements = array_size(statement->statements);
	printf("(block\n");
	for (Size i = 0; i < n_statements; i++) {
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

void tree_dump_expression_statement(const ExpressionStatement *statement, Sint32 depth) {
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
	const Size n_assignments = array_size(statement->lhs);
	for (Size i = 0; i < n_assignments; i++) {
		const Node *const lhs = statement->lhs[i];
		const Node *const rhs = statement->rhs[i];
		tree_dump_node(lhs, depth + 1);
		putchar('\n');
		tree_dump_node(rhs, depth + 1);
		if (i != n_assignments - 1) {
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
	const Size n_decls = array_size(statement->names);
	const Size n_values = array_size(statement->values);
	for (Size i = 0; i < n_decls; i++) {
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
	tree_dump_node(statement->cond, depth + 1);
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
	const Size n_results = array_size(statement->results);
	if (n_results == 0) {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	for (Size i = 0; i < n_results; i++) {
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
	const Size n_elements = array_size(compound_literal->elements);
	tree_dump_pad(depth);
	printf("(compound\n");
	if (compound_literal->type) {
		tree_dump_pad(depth + 1);
		tree_dump_node(compound_literal->type, 0);
	}
	if (n_elements) {
		putchar('\n');
	}
	for (Size i = 0; i < n_elements; i++) {
		const Node *const element = compound_literal->elements[i];
		tree_dump_node(element, depth + 1);
		if (i != n_elements - 1) {
			putchar('\n');
		}
	}
	putchar(')');
}

static void tree_dump_field(const Field *field, Sint32 depth) {
	tree_dump_pad(depth);
	printf("(field\n");
	tree_dump_node(field->type, depth + 1);
	putchar('\n');
	tree_dump_node(field->name, depth + 1);
	putchar(')');
}

static void tree_dump_field_list(const FieldList* field_list, Sint32 depth) {
	const Size n_fields = array_size(field_list->fields);
	tree_dump_pad(depth);
	printf("(fields\n");
	if (n_fields == 0) {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	for (Size i = 0; i < n_fields; i++) {
		const Node *field = field_list->fields[i];
		tree_dump_node(field, depth + 1);
		if (i != n_fields - 1) {
			putchar('\n');
		}
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
	printf("(cconv \"%s\")\n", calling_convention_to_string(procedure->convention));
	tree_dump_pad(depth);
	printf("(parameters\n");
	if (procedure->params) {
		tree_dump_node(procedure->params, depth + 1);
	} else {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	printf(")\n");
	tree_dump_pad(depth);
	printf("(results\n");
	if (procedure->results) {
		tree_dump_node(procedure->results, depth + 1);
	} else {
		tree_dump_pad(depth + 1);
		printf("<empty>");
	}
	putchar(')');
}

static void tree_dump_procedure_group(const ProcedureGroup *group, Sint32 depth) {
	tree_dump_pad(depth);
	const Size n_procedures = array_size(group->procedures);
	printf("(procgroup\n");
	for (Size i = 0; i < n_procedures; i++) {
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

static void tree_dump_type(const Type* type, Sint32 depth) {
	switch (type->kind) {
	case TYPE_PROCEDURE:
		return tree_dump_procedure_type(&type->procedure, depth);
	case TYPE_TYPEID:
		tree_dump_pad(depth);
		printf("(typeid)");
		break;
	case TYPE_SLICE:
		tree_dump_pad(depth);
		printf("(slice\n");
		tree_dump_node(type->slice.type, depth + 1);
		putchar(')');
		break;
	case TYPE_ARRAY:
		tree_dump_pad(depth);
		printf("(array\n");
		tree_dump_node(type->array.count, depth + 1);
		putchar('\n');
		tree_dump_node(type->array.type, depth + 1);
		putchar(')');
		break;
	case TYPE_DYNAMIC_ARRAY:
		tree_dump_pad(depth);
		printf("(vector\n");
		tree_dump_node(type->dynamic_array.type, depth + 1);
		putchar(')');
		break;
	case TYPE_POINTER:
		tree_dump_pad(depth);
		printf("(pointer\n");
		tree_dump_node(type->pointer.type, depth + 1);
		putchar(')');
		break;
	case TYPE_MULTI_POINTER:
		tree_dump_pad(depth);
		printf("(multipointer\n");
		tree_dump_node(type->multi_pointer.type, depth + 1);
		putchar(')');
		break;
	}
}

void tree_dump_node(const Node *node, Sint32 depth) {
	switch (node->kind) {
	case NODE_EXPRESSION:
		return tree_dump_expression(&node->expression, depth);
	case NODE_STATEMENT:
		return tree_dump_statement(&node->statement, depth);
	case NODE_IDENTIFIER:
		return tree_dump_identifier(&node->identifier, depth);
	case NODE_VALUE:
		return tree_dump_value(&node->value, depth);
	case NODE_LITERAL_VALUE:
		return tree_dump_literal_value(&node->literal_value, depth);
	case NODE_COMPOUND_LITERAL:
		return tree_dump_compound_literal(&node->compound_literal, depth);
	case NODE_FIELD:
		return tree_dump_field(&node->field, depth);
	case NODE_FIELD_LIST:
		return tree_dump_field_list(&node->field_list, depth);
	case NODE_PROCEDURE:
		return tree_dump_procedure(&node->procedure, depth);
	case NODE_PROCEDURE_GROUP:
		return tree_dump_procedure_group(&node->procedure_group, depth);
	case NODE_DIRECTIVE:
		return tree_dump_directive(&node->directive, depth);
	case NODE_TYPE:
		return tree_dump_type(&node->type, depth);
	}
	UNREACHABLE();
}

void tree_dump(Tree *tree) {
	printf("(tree\n");
	const Size n_statements = array_size(tree->statements);
	for (Size i = 0; i < n_statements; i++) {
		tree_dump_node(tree->statements[i], 1);
		if (i != n_statements - 1) {
			putchar('\n');
		}
	}
	printf(")\n");
}

// Clone
static Node *tree_clone_unary_expression(Tree *tree, const UnaryExpression *expression) {
	Node *operand = tree_clone_node(tree, expression->operand);
	return operand ? tree_new_unary_expression(tree, expression->operation, operand) : 0;
}

static Node *tree_clone_binary_expression(Tree *tree, const BinaryExpression *expression) {
	Node *lhs = tree_clone_node(tree, expression->lhs);
	Node *rhs = tree_clone_node(tree, expression->rhs);
	return lhs && rhs ? tree_new_binary_expression(tree, expression->operation, lhs, rhs) : 0;
}

static Node *tree_clone_cast_expression(Tree *tree, const CastExpression *expression) {
	Node *type = 0;
	if (expression->type && !(type = tree_clone_node(tree, expression->type))) {
		return 0;
	}
	Node *operand = tree_clone_node(tree, expression->expression);
	return operand ? tree_new_cast_expression(tree, type, operand) : 0;
}

static Node *tree_clone_selector_expression(Tree *tree, const SelectorExpression *expression) {
	Node *operand = tree_clone_node(tree, expression->operand);
	Node *identifier = tree_clone_node(tree, expression->identifier);
	return operand && identifier ? tree_new_selector_expression(tree, operand, identifier) : 0;
}

static Node *tree_clone_call_expression(Tree *tree, const CallExpression *expression) {
	Context *context = tree->context;

	Node *operand = tree_clone_node(tree, expression->operand);
	if (!operand) {
		return 0;
	}
	Array(Node*) arguments = 0;
	const Size n_arguments = array_size(expression->arguments);
	for (Size i = 0; i < n_arguments; i++) {
		Node *argument = tree_clone_node(tree, expression->arguments[i]);
		if (!argument || !array_push(arguments, argument)) {
			return 0;
		}
	}
	return tree_new_call_expression(tree, operand, arguments);
}

static Node *tree_clone_assertion_expression(Tree *tree, const AssertionExpression *expression) {
	Node *type = 0;
	if (expression->type && !(type = tree_clone_node(tree, expression->type))) {
		return 0;
	}
	Node *operand = tree_clone_node(tree, expression->operand);
	return operand ? tree_new_assertion_expression(tree, operand, type) : 0;
}

static Node *tree_clone_in_expression(Tree *tree, const InExpression *expression) {
	Context *context = tree->context;

	Node *rhs = tree_clone_node(tree, expression->rhs);
	if (!rhs) {
		return 0;
	}
	Array(Node*) lhs = 0;
	const Size n_lhs = array_size(expression->lhs);
	for (Size i = 0; i < n_lhs; i++) {
		Node *node = tree_clone_node(tree, expression->lhs[i]);
		if (!node || !array_push(lhs, node)) {
			return 0;
		}
	}
	return tree_new_in_expression(tree, lhs, rhs);
}

static Node *tree_clone_dereference_expression(Tree *tree, const DereferenceExpression *expression) {
	Node *operand = tree_clone_node(tree, expression->operand);
	return operand ? tree_new_dereference_expression(tree, operand) : 0;
}

Node *tree_clone_expression(Tree *tree, const Expression *expression) {
	switch (expression->kind) {
	case EXPRESSION_UNARY:
		return tree_clone_unary_expression(tree, &expression->unary);
	case EXPRESSION_BINARY:
		return tree_clone_binary_expression(tree, &expression->binary);
	case EXPRESSION_CAST:
		return tree_clone_cast_expression(tree, &expression->cast);
	case EXPRESSION_SELECTOR:
		return tree_clone_selector_expression(tree, &expression->selector);
	case EXPRESSION_CALL:
		return tree_clone_call_expression(tree, &expression->call);
	case EXPRESSION_ASSERTION:
		return tree_clone_assertion_expression(tree, &expression->assertion);
	case EXPRESSION_IN:
		return tree_clone_in_expression(tree, &expression->in);
	case EXPRESSION_DEREFERENCE:
		return tree_clone_dereference_expression(tree, &expression->dereference);
	}
	UNREACHABLE();
}

static Node *tree_clone_block_statement(Tree *tree, const BlockStatement *statement) {
	Context *context = tree->context;

	Array(Node*) statements = 0;
	const Size n_statements = array_size(statement->statements);
	for (Size i = 0; i < n_statements; i++) {
		Node *node = tree_clone_node(tree, statement->statements[i]);
		if (!node || !array_push(statements, node)) {
			return 0;
		}
	}
	return tree_new_block_statement(tree, statement->flags, statements);
}

static Node *tree_clone_import_statement(Tree *tree, const ImportStatement *statement) {
	return tree_new_import_statement(tree, statement->name, statement->package);
}

static Node *tree_clone_expression_statement(Tree *tree, const ExpressionStatement *statement) {
	Node *expression = tree_clone_node(tree, statement->expression);
	return expression ? tree_new_expression_statement(tree, expression) : 0;
}

static Node *tree_clone_assignment_statement(Tree *tree, const AssignmentStatement *statement) {
	Context *context = tree->context;

	Array(Node*) lhs = 0;
	Array(Node*) rhs = 0;
	const Size n_lhs = array_size(statement->lhs);
	const Size n_rhs = array_size(statement->rhs);
	for (Size i = 0; i < n_lhs; i++) {
		Node *node = tree_clone_node(tree, statement->lhs[i]);
		if (!node || !array_push(lhs, node)) {
			return 0;
		}
	}
	for (Size i = 0; i < n_rhs; i++) {
		Node *node = tree_clone_node(tree, statement->rhs[i]);
		if (!node || !array_push(rhs, node)) {
			return 0;
		}
	}
	return tree_new_assignment_statement(tree, statement->assignment, lhs, rhs);
}

static Node *tree_clone_declaration_statement(Tree *tree, const DeclarationStatement *statement) {
	Context *context = tree->context;

	Node *type = 0;
	if (statement->type && !(type = tree_clone_node(tree, statement->type))) {
		return 0;
	}
	Array(Node*) names = 0;
	Array(Node*) values = 0;
	const Size n_names = array_size(statement->names);
	const Size n_values = array_size(statement->values);
	for (Size i = 0; i < n_names; i++) {
		Node *node = tree_clone_node(tree, statement->names[i]);
		if (!node || !array_push(names, node)) {
			return 0;
		}
	}
	for (Size i = 0; i < n_values; i++) {
		Node *node = tree_clone_node(tree, statement->values[i]);
		if (!node || !array_push(values, node)) {
			return 0;
		}
	}
	return tree_new_declaration_statement(tree, type, names, values);
}

static Node *tree_clone_if_statement(Tree *tree, const IfStatement *statement) {
	Node *init = 0;
	Node *elif = 0;
	if ((statement->init && !(init = tree_clone_node(tree, statement->init))) ||
	    (statement->elif && !(elif = tree_clone_node(tree, statement->elif))))
	{
		return 0;
	}
	Node *cond = tree_clone_node(tree, statement->cond);
	Node *body = tree_clone_node(tree, statement->body);
	return cond && body ? tree_new_if_statement(tree, init, cond, body, elif) : 0;
}

Node *tree_clone_return_statement(Tree *tree, const ReturnStatement *statement) {
	Context *context = tree->context;
	Array(Node*) results = 0;
	const Size n_results = array_size(statement->results);
	for (Size i = 0; i < n_results; i++) {
		Node *result = tree_clone_node(tree, statement->results[i]);
		if (!result || !array_push(results, result)) {
			return 0;
		}
	}
	return tree_new_return_statement(tree, results);
}

static Node *tree_clone_for_statement(Tree *tree, const ForStatement *statement) {
	Node *init = 0;
	Node *post = 0;
	if ((statement->init && !(init = tree_clone_node(tree, statement->init))) ||
	    (statement->post && !(post = tree_clone_node(tree, statement->post))))
	{
		return 0;
	}
	Node *cond = tree_clone_node(tree, statement->cond);
	Node *body = tree_clone_node(tree, statement->body);
	return cond && body ? tree_new_for_statement(tree, init, cond, body, post) : 0;
}

static Node *tree_clone_defer_statement(Tree *tree, const DeferStatement *statement) {
	Node *node = tree_clone_node(tree, statement->statement);
	return node ? tree_new_defer_statement(tree, node) : 0;
}

Node *tree_clone_statement(Tree *tree, const Statement *statement) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:
		return tree_new_empty_statement(tree);
	case STATEMENT_BLOCK:
		return tree_clone_block_statement(tree, &statement->block);
	case STATEMENT_IMPORT:
		return tree_clone_import_statement(tree, &statement->import);
	case STATEMENT_EXPRESSION:
		return tree_clone_expression_statement(tree, &statement->expression);
	case STATEMENT_ASSIGNMENT:
		return tree_clone_assignment_statement(tree, &statement->assignment);
	case STATEMENT_DECLARATION:
		return tree_clone_declaration_statement(tree, &statement->declaration);
	case STATEMENT_IF:
		return tree_clone_if_statement(tree, &statement->if_);
	case STATEMENT_RETURN:
		return tree_clone_return_statement(tree, &statement->return_);
	case STATEMENT_FOR:
		return tree_clone_for_statement(tree, &statement->for_);
	case STATEMENT_DEFER:
		return tree_clone_defer_statement(tree, &statement->defer);
	}
	UNREACHABLE();
}

static Node *tree_clone_identifier(Tree *tree, const Identifier *identifier) {
	return tree_new_identifier(tree, identifier->contents);
}

static Node *tree_clone_value(Tree *tree, const Value *value_) {
	Node *field = tree_clone_node(tree, value_->field);
	Node *value = tree_clone_node(tree, value_->value);
	return field && value ? tree_new_value(tree, field, value) : 0;
}

static Node *tree_clone_literal_value(Tree *tree, const LiteralValue *literal_value) {
	return tree_new_literal_value(tree, literal_value->literal, literal_value->value);
}

static Node *tree_clone_compound_literal(Tree *tree, const CompoundLiteral *compound_literal) {
	Context *context = tree->context;

	Node *type = tree_clone_node(tree, compound_literal->type);
	if (!type) {
		return 0;
	}
	Array(Node*) elements = 0;
	const Size n_elements = array_size(compound_literal->elements);
	for (Size i = 0; i < n_elements; i++) {
		Node *element = tree_clone_node(tree, compound_literal->elements[i]);
		if (!element || !array_push(elements, element)) {
			return 0;
		}
	}
	return tree_new_compound_literal(tree, type, elements);
}

static Node *tree_clone_field(Tree *tree, const Field *field) {
	Node *name = tree_clone_node(tree, field->name);
	Node *type = tree_clone_node(tree, field->type);
	return name && type ? tree_new_field(tree, name, type) : 0;
}

static Node *tree_clone_field_list(Tree *tree, const FieldList *field_list) {
	Context *context = tree->context;

	Array(Node*) fields = 0;
	const Size n_fields = array_size(field_list->fields);
	for (Size i = 0; i < n_fields; i++) {
		Node *field = tree_clone_node(tree, field_list->fields[i]);
		if (!field || !array_push(fields, field)) {
			return 0;
		}
	}
	return tree_new_field_list(tree, fields);
}

static Node *tree_clone_procedure(Tree *tree, const Procedure *procedure) {
	Node *type = tree_clone_node(tree, procedure->type);
	Node *body = tree_clone_node(tree, procedure->body);
	return type && body ? tree_new_procedure(tree, procedure->flags, type, body) : 0;
}

static Node *tree_clone_procedure_group(Tree *tree, const ProcedureGroup *procedure_group) {
	Context *context = tree->context;

	Array(Node*) procedures = 0;
	const Size n_procedures = array_size(procedure_group->procedures);
	for (Size i = 0; i < n_procedures; i++) {
		Node *procedure = tree_clone_node(tree, procedure_group->procedures[i]);
		if (!procedure || !array_push(procedures, procedure)) {
			return 0;
		}
	}
	return tree_new_procedure_group(tree, procedures);
}

static Node *tree_clone_directive(Tree *tree, const Directive *directive) {
	return tree_new_directive(tree, directive->kind);
}

static Node *tree_clone_procedure_type(Tree *tree, const ProcedureType *type) {
	Node *results = 0;
	if (type->results && !(results = tree_clone_node(tree, type->results))) {
		return 0;
	}
	Node *params = tree_clone_node(tree, type->params);
	return params ? tree_new_procedure_type(tree, params, results, type->flags, type->convention) : 0;
}

static Node *tree_clone_slice_type(Tree *tree, const SliceType *slice_type) {
	Node *type = tree_clone_node(tree, slice_type->type);
	return type ? tree_new_slice_type(tree, type) : 0;
}

static Node *tree_clone_array_type(Tree *tree, const ArrayType *array_type) {
	Node *count = tree_clone_node(tree, array_type->count);
	Node *type = tree_clone_node(tree, array_type->type);
	return type && count ? tree_new_array_type(tree, count, type) : 0;
}

static Node *tree_clone_dynamic_array_type(Tree *tree, const DynamicArrayType *dynamic_array_type) {
	Node *type = tree_clone_node(tree, dynamic_array_type->type);
	return type ? tree_new_dynamic_array_type(tree, type) : 0;
}

static Node *tree_clone_pointer_type(Tree *tree, const PointerType *pointer_type) {
	Node *type = tree_clone_node(tree, pointer_type->type);
	return type ? tree_new_pointer_type(tree, type) : 0;
}

static Node *tree_clone_multi_pointer_type(Tree *tree, const MultiPointerType *multi_pointer_type) {
	Node *type = tree_clone_node(tree, multi_pointer_type->type);
	return type ? tree_new_multi_pointer_type(tree, type) : 0;
}

static Node *tree_clone_type(Tree *tree, const Type *type) {
	switch (type->kind) {
	case TYPE_PROCEDURE:
		return tree_clone_procedure_type(tree, &type->procedure);
	case TYPE_SLICE:
		return tree_clone_slice_type(tree, &type->slice);
	case TYPE_ARRAY:
		return tree_clone_array_type(tree, &type->array);
	case TYPE_DYNAMIC_ARRAY:
		return tree_clone_dynamic_array_type(tree, &type->dynamic_array);
	case TYPE_POINTER:
		return tree_clone_pointer_type(tree, &type->pointer);
	case TYPE_MULTI_POINTER:
		return tree_clone_multi_pointer_type(tree, &type->multi_pointer);
	case TYPE_TYPEID:
		return tree_new_typeid_type(tree);
	}
	UNREACHABLE();
}

Node *tree_clone_node(Tree *tree, const Node *node) {
	switch (node->kind) {
	case NODE_EXPRESSION:
		return tree_clone_expression(tree, &node->expression);
	case NODE_STATEMENT:
		return tree_clone_statement(tree, &node->statement);
	case NODE_IDENTIFIER:
		return tree_clone_identifier(tree, &node->identifier);
	case NODE_VALUE:
		return tree_clone_value(tree, &node->value);
	case NODE_LITERAL_VALUE:
		return tree_clone_literal_value(tree, &node->literal_value);
	case NODE_COMPOUND_LITERAL:
		return tree_clone_compound_literal(tree, &node->compound_literal);
	case NODE_FIELD:
		return tree_clone_field(tree, &node->field);
	case NODE_FIELD_LIST:
		return tree_clone_field_list(tree, &node->field_list);
	case NODE_PROCEDURE:
		return tree_clone_procedure(tree, &node->procedure);
	case NODE_PROCEDURE_GROUP:
		return tree_clone_procedure_group(tree, &node->procedure_group);
	case NODE_DIRECTIVE:
		return tree_clone_directive(tree, &node->directive);
	case NODE_TYPE:
		return tree_clone_type(tree, &node->type);
	}
	UNREACHABLE();
}