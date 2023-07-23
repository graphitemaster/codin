#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "tree.h"
#include "context.h"

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
	Context *context = tree->context;
	Node *node = new_statement(tree, STATEMENT_IMPORT);
	ImportStatement *statement = &node->statement.import;
	statement->name = string_copy(name);
	statement->package = string_copy(package);
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
	Context *context = tree->context;
	Node *node = new_node(tree, NODE_IDENTIFIER);
	Identifier *identifier = &node->identifier;
	identifier->contents = string_copy(contents);
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
	Context *context = tree->context;
	Node *node = new_node(tree, NODE_LITERAL_VALUE);
	LiteralValue *literal_value = &node->literal_value;
	literal_value->literal = literal;
	literal_value->value = string_copy(value);
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

static void tree_dump_break_statement(const BreakStatement *statement, Sint32 depth) {
	(void)statement;
	(void)depth;
	printf("(break)");
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
	case STATEMENT_BREAK:
		return tree_dump_break_statement(&statement->break_, depth);
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