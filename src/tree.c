#include "tree.h"
#include "context.h"

ListExpression *tree_new_list_expression(Tree *tree, Array(Expression*) expressions) {
	Allocator *allocator = tree->context->allocator;
	ListExpression *expression = CAST(ListExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_LIST;
	expression->expressions = expressions;
	return expression;
}

UnaryExpression *tree_new_unary_expression(Tree *tree, OperatorKind operation, Expression *operand) {
	Allocator *allocator = tree->context->allocator;
	UnaryExpression *expression = CAST(UnaryExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_UNARY;
	expression->operation = operation;
	expression->operand = operand;
	return expression;
}

BinaryExpression *tree_new_binary_expression(Tree *tree, OperatorKind operation, Expression *lhs, Expression *rhs) {
	Allocator *allocator = tree->context->allocator;
	BinaryExpression *expression = CAST(BinaryExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_BINARY;
	expression->operation = operation;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return expression;
}

TernaryExpression *tree_new_ternary_expression(Tree *tree, Expression *on_true, KeywordKind operation, Expression *cond, Expression *on_false) {
	Allocator *allocator = tree->context->allocator;
	TernaryExpression *expression = CAST(TernaryExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_TERNARY;
	expression->on_true = on_true;
	expression->operation = operation;
	expression->cond = cond;
	expression->on_false = on_false;
	return expression;
}

SelectorExpression *tree_new_selector_expression(Tree *tree, Expression *operand, Identifier *identifier) {
	Allocator *allocator = tree->context->allocator;
	SelectorExpression *expression = CAST(SelectorExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_SELECTOR;
	expression->operand = operand;
	expression->identifier = identifier;
	return expression;
}

CallExpression *tree_new_call_expression(Tree *tree, Expression *operand, Array(Expression*) arguments) {
	Allocator *allocator = tree->context->allocator;
	CallExpression *expression = CAST(CallExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_CALL;
	expression->operand = operand;
	expression->arguments = arguments;
	return expression;
}

AssertionExpression *tree_new_assertion_expression(Tree *tree, Expression *operand, Type *type) {
	Allocator *allocator = tree->context->allocator;
	AssertionExpression *expression = CAST(AssertionExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_ASSERTION;
	expression->operand = operand;
	expression->type = type;
	return expression;
}

ProcedureExpression *tree_new_procedure_expression(Tree *tree, ProcedureType *type, ListExpression *where_clauses,BlockStatement *body) {
	Allocator *allocator = tree->context->allocator;
	ProcedureExpression *expression = CAST(ProcedureExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_PROCEDURE;
	expression->type = type;
	expression->where_clauses = where_clauses;
	expression->body = body;
	return expression;
}

TypeExpression *tree_new_type_expression(Tree *tree, Type *type) {
	Allocator *allocator = tree->context->allocator;
	TypeExpression *expression = CAST(TypeExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_TYPE;
	expression->type = type;
	return expression;
}

IndexExpression *tree_new_index_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs) {
	Allocator *allocator = tree->context->allocator;
	IndexExpression *expression = CAST(IndexExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_INDEX;
	expression->operand = operand;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return expression;
}

SliceExpression *tree_new_slice_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs) {
	Allocator *allocator = tree->context->allocator;
	SliceExpression *expression = CAST(SliceExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_SLICE;
	expression->operand = operand;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return expression;
}

LiteralExpression *tree_new_literal_expression(Tree *tree, LiteralKind kind, String value) {
	Allocator *allocator = tree->context->allocator;
	LiteralExpression *expression = CAST(LiteralExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_LITERAL;
	expression->kind = kind;
	expression->value = value;
	return expression;
}

CompoundLiteralExpression *tree_new_compound_literal_expression(Tree *tree, Expression *expression, Array(Expression*) expressions) {
	Allocator *allocator = tree->context->allocator;
	CompoundLiteralExpression *expr = CAST(CompoundLiteralExpression*, allocator->allocate(allocator, sizeof *expression));
	expr->base.kind = EXPRESSION_COMPOUND_LITERAL;
	expr->expression = expression;
	expr->expressions = expressions;
	return expr;
}

IdentifierExpression *tree_new_identifier_expression(Tree *tree, Identifier *identifier) {
	Allocator *allocator = tree->context->allocator;
	IdentifierExpression *expression = CAST(IdentifierExpression*, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_IDENTIFIER;
	expression->identifier = identifier;
	return expression;
}

// Statements.
EmptyStatement *tree_new_empty_statement(Tree *tree) {
	Allocator *allocator = tree->context->allocator;
	EmptyStatement *statement = CAST(EmptyStatement*, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_EMPTY;
	return statement;
}

ImportStatement *tree_new_import_statement(Tree *tree, String name, String package) {
	Context *context = tree->context;
	Allocator *allocator = context->allocator;
	ImportStatement *statement = CAST(ImportStatement*, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_IMPORT;
	statement->name = string_copy(name);
	statement->package = string_copy(package);
	return statement;
}

ExpressionStatement *tree_new_expression_statement(Tree *tree, Expression *expression) {
	Allocator *allocator = tree->context->allocator;
	ExpressionStatement *statement = CAST(ExpressionStatement*, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_EXPRESSION;
	statement->expression = expression;
	return statement;
}

BlockStatement *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Statement*) statements) {
	Allocator *allocator = tree->context->allocator;
	BlockStatement *statement = CAST(BlockStatement*, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_BLOCK;
	statement->flags = flags;
	statement->statements = statements;
	return statement;
}

AssignmentStatement *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, ListExpression *lhs, ListExpression *rhs) {
	Allocator *allocator = tree->context->allocator;
	AssignmentStatement *statement = CAST(AssignmentStatement*, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_ASSIGNMENT;
	statement->assignment = assignment;
	statement->lhs = lhs;
	statement->rhs = rhs;
	return statement;
}

DeclarationStatement *tree_new_declaration_statement(Tree *tree, Type *type, Array(Identifier*) names, ListExpression *values) {
	Allocator *allocator = tree->context->allocator;
	DeclarationStatement *statement = CAST(DeclarationStatement *, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_DECLARATION;
	statement->type = type;
	statement->names = names;
	statement->values = values;
	return statement;
}

IfStatement *tree_new_if_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, BlockStatement *elif) {
	Allocator *allocator = tree->context->allocator;
	IfStatement *statement = CAST(IfStatement *, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_IF;
	statement->body = body;
	statement->cond = cond;
	statement->elif = elif;
	statement->init = init;
	return statement;
}

ForStatement *tree_new_for_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, Statement *post) {
	Allocator *allocator = tree->context->allocator;
	ForStatement *statement = CAST(ForStatement *, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_FOR;
	statement->body = body;
	statement->cond = cond;
	statement->init = init;
	statement->post = post;
	return statement;
}

ReturnStatement *tree_new_return_statement(Tree *tree, Array(Expression*) results) {
	Allocator *allocator = tree->context->allocator;
	ReturnStatement *statement = CAST(ReturnStatement *, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_RETURN;
	statement->results = results;
	return statement;
}

BranchStatement *tree_new_branch_statement(Tree *tree, KeywordKind branch, Identifier *label) {
	Allocator *allocator = tree->context->allocator;
	BranchStatement *statement = CAST(BranchStatement *, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_BRANCH;
	statement->branch = branch;
	statement->label = label;
	return statement;
}

DeferStatement *tree_new_defer_statement(Tree *tree, Statement *stmt) {
	Allocator *allocator = tree->context->allocator;
	DeferStatement *statement = CAST(DeferStatement *, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_DEFER;
	statement->statement = stmt;
	return statement;
}

Identifier *tree_new_identifier(Tree *tree, String contents, Bool poly) {
	Context *context = tree->context;
	Allocator *allocator = context->allocator;
	Identifier *identifier = CAST(Identifier *, allocator->allocate(allocator, sizeof *identifier));
	identifier->contents = string_copy(contents);
	identifier->poly = poly;
	return identifier;
}

// Types
IdentifierType *tree_new_identifier_type(Tree *tree, Identifier *identifier) {
	Allocator *allocator = tree->context->allocator;
	IdentifierType *type = CAST(IdentifierType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_IDENTIFIER;
	type->identifier = identifier;
	return type;
}

// ^T
PointerType *tree_new_pointer_type(Tree *tree, Type *value_type) {
	Allocator *allocator = tree->context->allocator;
	PointerType *type = CAST(PointerType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_POINTER;
	type->type = value_type;
	return type;
}

// [^]T
MultiPointerType *tree_new_multi_pointer_type(Tree *tree, Type *value_type) {
	Allocator *allocator = tree->context->allocator;
	MultiPointerType *type = CAST(MultiPointerType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_MULTI_POINTER;
	type->type = value_type;
	return type;
}

// []T
SliceType *tree_new_slice_type(Tree *tree, Type *value_type) {
	Allocator *allocator = tree->context->allocator;
	SliceType *type = CAST(SliceType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_SLICE;
	type->type = value_type;
	return type;
}

// [N]T
// [?]T
ArrayType *tree_new_array_type(Tree *tree, Type *value_type, Expression *count) {
	Allocator *allocator = tree->context->allocator;
	ArrayType *type = CAST(ArrayType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_ARRAY;
	type->type = value_type;
	type->count = count;
	return type;
}

// [dynamic]T
DynamicArrayType *tree_new_dynamic_array_type(Tree *tree, Type *value_type) {
	Allocator *allocator = tree->context->allocator;
	DynamicArrayType *type = CAST(DynamicArrayType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_DYNAMIC_ARRAY;
	type->type = value_type;
	return type;
}

// bit_set[T]
// bit_set[T; U]
BitSetType *tree_new_bit_set_type(Tree *tree, Expression *expression, Type *underlying) {
	Allocator *allocator = tree->context->allocator;
	BitSetType *type = CAST(BitSetType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_BIT_SET;
	type->expression = expression;
	type->underlying = underlying;
	return type;
}

// typeid
TypeidType *tree_new_typeid_type(Tree *tree, Type *specialization) {
	Allocator *allocator = tree->context->allocator;
	TypeidType *type = CAST(TypeidType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_TYPEID;
	type->specialization = specialization;
	return type;
}

// map[K]V
MapType *tree_new_map_type(Tree *tree, Type *key, Type *value) {
	Allocator *allocator = tree->context->allocator;
	MapType *type = CAST(MapType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_MAP;
	type->key = key;
	type->value = value;
	return type;
}

// matrix[R,C]T
MatrixType *tree_new_matrix_type(Tree *tree, Expression *rows, Expression *columns, Type *base_type) {
	Allocator *allocator = tree->context->allocator;
	MatrixType *type = CAST(MatrixType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_MATRIX;
	type->rows = rows;
	type->columns = columns;
	type->type = base_type;
	return type;
}

ExpressionType *tree_new_expression_type(Tree *tree, Expression *expression) {
	Allocator *allocator = tree->context->allocator;
	ExpressionType *type = CAST(ExpressionType *, allocator->allocate(allocator, sizeof *type));
	type->base.kind = TYPE_EXPRESSION;
	ASSERT(expression);
	type->expression = expression;
	return type;
}

ConcreteProcedureType *tree_new_concrete_procedure_type(Tree *tree, Array(Field*) params, Array(Field*) results, ProcedureFlag flags, CallingConvention convention) {
	Allocator *allocator = tree->context->allocator;
	ConcreteProcedureType *type = CAST(ConcreteProcedureType *, allocator->allocate(allocator, sizeof *type));
	type->base.base.kind = TYPE_PROCEDURE;
	type->base.kind = PROCEDURE_CONCRETE;
	type->base.convention = convention;
	type->base.params = params;
	type->base.results = results;
	type->base.flags = flags;
	return type;
}

GenericProcedureType *tree_new_generic_procedure_type(Tree *tree, Array(Field*) params, Array(Field*) results, ProcedureFlag flags, CallingConvention convention) {
	Allocator *allocator = tree->context->allocator;
	GenericProcedureType *type = CAST(GenericProcedureType*, allocator->allocate(allocator, sizeof *type));
	type->base.base.kind = TYPE_PROCEDURE;
	type->base.kind = PROCEDURE_GENERIC;
	type->base.convention = convention;
	type->base.params = params;
	type->base.results = results;
	type->base.flags = flags;
	return type;
}

Field *tree_new_field(Tree *tree, Type *type, Identifier *name, Expression *value) {
	Allocator *allocator = tree->context->allocator;
	Field *field = CAST(Field*, allocator->allocate(allocator, sizeof *field));
	field->type = type;
	field->name = name;
	field->value = value;
	return field;
}

void tree_init(Tree *tree, Context *context) {
	tree->context = context;
	tree->package_name = STRING_NIL;
	tree->file_name = STRING_NIL;
	tree->statements = 0;
}