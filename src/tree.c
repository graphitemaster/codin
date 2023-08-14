#include "tree.h"
#include "context.h"
#include "allocator.h"
#include "path.h"

ListExpression *tree_new_list_expression(Tree *tree, Array(Expression*) expressions) {
	Allocator *const allocator = &tree->context->allocator;
	ListExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_LIST;
	expression->expressions = expressions;
	return expression;
}

UnaryExpression *tree_new_unary_expression(Tree *tree, OperatorKind operation, Expression *operand) {
	Allocator *const allocator = &tree->context->allocator;
	UnaryExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_UNARY;
	expression->operation = operation;
	expression->operand = operand;
	return expression;
}

BinaryExpression *tree_new_binary_expression(Tree *tree, OperatorKind operation, Expression *lhs, Expression *rhs) {
	Allocator *const allocator = &tree->context->allocator;
	BinaryExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_BINARY;
	expression->operation = operation;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return expression;
}

TernaryExpression *tree_new_ternary_expression(Tree *tree, Expression *on_true, KeywordKind operation, Expression *cond, Expression *on_false) {
	Allocator *const allocator = &tree->context->allocator;
	TernaryExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_TERNARY;
	expression->on_true = on_true;
	expression->operation = operation;
	expression->cond = cond;
	expression->on_false = on_false;
	return expression;
}

CastExpression *tree_new_cast_expression(Tree *tree, OperatorKind kind, Type *type, Expression *expression) {
	Allocator *const allocator = &tree->context->allocator;
	CastExpression *const expr = allocator_allocate(allocator, sizeof *expr);
	expr->base.kind = EXPRESSION_CAST;
	expr->kind = kind;
	expr->type = type;
	expr->expression = expression;
	return expr;
}

SelectorExpression *tree_new_selector_expression(Tree *tree, Expression *operand, Identifier *identifier) {
	Allocator *const allocator = &tree->context->allocator;
	SelectorExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_SELECTOR;
	expression->operand = operand;
	expression->identifier = identifier;
	return expression;
}

CallExpression *tree_new_call_expression(Tree *tree, Expression *operand, Array(Field*) arguments) {
	Allocator *const allocator = &tree->context->allocator;
	CallExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_CALL;
	expression->operand = operand;
	expression->arguments = arguments;
	return expression;
}

AssertionExpression *tree_new_assertion_expression(Tree *tree, Expression *operand, Type *type) {
	Allocator *const allocator = &tree->context->allocator;
	AssertionExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_ASSERTION;
	expression->operand = operand;
	expression->type = type;
	return expression;
}

ProcedureExpression *tree_new_procedure_expression(Tree *tree, ProcedureType *type, ListExpression *where_clauses,BlockStatement *body) {
	Allocator *const allocator = &tree->context->allocator;
	ProcedureExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_PROCEDURE;
	expression->type = type;
	expression->where_clauses = where_clauses;
	expression->body = body;
	return expression;
}

TypeExpression *tree_new_type_expression(Tree *tree, Type *type) {
	Allocator *const allocator = &tree->context->allocator;
	TypeExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_TYPE;
	expression->type = type;
	return expression;
}

IndexExpression *tree_new_index_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs) {
	Allocator *const allocator = &tree->context->allocator;
	IndexExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_INDEX;
	expression->operand = operand;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return expression;
}

SliceExpression *tree_new_slice_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs) {
	Allocator *const allocator = &tree->context->allocator;
	SliceExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_SLICE;
	expression->operand = operand;
	expression->lhs = lhs;
	expression->rhs = rhs;
	return expression;
}

LiteralExpression *tree_new_literal_expression(Tree *tree, LiteralKind kind, String value) {
	Allocator *const allocator = &tree->context->allocator;
	LiteralExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_LITERAL;
	expression->kind = kind;
	expression->value = value;
	return expression;
}

CompoundLiteralExpression *tree_new_compound_literal_expression(Tree *tree, Type *type, Array(Field*) fields) {
	Allocator *const allocator = &tree->context->allocator;
	CompoundLiteralExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_COMPOUND_LITERAL;
	expression->type = type;
	expression->fields = fields;
	return expression;
}

IdentifierExpression *tree_new_identifier_expression(Tree *tree, Identifier *identifier) {
	Allocator *const allocator = &tree->context->allocator;
	IdentifierExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_IDENTIFIER;
	expression->identifier = identifier;
	return expression;
}

UndefinedExpression *tree_new_undefined_expression(Tree *tree) {
	Allocator *const allocator = &tree->context->allocator;
	UndefinedExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_UNDEFINED;
	return expression;
}

ProcedureGroupExpression *tree_new_procedure_group_expression(Tree *tree, Array(Expression*) expressions) {
	Allocator *const allocator = &tree->context->allocator;
	ProcedureGroupExpression *const expression = allocator_allocate(allocator, sizeof *expression);
	expression->base.kind = EXPRESSION_PROCEDURE_GROUP;
	expression->expressions = expressions;
	return expression;
}

// Statements.
EmptyStatement *tree_new_empty_statement(Tree *tree) {
	Allocator *const allocator = &tree->context->allocator;
	EmptyStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_EMPTY;
	return statement;
}

ImportStatement *tree_new_import_statement(Tree *tree, String name, String collection, String pathname, Bool is_using) {
	Allocator *const allocator = &tree->context->allocator;
	ImportStatement *statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_IMPORT;
	statement->name = name;
	statement->collection = collection;
	if (string_starts_with(pathname, SCLIT("."))) {
		statement->pathname = pathname;
	} else {
		statement->pathname = path_cat(SCLIT("."), pathname, tree->context);
	}
	statement->is_using = is_using;
	return statement;
}

ExpressionStatement *tree_new_expression_statement(Tree *tree, Expression *expression) {
	Allocator *const allocator = &tree->context->allocator;
	ExpressionStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_EXPRESSION;
	statement->expression = expression;
	return statement;
}

BlockStatement *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Statement*) statements) {
	Allocator *const allocator = &tree->context->allocator;
	BlockStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_BLOCK;
	statement->flags = flags;
	statement->statements = statements;
	return statement;
}

AssignmentStatement *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, ListExpression *lhs, ListExpression *rhs) {
	Allocator *const allocator = &tree->context->allocator;
	AssignmentStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_ASSIGNMENT;
	statement->assignment = assignment;
	statement->lhs = lhs;
	statement->rhs = rhs;
	return statement;
}

DeclarationStatement *tree_new_declaration_statement(Tree *tree, Type *type, Array(Identifier*) names, ListExpression *values, Bool is_using) {
	Allocator *const allocator = &tree->context->allocator;
	DeclarationStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_DECLARATION;
	statement->type = type;
	statement->names = names;
	statement->values = values;
	statement->is_using = is_using;
	statement->attributes = 0;
	return statement;
}

IfStatement *tree_new_if_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, BlockStatement *elif) {
	Allocator *const allocator = &tree->context->allocator;
	IfStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_IF;
	statement->body = body;
	statement->cond = cond;
	statement->elif = elif;
	statement->init = init;
	return statement;
}

WhenStatement *tree_new_when_statement(Tree *tree, Expression *cond, BlockStatement *body, BlockStatement *elif) {
	Allocator *const allocator = &tree->context->allocator;
	WhenStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_WHEN;
	statement->body = body;
	statement->cond = cond;
	statement->elif = elif;
	return statement;
}

ForStatement *tree_new_for_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, Statement *post) {
	Allocator *const allocator = &tree->context->allocator;
	ForStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_FOR;
	statement->body = body;
	statement->cond = cond;
	statement->init = init;
	statement->post = post;
	return statement;
}

SwitchStatement *tree_new_switch_statement(Tree *tree, Statement *init, Expression *cond, Array(CaseClause*) clauses) {
	Allocator *const allocator = &tree->context->allocator;
	SwitchStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_SWITCH;
	statement->init = init;
	statement->cond = cond;
	statement->clauses = clauses;
	return statement;
}

ReturnStatement *tree_new_return_statement(Tree *tree, Array(Expression*) results) {
	Allocator *const allocator = &tree->context->allocator;
	ReturnStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_RETURN;
	statement->results = results;
	return statement;
}

DeferStatement *tree_new_defer_statement(Tree *tree, Statement *stmt) {
	Allocator *const allocator = &tree->context->allocator;
	DeferStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_DEFER;
	statement->statement = stmt;
	return statement;
}

BranchStatement *tree_new_branch_statement(Tree *tree, KeywordKind branch, Identifier *label) {
	Allocator *const allocator = &tree->context->allocator;
	BranchStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_BRANCH;
	statement->branch = branch;
	statement->label = label;
	return statement;
}

ForeignBlockStatement *tree_new_foreign_block_statement(Tree *tree, Identifier *name, BlockStatement *body) {
	Allocator *const allocator = &tree->context->allocator;
	ForeignBlockStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_FOREIGN_BLOCK;
	statement->name = name;
	statement->body = body;
	statement->attributes = 0;
	return statement;
}

ForeignImportStatement *tree_new_foreign_import_statement(Tree *tree, String name, Array(String) sources) {
	Allocator *const allocator = &tree->context->allocator;
	ForeignImportStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_FOREIGN_IMPORT;
	statement->name = name;
	statement->sources = sources;
	statement->attributes = 0;
	return statement;
}

UsingStatement *tree_new_using_statement(Tree *tree, ListExpression *list) {
	Allocator *const allocator = &tree->context->allocator;
	UsingStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_USING;
	statement->list = list;
	return statement;
}

PackageStatement *tree_new_package_statement(Tree *tree, String name) {
	Allocator *const allocator = &tree->context->allocator;
	PackageStatement *const statement = allocator_allocate(allocator, sizeof *statement);
	statement->base.kind = STATEMENT_PACKAGE;
	statement->name = name;
	return statement;
}

Identifier *tree_new_identifier(Tree *tree, String contents, Bool poly) {
	Allocator *const allocator = &tree->context->allocator;
	Identifier *const identifier = allocator_allocate(allocator, sizeof *identifier);
	identifier->contents = contents;
	identifier->poly = poly;
	identifier->token = array_size(tree->tokens) - 1;
	return identifier;
}

CaseClause *tree_new_case_clause(Tree *tree, ListExpression *expressions, Array(Statement*) statements) {
	Allocator *const allocator = &tree->context->allocator;
	CaseClause *const clause = allocator_allocate(allocator, sizeof *clause);
	clause->expressions = expressions;
	clause->statements = statements;
	return clause;
}

// Types

static Ptr new_type(Tree *tree, TypeKind kind, Size sizeof_type) {
	Allocator *const allocator = &tree->context->allocator;
	Type *const type = allocator_allocate(allocator, sizeof_type);
	type->kind = kind;
	type->poly = false;
	return type;
}

// ^T
PointerType *tree_new_pointer_type(Tree *tree, Type *value_type) {
	PointerType *type = new_type(tree, TYPE_POINTER, sizeof *type);
	type->type = value_type;
	return type;
}

// [^]T
MultiPointerType *tree_new_multi_pointer_type(Tree *tree, Type *value_type) {
	MultiPointerType *type = new_type(tree, TYPE_MULTI_POINTER, sizeof *type);
	type->type = value_type;
	return type;
}

// []T
SliceType *tree_new_slice_type(Tree *tree, Type *value_type) {
	SliceType *type = new_type(tree, TYPE_SLICE, sizeof *type);
	type->type = value_type;
	return type;
}

// [N]T
// [?]T
ArrayType *tree_new_array_type(Tree *tree, Type *value_type, Expression *count) {
	ArrayType *type = new_type(tree, TYPE_ARRAY, sizeof *type);
	type->type = value_type;
	type->count = count;
	return type;
}

// [dynamic]T
DynamicArrayType *tree_new_dynamic_array_type(Tree *tree, Type *value_type) {
	DynamicArrayType *type = new_type(tree, TYPE_DYNAMIC_ARRAY, sizeof *type);
	type->type = value_type;
	return type;
}

// bit_set[T]
// bit_set[T; U]
BitSetType *tree_new_bit_set_type(Tree *tree, Expression *expression, Type *underlying) {
	BitSetType *type = new_type(tree, TYPE_BIT_SET, sizeof *type);
	type->expression = expression;
	type->underlying = underlying;
	return type;
}

// typeid
TypeidType *tree_new_typeid_type(Tree *tree, Type *specialization) {
	TypeidType *type = new_type(tree, TYPE_TYPEID, sizeof *type);
	type->specialization = specialization;
	return type;
}

// map[K]V
MapType *tree_new_map_type(Tree *tree, Type *key, Type *value) {
	MapType *type = new_type(tree, TYPE_MAP, sizeof *type);
	type->key = key;
	type->value = value;
	return type;
}

// matrix[R,C]T
MatrixType *tree_new_matrix_type(Tree *tree, Expression *rows, Expression *columns, Type *base_type) {
	MatrixType *type = new_type(tree, TYPE_MATRIX, sizeof *type);
	type->rows = rows;
	type->columns = columns;
	type->type = base_type;
	return type;
}

// distinct T
DistinctType *tree_new_distinct_type(Tree *tree, Type *base_type) {
	DistinctType *type = new_type(tree, TYPE_DISTINCT, sizeof *type);
	type->type = base_type;
	return type;
}

// enum
EnumType *tree_new_enum_type(Tree *tree, Type *base_type, Array(Field*) fields) {
	EnumType *type = new_type(tree, TYPE_ENUM, sizeof *type);
	type->type = base_type;
	type->fields = fields;
	return type;
}

// struct
ConcreteStructType *tree_new_concrete_struct_type(Tree *tree, StructFlag flags, Expression *align, Array(Field*) fields, ListExpression *where_clauses) {
	ConcreteStructType *type = new_type(tree, TYPE_STRUCT, sizeof *type);
	type->base.kind = STRUCT_CONCRETE;
	type->base.flags = flags;
	type->base.align = align;
	type->base.fields = fields;
	type->base.where_clauses = where_clauses;
	return type;
}

// struct()
GenericStructType *tree_new_generic_struct_type(Tree *tree, StructFlag flags, Expression *align, Array(Field*) parameters, Array(Field*) fields, ListExpression *where_clauses) {
	GenericStructType *type = new_type(tree, TYPE_STRUCT, sizeof *type);
	type->base.kind = STRUCT_GENERIC;
	type->base.flags = flags;
	type->base.align = align;
	type->base.fields = fields;
	type->base.where_clauses = where_clauses;
	type->parameters = parameters;
	return type;
}

// union
ConcreteUnionType *tree_new_concrete_union_type(Tree *tree, UnionFlag flags, Expression *align, Array(Type*) variants, ListExpression *where_clauses) {
	ConcreteUnionType *type = new_type(tree, TYPE_UNION, sizeof *type);
	type->base.kind = UNION_GENERIC;
	type->base.flags = flags;
	type->base.align = align;
	type->base.variants = variants;
	type->base.where_clauses = where_clauses;
	return type;
}

// union()
GenericUnionType *tree_new_generic_union_type(Tree *tree, UnionFlag flags, Expression *align, Array(Field*) parameters, Array(Type*) variants, ListExpression *where_clauses) {
	GenericUnionType *type = new_type(tree, TYPE_UNION, sizeof *type);
	type->base.kind = UNION_GENERIC;
	type->base.flags = flags;
	type->base.align = align;
	type->base.variants = variants;
	type->base.where_clauses = where_clauses;
	type->parameters = parameters;
	return type;
}

// $T or $T/$U
PolyType *tree_new_poly_type(Tree *tree, Type *base_type, Type *specialization) {
	PolyType *type = new_type(tree, TYPE_POLY, sizeof *type);
	type->type = base_type;
	type->specialization = specialization;
	return type;
}

ExpressionType *tree_new_expression_type(Tree *tree, Expression *expression) {
	ExpressionType *type = new_type(tree, TYPE_EXPRESSION, sizeof *type);
	type->expression = expression;
	return type;
}

ConcreteProcedureType *tree_new_concrete_procedure_type(Tree *tree, Array(Field*) params, Array(Field*) results, ProcedureFlag flags, CallingConvention convention) {
	ConcreteProcedureType *type = new_type(tree, TYPE_PROCEDURE, sizeof *type);
	type->base.kind = PROCEDURE_CONCRETE;
	type->base.convention = convention;
	type->base.params = params;
	type->base.results = results;
	type->base.flags = flags;
	return type;
}

GenericProcedureType *tree_new_generic_procedure_type(Tree *tree, Array(Field*) params, Array(Field*) results, ProcedureFlag flags, CallingConvention convention) {
	GenericProcedureType *const type = new_type(tree, TYPE_PROCEDURE, sizeof *type);
	type->base.kind = PROCEDURE_GENERIC;
	type->base.convention = convention;
	type->base.params = params;
	type->base.results = results;
	type->base.flags = flags;
	return type;
}

Field *tree_new_field(Tree *tree, Type *type, Identifier *name, Expression *value, String tag, FieldFlag flags) {
	Allocator *const allocator = &tree->context->allocator;
	Field *field = allocator_allocate(allocator, sizeof *field);
	field->type = type;
	field->name = name;
	field->value = value;
	field->tag = tag;
	field->flags = flags;
	return field;
}

void tree_init(Tree *tree, String filename, Context *context) {
	tree->context = context;
	tree->filename = filename;
	tree->statements = array_make(context);
	tree->tokens = array_make(context);
}

void tree_fini(Tree *tree) {
	array_free(tree->statements);
	array_free(tree->tokens);
}

void tree_record_token(Tree *tree, Token token) {
	array_push(tree->tokens, token);
}