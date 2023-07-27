#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

#include "tree.h"
#include "context.h"
#include "strbuf.h"

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

ValueExpression *tree_new_value_expression(Tree *tree, Value *value) {
	Allocator *allocator = tree->context->allocator;
	ValueExpression *expression = CAST(ValueExpression *, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_VALUE;
	expression->value = value;
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

// Values
LiteralValue *tree_new_literal_value(Tree *tree, LiteralKind kind, String input) {
	Allocator *allocator = tree->context->allocator;
	LiteralValue *value = CAST(LiteralValue *, allocator->allocate(allocator, sizeof *value));
	value->base.kind = VALUE_LITERAL;
	value->kind = kind;
	value->input = input;
	return value;
}

CompoundLiteralValue *tree_new_compound_literal_value(Tree *tree, Expression *expression, Array(Expression*) expressions) {
	Allocator *allocator = tree->context->allocator;
	CompoundLiteralValue *value = CAST(CompoundLiteralValue *, allocator->allocate(allocator, sizeof *value));
	value->base.kind = VALUE_COMPOUND_LITERAL;
	value->expression = expression;
	value->expressions = expressions;
	return value;
}

IdentifierValue *tree_new_identifier_value(Tree *tree, Identifier *identifier) {
	Allocator *allocator = tree->context->allocator;
	IdentifierValue *value = CAST(IdentifierValue *, allocator->allocate(allocator, sizeof *value));
	value->base.kind = VALUE_IDENTIFIER;
	value->identifier = identifier;
	return value;
}

Identifier *tree_new_identifier(Tree *tree, String contents) {
	Context *context = tree->context;
	Allocator *allocator = context->allocator;
	Identifier *identifier = CAST(Identifier *, allocator->allocate(allocator, sizeof *identifier));
	identifier->contents = string_copy(contents);
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
	tree->package = STRING_NIL;
	tree->statements = 0;
}

void pad(Sint32 depth) {
	Sint32 dots = depth * 2 - 1;
	for (Sint32 i = 0; i < dots; i++) {
		printf(".");
	}
	if (dots > 0) printf(" ");
}

Bool tree_dump_literal_value(const LiteralValue *value, Sint32 depth) {
	pad(depth);
	const String kind = literal_to_string(value->kind);
	printf("(value '%.*s' %.*s)", SFMT(kind), SFMT(value->input));
	return true;
}

Bool tree_dump_compound_literal_value(const CompoundLiteralValue *value, Sint32 depth) {
	pad(depth);
	(void)value;
	return false;
}

Bool tree_dump_identifier_value(const IdentifierValue *value, Sint32 depth) {
	pad(depth);
	printf("'%.*s'", SFMT(value->identifier->contents));
	return true;
}

Bool tree_dump_value(const Value *value, Sint32 depth) {
	switch (value->kind) {
	case VALUE_LITERAL:          return tree_dump_literal_value(RCAST(const LiteralValue *, value), depth);
	case VALUE_COMPOUND_LITERAL: return tree_dump_compound_literal_value(RCAST(const CompoundLiteralValue *, value), depth);
	case VALUE_IDENTIFIER:       return tree_dump_identifier_value(RCAST(const IdentifierValue *, value), depth);
	}
	return false;
}

Bool tree_dump_expression(const Expression *expression, Sint32 depth);

Bool tree_dump_list_expression(const ListExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(list\n");
	const Uint64 n_elements = array_size(expression->expressions);
	for (Uint64 i = 0; i < n_elements; i++) {
		const Expression *expr = expression->expressions[i];
		tree_dump_expression(expr, depth + 1);
		if (i != n_elements - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool tree_dump_unary_expression(const UnaryExpression *expression, Sint32 depth) {
	const String op = operator_to_string(expression->operation);
	pad(depth);
	printf("(uop '%.*s'\n", SFMT(op));
	tree_dump_expression(expression->operand, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_binary_expression(const BinaryExpression *expression, Sint32 depth) {
	const String op = operator_to_string(expression->operation);
	pad(depth);
	printf("(bop '%.*s'\n", SFMT(op));
	tree_dump_expression(expression->lhs, depth + 1);
	printf("\n");
	tree_dump_expression(expression->rhs, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_cast_expression(const CastExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(cast");
	if (expression->type) {
		printf(" '%.*s'", SFMT(expression->type->contents));
	}
	tree_dump_expression(expression->expression, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_selector_expression(const SelectorExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(sel\n");
	tree_dump_expression(expression->operand, depth + 1);
	printf("\n");
	const String target = expression->identifier->contents;
	pad(depth + 1);
	printf("'%.*s'", SFMT(target));
	printf(")");
	return true;
}

Bool tree_dump_call_expression(const CallExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(call\n");
	tree_dump_expression(expression->operand, depth + 1);
	const Uint64 n_arguments = array_size(expression->arguments);
	if (n_arguments != 0) {
		printf("\n");
	}
	for (Uint64 i = 0; i < n_arguments; i++) {
		const Expression *argument = expression->arguments[i];
		tree_dump_expression(argument, depth + 1);
		if (i != n_arguments - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool tree_dump_type(const Type *type, Sint32 depth);
Bool tree_dump_assertion_expression(const AssertionExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(assert-type\n");
	tree_dump_type(expression->type, depth + 1);
	printf("\n");
	tree_dump_expression(expression->operand, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_value_expression(const ValueExpression *expression, Sint32 depth) {
	return tree_dump_value(expression->value, depth);
}

Bool tree_dump_block_statement(const BlockStatement *statement, Sint32 depth);

String procedure_flags_to_string(ProcedureFlag flags, Context *context) {
	StrBuf buf;
	strbuf_init(&buf, context);
	Sint32 i = 0;
	if (flags & PROC_FLAG_DIVERGING) {
		if (i) strbuf_put_string(&buf, SCLIT(" "));
		strbuf_put_string(&buf, SCLIT("'#diverging'"));
		i++;
	}
	if (flags & PROC_FLAG_OPTIONAL_OK) {
		if (i) strbuf_put_string(&buf, SCLIT(" "));
		strbuf_put_string(&buf, SCLIT("'#optional_ok'"));
		i++;
	}
	if (flags & PROC_FLAG_OPTIONAL_ALLOCATION_ERROR) {
		if (i) strbuf_put_string(&buf, SCLIT(" "));
		strbuf_put_string(&buf, SCLIT("'#optional_allocation_error'"));
		i++;
	}
	if (flags & PROC_FLAG_BOUNDS_CHECK) {
		if (i) strbuf_put_string(&buf, SCLIT(" "));
		strbuf_put_string(&buf, SCLIT("'#bounds_check'"));
		i++;
	}
	if (flags & PROC_FLAG_TYPE_ASSERT) {
		if (i) strbuf_put_string(&buf, SCLIT(" "));
		strbuf_put_string(&buf, SCLIT("'#type_assert'"));
		i++;
	}
	if (flags & PROC_FLAG_FORCE_INLINE) {
		if (i) strbuf_put_string(&buf, SCLIT(" "));
		strbuf_put_string(&buf, SCLIT("'#force_inline'"));
		i++;
	}
	return strbuf_result(&buf);
}


Bool tree_dump_type(const Type *type, Sint32 depth);

Bool tree_dump_identifier_type(const IdentifierType *type, Sint32 depth) {
	pad(depth);
	printf("(ident '%.*s')", SFMT(type->identifier->contents));
	return true;
}

Bool tree_dump_fields(Array(Field*) const fields, Sint32 depth) {
	pad(depth);
	const Uint64 n_fields = array_size(fields);
	for (Uint64 i = 0; i < n_fields; i++) {
		const Field *field = fields[i];
		printf("(field\n");
		pad(depth + 1);
		printf("'%.*s'", SFMT(field->name->contents));
		if (field->type) {
			printf("\n");
			tree_dump_type(field->type, depth + 1);
		}
		if (field->value) {
			printf("\n");
			tree_dump_expression(field->value, depth + 1);
		}
		printf(")");
		if (i != n_fields - 1) {
			printf("\n");
			pad(depth);
		}
	}
	return true;
}

Bool tree_dump_procedure_type(const ProcedureType *type, Sint32 depth) {
	pad(depth);
	const String cc = calling_convention_to_string(type->convention);
	printf("(cc '%.*s')\n", SFMT(cc));
	if (type->flags) {
		Context context;
		context.allocator = &DEFAULT_ALLOCATOR;
		const String flags = procedure_flags_to_string(type->flags, &context);
		pad(depth);
		printf("(flags %.*s)", SFMT(flags));
	}
	if (type->params) {
		printf("\n");
		pad(depth);
		printf("(args\n");
		tree_dump_fields(type->params, depth + 1);
		printf(")");
	}
	if (type->results) {
		printf("\n");
		pad(depth);
		printf("(results\n");
		tree_dump_fields(type->results, depth + 1);
		printf(")");
	}
	return true;
}

Bool tree_dump_pointer_type(const PointerType *type, Sint32 depth) {
	pad(depth);
	printf("(pointer\n");
	tree_dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_multi_pointer_type(const MultiPointerType *type, Sint32 depth) {
	pad(depth);
	printf("(multi-pointer\n");
	tree_dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_slice_type(const SliceType *type, Sint32 depth) {
	pad(depth);
	printf("(slice\n");
	tree_dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_array_type(const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(array\n");
	if (type->count) {
		pad(depth + 1);
		printf("(count\n");
		if (type->count) {
			tree_dump_expression(type->count, depth + 2);
		}
		printf(")");
		printf("\n");
	}
	tree_dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_dynamic_array_type(const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(dynamic-array\n");
	tree_dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_bit_set_type(const BitSetType *type, Sint32 depth) {
	pad(depth);
	printf("(bit-set\n");
	tree_dump_expression(type->expression, depth + 1);
	if (type->underlying) {
		printf("\n");
		tree_dump_type(type->underlying, depth + 1);
	}
	printf(")");
	return true;
}

Bool tree_dump_list_expression(const ListExpression *expression, Sint32 depth);

Bool tree_dump_procedure_expression(const ProcedureExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(proc\n");
	tree_dump_procedure_type(expression->type, depth + 1);
	printf("\n");
	if (expression->where_clauses) {
		pad(depth + 1);
		printf("(where\n");
		tree_dump_list_expression(expression->where_clauses, depth + 2);
		printf(")");
		printf("\n");
	}
	tree_dump_block_statement(expression->body, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_type(const Type *type, Sint32 depth) {
	switch (type->kind) {
	case TYPE_IDENTIFIER:    return tree_dump_identifier_type(RCAST(const IdentifierType *, type), depth);
	case TYPE_PROCEDURE:     return tree_dump_procedure_type(RCAST(const ProcedureType *, type), depth);
	case TYPE_POINTER:       return tree_dump_pointer_type(RCAST(const PointerType *, type), depth);
	case TYPE_MULTI_POINTER: return tree_dump_multi_pointer_type(RCAST(const MultiPointerType *, type), depth);
	case TYPE_SLICE:         return tree_dump_slice_type(RCAST(const SliceType *, type), depth);
	case TYPE_ARRAY:         return tree_dump_array_type(RCAST(const ArrayType *, type), depth);
	case TYPE_DYNAMIC_ARRAY: return tree_dump_dynamic_array_type(RCAST(const ArrayType *, type), depth);
	case TYPE_BIT_SET:       return tree_dump_bit_set_type(RCAST(const BitSetType *, type), depth);
	}
	return false;
}

Bool tree_dump_type_expression(const TypeExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(type\n");
	tree_dump_type(expression->type, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_expression(const Expression *expression, Sint32 depth) {
	switch (expression->kind) {
	case EXPRESSION_LIST:      return tree_dump_list_expression(RCAST(const ListExpression *, expression), depth);
	case EXPRESSION_UNARY:     return tree_dump_unary_expression(RCAST(const UnaryExpression *, expression), depth);
	case EXPRESSION_BINARY:    return tree_dump_binary_expression(RCAST(const BinaryExpression *, expression), depth);
	case EXPRESSION_CAST:      return tree_dump_cast_expression(RCAST(const CastExpression *, expression), depth);
	case EXPRESSION_SELECTOR:  return tree_dump_selector_expression(RCAST(const SelectorExpression *, expression), depth);
	case EXPRESSION_CALL:      return tree_dump_call_expression(RCAST(const CallExpression *, expression), depth);
	case EXPRESSION_ASSERTION: return tree_dump_assertion_expression(RCAST(const AssertionExpression *, expression), depth);
	case EXPRESSION_VALUE:     return tree_dump_value_expression(RCAST(const ValueExpression *, expression), depth);
	case EXPRESSION_PROCEDURE: return tree_dump_procedure_expression(RCAST(const ProcedureExpression *, expression), depth);
	case EXPRESSION_TYPE:      return tree_dump_type_expression(RCAST(const TypeExpression *, expression), depth);
	}
	return false;
}

Bool tree_dump_statement(const Statement *statement, Sint32 depth);

Bool tree_dump_block_statement(const BlockStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(block\n");
	if (statement->flags) {
		const String flags = block_flags_to_string(statement->flags);
		pad(depth + 1);
		printf("(flags %.*s)", SFMT(flags));
	}
	const Uint64 n_statements = array_size(statement->statements);
	if (n_statements != 0) {
		printf("\n");
	}
	for (Uint64 i = 0; i < n_statements; i++) {
		const Statement *stmt = statement->statements[i];
		tree_dump_statement(stmt, depth + 1);
		if (i != n_statements - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool tree_dump_import_statement(const ImportStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(import '%.*s')", SFMT(statement->package));
	return true;
}

Bool tree_dump_expression_statement(const ExpressionStatement *statement, Sint32 depth) {
	return tree_dump_expression(statement->expression, depth);
}

Bool tree_dump_assignment_statement(const AssignmentStatement *statement, Sint32 depth) {
	pad(depth);
	const String assign = assignment_to_string(statement->assignment);
	printf("(assign '%.*s'\n", SFMT(assign));
	tree_dump_list_expression(statement->lhs, depth + 1);
	printf("\n");
	tree_dump_list_expression(statement->rhs, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_declaration_statement(const DeclarationStatement *statement, Sint32 depth) {
	const Uint64 n_values = array_size(statement->values->expressions);
	for (Uint64 i = 0; i < n_values; i++) {
		const Expression *value = statement->values->expressions[i];
		const Identifier *name = statement->names[i];
		pad(depth);
		printf("(decl '%.*s'\n", SFMT(name->contents));
		tree_dump_expression(value, depth + 1);
		printf(")");
		if (i != n_values - 1) {
			printf("\n");
		}
	}
	return true;
}

Bool tree_dump_if_statement(const IfStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(if");
	if (statement->init) {
		printf(" ");
		tree_dump_statement(statement->init, 0);
	}
	printf("\n");
	tree_dump_expression(statement->cond, depth + 1);
	printf("\n");
	tree_dump_block_statement(statement->body, depth + 1);
	if (statement->elif) {
		printf("\n");
		tree_dump_block_statement(statement->elif, depth + 1);
	}
	printf(")");
	return true;
}

Bool tree_dump_return_statement(const ReturnStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(return");
	const Uint64 n_results = array_size(statement->results);
	if (n_results != 0) {
		printf("\n");
	}
	for (Uint64 i = 0; i < n_results; i++) {
		const Expression *result = statement->results[i];
		tree_dump_expression(result, depth + 1);
		if (i != n_results - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool tree_dump_for_statement(const ForStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(for\n");
	if (statement->init) {
		tree_dump_statement(statement->init, depth + 1);
		printf("\n");
	}
	if (statement->cond) {
		tree_dump_expression(statement->cond, depth + 1);
		printf("\n");
	}
	if (statement->post) {
		tree_dump_statement(statement->post, depth + 1);
		printf("\n");
	}
	tree_dump_block_statement(statement->body, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_defer_statement(const DeferStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(defer\n");
	tree_dump_statement(statement->statement, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_branch_statement(const BranchStatement *statement, Sint32 depth) {
	(void)statement;
	pad(depth);
	const String branch = keyword_to_string(statement->branch);
	printf("(%.*s", SFMT(branch));
	if (statement->label) {
		printf(" '%.*s'", SFMT(statement->label->contents));
	}
	printf(")");
	return true;
}

Bool tree_dump_statement(const Statement *statement, Sint32 depth) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:       return false;
	case STATEMENT_BLOCK:       return tree_dump_block_statement(RCAST(const BlockStatement *, statement), depth);
	case STATEMENT_IMPORT:      return tree_dump_import_statement(RCAST(const ImportStatement *, statement), depth);
	case STATEMENT_EXPRESSION:  return tree_dump_expression_statement(RCAST(const ExpressionStatement *, statement), depth);
	case STATEMENT_ASSIGNMENT:  return tree_dump_assignment_statement(RCAST(const AssignmentStatement *, statement), depth);
	case STATEMENT_DECLARATION: return tree_dump_declaration_statement(RCAST(const DeclarationStatement *, statement), depth);
	case STATEMENT_IF:          return tree_dump_if_statement(RCAST(const IfStatement *, statement), depth);
	case STATEMENT_RETURN:      return tree_dump_return_statement(RCAST(const ReturnStatement *, statement), depth);
	case STATEMENT_FOR:         return tree_dump_for_statement(RCAST(const ForStatement *, statement), depth);
	case STATEMENT_DEFER:       return tree_dump_defer_statement(RCAST(const DeferStatement *, statement), depth);
	case STATEMENT_BRANCH:      return tree_dump_branch_statement(RCAST(const BranchStatement *, statement), depth);
	}
	return false;
}

void tree_dump(Tree *tree) {
	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		const Statement *statement = tree->statements[i];
		tree_dump_statement(statement, 0);
		printf("\n");
	}
}