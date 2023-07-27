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

AssertionExpression *tree_new_assertion_expression(Tree *tree, Expression *operand, Identifier *type) {
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

IdentifierExpression *tree_new_identifier_expression(Tree *tree, Identifier *identifier) {
	Allocator *allocator = tree->context->allocator;
	IdentifierExpression *expression = CAST(IdentifierExpression *, allocator->allocate(allocator, sizeof *expression));
	expression->base.kind = EXPRESSION_IDENTIFIER;
	expression->identifier = identifier;
	return expression;
}

ProcedureExpression *tree_new_procedure_expression(Tree *tree, ProcedureFlag flags, ProcedureType *type, BlockStatement *body) {
	Allocator *allocator = tree->context->allocator;
	ProcedureExpression *procedure = CAST(ProcedureExpression*, allocator->allocate(allocator, sizeof *type));
	procedure->base.kind = EXPRESSION_PROCEDURE;
	procedure->body = body;
	procedure->flags = flags;
	procedure->type = type;
	return procedure;
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

DeclarationStatement *tree_new_declaration_statement(Tree *tree, Identifier *type, Array(Identifier*) names, ListExpression *values) {
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

BreakStatement *tree_new_break_statement(Tree *tree) {
	Allocator *allocator = tree->context->allocator;
	BreakStatement *statement = CAST(BreakStatement *, allocator->allocate(allocator, sizeof *statement));
	statement->base.kind = STATEMENT_BREAK;
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

ExpressionValue *tree_new_expression_value(Tree *tree, Expression *expression) {
	Allocator *allocator = tree->context->allocator;
	ExpressionValue *value = CAST(ExpressionValue *, allocator->allocate(allocator, sizeof *value));
	value->base.kind = VALUE_EXPRESSION;
	value->expression = expression;
	return value;
}

Identifier *tree_new_identifier(Tree *tree, String contents) {
	Context *context = tree->context;
	Allocator *allocator = context->allocator;
	Identifier *identifier = CAST(Identifier *, allocator->allocate(allocator, sizeof *identifier));
	identifier->contents = string_copy(contents);
	return identifier;
}

ProcedureType *tree_new_procedure_type(Tree *tree, void *params, void *results, Uint64 flags, CallingConvention convention) {
	Allocator *allocator = tree->context->allocator;
	ProcedureType *type = CAST(ProcedureType*, allocator->allocate(allocator, sizeof *type));
	type->convention = convention;
	type->flags = flags;
	return type;
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
	return false;
}

Bool tree_dump_value(const Value *value, Sint32 depth) {
	switch (value->kind) {
	case VALUE_LITERAL:           return tree_dump_literal_value(CAST(const LiteralValue *, value), depth);
	case VALUE_COMPOUND_LITERAL:  return tree_dump_compound_literal_value(CAST(const CompoundLiteralValue *, value), depth);
	case VALUE_EXPRESSION:        break; // return tree_dump_literal_value()
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
	return true;
}

Bool tree_dump_selector_expression(const SelectorExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(sel ");
	tree_dump_expression(expression->operand, 0);
	const String target = expression->identifier->contents;
	printf(" '%.*s'", SFMT(target));
	printf(")");
	return true;
}

Bool tree_dump_call_expression(const CallExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(call ");
	tree_dump_expression(expression->operand, 0);
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

Bool tree_dump_assertion_expression(const AssertionExpression *expression, Sint32 depth) {
	pad(depth);
	return true;
}

Bool tree_dump_value_expression(const ValueExpression *expression, Sint32 depth) {
	return tree_dump_value(expression->value, depth);
}

Bool tree_dump_identifier_expression(const IdentifierExpression *expression, Sint32 depth) {
	pad(depth);
	printf("'%.*s'", SFMT(expression->identifier->contents));
	return true;
}

Bool tree_dump_block_statement(const BlockStatement *statement, Sint32 depth);

String procedure_flags_to_string(ProcedureFlag flags, Context *context) {
	StrBuf buf;
	strbuf_init(&buf, context);
	Sint32 i = 0;
	if (flags & PROC_FLAG_DIVERGING) {
		if (i) strbuf_put_string(&buf, SCLIT(", "));
		strbuf_put_string(&buf, SCLIT("#diverging"));
		i++;
	}
	if (flags & PROC_FLAG_OPTIONAL_OK) {
		if (i) strbuf_put_string(&buf, SCLIT(", "));
		strbuf_put_string(&buf, SCLIT("#optional_ok"));
		i++;
	}
	if (flags & PROC_FLAG_OPTIONAL_ALLOCATION_ERROR) {
		if (i) strbuf_put_string(&buf, SCLIT(", "));
		strbuf_put_string(&buf, SCLIT("#optional_allocation_error"));
		i++;
	}
	if (flags & PROC_FLAG_BOUNDS_CHECK) {
		if (i) strbuf_put_string(&buf, SCLIT(", "));
		strbuf_put_string(&buf, SCLIT("#bounds_check"));
		i++;
	}
	if (flags & PROC_FLAG_TYPE_ASSERT) {
		if (i) strbuf_put_string(&buf, SCLIT(", "));
		strbuf_put_string(&buf, SCLIT("#type_assert"));
		i++;
	}
	if (flags & PROC_FLAG_FORCE_INLINE) {
		if (i) strbuf_put_string(&buf, SCLIT(", "));
		strbuf_put_string(&buf, SCLIT("#force_inline"));
		i++;
	}
	return strbuf_result(&buf);
}

Bool tree_dump_procedure_expression(const ProcedureExpression *expression, Sint32 depth) {
	// ProcedureFlag flags;
	// ProcedureType *type;
	// BlockStatement *body;
	pad(depth);
	printf("(proc");
	const String cc = calling_convention_to_string(expression->type->convention);
	printf(" '%.*s'", SFMT(cc));
	if (expression->flags) {
		Context context;
		context.allocator = &DEFAULT_ALLOCATOR;
		const String flags = procedure_flags_to_string(expression->flags, &context);
		printf(" '%.*s'", SFMT(flags));
	}
	printf("\n");
	tree_dump_block_statement(expression->body, depth + 1);
	printf(")");
	return true;
}

Bool tree_dump_expression(const Expression *expression, Sint32 depth) {
	switch (expression->kind) {
	case EXPRESSION_LIST:        return tree_dump_list_expression(CAST(const ListExpression *, expression), depth);
	case EXPRESSION_UNARY:       return tree_dump_unary_expression(CAST(const UnaryExpression *, expression), depth);
	case EXPRESSION_BINARY:      return tree_dump_binary_expression(CAST(const BinaryExpression *, expression), depth);
	case EXPRESSION_CAST:        return tree_dump_cast_expression(CAST(const CastExpression *, expression), depth);
	case EXPRESSION_SELECTOR:    return tree_dump_selector_expression(CAST(const SelectorExpression *, expression), depth);
	case EXPRESSION_CALL:        return tree_dump_call_expression(CAST(const CallExpression *, expression), depth);
	case EXPRESSION_ASSERTION:   return tree_dump_assertion_expression(CAST(const AssertionExpression *, expression), depth);
	case EXPRESSION_VALUE:       return tree_dump_value_expression(CAST(const ValueExpression *, expression), depth);
	case EXPRESSION_IDENTIFIER:  return tree_dump_identifier_expression(CAST(const IdentifierExpression *, expression), depth);
	case EXPRESSION_PROCEDURE:   return tree_dump_procedure_expression(CAST(const ProcedureExpression *, expression), depth);
	}
	return false;
}

Bool tree_dump_statement(const Statement *statement, Sint32 depth);

Bool tree_dump_empty_statement(const EmptyStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(empty)\n");
	return true;
}

Bool tree_dump_block_statement(const BlockStatement *statement, Sint32 depth) {
	pad(depth);
	const String flags = block_flags_to_string(statement->flags);
	printf("(block '%.*s'", SFMT(flags));
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
	return true;
}

Bool tree_dump_break_statement(const BreakStatement *statement, Sint32 depth) {
	pad(depth);
	return true;
}

Bool tree_dump_statement(const Statement *statement, Sint32 depth) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:       return tree_dump_empty_statement(CAST(const EmptyStatement *, statement), depth);
	case STATEMENT_BLOCK:       return tree_dump_block_statement(CAST(const BlockStatement *, statement), depth);
	case STATEMENT_IMPORT:      return tree_dump_import_statement(CAST(const ImportStatement *, statement), depth);
	case STATEMENT_EXPRESSION:  return tree_dump_expression_statement(CAST(const ExpressionStatement *, statement), depth);
	case STATEMENT_ASSIGNMENT:  return tree_dump_assignment_statement(CAST(const AssignmentStatement *, statement), depth);
	case STATEMENT_DECLARATION: return tree_dump_declaration_statement(CAST(const DeclarationStatement *, statement), depth);
	case STATEMENT_IF:          return tree_dump_if_statement(CAST(const IfStatement *, statement), depth);
	case STATEMENT_RETURN:      return tree_dump_return_statement(CAST(const ReturnStatement *, statement), depth);
	case STATEMENT_FOR:         return tree_dump_for_statement(CAST(const ForStatement *, statement), depth);
	case STATEMENT_DEFER:       return tree_dump_defer_statement(CAST(const DeferStatement *, statement), depth);
	case STATEMENT_BREAK:       return tree_dump_break_statement(CAST(const BreakStatement *, statement), depth);
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