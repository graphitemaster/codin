#include <stdio.h>

#include "dump.h"

static void pad(Sint32 depth) {
	Sint32 dots = depth * 2 - 1;
	for (Sint32 i = 0; i < dots; i++) {
		printf(".");
	}
	if (dots > 0) printf(" ");
}

Bool dump_literal_value(const LiteralValue *value, Sint32 depth) {
	pad(depth);
	const String kind = literal_to_string(value->kind);
	printf("(lit '%.*s' %.*s)", SFMT(kind), SFMT(value->input));
	return true;
}

Bool dump_compound_literal_value(const CompoundLiteralValue *value, Sint32 depth) {
	pad(depth);
	(void)value;
	return false;
}

Bool dump_identifier_value(const IdentifierValue *value, Sint32 depth) {
	pad(depth);
	printf("'(ident %.*s')", SFMT(value->identifier->contents));
	return true;
}

Bool dump_value(const Value *value, Sint32 depth) {
	switch (value->kind) {
	case VALUE_LITERAL:          return dump_literal_value(RCAST(const LiteralValue *, value), depth);
	case VALUE_COMPOUND_LITERAL: return dump_compound_literal_value(RCAST(const CompoundLiteralValue *, value), depth);
	case VALUE_IDENTIFIER:       return dump_identifier_value(RCAST(const IdentifierValue *, value), depth);
	}
	return false;
}

Bool dump_list_expression(const ListExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(list\n");
	const Uint64 n_elements = array_size(expression->expressions);
	for (Uint64 i = 0; i < n_elements; i++) {
		const Expression *expr = expression->expressions[i];
		dump_expression(expr, depth + 1);
		if (i != n_elements - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool dump_unary_expression(const UnaryExpression *expression, Sint32 depth) {
	const String op = operator_to_string(expression->operation);
	pad(depth);
	printf("(uop '%.*s'\n", SFMT(op));
	dump_expression(expression->operand, depth + 1);
	printf(")");
	return true;
}

Bool dump_binary_expression(const BinaryExpression *expression, Sint32 depth) {
	const String op = operator_to_string(expression->operation);
	pad(depth);
	printf("(bop '%.*s'\n", SFMT(op));
	dump_expression(expression->lhs, depth + 1);
	printf("\n");
	dump_expression(expression->rhs, depth + 1);
	printf(")");
	return true;
}

Bool dump_ternary_expression(const TernaryExpression *expression, Sint32 depth) {
	const String keyword = keyword_to_string(expression->operation);
	pad(depth);
	printf("(top '%.*s'\n", SFMT(keyword));
	dump_expression(expression->cond, depth + 1);
	printf("\n");
	dump_expression(expression->on_true, depth + 1);
	printf("\n");
	dump_expression(expression->on_false, depth + 1);
	return true;
}

Bool dump_cast_expression(const CastExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(cast");
	if (expression->type) {
		printf(" '%.*s'", SFMT(expression->type->contents));
	}
	dump_expression(expression->expression, depth + 1);
	printf(")");
	return true;
}

Bool dump_selector_expression(const SelectorExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(sel\n");
	dump_expression(expression->operand, depth + 1);
	printf("\n");
	const String target = expression->identifier->contents;
	pad(depth + 1);
	printf("'%.*s'", SFMT(target));
	printf(")");
	return true;
}

Bool dump_call_expression(const CallExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(call\n");
	dump_expression(expression->operand, depth + 1);
	const Uint64 n_arguments = array_size(expression->arguments);
	if (n_arguments != 0) {
		printf("\n");
	}
	for (Uint64 i = 0; i < n_arguments; i++) {
		const Expression *argument = expression->arguments[i];
		dump_expression(argument, depth + 1);
		if (i != n_arguments - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool dump_assertion_expression(const AssertionExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(assert-type\n");
	dump_type(expression->type, depth + 1);
	printf("\n");
	dump_expression(expression->operand, depth + 1);
	printf(")");
	return true;
}

Bool dump_value_expression(const ValueExpression *expression, Sint32 depth) {
	return dump_value(expression->value, depth);
}

Bool dump_identifier_type(const IdentifierType *type, Sint32 depth) {
	pad(depth);
	printf("(ident '%.*s')", SFMT(type->identifier->contents));
	return true;
}

Bool dump_builtin_type(const BuiltinType *type, Sint32 depth) {
	pad(depth);
	printf("(builtin '%.*s')", SFMT(type->identifier));
	return true;
}

Bool dump_fields(Array(Field*) const fields, Sint32 depth) {
	pad(depth);
	printf("(fields\n");
	pad(depth + 1);
	const Uint64 n_fields = array_size(fields);
	for (Uint64 i = 0; i < n_fields; i++) {
		const Field *field = fields[i];
		printf("(field\n");
		pad(depth + 1);
		printf("'%.*s'", SFMT(field->name->contents));
		if (field->type) {
			printf("\n");
			dump_type(field->type, depth + 1);
		}
		if (field->value) {
			printf("\n");
			dump_expression(field->value, depth + 1);
		}
		printf(")");
		if (i != n_fields - 1) {
			printf("\n");
			pad(depth);
		}
	}
	printf(")");
	return true;
}

Bool dump_procedure_type(const ProcedureType *type, Sint32 depth) {
	pad(depth);
	printf("(proc\n");
	pad(depth + 1);
	const String cc = calling_convention_to_string(type->convention);
	printf("(cc '%.*s')", SFMT(cc));
	if (type->params) {
		printf("\n");
		dump_fields(type->params, depth + 1);
	}
	if (type->results) {
		printf("\n");
		dump_fields(type->results, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_pointer_type(const PointerType *type, Sint32 depth) {
	pad(depth);
	printf("(ptr\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_multi_pointer_type(const MultiPointerType *type, Sint32 depth) {
	pad(depth);
	printf("(mptr\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_slice_type(const SliceType *type, Sint32 depth) {
	pad(depth);
	printf("(slice\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_array_type(const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(arr\n");
	if (type->count) {
		pad(depth + 1);
		printf("(count\n");
		if (type->count) {
			dump_expression(type->count, depth + 2);
		}
		printf(")");
		printf("\n");
	}
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_dynamic_array_type(const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(dynarr\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_bit_set_type(const BitSetType *type, Sint32 depth) {
	pad(depth);
	printf("(bitset\n");
	dump_expression(type->expression, depth + 1);
	if (type->underlying) {
		printf("\n");
		dump_type(type->underlying, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_typeid_type(const TypeidType *type, Sint32 depth) {
	(void)type;
	pad(depth);
	printf("(typeid)");
	return true;
}

Bool dump_map_type(const MapType *type, Sint32 depth) {
	pad(depth);
	printf("(map\n");
	dump_type(type->key, depth + 1);
	printf("\n");
	dump_type(type->value, depth + 1);
	printf(")");
	return true;
}

Bool dump_matrix_type(const MatrixType *type, Sint32 depth) {
	pad(depth);
	printf("(mat\n");
	dump_type(type->type, depth + 1);
	printf("\n");
	dump_expression(type->rows, depth + 1);
	printf("\n");
	dump_expression(type->columns, depth + 1);
	printf(")");
	return true;
}

Bool dump_procedure_expression(const ProcedureExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(proc\n");
	dump_procedure_type(expression->type, depth + 1);
	printf("\n");
	if (expression->where_clauses) {
		pad(depth + 1);
		printf("(where\n");
		dump_list_expression(expression->where_clauses, depth + 2);
		printf(")");
		printf("\n");
	}
	dump_block_statement(expression->body, depth + 1);
	printf(")");
	return true;
}

Bool dump_type(const Type *type, Sint32 depth) {
	switch (type->kind) {
	case TYPE_IDENTIFIER:    return dump_identifier_type(RCAST(const IdentifierType *, type), depth);
	case TYPE_BUILTIN:       return dump_builtin_type(RCAST(const BuiltinType *, type), depth);
	case TYPE_PROCEDURE:     return dump_procedure_type(RCAST(const ProcedureType *, type), depth);
	case TYPE_POINTER:       return dump_pointer_type(RCAST(const PointerType *, type), depth);
	case TYPE_MULTI_POINTER: return dump_multi_pointer_type(RCAST(const MultiPointerType *, type), depth);
	case TYPE_SLICE:         return dump_slice_type(RCAST(const SliceType *, type), depth);
	case TYPE_ARRAY:         return dump_array_type(RCAST(const ArrayType *, type), depth);
	case TYPE_DYNAMIC_ARRAY: return dump_dynamic_array_type(RCAST(const ArrayType *, type), depth);
	case TYPE_BIT_SET:       return dump_bit_set_type(RCAST(const BitSetType *, type), depth);
	case TYPE_TYPEID:        return dump_typeid_type(RCAST(const TypeidType *, type), depth);
	case TYPE_MAP:           return dump_map_type(RCAST(const MapType *, type), depth);
	case TYPE_MATRIX:        return dump_matrix_type(RCAST(const MatrixType *, type), depth);
	}
	return false;
}

Bool dump_type_expression(const TypeExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(type\n");
	dump_type(expression->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_index_expression(const IndexExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(idx\n");
	dump_expression(expression->operand, depth + 1);
	printf("\n");
	dump_expression(expression->lhs, depth + 1);
	if (expression->rhs) {
		printf("\n");
		dump_expression(expression->rhs, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_slice_expression(const SliceExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(slice\n");
	dump_expression(expression->operand, depth + 1);
	if (expression->lhs) {
		printf("\n");
		dump_expression(expression->lhs, depth + 1);
	}
	if (expression->rhs) {
		printf("\n");
		dump_expression(expression->rhs, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_expression(const Expression *expression, Sint32 depth) {
	switch (expression->kind) {
	case EXPRESSION_LIST:      return dump_list_expression(RCAST(const ListExpression *, expression), depth);
	case EXPRESSION_UNARY:     return dump_unary_expression(RCAST(const UnaryExpression *, expression), depth);
	case EXPRESSION_BINARY:    return dump_binary_expression(RCAST(const BinaryExpression *, expression), depth);
	case EXPRESSION_TERNARY:   return dump_ternary_expression(RCAST(const TernaryExpression *, expression), depth);
	case EXPRESSION_CAST:      return dump_cast_expression(RCAST(const CastExpression *, expression), depth);
	case EXPRESSION_SELECTOR:  return dump_selector_expression(RCAST(const SelectorExpression *, expression), depth);
	case EXPRESSION_CALL:      return dump_call_expression(RCAST(const CallExpression *, expression), depth);
	case EXPRESSION_ASSERTION: return dump_assertion_expression(RCAST(const AssertionExpression *, expression), depth);
	case EXPRESSION_VALUE:     return dump_value_expression(RCAST(const ValueExpression *, expression), depth);
	case EXPRESSION_PROCEDURE: return dump_procedure_expression(RCAST(const ProcedureExpression *, expression), depth);
	case EXPRESSION_TYPE:      return dump_type_expression(RCAST(const TypeExpression *, expression), depth);
	case EXPRESSION_INDEX:     return dump_index_expression(RCAST(const IndexExpression *, expression), depth);
	case EXPRESSION_SLICE:     return dump_slice_expression(RCAST(const SliceExpression *, expression), depth);
	}
	return false;
}

Bool dump_block_statement(const BlockStatement *statement, Sint32 depth) {
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
		dump_statement(stmt, depth + 1);
		if (i != n_statements - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool dump_import_statement(const ImportStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(import '%.*s')", SFMT(statement->package));
	return true;
}

Bool dump_expression_statement(const ExpressionStatement *statement, Sint32 depth) {
	return dump_expression(statement->expression, depth);
}

Bool dump_assignment_statement(const AssignmentStatement *statement, Sint32 depth) {
	pad(depth);
	const String assign = assignment_to_string(statement->assignment);
	printf("(assign '%.*s'\n", SFMT(assign));
	dump_list_expression(statement->lhs, depth + 1);
	printf("\n");
	dump_list_expression(statement->rhs, depth + 1);
	printf(")");
	return true;
}

Bool dump_declaration_statement(const DeclarationStatement *statement, Sint32 depth) {
	const Uint64 n_names = array_size(statement->names);
 	Array(Expression*) const values = statement->values ? statement->values->expressions : 0;
	for (Uint64 i = 0; i < n_names; i++) {
		const Identifier *name = statement->names[i];
		pad(depth);
		printf("(decl '%.*s'", SFMT(name->contents));
		if (statement->type) {
			printf("\n");
			dump_type(statement->type, depth + 1);
		}
		if (i < array_size(values)) {
			const Expression *value = values[i];
			printf("\n");
			dump_expression(value, depth + 1);
		}
		printf(")");
		if (i != n_names - 1) {
			printf("\n");
		}
	}
	return true;
}

Bool dump_if_statement(const IfStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(if");
	if (statement->init) {
		printf(" ");
		dump_statement(statement->init, 0);
	}
	printf("\n");
	dump_expression(statement->cond, depth + 1);
	printf("\n");
	dump_block_statement(statement->body, depth + 1);
	if (statement->elif) {
		printf("\n");
		dump_block_statement(statement->elif, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_return_statement(const ReturnStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(ret");
	const Uint64 n_results = array_size(statement->results);
	if (n_results != 0) {
		printf("\n");
	}
	for (Uint64 i = 0; i < n_results; i++) {
		const Expression *result = statement->results[i];
		dump_expression(result, depth + 1);
		if (i != n_results - 1) {
			printf("\n");
		}
	}
	printf(")");
	return true;
}

Bool dump_for_statement(const ForStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(for\n");
	if (statement->init) {
		dump_statement(statement->init, depth + 1);
		printf("\n");
	}
	if (statement->cond) {
		dump_expression(statement->cond, depth + 1);
		printf("\n");
	}
	if (statement->post) {
		dump_statement(statement->post, depth + 1);
		printf("\n");
	}
	dump_block_statement(statement->body, depth + 1);
	printf(")");
	return true;
}

Bool dump_defer_statement(const DeferStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(defer\n");
	dump_statement(statement->statement, depth + 1);
	printf(")");
	return true;
}

Bool dump_branch_statement(const BranchStatement *statement, Sint32 depth) {
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

Bool dump_statement(const Statement *statement, Sint32 depth) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:       return false;
	case STATEMENT_BLOCK:       return dump_block_statement(RCAST(const BlockStatement *, statement), depth);
	case STATEMENT_IMPORT:      return dump_import_statement(RCAST(const ImportStatement *, statement), depth);
	case STATEMENT_EXPRESSION:  return dump_expression_statement(RCAST(const ExpressionStatement *, statement), depth);
	case STATEMENT_ASSIGNMENT:  return dump_assignment_statement(RCAST(const AssignmentStatement *, statement), depth);
	case STATEMENT_DECLARATION: return dump_declaration_statement(RCAST(const DeclarationStatement *, statement), depth);
	case STATEMENT_IF:          return dump_if_statement(RCAST(const IfStatement *, statement), depth);
	case STATEMENT_RETURN:      return dump_return_statement(RCAST(const ReturnStatement *, statement), depth);
	case STATEMENT_FOR:         return dump_for_statement(RCAST(const ForStatement *, statement), depth);
	case STATEMENT_DEFER:       return dump_defer_statement(RCAST(const DeferStatement *, statement), depth);
	case STATEMENT_BRANCH:      return dump_branch_statement(RCAST(const BranchStatement *, statement), depth);
	}
	return false;
}

void dump(Tree *tree) {
	const Uint64 n_statements = array_size(tree->statements);
	for (Uint64 i = 0; i < n_statements; i++) {
		const Statement *statement = tree->statements[i];
		dump_statement(statement, 0);
		printf("\n");
	}
}