#include <stdio.h>

#include "dump.h"

static void pad(Sint32 depth) {
	Sint32 dots = depth * 2 - 1;
	for (Sint32 i = 0; i < dots; i++) {
		printf(".");
	}
	if (dots > 0) printf(" ");
}

Bool dump_list_expression(const ListExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(list");
	const Size n_elements = array_size(expression->expressions);
	for (Size i = 0; i < n_elements; i++) {
		printf("\n");
		dump_expression(expression->expressions[i], depth + 1);
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
	printf("(cast\n");
	if (expression->type) {
		dump_type(expression->type, depth + 1);
		printf("\n");
	}
	dump_expression(expression->expression, depth + 1);
	printf(")");
	return true;
}

Bool dump_selector_expression(const SelectorExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(sel\n");
	if (expression->operand) {
		dump_expression(expression->operand, depth + 1);
		printf("\n");
	}
	dump_identifier(expression->identifier, depth + 1);
	printf(")");
	return true;
}

Bool dump_call_expression(const CallExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(call");
	printf("\n");
	dump_expression(expression->operand, depth + 1);
	const Size n_arguments = array_size(expression->arguments);
	for (Size i = 0; i < n_arguments; i++) {
		printf("\n");
		dump_expression(expression->arguments[i], depth + 1);
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

Bool dump_literal_expression(const LiteralExpression *expression, Sint32 depth) {
	pad(depth);
	const String kind = literal_to_string(expression->kind);
	printf("(lit '%.*s' %.*s)", SFMT(kind), SFMT(expression->value));
	return true;
}

Bool dump_compound_literal_expression(const CompoundLiteralExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(comp");
	if (expression->type) {
		printf("\n");
		dump_type(expression->type, depth + 1);
	}
	printf("\n");
	dump_fields(expression->fields, depth + 1);
	printf(")");
	return true;
}

Bool dump_identifier_expression(const IdentifierExpression *expression, Sint32 depth) {
	return dump_identifier(expression->identifier, depth);
}

Bool dump_undefined_expression(const UndefinedExpression *expression, Sint32 depth) {
	(void)expression;
	pad(depth);
	printf("(undef)");
	return true;
}

Bool dump_builtin_type(const BuiltinType *type, Sint32 depth) {
	pad(depth);
	printf("(builtin '%.*s')", SFMT(type->identifier));
	return true;
}

Bool dump_field(const Field *field, Sint32 depth) {
	pad(depth);
	printf("(field");
	if (field->flags) {
		printf("\n");
		pad(depth + 1);
		printf("(flags");
		printf("\n");
		pad(depth + 2);
		if (field->flags & FIELD_FLAG_ANY_INT) {
			printf("'#any_int'");
		}
		printf(")");
	}
	if (field->name) {
		printf("\n");
		dump_identifier(field->name, depth + 1);
	}
	if (field->type) {
		printf("\n");
		dump_type(field->type, depth + 1);
	}
	if (field->value) {
		printf("\n");
		dump_expression(field->value, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_fields(Array(Field*) const fields, Sint32 depth) {
	pad(depth);
	printf("(fields");
	const Size n_fields = array_size(fields);
	for (Size i = 0; i < n_fields; i++) {
		printf("\n");
		dump_field(fields[i], depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_procedure_type(const ProcedureType *type, Sint32 depth) {
	pad(depth);
	printf("(proc");
	printf("\n");
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
	printf("(ptr");
	printf("\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_multi_pointer_type(const MultiPointerType *type, Sint32 depth) {
	pad(depth);
	printf("(mptr");
	printf("\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_slice_type(const SliceType *type, Sint32 depth) {
	pad(depth);
	printf("(slice");
	printf("\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_array_type(const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(arr");
	if (type->count) {
		printf("\n");
		pad(depth + 1);
		printf("(count");
		printf("\n");
		dump_expression(type->count, depth + 2);
		printf(")");
	}
	printf("\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_dynamic_array_type(const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(dynarr");
	printf("\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_bit_set_type(const BitSetType *type, Sint32 depth) {
	pad(depth);
	printf("(bitset");
	printf("\n");
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
	printf("(map");
	printf("\n");
	dump_type(type->key, depth + 1);
	printf("\n");
	dump_type(type->value, depth + 1);
	printf(")");
	return true;
}

Bool dump_matrix_type(const MatrixType *type, Sint32 depth) {
	pad(depth);
	printf("(mat");
	printf("\n");
	dump_type(type->type, depth + 1);
	printf("\n");
	dump_expression(type->rows, depth + 1);
	printf("\n");
	dump_expression(type->columns, depth + 1);
	printf(")");
	return true;
}

Bool dump_distinct_type(const DistinctType *type, Sint32 depth) {
	pad(depth);
	printf("(distinct");
	printf("\n");
	dump_type(type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_enum_type(const EnumType *type, Sint32 depth) {
	pad(depth);
	printf("(enum");
	if (type->base_type) {
		printf("\n");
		dump_type(type->base_type, depth + 1);
	}
	printf("\n");
	dump_fields(type->fields, depth + 1);
	printf(")");
	return true;
}

Bool dump_expression_type(const ExpressionType *type, Sint32 depth) {
	return dump_expression(type->expression, depth);
}

Bool dump_struct_type(const StructType *type, Sint32 depth) {
	pad(depth);
	printf("(struct");
	if (type->align) {
		printf("\n");
		pad(depth + 1);
		printf("(align");
		printf("\n");
		dump_expression(type->align, depth + 2);
		printf(")\n");
	}
	printf("\n");
	dump_fields(type->fields, depth + 1);
	printf(")");
	return true;
}

Bool dump_union_type(const UnionType *type, Sint32 depth) {
	pad(depth);
	printf("(union");
	if (type->align) {
		printf("\n");
		pad(depth + 1);
		printf("(align");
		printf("\n");
		dump_expression(type->align, depth + 2);
		printf(")\n");
	}
	const Size n_variants = array_size(type->variants);
	for (Size i = 0; i < n_variants; i++) {
		printf("\n");
		dump_type(type->variants[i], depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_poly_type(const PolyType *type, Sint32 depth) {
	pad(depth);
	printf("(poly");
	printf("\n");
	dump_type(type->type, depth + 1);
	if (type->specialization) {
		printf("\n");
		dump_type(type->specialization, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_procedure_expression(const ProcedureExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(proc");
	printf("\n");
	dump_procedure_type(expression->type, depth + 1);
	if (expression->where_clauses) {
		printf("\n");
		pad(depth + 1);
		printf("(where");
		printf("\n");
		dump_list_expression(expression->where_clauses, depth + 2);
		printf(")");
	}
	printf("\n");
	dump_block_statement(expression->body, depth + 1);
	printf(")");
	return true;
}

Bool dump_type(const Type *type, Sint32 depth) {
	pad(depth);
	printf("(type");
	printf("\n");
	switch (type->kind) {
	       case TYPE_BUILTIN:       dump_builtin_type(RCAST(const BuiltinType *, type), depth + 1);
	break; case TYPE_PROCEDURE:     dump_procedure_type(RCAST(const ProcedureType *, type), depth + 1);
	break; case TYPE_POINTER:       dump_pointer_type(RCAST(const PointerType *, type), depth + 1);
	break; case TYPE_MULTI_POINTER: dump_multi_pointer_type(RCAST(const MultiPointerType *, type), depth + 1);
	break; case TYPE_SLICE:         dump_slice_type(RCAST(const SliceType *, type), depth + 1);
	break; case TYPE_ARRAY:         dump_array_type(RCAST(const ArrayType *, type), depth + 1);
	break; case TYPE_DYNAMIC_ARRAY: dump_dynamic_array_type(RCAST(const ArrayType *, type), depth + 1);
	break; case TYPE_BIT_SET:       dump_bit_set_type(RCAST(const BitSetType *, type), depth + 1);
	break; case TYPE_TYPEID:        dump_typeid_type(RCAST(const TypeidType *, type), depth + 1);
	break; case TYPE_MAP:           dump_map_type(RCAST(const MapType *, type), depth + 1);
	break; case TYPE_MATRIX:        dump_matrix_type(RCAST(const MatrixType *, type), depth + 1);
	break; case TYPE_DISTINCT:      dump_distinct_type(RCAST(const DistinctType *, type), depth + 1);
	break; case TYPE_ENUM:          dump_enum_type(RCAST(const EnumType *, type), depth + 1);
	break; case TYPE_EXPRESSION:    dump_expression_type(RCAST(const ExpressionType *, type), depth + 1);
	break; case TYPE_STRUCT:        dump_struct_type(RCAST(const StructType *, type), depth + 1);
	break; case TYPE_UNION:         dump_union_type(RCAST(const UnionType *, type), depth + 1);
	break; case TYPE_POLY:          dump_poly_type(RCAST(const PolyType *, type), depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_type_expression(const TypeExpression *expression, Sint32 depth) {
	return dump_type(expression->type, depth);
}

Bool dump_index_expression(const IndexExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(index");
	printf("\n");
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
	printf("(slice");
	printf("\n");
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
	pad(depth);
	printf("(expr");
	printf("\n");
	switch (expression->kind) {
	       case EXPRESSION_LIST:             dump_list_expression(RCAST(const ListExpression *, expression), depth + 1);
	break; case EXPRESSION_UNARY:            dump_unary_expression(RCAST(const UnaryExpression *, expression), depth + 1);
	break; case EXPRESSION_BINARY:           dump_binary_expression(RCAST(const BinaryExpression *, expression), depth + 1);
	break; case EXPRESSION_TERNARY:          dump_ternary_expression(RCAST(const TernaryExpression *, expression), depth + 1);
	break; case EXPRESSION_CAST:             dump_cast_expression(RCAST(const CastExpression *, expression), depth + 1);
	break; case EXPRESSION_SELECTOR:         dump_selector_expression(RCAST(const SelectorExpression *, expression), depth + 1);
	break; case EXPRESSION_CALL:             dump_call_expression(RCAST(const CallExpression *, expression), depth + 1);
	break; case EXPRESSION_ASSERTION:        dump_assertion_expression(RCAST(const AssertionExpression *, expression), depth + 1);
	break; case EXPRESSION_PROCEDURE:        dump_procedure_expression(RCAST(const ProcedureExpression *, expression), depth + 1);
	break; case EXPRESSION_TYPE:             dump_type_expression(RCAST(const TypeExpression *, expression), depth + 1);
	break; case EXPRESSION_INDEX:            dump_index_expression(RCAST(const IndexExpression *, expression), depth + 1);
	break; case EXPRESSION_SLICE:            dump_slice_expression(RCAST(const SliceExpression *, expression), depth + 1);
	break; case EXPRESSION_LITERAL:          dump_literal_expression(RCAST(const LiteralExpression *, expression), depth + 1);
	break; case EXPRESSION_COMPOUND_LITERAL: dump_compound_literal_expression(RCAST(const CompoundLiteralExpression *, expression), depth + 1);
	break; case EXPRESSION_IDENTIFIER:       dump_identifier_expression(RCAST(const IdentifierExpression *, expression), depth + 1);
	break; case EXPRESSION_UNDEFINED:        dump_undefined_expression(RCAST(const UndefinedExpression *, expression), depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_block_statement(const BlockStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(block");
	if (statement->flags) {
		printf("\n");
		const String flags = block_flags_to_string(statement->flags);
		pad(depth + 1);
		printf("(flags %.*s)", SFMT(flags));
	}
	const Size n_statements = array_size(statement->statements);
	for (Size i = 0; i < n_statements; i++) {
		printf("\n");
		dump_statement(statement->statements[i], depth + 1);
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
	printf("(assign '%.*s'", SFMT(assign));
	printf("\n");
	dump_list_expression(statement->lhs, depth + 1);
	printf("\n");
	dump_list_expression(statement->rhs, depth + 1);
	printf(")");
	return true;
}

Bool dump_declaration_statement(const DeclarationStatement *statement, Sint32 depth) {
	Array(Expression*) const values = statement->values->expressions;
	const Size n_names = array_size(statement->names);
	const Size n_values = array_size(values);
	for (Size i = 0; i < n_names; i++) {
		const Identifier *name = statement->names[i];
		pad(depth);
		printf("(decl");
		printf("\n");
		dump_identifier(name, depth + 1);
		if (statement->type) {
			printf("\n");
			dump_type(statement->type, depth + 1);
		}
		if (i < n_values) {
			printf("\n");
			dump_expression(values[i], depth + 1);
		}
		printf(")");
	}
	return true;
}

Bool dump_if_statement(const IfStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(if");
	if (statement->init) {
		printf("\n");
		dump_statement(statement->init, depth + 1);
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

Bool dump_when_statement(const WhenStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(when");
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
	const Size n_results = array_size(statement->results);
	for (Size i = 0; i < n_results; i++) {
		printf("\n");
		dump_expression(statement->results[i], depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_for_statement(const ForStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(for");
	if (statement->init) {
		printf("\n");
		dump_statement(statement->init, depth + 1);
	}
	if (statement->cond) {
		printf("\n");
		dump_expression(statement->cond, depth + 1);
	}
	if (statement->post) {
		printf("\n");
		dump_statement(statement->post, depth + 1);
	}
	printf("\n");
	dump_block_statement(statement->body, depth + 1);
	printf(")");
	return true;
}

Bool dump_defer_statement(const DeferStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(defer");
	printf("\n");
	dump_statement(statement->statement, depth + 1);
	printf(")");
	return true;
}

Bool dump_branch_statement(const BranchStatement *statement, Sint32 depth) {
	pad(depth);
	const String branch = keyword_to_string(statement->branch);
	printf("(%.*s", SFMT(branch));
	if (statement->label) {
		printf("\n");
		dump_identifier(statement->label, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_foreign_block_statement(const ForeignBlockStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(foreign");
	if (statement->name) {
		printf("\n");
		dump_identifier(statement->name, depth + 1);
	}
	printf("\n");
	dump_block_statement(statement->body, depth + 1);
	printf(")");
	return true;
}

Bool dump_statement(const Statement *statement, Sint32 depth) {
	pad(depth);
	printf("(stmt");
	printf("\n");
	switch (statement->kind) {
	       case STATEMENT_EMPTY:         // Nothing
	break; case STATEMENT_BLOCK:         dump_block_statement(RCAST(const BlockStatement *, statement), depth + 1);
	break; case STATEMENT_IMPORT:        dump_import_statement(RCAST(const ImportStatement *, statement), depth + 1);
	break; case STATEMENT_EXPRESSION:    dump_expression_statement(RCAST(const ExpressionStatement *, statement), depth + 1);
	break; case STATEMENT_ASSIGNMENT:    dump_assignment_statement(RCAST(const AssignmentStatement *, statement), depth + 1);
	break; case STATEMENT_DECLARATION:   dump_declaration_statement(RCAST(const DeclarationStatement *, statement), depth + 1);
	break; case STATEMENT_IF:            dump_if_statement(RCAST(const IfStatement *, statement), depth + 1);
	break; case STATEMENT_WHEN:          dump_when_statement(RCAST(const WhenStatement *, statement), depth + 1);
	break; case STATEMENT_RETURN:        dump_return_statement(RCAST(const ReturnStatement *, statement), depth + 1);
	break; case STATEMENT_FOR:           dump_for_statement(RCAST(const ForStatement *, statement), depth + 1);
	break; case STATEMENT_DEFER:         dump_defer_statement(RCAST(const DeferStatement *, statement), depth + 1);
	break; case STATEMENT_BRANCH:        dump_branch_statement(RCAST(const BranchStatement *, statement), depth + 1);
	break; case STATEMENT_FOREIGN_BLOCK: dump_foreign_block_statement(RCAST(const ForeignBlockStatement *, statement), depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_identifier(const Identifier *identifier, Sint32 depth) {
	pad(depth);
	const String ident = identifier->contents;
	printf("(ident '%.*s')", SFMT(ident));
	return true;
}

void dump(Tree *tree) {
	const Size n_statements = array_size(tree->statements);
	for (Size i = 0; i < n_statements; i++) {
		const Statement *statement = tree->statements[i];
		dump_statement(statement, 0);
		printf("\n");
	}
}