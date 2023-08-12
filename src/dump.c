#include <stdio.h>

#include "dump.h"

static void pad(Sint32 depth) {
	Sint32 dots = depth * 2 - 1;
	for (Sint32 i = 0; i < dots; i++) {
		printf(".");
	}
	if (dots > 0) printf(" ");
}

Bool dump_list_expression(const Tree *tree, const ListExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(list");
	printf("\n");

	pad(depth + 1);
	printf("(expressions");
	const Size n_elements = array_size(expression->expressions);
	for (Size i = 0; i < n_elements; i++) {
		printf("\n");
		dump_expression(tree, expression->expressions[i], depth + 2);
	}
	printf(")"); // expressions

	printf(")"); // list
	return true;
}

Bool dump_unary_expression(const Tree *tree, const UnaryExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(unary");
	printf("\n");

	pad(depth + 1);
	const String op = operator_to_string(expression->operation);
	printf("(operation '%.*s')", SFMT(op));
	printf("\n");

	pad(depth + 1);
	printf("(operands");
	printf("\n");
	dump_expression(tree, expression->operand, depth + 2);
	printf(")"); // operands

	printf(")"); // uop
	return true;
}

Bool dump_binary_expression(const Tree *tree, const BinaryExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(binary");
	printf("\n");
	
	pad(depth + 1);
	const String op = operator_to_string(expression->operation);
	printf("(operation '%.*s')", SFMT(op));
	printf("\n");

	pad(depth + 1);
	printf("(lhs");
	printf("\n");
	dump_expression(tree, expression->lhs, depth + 2);
	printf(")"); // lhs

	printf("\n");

	pad(depth + 1);
	printf("(rhs");
	printf("\n");
	dump_expression(tree, expression->rhs, depth + 2);
	printf(")"); // rhs

	printf(")"); // bop
	return true;
}

Bool dump_ternary_expression(const Tree *tree, const TernaryExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(ternary");
	printf("\n");

	pad(depth + 1);
	const String keyword = keyword_to_string(expression->operation);
	printf("(operation '%.*s')", SFMT(keyword));

	printf("\n");
	pad(depth + 1);
	printf("(cond");
	printf("\n");
	dump_expression(tree, expression->cond, depth + 2);
	printf(")"); // cond

	printf("\n");
	pad(depth + 1);
	printf("(true");
	printf("\n");
	dump_expression(tree, expression->on_true, depth + 2);
	printf(")"); // true

	printf("\n");
	pad(depth + 1);
	printf("\n");
	dump_expression(tree, expression->on_false, depth + 2);
	printf(")"); // false

	printf(")"); // top
	return true;
}

Bool dump_cast_expression(const Tree *tree, const CastExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(cast");
	if (expression->type) {
		printf("\n");
		pad(depth + 1);
		printf("(type");
		printf("\n");
		dump_type(tree, expression->type, depth + 2);
		printf(")"); // type
	}

	printf("\n");
	pad(depth + 1);
	printf("(expression");
	printf("\n");
	dump_expression(tree, expression->expression, depth + 2);
	printf(")"); // expression

	printf(")"); // cast
	return true;
}

Bool dump_selector_expression(const Tree *tree, const SelectorExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(select");
	if (expression->operand) {
		printf("\n");
		pad(depth + 1);
		printf("(operand");
		printf("\n");
		dump_expression(tree, expression->operand, depth + 2);
		printf(")"); // operand
	}
	printf("\n");
	dump_identifier(tree, expression->identifier, depth + 1);
	printf(")"); // select
	return true;
}

Bool dump_call_expression(const Tree *tree, const CallExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(call");

	printf("\n");
	pad(depth + 1);
	printf("(operand");
	printf("\n");
	dump_expression(tree, expression->operand, depth + 2);
	printf(")"); // operand

	printf("\n");
	pad(depth + 1);
	printf("(arguments");
	printf("\n");
	dump_fields(tree, expression->arguments, depth + 2);
	printf(")"); // arguments

	printf(")"); // call
	return true;
}

Bool dump_assertion_expression(const Tree *tree, const AssertionExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(assert");
	if (expression->type) {
		printf("\n");
		pad(depth + 1);
		printf("(type");
		printf("\n");
		dump_type(tree, expression->type, depth + 2);
		printf(")"); // type
	}

	printf("\n");
	pad(depth + 1);
	printf("(operand");
	printf("\n");
	dump_expression(tree, expression->operand, depth + 2);
	printf(")"); // operand

	printf(")"); // assert
	return true;
}

Bool dump_literal_expression(const Tree *tree, const LiteralExpression *expression, Sint32 depth) {
	(void)tree;
	pad(depth);
	const String kind = literal_to_string(expression->kind);
	printf("(literal '%.*s' %.*s)", SFMT(kind), SFMT(expression->value));
	return true;
}

Bool dump_compound_literal_expression(const Tree *tree, const CompoundLiteralExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(compound");
	if (expression->type) {
		printf("\n");
		pad(depth + 1);
		printf("(type");
		printf("\n");
		dump_type(tree, expression->type, depth + 2);
		printf(")"); // type
	}

	if (array_size(expression->fields)) {
		printf("\n");
		pad(depth + 1);
		printf("(fields");
		printf("\n");
		dump_fields(tree, expression->fields, depth + 2);
		printf(")"); // fields
	}

	printf(")"); // compound
	return true;
}

Bool dump_identifier_expression(const Tree *tree, const IdentifierExpression *expression, Sint32 depth) {
	return dump_identifier(tree, expression->identifier, depth);
}

Bool dump_undefined_expression(const Tree *tree, const UndefinedExpression *expression, Sint32 depth) {
	(void)tree;
	(void)expression;
	pad(depth);
	printf("(undef)");
	return true;
}

Bool dump_procedure_group_expression(const Tree *tree, const ProcedureGroupExpression *expression, Sint32 depth) {
	printf("(procgroup");
	const Size n_procs = array_size(expression->expressions);
	for (Size i = 0; i < n_procs; i++) {
		printf("\n");
		dump_expression(tree, expression->expressions[i], depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_builtin_type(const Tree *tree, const BuiltinType *type, Sint32 depth) {
	(void)tree;
	pad(depth);
	printf("(builtin '%.*s')", SFMT(type->identifier));
	return true;
}

Bool dump_field(const Tree *tree, const Field *field, Sint32 depth) {
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
		printf(")"); // flags
	}
	if (field->name) {
		printf("\n");
		dump_identifier(tree, field->name, depth + 1);
	}
	if (field->type) {
		printf("\n");
		dump_type(tree, field->type, depth + 1);
	}
	if (field->value) {
		printf("\n");
		dump_expression(tree, field->value, depth + 1);
	}
	printf(")"); // field
	return true;
}

Bool dump_fields(const Tree *tree, Array(Field*) const fields, Sint32 depth) {
	const Size n_fields = array_size(fields);
	for (Size i = 0; i < n_fields; i++) {
		dump_field(tree, fields[i], depth);
		if (i != n_fields - 1) {
			printf("\n");
		}
	}
	return true;
}

Bool dump_procedure_type(const Tree *tree, const ProcedureType *type, Sint32 depth) {
	pad(depth);
	printf("(proc");

	printf("\n");
	pad(depth + 1);
	const String cc = calling_convention_to_string(type->convention);
	printf("(convention '%.*s')", SFMT(cc));

	if (array_size(type->params) != 0) {
		printf("\n");
		pad(depth + 1);
		printf("(params");
		printf("\n");
		dump_fields(tree, type->params, depth + 2);
		printf(")"); // params
	}

	if (array_size(type->results) != 0) {
		printf("\n");
		pad(depth + 1);
		printf("(results");
		printf("\n");
		dump_fields(tree, type->results, depth + 2);
		printf(")"); // results
	}

	printf(")"); // proc

	return true;
}

Bool dump_pointer_type(const Tree *tree, const PointerType *type, Sint32 depth) {
	pad(depth);
	printf("(pointer");
	printf("\n");
	dump_type(tree, type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_multi_pointer_type(const Tree *tree, const MultiPointerType *type, Sint32 depth) {
	pad(depth);
	printf("(multi-pointer");
	printf("\n");
	dump_type(tree, type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_slice_type(const Tree *tree, const SliceType *type, Sint32 depth) {
	pad(depth);
	printf("(slice");
	printf("\n");
	dump_type(tree, type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_array_type(const Tree *tree, const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(array");
	if (type->count) {
		printf("\n");
		pad(depth + 1);
		printf("(count");
		printf("\n");
		dump_expression(tree, type->count, depth + 2);
		printf(")"); // count
	}
	printf("\n");
	dump_type(tree, type->type, depth + 1);
	printf(")"); // arr
	return true;
}

Bool dump_dynamic_array_type(const Tree *tree, const ArrayType *type, Sint32 depth) {
	pad(depth);
	printf("(dynamic-array");
	printf("\n");
	dump_type(tree, type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_bit_set_type(const Tree *tree, const BitSetType *type, Sint32 depth) {
	pad(depth);
	printf("(bit-set");
	printf("\n");
	dump_expression(tree, type->expression, depth + 1);
	if (type->underlying) {
		printf("\n");
		dump_type(tree, type->underlying, depth + 1);
	}
	printf(")");
	return true;
}

Bool dump_typeid_type(const Tree *tree, const TypeidType *type, Sint32 depth) {
	(void)tree;
	(void)type;
	pad(depth);
	printf("(typeid)");
	return true;
}

Bool dump_map_type(const Tree *tree, const MapType *type, Sint32 depth) {
	pad(depth);
	printf("(map");

	printf("\n");
	pad(depth + 1);
	printf("(key");
	printf("\n");
	dump_type(tree, type->key, depth + 2);
	printf(")"); // key

	printf("\n");
	pad(depth + 1);
	printf("(val");
	printf("\n");
	dump_type(tree, type->value, depth + 2);
	printf(")"); // val

	printf(")"); // map

	return true;
}

Bool dump_matrix_type(const Tree *tree, const MatrixType *type, Sint32 depth) {
	pad(depth);
	printf("(matrix");
	printf("\n");
	pad(depth + 1);
	printf("(type");
	printf("\n");
	dump_type(tree, type->type, depth + 2);
	printf(")"); // type

	printf("\n");
	pad(depth + 1);
	printf("(rows");
	printf("\n");
	dump_expression(tree, type->rows, depth + 2);
	printf(")"); // rows

	printf("\n");
	pad(depth + 1);
	printf("(cols");
	printf("\n");
	dump_expression(tree, type->columns, depth + 2);
	printf(")"); // cols

	printf(")"); // mat

	return true;
}

Bool dump_distinct_type(const Tree *tree, const DistinctType *type, Sint32 depth) {
	pad(depth);
	printf("(distinct");
	printf("\n");
	dump_type(tree, type->type, depth + 1);
	printf(")");
	return true;
}

Bool dump_enum_type(const Tree *tree, const EnumType *type, Sint32 depth) {
	pad(depth);
	printf("(enum");
	if (type->type) {
		printf("\n");
		pad(depth + 1);
		printf("(type");
		printf("\n");
		dump_type(tree, type->type, depth + 1);
		printf(")"); // type
	}
	printf("\n");
	dump_fields(tree, type->fields, depth + 1);
	printf(")"); // enum
	return true;
}

Bool dump_expression_type(const Tree *tree, const ExpressionType *type, Sint32 depth) {
	return dump_expression(tree, type->expression, depth);
}

Bool dump_struct_type(const Tree *tree, const StructType *type, Sint32 depth) {
	pad(depth);
	printf("(struct");
	if (type->align) {
		printf("\n");
		pad(depth + 1);
		printf("(align");
		printf("\n");
		dump_expression(tree, type->align, depth + 2);
		printf(")"); // align
	}

	if (array_size(type->fields)) {
		printf("\n");
		pad(depth + 1);
		printf("(fields");
		printf("\n");
		dump_fields(tree, type->fields, depth + 2);
		printf(")"); // fields
	}

	printf(")"); // struct

	return true;
}

Bool dump_union_type(const Tree *tree, const UnionType *type, Sint32 depth) {
	pad(depth);
	printf("(union");
	if (type->align) {
		printf("\n");
		pad(depth + 1);
		printf("(align");
		printf("\n");
		dump_expression(tree, type->align, depth + 2);
		printf(")"); // align
	}

	printf("\n");
	pad(depth + 1);
	printf("(variants");
	const Size n_variants = array_size(type->variants);
	for (Size i = 0; i < n_variants; i++) {
		printf("\n");
		dump_type(tree, type->variants[i], depth + 2);
	}
	printf("\n");
	printf(")"); // variants

	printf(")"); // union

	return true;
}

Bool dump_poly_type(const Tree *tree, const PolyType *type, Sint32 depth) {
	pad(depth);
	printf("(poly");

	printf("\n");
	pad(depth + 1);
	printf("(type");
	printf("\n");
	dump_type(tree, type->type, depth + 2);
	printf(")"); // type

	if (type->specialization) {
		printf("\n");
		printf("(special");
		printf("\n");
		dump_type(tree, type->specialization, depth + 2);
		printf(")"); // special
	}
	printf(")"); // poly

	return true;
}

Bool dump_procedure_expression(const Tree *tree, const ProcedureExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(proc");

	printf("\n");
	pad(depth + 1);
	printf("(type");
	printf("\n");
	dump_procedure_type(tree, expression->type, depth + 2);
	printf(")"); // type

	if (expression->where_clauses) {
		printf("\n");
		pad(depth + 1);
		printf("(where");
		printf("\n");
		dump_list_expression(tree, expression->where_clauses, depth + 2);
		printf(")"); // where
	}

	if (expression->body) {
		printf("\n");
		pad(depth + 1);
		printf("(body");
		printf("\n");
		dump_block_statement(tree, expression->body, depth + 2);
		printf(")"); // body
	}

	printf(")"); // proc

	return true;
}

Bool dump_type(const Tree *tree, const Type *type, Sint32 depth) {
	switch (type->kind) {
	case TYPE_BUILTIN:       return dump_builtin_type(tree, RCAST(const BuiltinType *, type), depth);
	case TYPE_PROCEDURE:     return dump_procedure_type(tree, RCAST(const ProcedureType *, type), depth);
	case TYPE_POINTER:       return dump_pointer_type(tree, RCAST(const PointerType *, type), depth);
	case TYPE_MULTI_POINTER: return dump_multi_pointer_type(tree, RCAST(const MultiPointerType *, type), depth);
	case TYPE_SLICE:         return dump_slice_type(tree, RCAST(const SliceType *, type), depth);
	case TYPE_ARRAY:         return dump_array_type(tree, RCAST(const ArrayType *, type), depth);
	case TYPE_DYNAMIC_ARRAY: return dump_dynamic_array_type(tree, RCAST(const ArrayType *, type), depth);
	case TYPE_BIT_SET:       return dump_bit_set_type(tree, RCAST(const BitSetType *, type), depth);
	case TYPE_TYPEID:        return dump_typeid_type(tree, RCAST(const TypeidType *, type), depth);
	case TYPE_MAP:           return dump_map_type(tree, RCAST(const MapType *, type), depth);
	case TYPE_MATRIX:        return dump_matrix_type(tree, RCAST(const MatrixType *, type), depth);
	case TYPE_DISTINCT:      return dump_distinct_type(tree, RCAST(const DistinctType *, type), depth);
	case TYPE_ENUM:          return dump_enum_type(tree, RCAST(const EnumType *, type), depth);
	case TYPE_EXPRESSION:    return dump_expression_type(tree, RCAST(const ExpressionType *, type), depth);
	case TYPE_STRUCT:        return dump_struct_type(tree, RCAST(const StructType *, type), depth);
	case TYPE_UNION:         return dump_union_type(tree, RCAST(const UnionType *, type), depth);
	case TYPE_POLY:          return dump_poly_type(tree, RCAST(const PolyType *, type), depth);
	}
	UNREACHABLE();
}

Bool dump_type_expression(const Tree *tree, const TypeExpression *expression, Sint32 depth) {
	return dump_type(tree, expression->type, depth);
}

Bool dump_index_expression(const Tree *tree, const IndexExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(index");

	printf("\n");
	pad(depth + 1);
	printf("(operand");
	printf("\n");
	dump_expression(tree, expression->operand, depth + 2);
	printf(")"); // operand

	printf("\n");
	pad(depth + 1);
	printf("(lhs");
	printf("\n");
	dump_expression(tree, expression->lhs, depth + 2);
	printf(")"); // lhs

	if (expression->rhs) {
		printf("\n");
		pad(depth + 1);
		printf("(rhs");
		printf("\n");
		dump_expression(tree, expression->rhs, depth + 2);
		printf(")"); // rhs
	}

	printf(")"); // index

	return true;
}

Bool dump_slice_expression(const Tree *tree, const SliceExpression *expression, Sint32 depth) {
	pad(depth);
	printf("(slice");

	printf("\n");
	pad(depth + 1);
	printf("(operand");
	printf("\n");
	dump_expression(tree, expression->operand, depth + 2);
	printf(")"); // operand

	if (expression->lhs) {
		printf("\n");
		pad(depth + 1);
		printf("(lhs");
		printf("\n");
		dump_expression(tree, expression->lhs, depth + 2);
		printf(")"); // lhs
	}

	if (expression->rhs) {
		printf("\n");
		pad(depth + 1);
		printf("(rhs");
		printf("\n");
		dump_expression(tree, expression->rhs, depth + 2);
		printf(")"); // rhs
	}

	printf(")"); // slice

	return true;
}

Bool dump_expression(const Tree *tree, const Expression *expression, Sint32 depth) {
	switch (expression->kind) {
	case EXPRESSION_LIST:             return dump_list_expression(tree, RCAST(const ListExpression *, expression), depth);
	case EXPRESSION_UNARY:            return dump_unary_expression(tree, RCAST(const UnaryExpression *, expression), depth);
	case EXPRESSION_BINARY:           return dump_binary_expression(tree, RCAST(const BinaryExpression *, expression), depth);
	case EXPRESSION_TERNARY:          return dump_ternary_expression(tree, RCAST(const TernaryExpression *, expression), depth);
	case EXPRESSION_CAST:             return dump_cast_expression(tree, RCAST(const CastExpression *, expression), depth);
	case EXPRESSION_SELECTOR:         return dump_selector_expression(tree, RCAST(const SelectorExpression *, expression), depth);
	case EXPRESSION_CALL:             return dump_call_expression(tree, RCAST(const CallExpression *, expression), depth);
	case EXPRESSION_ASSERTION:        return dump_assertion_expression(tree, RCAST(const AssertionExpression *, expression), depth);
	case EXPRESSION_PROCEDURE:        return dump_procedure_expression(tree, RCAST(const ProcedureExpression *, expression), depth);
	case EXPRESSION_TYPE:             return dump_type_expression(tree, RCAST(const TypeExpression *, expression), depth);
	case EXPRESSION_INDEX:            return dump_index_expression(tree, RCAST(const IndexExpression *, expression), depth);
	case EXPRESSION_SLICE:            return dump_slice_expression(tree, RCAST(const SliceExpression *, expression), depth);
	case EXPRESSION_LITERAL:          return dump_literal_expression(tree, RCAST(const LiteralExpression *, expression), depth);
	case EXPRESSION_COMPOUND_LITERAL: return dump_compound_literal_expression(tree, RCAST(const CompoundLiteralExpression *, expression), depth);
	case EXPRESSION_IDENTIFIER:       return dump_identifier_expression(tree, RCAST(const IdentifierExpression *, expression), depth);
	case EXPRESSION_UNDEFINED:        return dump_undefined_expression(tree, RCAST(const UndefinedExpression *, expression), depth);
	case EXPRESSION_PROCEDURE_GROUP:  return dump_procedure_group_expression(tree, RCAST(const ProcedureGroupExpression *, expression), depth);
	}
	UNREACHABLE();
}

Bool dump_block_statement(const Tree *tree, const BlockStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(block");
	if (statement->flags) {
		printf("\n");
		pad(depth + 1);
		printf("(flags");
		if (statement->flags & BLOCK_FLAG_BOUNDS_CHECK) {
			printf("\n");
			pad(depth + 2);
			printf("'#bounds_check'");
		}
		if (statement->flags & BLOCK_FLAG_TYPE_ASSERT) {
			printf("\n");
			pad(depth + 2);
			printf("'#type_assert'");
		}
		printf(")"); // flags
	}

	printf("\n");
	pad(depth + 1);
	printf("(statements");
	const Size n_statements = array_size(statement->statements);
	for (Size i = 0; i < n_statements; i++) {
		printf("\n");
		dump_statement(tree, statement->statements[i], depth + 2);
	}
	printf(")"); // statements

	printf(")"); // blockl
	return true;
}

Bool dump_import_statement(const Tree *tree, const ImportStatement *statement, Sint32 depth) {
	(void)tree;
	pad(depth);
	printf("(import '%.*s')", SFMT(statement->package));
	return true;
}

Bool dump_expression_statement(const Tree *tree, const ExpressionStatement *statement, Sint32 depth) {
	return dump_expression(tree, statement->expression, depth);
}

Bool dump_assignment_statement(const Tree *tree, const AssignmentStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(assign");

	printf("\n");
	pad(depth + 1);
	const String assign = assignment_to_string(statement->assignment);
	printf("(kind '%.*s')", SFMT(assign));

	printf("\n");
	pad(depth + 1);
	printf("(lhs");
	printf("\n");
	dump_list_expression(tree, statement->lhs, depth + 2);
	printf(")"); // lhs

	printf("\n");
	pad(depth + 1);
	printf("(rhs");
	printf("\n");
	dump_list_expression(tree, statement->rhs, depth + 2);
	printf(")"); // rhs

	printf(")"); // assign
	return true;
}

Bool dump_declaration_statement(const Tree *tree, const DeclarationStatement *statement, Sint32 depth) {
	Array(Expression*) const values = statement->values->expressions;
	const Size n_names = array_size(statement->names);
	const Size n_values = array_size(values);

	pad(depth);
	printf("(decl");
	printf("\n");

	pad(depth + 1);
	printf("(names");
	for (Size i = 0; i < n_names; i++) {
		printf("\n");
		dump_identifier(tree, statement->names[i], depth + 2);
	}
	printf("\n");

	pad(depth + 1);
	printf("(values");
	for (Size i = 0; i < n_values; i++) {
		printf("\n");
		dump_expression(tree, values[i], depth + 2);
	}
	printf(")");

	return true;
}

Bool dump_if_statement(const Tree *tree, const IfStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(if");

	if (statement->init) {
		printf("\n");
		pad(depth + 1);
		printf("(init");
		printf("\n");
		dump_statement(tree, statement->init, depth + 2);
		printf(")"); // init
	}

	printf("\n");
	pad(depth + 1);
	printf("(cond");
	printf("\n");
	dump_expression(tree, statement->cond, depth + 2);
	printf(")"); // cond

	printf("\n");
	pad(depth + 1);
	printf("(body");
	printf("\n");
	dump_block_statement(tree, statement->body, depth + 2);
	printf(")"); // body

	if (statement->elif) {
		printf("\n");
		pad(depth + 1);
		printf("(elif");
		printf("\n");
		dump_block_statement(tree, statement->elif, depth + 2);
		printf(")"); // elif
	}

	printf(")"); // if

	return true;
}

Bool dump_when_statement(const Tree *tree, const WhenStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(when");

	printf("\n");
	pad(depth + 1);
	printf("(cond");
	printf("\n");
	dump_expression(tree, statement->cond, depth + 2);
	printf(")"); // cond

	printf("\n");
	pad(depth + 1);
	printf("(body");
	printf("\n");
	dump_block_statement(tree, statement->body, depth + 2);
	printf(")"); // body

	if (statement->elif) {
		printf("\n");
		pad(depth + 1);
		printf("(elif");
		printf("\n");
		dump_block_statement(tree, statement->elif, depth + 2);
		printf(")"); // elif
	}

	printf(")"); // when

	return true;
}

Bool dump_return_statement(const Tree *tree, const ReturnStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(return");

	printf("\n");
	pad(depth + 1);
	printf("(results");
	const Size n_results = array_size(statement->results);
	for (Size i = 0; i < n_results; i++) {
		printf("\n");
		dump_expression(tree, statement->results[i], depth + 2);
	}
	printf(")"); // results

	printf(")"); // ret

	return true;
}

Bool dump_for_statement(const Tree *tree, const ForStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(for");

	if (statement->init) {
		printf("\n");
		pad(depth + 1);
		printf("(init");
		printf("\n");
		dump_statement(tree, statement->init, depth + 2);
		printf(")"); // init
	}

	if (statement->cond) {
		printf("\n");
		pad(depth + 1);
		printf("(cond");
		printf("\n");
		dump_expression(tree, statement->cond, depth + 2);
		printf(")"); // cond
	}

	if (statement->post) {
		printf("\n");
		pad(depth + 1);
		printf("(post");
		printf("\n");
		dump_statement(tree, statement->post, depth + 2);
		printf(")"); // post
	}

	printf("\n");
	pad(depth + 1);
	printf("(body");
	printf("\n");
	dump_block_statement(tree, statement->body, depth + 2);
	printf(")"); // body

	printf(")");

	return true;
}

static void dump_case_clause(const Tree *tree, const CaseClause *clause, Sint32 depth) {
	pad(depth);
	printf("(case");
	if (clause->expressions) {
		printf("\n");
		dump_list_expression(tree, clause->expressions, depth + 1);
	}
	printf("\n");
	pad(depth + 1);
	printf("(statements");
	const Size n_statements = array_size(clause->statements);
	for (Size i = 0; i < n_statements; i++) {
		printf("\n");
		dump_statement(tree, clause->statements[i], depth + 2);
	}
	printf(")"); // statements

	printf(")"); // case
}

Bool dump_switch_statement(const Tree *tree, const SwitchStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(switch");

	if (statement->init) {
		printf("\n");
		pad(depth + 1);
		printf("(init");
		printf("\n");
		dump_statement(tree, statement->init, depth + 2);
		printf(")"); // init
	}

	if (statement->cond) {
		printf("\n");
		pad(depth + 1);
		printf("(cond");
		printf("\n");
		dump_expression(tree, statement->cond, depth + 2);
		printf(")"); // cond
	}

	const Size n_clauses = array_size(statement->clauses);
	for (Size i = 0; i < n_clauses; i++) {
		printf("\n");
		dump_case_clause(tree, statement->clauses[i], depth + 1);
	}
	printf(")"); // switch

	return true;
}

Bool dump_defer_statement(const Tree *tree, const DeferStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(defer");

	printf("\n");
	pad(depth + 1);
	printf("(statement");
	printf("\n");
	dump_statement(tree, statement->statement, depth + 2);
	printf(")"); // statement

	printf(")"); // defer

	return true;
}

Bool dump_branch_statement(const Tree *tree, const BranchStatement *statement, Sint32 depth) {
	pad(depth);
	const String branch = keyword_to_string(statement->branch);
	printf("(%.*s", SFMT(branch));
	if (statement->label) {
		printf("\n");
		pad(depth + 1);
		printf("(label");
		printf("\n");
		dump_identifier(tree, statement->label, depth + 2);
		printf(")"); // label
	}
	printf(")");
	return true;
}

Bool dump_foreign_block_statement(const Tree *tree, const ForeignBlockStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(foreign");
	if (statement->name) {
		printf("\n");
		pad(depth + 1);
		printf("(name");
		printf("\n");
		dump_identifier(tree, statement->name, depth + 2);
		printf(")"); // name
	}

	printf("\n");
	pad(depth + 1);
	printf("(body");
	printf("\n");
	dump_block_statement(tree, statement->body, depth + 2);
	printf(")"); // body

	printf(")"); // foreign

	return true;
}

Bool dump_foreign_import_statement(const Tree *tree, const ForeignImportStatement *statement, Sint32 depth) {
	(void)tree;
	(void)statement;
	pad(depth);
	printf("(foreign-import)");
	return true;
}

Bool dump_using_statement(const Tree *tree, const UsingStatement *statement, Sint32 depth) {
	pad(depth);
	printf("(using");
	printf("\n");
	dump_list_expression(tree, statement->list, depth + 1);
	printf(")"); // using
	return true;
}

Bool dump_statement(const Tree *tree, const Statement *statement, Sint32 depth) {
	switch (statement->kind) {
	case STATEMENT_EMPTY:          return false; // Nothing
	case STATEMENT_BLOCK:          return dump_block_statement(tree, RCAST(const BlockStatement *, statement), depth);
	case STATEMENT_IMPORT:         return dump_import_statement(tree, RCAST(const ImportStatement *, statement), depth);
	case STATEMENT_EXPRESSION:     return dump_expression_statement(tree, RCAST(const ExpressionStatement *, statement), depth);
	case STATEMENT_ASSIGNMENT:     return dump_assignment_statement(tree, RCAST(const AssignmentStatement *, statement), depth);
	case STATEMENT_DECLARATION:    return dump_declaration_statement(tree, RCAST(const DeclarationStatement *, statement), depth);
	case STATEMENT_IF:             return dump_if_statement(tree, RCAST(const IfStatement *, statement), depth);
	case STATEMENT_WHEN:           return dump_when_statement(tree, RCAST(const WhenStatement *, statement), depth);
	case STATEMENT_RETURN:         return dump_return_statement(tree, RCAST(const ReturnStatement *, statement), depth);
	case STATEMENT_FOR:            return dump_for_statement(tree, RCAST(const ForStatement *, statement), depth);
	case STATEMENT_SWITCH:         return dump_switch_statement(tree, RCAST(const SwitchStatement *, statement), depth);
	case STATEMENT_DEFER:          return dump_defer_statement(tree, RCAST(const DeferStatement *, statement), depth);
	case STATEMENT_BRANCH:         return dump_branch_statement(tree, RCAST(const BranchStatement *, statement), depth);
	case STATEMENT_FOREIGN_BLOCK:  return dump_foreign_block_statement(tree, RCAST(const ForeignBlockStatement *, statement), depth);
	case STATEMENT_FOREIGN_IMPORT: return dump_foreign_import_statement(tree, RCAST(const ForeignImportStatement *, statement), depth);
	case STATEMENT_USING:          return dump_using_statement(tree, RCAST(const UsingStatement *, statement), depth);
	}
	UNREACHABLE();
}

Bool dump_identifier(const Tree *tree, const Identifier *identifier, Sint32 depth) {
	(void)tree;
	pad(depth);
	const String ident = identifier->contents;
	// const Location location = tree->tokens[identifier->token].location;
	printf("(ident '%.*s')", SFMT(ident));
	return true;
}

void dump(Tree *tree) {
	const Size n_statements = array_size(tree->statements);
	for (Size i = 0; i < n_statements; i++) {
		const Statement *statement = tree->statements[i];
		dump_statement(tree, statement, 0);
		printf("\n");
	}
}