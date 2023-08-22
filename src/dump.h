#ifndef CODIN_DUMP_H
#define CODIN_DUMP_H
#include "tree.h"

Bool dump_tuple_expression(const Tree *tree, const TupleExpression *expression, Sint32 depth);
Bool dump_unary_expression(const Tree *tree, const UnaryExpression *expression, Sint32 depth);
Bool dump_binary_expression(const Tree *tree, const BinaryExpression *expression, Sint32 depth);
Bool dump_ternary_expression(const Tree *tree, const TernaryExpression *expression, Sint32 depth);
Bool dump_cast_expression(const Tree *tree, const CastExpression *expression, Sint32 depth);
Bool dump_selector_expression(const Tree *tree, const SelectorExpression *expression, Sint32 depth);
Bool dump_call_expression(const Tree *tree, const CallExpression *expression, Sint32 depth);
Bool dump_type(const Tree *tree, const Type *type, Sint32 depth);
Bool dump_assertion_expression(const Tree *tree, const AssertionExpression *expression, Sint32 depth);
Bool dump_literal_expression(const Tree *tree, const LiteralExpression *expression, Sint32 depth);
Bool dump_compound_literal_expression(const Tree *tree, const CompoundLiteralExpression *expression, Sint32 depth);
Bool dump_identifier_expression(const Tree *tree, const IdentifierExpression *expression, Sint32 depth);

Bool dump_fields(const Tree *tree, Array(Field*) const fields, Sint32 depth);
Bool dump_procedure_type(const Tree *tree, const ProcedureType *type, Sint32 depth);
Bool dump_pointer_type(const Tree *tree, const PointerType *type, Sint32 depth);
Bool dump_multi_pointer_type(const Tree *tree, const MultiPointerType *type, Sint32 depth);
Bool dump_slice_type(const Tree *tree, const SliceType *type, Sint32 depth);
Bool dump_array_type(const Tree *tree, const ArrayType *type, Sint32 depth);
Bool dump_dynamic_array_type(const Tree *tree, const ArrayType *type, Sint32 depth);
Bool dump_bit_set_type(const Tree *tree, const BitSetType *type, Sint32 depth);
Bool dump_typeid_type(const Tree *tree, const TypeidType *type, Sint32 depth);
Bool dump_map_type(const Tree *tree, const MapType *type, Sint32 depth);
Bool dump_matrix_type(const Tree *tree, const MatrixType *type, Sint32 depth);
Bool dump_procedure_expression(const Tree *tree, const ProcedureExpression *expression, Sint32 depth);
Bool dump_type(const Tree *tree, const Type *type, Sint32 depth);
Bool dump_type_expression(const Tree *tree, const TypeExpression *expression, Sint32 depth);
Bool dump_expression(const Tree *tree, const Expression *expression, Sint32 depth);
Bool dump_block_statement(const Tree *tree, const BlockStatement *statement, Sint32 depth);
Bool dump_import_statement(const Tree *tree, const ImportStatement *statement, Sint32 depth);
Bool dump_expression_statement(const Tree *tree, const ExpressionStatement *statement, Sint32 depth);
Bool dump_assignment_statement(const Tree *tree, const AssignmentStatement *statement, Sint32 depth);
Bool dump_declaration_statement(const Tree *tree, const DeclarationStatement *statement, Sint32 depth);
Bool dump_if_statement(const Tree *tree, const IfStatement *statement, Sint32 depth);
Bool dump_return_statement(const Tree *tree, const ReturnStatement *statement, Sint32 depth);
Bool dump_for_statement(const Tree *tree, const ForStatement *statement, Sint32 depth);
Bool dump_defer_statement(const Tree *tree, const DeferStatement *statement, Sint32 depth);
Bool dump_branch_statement(const Tree *tree, const BranchStatement *statement, Sint32 depth);
Bool dump_statement(const Tree *tree, const Statement *statement, Sint32 depth);

Bool dump_identifier(const Tree *tree, const Identifier *identifier, Sint32 depth);

void dump(Tree *tree);

#endif // CODIN_DUMP_H