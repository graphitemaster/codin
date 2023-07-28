#ifndef CODIN_DUMP_H
#define CODIN_DUMP_H
#include "tree.h"

Bool dump_literal_value(const LiteralValue *value, Sint32 depth);
Bool dump_compound_literal_value(const CompoundLiteralValue *value, Sint32 depth);
Bool dump_identifier_value(const IdentifierValue *value, Sint32 depth);
Bool dump_value(const Value *value, Sint32 depth);
Bool dump_list_expression(const ListExpression *expression, Sint32 depth);
Bool dump_unary_expression(const UnaryExpression *expression, Sint32 depth);
Bool dump_binary_expression(const BinaryExpression *expression, Sint32 depth);
Bool dump_ternary_expression(const TernaryExpression *expression, Sint32 depth);
Bool dump_cast_expression(const CastExpression *expression, Sint32 depth);
Bool dump_selector_expression(const SelectorExpression *expression, Sint32 depth);
Bool dump_call_expression(const CallExpression *expression, Sint32 depth);
Bool dump_type(const Type *type, Sint32 depth);
Bool dump_assertion_expression(const AssertionExpression *expression, Sint32 depth);
Bool dump_value_expression(const ValueExpression *expression, Sint32 depth);
Bool dump_identifier_type(const IdentifierType *type, Sint32 depth);
Bool dump_fields(Array(Field*) const fields, Sint32 depth);
Bool dump_procedure_type(const ProcedureType *type, Sint32 depth);
Bool dump_pointer_type(const PointerType *type, Sint32 depth);
Bool dump_multi_pointer_type(const MultiPointerType *type, Sint32 depth);
Bool dump_slice_type(const SliceType *type, Sint32 depth);
Bool dump_array_type(const ArrayType *type, Sint32 depth);
Bool dump_dynamic_array_type(const ArrayType *type, Sint32 depth);
Bool dump_bit_set_type(const BitSetType *type, Sint32 depth);
Bool dump_typeid_type(const TypeidType *type, Sint32 depth);
Bool dump_map_type(const MapType *type, Sint32 depth);
Bool dump_matrix_type(const MatrixType *type, Sint32 depth);
Bool dump_procedure_expression(const ProcedureExpression *expression, Sint32 depth);
Bool dump_type(const Type *type, Sint32 depth);
Bool dump_type_expression(const TypeExpression *expression, Sint32 depth);
Bool dump_expression(const Expression *expression, Sint32 depth);
Bool dump_block_statement(const BlockStatement *statement, Sint32 depth);
Bool dump_import_statement(const ImportStatement *statement, Sint32 depth);
Bool dump_expression_statement(const ExpressionStatement *statement, Sint32 depth);
Bool dump_assignment_statement(const AssignmentStatement *statement, Sint32 depth);
Bool dump_declaration_statement(const DeclarationStatement *statement, Sint32 depth);
Bool dump_if_statement(const IfStatement *statement, Sint32 depth);
Bool dump_return_statement(const ReturnStatement *statement, Sint32 depth);
Bool dump_for_statement(const ForStatement *statement, Sint32 depth);
Bool dump_defer_statement(const DeferStatement *statement, Sint32 depth);
Bool dump_branch_statement(const BranchStatement *statement, Sint32 depth);
Bool dump_statement(const Statement *statement, Sint32 depth);
void dump(Tree *tree);

#endif // CODIN_DUMP_H