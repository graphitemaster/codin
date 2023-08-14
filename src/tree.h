#ifndef CODIN_TREE_H
#define CODIN_TREE_H
#include "array.h"
#include "lexer.h"

typedef struct Context Context;

typedef struct Tree Tree;

// Expressions.
typedef struct Expression Expression;
typedef struct ListExpression ListExpression;
typedef struct UnaryExpression UnaryExpression;
typedef struct BinaryExpression BinaryExpression;
typedef struct TernaryExpression TernaryExpression;
typedef struct CastExpression CastExpression;
typedef struct SelectorExpression SelectorExpression;
typedef struct CallExpression CallExpression;
typedef struct AssertionExpression AssertionExpression;
typedef struct ValueExpression ValueExpression;
typedef struct ProcedureExpression ProcedureExpression;
typedef struct TypeExpression TypeExpression;
typedef struct IndexExpression IndexExpression;
typedef struct SliceExpression SliceExpression;
typedef struct LiteralExpression LiteralExpression;
typedef struct CompoundLiteralExpression CompoundLiteralExpression;
typedef struct IdentifierExpression IdentifierExpression;
typedef struct UndefinedExpression UndefinedExpression;
typedef struct ProcedureGroupExpression ProcedureGroupExpression;

// Statements.
typedef struct Statement Statement;
typedef struct EmptyStatement EmptyStatement;
typedef struct ImportStatement ImportStatement;
typedef struct ExpressionStatement ExpressionStatement;
typedef struct BlockStatement BlockStatement;
typedef struct AssignmentStatement AssignmentStatement;
typedef struct DeclarationStatement DeclarationStatement;
typedef struct IfStatement IfStatement;
typedef struct WhenStatement WhenStatement;
typedef struct ReturnStatement ReturnStatement;
typedef struct ForStatement ForStatement;
typedef struct SwitchStatement SwitchStatement;
typedef struct DeferStatement DeferStatement;
typedef struct BranchStatement BranchStatement;
typedef struct ForeignBlockStatement ForeignBlockStatement;
typedef struct ForeignImportStatement ForeignImportStatement;
typedef struct UsingStatement UsingStatement;
typedef struct PackageStatement PackageStatement;

// Types.
typedef struct Type Type;
typedef struct BuiltinType BuiltinType;
typedef struct ProcedureType ProcedureType;
typedef struct ConcreteProcedureType ConcreteProcedureType;
typedef struct GenericProcedureType GenericProcedureType;
typedef struct PointerType PointerType;
typedef struct MultiPointerType MultiPointerType;
typedef struct SliceType SliceType;
typedef struct ArrayType ArrayType;
typedef struct DynamicArrayType DynamicArrayType;
typedef struct BitSetType BitSetType;
typedef struct TypeidType TypeidType;
typedef struct MapType MapType;
typedef struct MatrixType MatrixType;
typedef struct DistinctType DistinctType;
typedef struct EnumType EnumType;
typedef struct ExpressionType ExpressionType;
typedef struct StructType StructType;
typedef struct ConcreteStructType ConcreteStructType;
typedef struct GenericStructType GenericStructType;
typedef struct UnionType UnionType;
typedef struct ConcreteUnionType ConcreteUnionType;
typedef struct GenericUnionType GenericUnionType;
typedef struct PolyType PolyType;

// Misc.
typedef struct Identifier Identifier;
typedef struct Field Field;
typedef struct CaseClause CaseClause;

enum ExpressionKind {
	EXPRESSION_LIST             = 0,
  EXPRESSION_UNARY            = 1,  // <op> operand
	EXPRESSION_BINARY           = 2,  // lhs <op> rhs
	EXPRESSION_TERNARY          = 3,  // lhs <if|when> cond else rhs
	EXPRESSION_CAST             = 4,  // auto_cast operand, cast(T)operand, transmute(T)operand
	EXPRESSION_SELECTOR         = 5,  // base.field or .enumerator
	EXPRESSION_CALL             = 6,  // operand(..args)
	EXPRESSION_ASSERTION        = 7,  // operand.(T) or operand.?
	EXPRESSION_PROCEDURE        = 8,  // proc() {}
	EXPRESSION_TYPE             = 9,  // T
	EXPRESSION_INDEX            = 10, // x[n], x[:], x[n:], x[:n], x[a:b], x[a,b]
	EXPRESSION_SLICE            = 11, // []T
	EXPRESSION_LITERAL          = 12, // int, float, rune, string
	EXPRESSION_COMPOUND_LITERAL = 13, // T{...}
	EXPRESSION_IDENTIFIER       = 14, // ident
	EXPRESSION_UNDEFINED        = 15, // ---
	EXPRESSION_PROCEDURE_GROUP  = 16, // proc{...}
};

enum StatementKind {
	STATEMENT_EMPTY          = 0,
	STATEMENT_BLOCK          = 1,
	STATEMENT_IMPORT         = 2, // import
	STATEMENT_EXPRESSION     = 3, 
	STATEMENT_ASSIGNMENT     = 4, // =, +=, -=, *=, /=, %=, %%=, &=, |=, ~=, &~=, <<=, >>=, &&=, ||=
	STATEMENT_DECLARATION    = 5,
	STATEMENT_IF             = 6,
	STATEMENT_WHEN           = 7,
	STATEMENT_RETURN         = 8,
	STATEMENT_FOR            = 9,
	STATEMENT_SWITCH         = 10,
	STATEMENT_DEFER          = 11,
	STATEMENT_BRANCH         = 12, // break, continue, fallthrough
	STATEMENT_FOREIGN_BLOCK  = 13, // foreign <name>
	STATEMENT_FOREIGN_IMPORT = 14, // foreign import
	STATEMENT_USING          = 15, // using <name>
	STATEMENT_PACKAGE        = 16, // package <ident>
};

enum ProcedureKind {
	PROCEDURE_CONCRETE,
	PROCEDURE_GENERIC,
};

enum StructKind {
	STRUCT_CONCRETE,
	STRUCT_GENERIC,
};

enum UnionKind {
	UNION_CONCRETE,
	UNION_GENERIC,
};

enum TypeKind {
	TYPE_BUILTIN         = 0,  // b{8,16,32,64}, f{16,32,64}(le|be), (i|u)8, (i|u){16,32,64,128}(le|be), 
	TYPE_PROCEDURE       = 1,  // proc
	TYPE_POINTER         = 2,  // ^T
	TYPE_MULTI_POINTER   = 3,  // [^]T
	TYPE_SLICE           = 4,  // []T
	TYPE_ARRAY           = 5,  // [N]T or [?]T
	TYPE_DYNAMIC_ARRAY   = 6,  // [dynamic]T
	TYPE_BIT_SET         = 7,  // bit_set[T] or bit_set[T; U]
	TYPE_TYPEID          = 8,  // typeid
	TYPE_MAP             = 9,  // map[K]V
	TYPE_MATRIX          = 10, // matrix[R,C]T
	TYPE_DISTINCT        = 11, // distinct T
	TYPE_ENUM            = 12, // enum
	TYPE_STRUCT          = 13, // struct
	TYPE_UNION           = 14, // union
	TYPE_POLY            = 15, // $T or $T/$U
	TYPE_EXPRESSION      = 16, // Expression which evaluates to a Type*
};

enum BuiltinTypeKind {
	BUILTIN_TYPE_SINT    = 0, // i8,i{16,32,64,128}(le|be)
	BUILTIN_TYPE_UINT    = 1, // u8,u{16,32,64,128}(le|be)
	BUILTIN_TYPE_FLOAT   = 2, // f{16,32,64}(le|be)
	BUILTIN_TYPE_BOOL    = 3, // b{8,16,32,64}
	BUILTIN_TYPE_STRING  = 4, // string
	BUILTIN_TYPE_CSTRING = 5, // cstring
	BUILTIN_TYPE_POINTER = 6, // rawptr
	BUILTIN_TYPE_UINTPTR = 7, // uintptr
};

enum Endianess {
	ENDIANESS_NA,         // Not applicable.
	ENDIANESS_LITTLE,
	ENDIANESS_BIG,
};

enum BlockFlag {
	BLOCK_FLAG_BOUNDS_CHECK = 1 << 0,
	BLOCK_FLAG_TYPE_ASSERT  = 1 << 1,
};

enum ProcedureFlag {
	PROC_FLAG_DIVERGING                 = 1 << 0,
	PROC_FLAG_OPTIONAL_OK               = 1 << 1,
	PROC_FLAG_OPTIONAL_ALLOCATION_ERROR = 1 << 2,
	PROC_FLAG_BOUNDS_CHECK              = 1 << 3,
	PROC_FLAG_TYPE_ASSERT               = 1 << 4,
	PROC_FLAG_FORCE_INLINE              = 1 << 5,
};

enum StructFlag {
	STRUCT_FLAG_PACKED     = 1 << 0, // #packed
	STRUCT_FLAG_UNCOPYABLE = 1 << 1, // #no_copy
	STRUCT_FLAG_UNION      = 1 << 2, // #raw_union
};

enum UnionFlag {
	UNION_FLAG_NO_NIL     = 1 << 0, // #no_nil
	UNION_FLAG_SHARED_NIL = 1 << 1, // #shared_nil
	UNION_FLAG_MAYBE      = 1 << 2, // #maybe
};

enum FieldFlag {
	FIELD_FLAG_ANY_INT  = 1 << 0, // #any_int
	FIELD_FLAG_C_VARARG = 1 << 1, // #c_vararg
	FIELD_FLAG_NO_ALIAS = 1 << 2, // #no_alias
	FIELD_FLAG_SUBTYPE  = 1 << 3, // #subtype
	FIELD_FLAG_USING    = 1 << 3, // using
};

#define CCONVENTION(enumerator, string) CCONV_ ## enumerator,
enum CallingConvention {
	CCONV_INVALID,
	#include "lexemes.h"
};

typedef enum ExpressionKind ExpressionKind;
typedef enum StatementKind StatementKind;
typedef enum ProcedureKind ProcedureKind;
typedef enum StructKind StructKind;
typedef enum UnionKind UnionKind;
typedef enum TypeKind TypeKind;
typedef enum BuiltinTypeKind BuiltinTypeKind;
typedef enum Endianess Endianess;

typedef enum BlockFlag BlockFlag;
typedef enum ProcedureFlag ProcedureFlag;
typedef enum StructFlag StructFlag;
typedef enum UnionFlag UnionFlag;
typedef enum FieldFlag FieldFlag;

typedef enum CallingConvention CallingConvention;

String procedure_flags_to_string(ProcedureFlag flags, Context *context);

// Expressions.
struct Expression {
	ExpressionKind kind;
};

struct ListExpression {
	Expression base;
	Array(Expression*) expressions;
};

// <operation> <operand>
struct UnaryExpression {
	Expression base;
	OperatorKind operation;
	Expression *operand;
};

// <lhs> <operation> <rhs>
struct BinaryExpression {
	Expression base;
	OperatorKind operation;
	Expression *lhs;
	Expression *rhs;
};

// <on_true> <operation> <cond> <on_false>
// <cond> ? <on_true> : <on_false>
struct TernaryExpression {
	Expression base;
	// One of:
	//	KEYWORD_IF
	//	KEYWORD_WHEN
	KeywordKind operation;
	Expression *on_true;
	Expression *cond;
	Expression *on_false;
};

// cast(T)expr
// (T)expr
// T(expr)
struct CastExpression {
	Expression base;
	// One of:
	// 	OPERATOR_CAST
	//	OPERATOR_AUTO_CAST
	//	OPERATOR_TRANSMUTE
	OperatorKind kind;
	Type *type;
	Expression *expression;
};

// .<identifier>
// <operand>.<identifier>
struct SelectorExpression {
	Expression base;
	Expression *operand;
	Identifier *identifier;
};

// <operand>(..<arguments>)
struct CallExpression {
	Expression base;
	Expression *operand;
	Array(Field*) arguments;
};

// <operand>.(T)
// <operand>.?
struct AssertionExpression {
	Expression base;
	Expression *operand;
	Type *type;
};

struct ProcedureExpression {
	Expression base;
	ProcedureType *type;
	ListExpression *where_clauses;
	BlockStatement *body;
};

struct TypeExpression {
	Expression base;
	Type *type;
};

struct IndexExpression {
	Expression base;
	Expression *operand;
	Expression *lhs;
	Expression *rhs;
};

struct SliceExpression {
	Expression base;
	Expression *operand;
	Expression *lhs;
	Expression *rhs;
};

struct LiteralExpression {
	Expression base;
	LiteralKind kind;
	String value;
};

struct CompoundLiteralExpression {
	Expression base;
	Type *type;
	Array(Field*) fields;
};

struct IdentifierExpression {
	Expression base;
	Identifier *identifier;
};

struct UndefinedExpression {
	Expression base;
};

struct ProcedureGroupExpression {
	Expression base;
	Array(Expression*) expressions;
};

// Statements.
struct Statement {
	StatementKind kind;
};

struct EmptyStatement {
	Statement base;
};

struct ImportStatement {
	Statement base;
	String name;       // Optional
	String collection; // "core"
	String pathname;   // "./fmt"
	Bool is_using;
};

struct ExpressionStatement {
	Statement base;
	Expression *expression;
};

struct BlockStatement {
	Statement base;
	BlockFlag flags;
	Array(Statement*) statements;
	Identifier *label;
};

struct AssignmentStatement {
	Statement base;
	AssignmentKind assignment;
	ListExpression *lhs;
	ListExpression *rhs;
};

struct DeclarationStatement {
	Statement base;
	Type *type;
	Array(Identifier*) names;
	ListExpression *values;
	Array(Field*) attributes;
	Bool is_using;
};

struct IfStatement {
	Statement base;
	Statement *init;
	Expression *cond;
	BlockStatement *body;
	BlockStatement *elif;
	Identifier *label;
};

struct WhenStatement {
	Statement base;
	Expression *cond;
	BlockStatement *body;
	BlockStatement *elif;
};

struct ReturnStatement {
	Statement base;
	Array(Expression*) results;
};

struct ForStatement {
	Statement base;
	Statement *init;
	Expression *cond;
	BlockStatement *body;
	Statement *post;
	Identifier *label;
};

struct SwitchStatement {
	Statement base;
	Statement *init;
	Expression *cond;
	Array(CaseClause*) clauses;
};

struct DeferStatement {
	Statement base;
	Statement *statement;
};

struct BranchStatement {
	Statement base;
	KeywordKind branch;
	Identifier *label; // Optional label.
};

struct ForeignBlockStatement {
	Statement base;
	Identifier *name;
	BlockStatement *body;
	Array(Field*) attributes;
};

struct ForeignImportStatement {
	Statement base;
	String name;
	Array(String) sources;
	Array(Field*) attributes;
};

struct UsingStatement {
	Statement base;
	ListExpression *list;
};

struct PackageStatement {
	Statement base;
	String name;
};

// Types.
struct Type {
	TypeKind kind;
	Bool poly;
};

struct BuiltinType {
	Type base;
	String identifier;
	BuiltinTypeKind kind;
	Uint16 size_of;
	Uint16 align_of;
	Endianess endianess;
};

struct IdentifierType {
	Type base;
	Identifier *identifier;
};

struct ProcedureType {
	Type base;
	ProcedureKind kind;
	ProcedureFlag flags;
	CallingConvention convention;
	Array(Field*) params;
	Array(Field*) results;
};

struct ConcreteProcedureType {
	ProcedureType base;
};

struct GenericProcedureType {
	ProcedureType base;
};

struct PointerType {
	Type base;
	Type *type;
};

struct MultiPointerType {
	Type base;
	Type *type;
};

struct SliceType {
	Type base;
	Type *type;
};

struct ArrayType {
	Type base;
	Type *type;
	Expression *count;
};

struct DynamicArrayType {
	Type base;
	Type *type;
};

struct BitSetType {
	Type base;
	Expression *expression;
	Type *underlying;
};

struct TypeidType {
	Type base;
	Type *specialization;
};

struct MapType {
	Type base;
	Type *key;
	Type *value;
};

struct MatrixType {
	Type base;
	Expression *columns;
	Expression *rows;
	Type *type;
};

struct DistinctType {
	Type base;
	Type *type;
};

struct EnumType {
	Type base;
	Type *type; // The T in 'enum T'
	Array(Field*) fields;
};

struct ExpressionType {
	Type base;
	// Can be one of:
	//	EXPRESSION_IDENTIFIER
	//	EXPRESSION_TYPE
	Expression *expression;
};

struct StructType {
	Type base;
	StructKind kind;
	StructFlag flags;
	Expression *align;
	Array(Field*) fields;
	ListExpression *where_clauses;
};

struct ConcreteStructType {
	StructType base;
};

struct GenericStructType {
	StructType base;
	Array(Field*) parameters;
};

struct UnionType {
	Type base;
	UnionKind kind;
	UnionFlag flags;
	Expression *align;
	Array(Type*) variants;
	ListExpression *where_clauses;
};

struct ConcreteUnionType {
	UnionType base;
};

struct GenericUnionType {
	UnionType base;
	Array(Field*) parameters;
};

struct PolyType {
	Type base;
	Type *type;
	Type *specialization;
};

// Misc.
struct Identifier {
	String contents;
	Bool poly;
	Uint32 token;
};

struct Field {
	Identifier *name;  // Always present
	Type *type;        // Optional.
	Expression *value; // Optional.
	String tag;        // Optional.
	FieldFlag flags;
};

struct CaseClause {
	ListExpression *expressions;
	Array(Statement*) statements;
};

static inline String calling_convention_to_string(CallingConvention cc) {
	#define CCONVENTION(enumerator, name, ...) SLIT(name),
	static const String TABLE[] = {
		#include "lexemes.h"
	};
	return TABLE[cc];
};

struct Tree {
	Context *context;
	String filename;
	Array(Statement*) statements;
	Array(Token) tokens; // Recorded tokens for diagnostics.
};

void tree_init(Tree *tree, String filename, Context *context);
void tree_fini(Tree *tree);

void tree_record_token(Tree *tree, Token token);

// Expressions
ListExpression *tree_new_list_expression(Tree *tree, Array(Expression*) expressions);
UnaryExpression *tree_new_unary_expression(Tree *tree, OperatorKind operation, Expression *operand);
BinaryExpression *tree_new_binary_expression(Tree *tree, OperatorKind operation, Expression *lhs, Expression *rhs);
TernaryExpression *tree_new_ternary_expression(Tree *tree, Expression *on_true, KeywordKind operation, Expression *cond, Expression *on_false);
CastExpression *tree_new_cast_expression(Tree *tree, OperatorKind kind, Type *type, Expression *expression);
SelectorExpression *tree_new_selector_expression(Tree *tree, Expression *operand, Identifier *identifier);
CallExpression *tree_new_call_expression(Tree *tree, Expression *operand, Array(Field*) arguments);
AssertionExpression *tree_new_assertion_expression(Tree *tree, Expression *operand, Type *type);
ProcedureExpression *tree_new_procedure_expression(Tree *tree, ProcedureType *type, ListExpression *where_clauses, BlockStatement *body);
TypeExpression *tree_new_type_expression(Tree *tree, Type *type);
IndexExpression *tree_new_index_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs);
SliceExpression *tree_new_slice_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs);
LiteralExpression *tree_new_literal_expression(Tree *tree, LiteralKind kind, String value);
CompoundLiteralExpression *tree_new_compound_literal_expression(Tree *tree, Type *type, Array(Field*) fields);
IdentifierExpression *tree_new_identifier_expression(Tree *tree, Identifier *identifier);
UndefinedExpression *tree_new_undefined_expression(Tree *tree);
ProcedureGroupExpression *tree_new_procedure_group_expression(Tree *tree, Array(Expression*) expressions);

// Statements
EmptyStatement *tree_new_empty_statement(Tree *tree);
ImportStatement *tree_new_import_statement(Tree *tree, String name, String collection, String pathname, Bool is_using);
ExpressionStatement *tree_new_expression_statement(Tree *tree, Expression *expression);
BlockStatement *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Statement*) statements);
AssignmentStatement *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, ListExpression *lhs, ListExpression *rhs);
DeclarationStatement *tree_new_declaration_statement(Tree *tree, Type *type, Array(Identifier*) names, ListExpression *values, Bool is_using);
IfStatement *tree_new_if_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, BlockStatement *elif);
WhenStatement *tree_new_when_statement(Tree *tree, Expression *cond, BlockStatement *body, BlockStatement *elif);
ForStatement *tree_new_for_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, Statement *post);
SwitchStatement *tree_new_switch_statement(Tree *tree, Statement *init, Expression *cond, Array(CaseClause*) clauses);
ReturnStatement *tree_new_return_statement(Tree *tree, Array(Expression*) results);
DeferStatement *tree_new_defer_statement(Tree *tree, Statement *stmt);
BranchStatement *tree_new_branch_statement(Tree *tree, KeywordKind branch, Identifier *label);
ForeignBlockStatement *tree_new_foreign_block_statement(Tree *tree, Identifier *name, BlockStatement *body);
ForeignImportStatement *tree_new_foreign_import_statement(Tree *tree, String name, Array(String) sources);
UsingStatement *tree_new_using_statement(Tree *tree, ListExpression *list);
PackageStatement *tree_new_package_statement(Tree *tree, String package);

// Types
ConcreteProcedureType *tree_new_concrete_procedure_type(Tree *tree, Array(Field*) params, Array(Field*) results, ProcedureFlag flags, CallingConvention convention);
GenericProcedureType *tree_new_generic_procedure_type(Tree *tree, Array(Field*) params, Array(Field*) results, ProcedureFlag flags, CallingConvention convention);
PointerType *tree_new_pointer_type(Tree *Tree, Type *type);
MultiPointerType *tree_new_multi_pointer_type(Tree *Tree, Type *type);
SliceType *tree_new_slice_type(Tree *tree, Type *type);
ArrayType *tree_new_array_type(Tree *Tree, Type *type, Expression *count);
DynamicArrayType *tree_new_dynamic_array_type(Tree *tree, Type *type);
BitSetType *tree_new_bit_set_type(Tree *tree, Expression *expression, Type *underlying);
TypeidType *tree_new_typeid_type(Tree *tree, Type *specialization);
MapType *tree_new_map_type(Tree *tree, Type *key, Type *value);
MatrixType *tree_new_matrix_type(Tree *tree, Expression *rows, Expression *columns, Type *type);
DistinctType *tree_new_distinct_type(Tree *tree, Type *type);
EnumType *tree_new_enum_type(Tree *tree, Type *base_type, Array(Field*) fields);
ExpressionType *tree_new_expression_type(Tree *tree, Expression *expression);
ConcreteStructType *tree_new_concrete_struct_type(Tree *tree, StructFlag flags, Expression *align, Array(Field*) fields, ListExpression *where_clauses);
GenericStructType *tree_new_generic_struct_type(Tree *tree, StructFlag flags, Expression *align, Array(Field*) parameters, Array(Field*) fields, ListExpression *where_clauses);
ConcreteUnionType *tree_new_concrete_union_type(Tree *tree, UnionFlag flags, Expression *align, Array(Type*) variants, ListExpression *where_clauses);
GenericUnionType *tree_new_generic_union_type(Tree *tree, UnionFlag flags, Expression *align, Array(Field*) parameters, Array(Type*) variants, ListExpression *where_clauses);
PolyType *tree_new_poly_type(Tree *tree, Type *type, Type *specialization);

Field *tree_new_field(Tree *tree, Type *type, Identifier *name, Expression *value, String tag, FieldFlag flags);
Identifier *tree_new_identifier(Tree *tree, String contents, Bool poly);
CaseClause *tree_new_case_clause(Tree *tree, ListExpression *expressions, Array(Statement*) statements);

#endif // CODIN_TREE_H