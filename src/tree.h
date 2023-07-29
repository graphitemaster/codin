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
typedef struct DeferStatement DeferStatement;
typedef struct BranchStatement BranchStatement;

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

// Misc.
typedef struct Identifier Identifier;
typedef struct Field Field;

enum ExpressionKind {
	EXPRESSION_LIST,
	EXPRESSION_UNARY,            // <op> operand
	EXPRESSION_BINARY,           // lhs <op> rhs
	EXPRESSION_TERNARY,          // lhs <if|when> cond else rhs
	EXPRESSION_CAST,             // auto_cast operand, cast(T)operand, transmute(T)operand
	EXPRESSION_SELECTOR,         // base.field or .enumerator
	EXPRESSION_CALL,             // operand(..args)
	EXPRESSION_ASSERTION,        // operand.(T) or operand.?
	EXPRESSION_PROCEDURE,        // proc() {}
	EXPRESSION_TYPE,             // T
	EXPRESSION_INDEX,            // x[n], x[:], x[n:], x[:n], x[a:b], x[a,b]
	EXPRESSION_SLICE,            // []T
	EXPRESSION_LITERAL,          // int, float, rune, string
	EXPRESSION_COMPOUND_LITERAL, // T{...}
	EXPRESSION_IDENTIFIER,       // ident
};

enum StatementKind {
	STATEMENT_EMPTY,
	STATEMENT_BLOCK,
	STATEMENT_IMPORT,
	STATEMENT_EXPRESSION, 
	STATEMENT_ASSIGNMENT,
	STATEMENT_DECLARATION,
	STATEMENT_IF,
	STATEMENT_WHEN,
	STATEMENT_RETURN,
	STATEMENT_FOR,
	STATEMENT_DEFER,
	STATEMENT_BRANCH,   // break, continue, fallthrough
};

enum ProcedureKind {
	PROCEDURE_CONCRETE,
	PROCEDURE_GENERIC,
};

enum TypeKind {
	TYPE_BUILTIN,       // b{8,16,32,64}, f{16,32,64}(le|be), (i|u)8, (i|u){16,32,64,128}(le|be), 
	TYPE_PROCEDURE,     // proc
	TYPE_POINTER,       // ^T
	TYPE_MULTI_POINTER, // [^]T
	TYPE_SLICE,         // []T
	TYPE_ARRAY,         // [N]T or [?]T
	TYPE_DYNAMIC_ARRAY, // [dynamic]T
	TYPE_BIT_SET,       // bit_set[T] or bit_set[T; U]
	TYPE_TYPEID,        // typeid
	TYPE_MAP,           // map[K]V
	TYPE_MATRIX,        // matrix[R,C]T
	TYPE_DISTINCT,      // distinct T
	TYPE_ENUM,          // enum
	TYPE_EXPRESSION,    // Expression which evaluates to a Type*
};

enum BuiltinTypeKind {
	BUILTIN_TYPE_SINT,    // i8,i{16,32,64,128}(le|be)
	BUILTIN_TYPE_UINT,    // u8,u{16,32,64,128}(le|be)
	BUILTIN_TYPE_FLOAT,   // f{16,32,64}(le|be)
	BUILTIN_TYPE_BOOL,    // b{8,16,32,64}
	BUILTIN_TYPE_STRING,  // string
	BUILTIN_TYPE_CSTRING, // cstring
	BUILTIN_TYPE_POINTER, // rawptr
	BUILTIN_TYPE_UINTPTR, // uintptr
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

#define CCONVENTION(name, enumerator) CCONV_ ## enumerator,
enum CallingConvention {
	CCONV_INVALID,
	#include "lexemes.h"
};
#undef CCONVENTION

typedef enum ExpressionKind ExpressionKind;
typedef enum StatementKind StatementKind;
typedef enum ProcedureKind ProcedureKind;
typedef enum TypeKind TypeKind;
typedef enum BuiltinTypeKind BuiltinTypeKind;
typedef enum Endianess Endianess;

typedef enum BlockFlag BlockFlag;
typedef enum ProcedureFlag ProcedureFlag;

typedef enum CallingConvention CallingConvention;

static inline String block_flags_to_string(BlockFlag flags) {
	switch (CAST(Sint32, flags)) {
	case BLOCK_FLAG_BOUNDS_CHECK:
		return SCLIT("'#bounds_check'");
	case BLOCK_FLAG_TYPE_ASSERT:
		return SCLIT("'#type_assert'");
	case BLOCK_FLAG_BOUNDS_CHECK | BLOCK_FLAG_TYPE_ASSERT:
		return SCLIT("'#bounds_check' '#type_assert'");
	default:
		return SCLIT("");
	}
	UNREACHABLE();
}

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
	Array(Expression*) arguments;
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
	Expression *expression;
	Array(Expression*) expressions;
};

struct IdentifierExpression {
	Expression base;
	Identifier *identifier;
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
	String name;
	String package;
};

struct ExpressionStatement {
	Statement base;
	Expression *expression;
};

struct BlockStatement {
	Statement base;
	BlockFlag flags;
	Array(Statement*) statements;
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
};

struct IfStatement {
	Statement base;
	Statement *init;
	Expression *cond;
	BlockStatement *body;
	BlockStatement *elif;
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
	Type *base_type; // The T in 'enum T'
	Array(Field*) fields;
};

struct ExpressionType {
	Type base;
	// Can be one of:
	//	EXPRESSION_IDENTIFIER
	//	EXPRESSION_TYPE
	Expression *expression;
};

// Misc.
struct Identifier {
	String contents;
	Bool poly;
};

struct Field {
	Identifier *name; // Always present
	Type *type; // Optional.
	Expression *value;
};

static inline String calling_convention_to_string(CallingConvention cc) {
	#define CCONVENTION(name, ...) SLIT(name),
	static const String TABLE[] = {
		SLIT("invalid"),
		#include "lexemes.h"
	};
	#undef CCONVENTION
	return TABLE[cc];
};

struct Tree {
	Context *context;
	String package_name;
	String file_name;
	Array(Statement*) statements;
};

void tree_init(Tree *tree, Context *context);
void tree_dump(Tree *tree);

// Expressions
ListExpression *tree_new_list_expression(Tree *tree, Array(Expression*) expressions);
UnaryExpression *tree_new_unary_expression(Tree *tree, OperatorKind operation, Expression *operand);
BinaryExpression *tree_new_binary_expression(Tree *tree, OperatorKind operation, Expression *lhs, Expression *rhs);
TernaryExpression *tree_new_ternary_expression(Tree *tree, Expression *on_true, KeywordKind operation, Expression *cond, Expression *on_false);
CastExpression *tree_new_cast_expression(Tree *tree, OperatorKind kind, Type *type, Expression *expression);
SelectorExpression *tree_new_selector_expression(Tree *tree, Expression *operand, Identifier *identifier);
CallExpression *tree_new_call_expression(Tree *tree, Expression *operand, Array(Expression*) arguments);
AssertionExpression *tree_new_assertion_expression(Tree *tree, Expression *operand, Type *type);
ProcedureExpression *tree_new_procedure_expression(Tree *tree, ProcedureType *type, ListExpression *where_clauses, BlockStatement *body);
TypeExpression *tree_new_type_expression(Tree *tree, Type *type);
IndexExpression *tree_new_index_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs);
SliceExpression *tree_new_slice_expression(Tree *tree, Expression *operand, Expression *lhs, Expression *rhs);
LiteralExpression *tree_new_literal_expression(Tree *tree, LiteralKind kind, String value);
CompoundLiteralExpression *tree_new_compound_literal_expression(Tree *tree, Expression *expression, Array(Expression*) expressions);
IdentifierExpression *tree_new_identifier_expression(Tree *tree, Identifier *identifier);

// Statements
EmptyStatement *tree_new_empty_statement(Tree *tree);
ImportStatement *tree_new_import_statement(Tree *tree, String name, String package);
ExpressionStatement *tree_new_expression_statement(Tree *tree, Expression *expression);
BlockStatement *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Statement*) statements);
AssignmentStatement *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, ListExpression *lhs, ListExpression *rhs);
DeclarationStatement *tree_new_declaration_statement(Tree *tree, Type *type, Array(Identifier*) names, ListExpression *values);
IfStatement *tree_new_if_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, BlockStatement *elif);
WhenStatement *tree_new_when_statement(Tree *tree, Expression *cond, BlockStatement *body, BlockStatement *elif);
ForStatement *tree_new_for_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, Statement *post);
ReturnStatement *tree_new_return_statement(Tree *tree, Array(Expression*) results);
DeferStatement *tree_new_defer_statement(Tree *tree, Statement *stmt);
BranchStatement *tree_new_branch_statement(Tree *tree, KeywordKind branch, Identifier *label);

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

Field *tree_new_field(Tree *tree, Type *type, Identifier *name, Expression *value);
Identifier *tree_new_identifier(Tree *tree, String contents, Bool poly);

#endif // CODIN_TREE_H