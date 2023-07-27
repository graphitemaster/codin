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
typedef struct CastExpression CastExpression;
typedef struct SelectorExpression SelectorExpression;
typedef struct CallExpression CallExpression;
typedef struct AssertionExpression AssertionExpression;
typedef struct ValueExpression ValueExpression;
typedef struct ProcedureExpression ProcedureExpression;
typedef struct OrReturnExpression OrReturnExpression;

// Statements.
typedef struct Statement Statement;
typedef struct EmptyStatement EmptyStatement;
typedef struct ImportStatement ImportStatement;
typedef struct ExpressionStatement ExpressionStatement;
typedef struct BlockStatement BlockStatement;
typedef struct AssignmentStatement AssignmentStatement;
typedef struct DeclarationStatement DeclarationStatement;
typedef struct IfStatement IfStatement;
typedef struct ReturnStatement ReturnStatement;
typedef struct ForStatement ForStatement;
typedef struct DeferStatement DeferStatement;
typedef struct BranchStatement BranchStatement;

// Values.
typedef struct Value Value;
typedef struct LiteralValue LiteralValue;
typedef struct CompoundLiteralValue CompoundLiteralValue;
typedef struct IdentifierValue IdentifierValue;

// Types.
typedef struct Type Type;
typedef struct ProcedureType ProcedureType;
typedef struct ConcreteProcedureType ConcreteProcedureType;
typedef struct GenericProcedureType GenericProcedureType;

// Misc.
typedef struct Identifier Identifier;

typedef struct Field Field;

enum ExpressionKind {
	EXPRESSION_LIST,
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_CAST,
	EXPRESSION_SELECTOR,
	EXPRESSION_CALL,
	EXPRESSION_ASSERTION,
	EXPRESSION_VALUE,
	EXPRESSION_PROCEDURE,
	EXPRESSION_OR_RETURN,
};

enum StatementKind {
	STATEMENT_EMPTY,
	STATEMENT_BLOCK,
	STATEMENT_IMPORT,
	STATEMENT_EXPRESSION,
	STATEMENT_ASSIGNMENT,
	STATEMENT_DECLARATION,
	STATEMENT_IF,
	STATEMENT_RETURN,
	STATEMENT_FOR,
	STATEMENT_DEFER,
	STATEMENT_BRANCH, // break, continue, fallthrough
};

enum ValueKind {
	VALUE_LITERAL,
	VALUE_COMPOUND_LITERAL,
	VALUE_IDENTIFIER,
};

enum ProcedureKind {
	PROCEDURE_CONCRETE,
	PROCEDURE_GENERIC,
};

enum TypeKind {
	TYPE_PROCEDURE,
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
typedef enum ValueKind ValueKind;
typedef enum ProcedureKind ProcedureKind;
typedef enum TypeKind TypeKind;

typedef enum BlockFlag BlockFlag;
typedef enum ProcedureFlag ProcedureFlag;

typedef enum CallingConvention CallingConvention;

inline String block_flags_to_string(BlockFlag flags) {
	switch (CAST(Sint32, flags)) {
	case BLOCK_FLAG_BOUNDS_CHECK:
		return SCLIT("#bounds_check");
	case BLOCK_FLAG_TYPE_ASSERT:
		return SCLIT("#type_assert");
	case BLOCK_FLAG_BOUNDS_CHECK | BLOCK_FLAG_TYPE_ASSERT:
		return SCLIT("#bounds_check, #type_assert");
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

struct UnaryExpression {
	Expression base;
	OperatorKind operation;
	Expression *operand;
};

struct BinaryExpression {
	Expression base;
	OperatorKind operation;
	Expression *lhs;
	Expression *rhs;
};

struct CastExpression {
	Expression base;
	Identifier *type; // When nullptr this is an implicit cast expression.
	Expression *expression;
};

struct SelectorExpression {
	Expression base;
	Expression *operand; // When nullptr this is an implicit selector expression.
	Identifier *identifier;
};

struct CallExpression {
	Expression base;
	Expression *operand;
	Array(Expression*) arguments;
};

struct AssertionExpression {
	Expression base;
	Expression *operand;
	Identifier *type;
};

struct ValueExpression {
	Expression base;
	Value *value;
};

struct ProcedureExpression {
	Expression base;
	ProcedureFlag flags;
	ProcedureType *type;
	ListExpression *where_clauses;
	BlockStatement *body;
};

struct OrReturnExpression {
	Expression base;
	Expression *operand;
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
	Identifier *type;
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

// Values.
struct Value {
	ValueKind kind;
};

struct LiteralValue {
	Value base;
	LiteralKind kind;
	String input;
};

struct CompoundLiteralValue {
	Value base;
	Expression *expression;
	Array(Expression*) expressions;
};

struct IdentifierValue {
	Value base;
	Identifier *identifier;
};

// Types.
struct Type {
	TypeKind kind;
};

struct ProcedureType {
	Type base;
	ProcedureKind kind;
	ProcedureFlag flags;
	CallingConvention convention;
	Array(Field*) params;
};

struct ConcreteProcedureType {
	ProcedureType base;
};

struct GenericProcedureType {
	ProcedureType base;
};

// Misc.
struct Identifier {
	String contents;
	Bool poly;
};

struct Field {
	Identifier *name; // Always present
	Identifier *type; // Optional.
	Expression *value;
};

inline String calling_convention_to_string(CallingConvention cc) {
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
	String package;
	Array(Statement*) statements;
};

void tree_init(Tree *tree, Context *context);
void tree_dump(Tree *tree);

ListExpression *tree_new_list_expression(Tree *tree, Array(Expression*) expressions);
UnaryExpression *tree_new_unary_expression(Tree *tree, OperatorKind operation, Expression *operand);
BinaryExpression *tree_new_binary_expression(Tree *tree, OperatorKind operation, Expression *lhs, Expression *rhs);
SelectorExpression *tree_new_selector_expression(Tree *tree, Expression *operand, Identifier *identifier);
CallExpression *tree_new_call_expression(Tree *tree, Expression *operand, Array(Expression*) arguments);
AssertionExpression *tree_new_assertion_expression(Tree *tree, Expression *operand, Identifier *type);
ValueExpression *tree_new_value_expression(Tree *tree, Value *value);
ProcedureExpression *tree_new_procedure_expression(Tree *tree, ProcedureFlag flags, ProcedureType *type, ListExpression *where_clauses, BlockStatement *body);
OrReturnExpression *tree_new_or_return_expression(Tree *tree, Expression *operand);

EmptyStatement *tree_new_empty_statement(Tree *tree);
ImportStatement *tree_new_import_statement(Tree *tree, String name, String package);
ExpressionStatement *tree_new_expression_statement(Tree *tree, Expression *expression);
BlockStatement *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Statement*) statements);
AssignmentStatement *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, ListExpression *lhs, ListExpression *rhs);
DeclarationStatement *tree_new_declaration_statement(Tree *tree, Identifier *type, Array(Identifier*) names, ListExpression *values);
IfStatement *tree_new_if_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, BlockStatement *elif);
ForStatement *tree_new_for_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, Statement *post);
ReturnStatement *tree_new_return_statement(Tree *tree, Array(Expression*) results);
DeferStatement *tree_new_defer_statement(Tree *tree, Statement *stmt);
BranchStatement *tree_new_branch_statement(Tree *tree, KeywordKind branch, Identifier *label);

// Values should be usable in constant contexts provided everything inside them is constant.
LiteralValue *tree_new_literal_value(Tree *tree, LiteralKind kind, String value);
CompoundLiteralValue *tree_new_compound_literal_value(Tree *tree, Expression *expression, Array(Expression*) expressions);
IdentifierValue *tree_new_identifier_value(Tree *tree, Identifier *identifier);

Identifier *tree_new_identifier(Tree *tree, String contents);

// The first actual type!
ConcreteProcedureType *tree_new_concrete_procedure_type(Tree *tree, Array(Field*) params, ProcedureFlag flags, CallingConvention convention);
GenericProcedureType *tree_new_generic_procedure_type(Tree *tree, Array(Field*) params, ProcedureFlag flags, CallingConvention convention);

Field *tree_new_field(Tree *tree, Identifier *type, Identifier *name, Expression *value);

#endif // CODIN_TREE_H