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
typedef struct IdentifierExpression IdentifierExpression;
typedef struct ProcedureExpression ProcedureExpression;

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
typedef struct BreakStatement BreakStatement;

// Values.
typedef struct Value Value;
typedef struct LiteralValue LiteralValue;
typedef struct CompoundLiteralValue CompoundLiteralValue;
typedef struct ExpressionValue ExpressionValue;

// Cleanup.
typedef struct Identifier Identifier;
typedef struct ProcedureType ProcedureType;

enum ExpressionKind {
	EXPRESSION_LIST,
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_CAST,
	EXPRESSION_SELECTOR,
	EXPRESSION_CALL,
	EXPRESSION_ASSERTION,
	EXPRESSION_VALUE,
	EXPRESSION_IDENTIFIER,
	EXPRESSION_PROCEDURE,
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
	STATEMENT_BREAK,
};

enum ValueKind {
	VALUE_LITERAL,
	VALUE_COMPOUND_LITERAL,
	VALUE_EXPRESSION, // TODO(dweiler): Remove?
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

typedef enum ExpressionKind ExpressionKind;
typedef enum StatementKind StatementKind;
typedef enum ValueKind ValueKind;
typedef enum TypeKind TypeKind;

typedef enum BlockFlag BlockFlag;
typedef enum ProcedureFlag ProcedureFlag;

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

struct IdentifierExpression {
	Expression base;
	Identifier *identifier;
};

struct ProcedureExpression {
	Expression base;
	ProcedureFlag flags;
	ProcedureType *type;
	BlockStatement *body;
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

struct BreakStatement {
	Statement base;
};

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

struct ExpressionValue {
	Value base;
	Expression *expression;
};

// Identifier
struct Identifier {
	String contents;
};

enum CallingConvention {
	CCONV_INVALID,
	CCONV_ODIN,
	CCONV_CONTEXTLESS,
	CCONV_CDECL,
	CCONV_STDCALL,
	CCONV_FASTCALL,
	CCONV_NAKED,
	CCONV_NONE,
};

typedef enum CallingConvention CallingConvention;

struct Directive {
	DirectiveKind kind;
};

struct ProcedureType {
	Uint64 flags;
	CallingConvention convention;
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
IdentifierExpression *tree_new_identifier_expression(Tree *tree, Identifier *identifier);
ProcedureExpression *tree_new_procedure_expression(Tree *tree, ProcedureFlag flags, ProcedureType *type, BlockStatement *body);

EmptyStatement *tree_new_empty_statement(Tree *tree);
ImportStatement *tree_new_import_statement(Tree *tree, String name, String package);
ExpressionStatement *tree_new_expression_statement(Tree *tree, Expression *expression);
BlockStatement *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Statement*) statements);
AssignmentStatement *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, ListExpression *lhs, ListExpression *rhs);
DeclarationStatement *tree_new_declaration_statement(Tree *tree, Identifier *type, Array(Identifier*) names, ListExpression *values);
IfStatement *tree_new_if_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, BlockStatement *elif);
ForStatement *tree_new_for_statement(Tree *tree, Statement *init, Expression *cond, BlockStatement *body, Statement *post);
ReturnStatement *tree_new_return_statement(Tree *tree, Array(Expression*) results);
BreakStatement *tree_new_break_statement(Tree *tree);
DeferStatement *tree_new_defer_statement(Tree *tree, Statement *stmt);

LiteralValue *tree_new_literal_value(Tree *tree, LiteralKind kind, String value);
CompoundLiteralValue *tree_new_compound_literal_value(Tree *tree, Expression *expression, Array(Expression*) expressions);
ExpressionValue *tree_new_expression_value(Tree *tree, Expression *expression);

Identifier *tree_new_identifier(Tree *tree, String contents);

// The first actual type!
ProcedureType *tree_new_procedure_type(Tree *tree, void *params, void *results, Uint64 flags, CallingConvention convention);

#endif // CODIN_TREE_H