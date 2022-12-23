#ifndef CODIN_TREE_H
#define CODIN_TREE_H
#include "array.h"
#include "lexer.h"

typedef enum NodeKind NodeKind;
typedef enum ExpressionKind ExpressionKind;
typedef enum StatementKind StatementKind;

typedef struct Tree Tree;
typedef struct Node Node;

typedef struct Expression Expression;
typedef struct UnaryExpression UnaryExpression;
typedef struct BinaryExpression BinaryExpression;
typedef struct CastExpression CastExpression;
typedef struct SelectorExpression SelectorExpression;
typedef struct CallExpression CallExpression;
typedef struct AssertionExpression AssertionExpression;

typedef struct Statement Statement;
typedef struct EmptyStatement EmptyStatement;
typedef struct ImportStatement ImportStatement;
typedef struct ExpressionStatement ExpressionStatement;
typedef struct BlockStatement BlockStatement;
typedef struct AssignmentStatement AssignmentStatement;
typedef struct DeclarationStatement DeclarationStatement;

typedef struct Identifier Identifier;
typedef struct Value Value;
typedef struct LiteralValue LiteralValue;
typedef struct CompoundLiteral CompoundLiteral;
typedef struct FieldList FieldList;

enum NodeKind {
	NODE_EXPRESSION,
	NODE_STATEMENT,
	NODE_IDENTIFIER,
	NODE_VALUE,
	NODE_LITERAL_VALUE,
	NODE_COMPOUND_LITERAL,
	NODE_FIELD_LIST,
};

enum ExpressionKind {
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_CAST,
	EXPRESSION_SELECTOR,
	EXPRESSION_CALL,
	EXPRESSION_ASSERTION,
};

enum StatementKind {
	STATEMENT_EMPTY,
	STATEMENT_BLOCK,
	STATEMENT_IMPORT,
	STATEMENT_EXPRESSION,
	STATEMENT_ASSIGNMENT,
	STATEMENT_DECLARATION,
};

struct UnaryExpression {
	Operator operation;
	Node *operand;
};

struct BinaryExpression {
	Operator operation;
	Node *lhs;
	Node *rhs;
};

struct CastExpression {
	Node *type; // When nullptr this is an implicit cast expression.
	Node *expression;
};

struct SelectorExpression {
	Node *operand; // When nullptr this is an implicit selector expression.
	Node *identifier;
};

struct CallExpression {
	Node *operand;
	Array(Node*) arguments;
};

struct AssertionExpression {
	Node *operand;
	Node *type;
};

struct Expression {
	ExpressionKind kind;
	union {
		UnaryExpression     unary;
		BinaryExpression    binary;
		CastExpression      cast;
		SelectorExpression  selector;
		CallExpression      call;
		AssertionExpression assertion;
	};
};

struct EmptyStatement {
	// Empty, shocker.
};

struct ImportStatement {
	String package;
};

struct ExpressionStatement {
	Node *expression;
};

struct BlockStatement {
	Array(Node*) statements;
};

struct AssignmentStatement {
	Assignment assignment;
	Array(Node*) lhs;
	Array(Node*) rhs;
};

struct DeclarationStatement {
	Node *type;
	Array(Node*) names;
	Array(Node*) values;
};

struct Statement {
	StatementKind kind;
	union {
		EmptyStatement       empty;
		ImportStatement      import;
		ExpressionStatement  expression;
		BlockStatement       block;
		AssignmentStatement  assignment;
		DeclarationStatement declaration;
	};
};

struct Identifier {
	String contents;
};

struct Value {
	Node *field;
	Node *value;
};

struct LiteralValue {
	Literal literal;
	String value;
};

struct CompoundLiteral {
	Node *type;
	Array(Node*) elements;
};

struct FieldList {
	Array(Node*) fields;
};

struct Node {
	NodeKind kind;
	union {
		Expression      expression;
		Statement       statement;
		Identifier      identifier;
		Value           value;
		LiteralValue    literal_value;
		CompoundLiteral compound_literal;
		FieldList       field_list;
	};
};

typedef enum CallingConvention CallingConvention;

enum CallingConvention {
	CCONV_INVALID,
	CCONV_ODIN,
	CCONV_CONTEXTLESS,
	CCONV_CDECL,
	CCONV_STDCALL,
	CCONV_FASTCALL,
	CCONV_NONE,
	CCONV_NAKED,
};

_Static_assert(sizeof(Node) <= 64, "Too big");

struct Tree {
	Array(Node*) nodes;
	Array(Node*) statements;
};

Node *tree_new_unary_expression(Tree *tree, Operator operation, Node *operand);
Node *tree_new_binary_expression(Tree *tree, Operator operation, Node *lhs, Node *rhs);
Node *tree_new_cast_expression(Tree *tree, Node *type, Node *expr);
Node *tree_new_selector_expression(Tree *tree, Node *operand, Node *identifier);
Node *tree_new_call_expression(Tree *tree, Node *operand, Array(Node*) arguments);
Node *tree_new_assertion_expression(Tree *tree, Node *operand, Node *type);
Node *tree_new_empty_statement(Tree *tree);
Node *tree_new_import_statement(Tree *tree, String package);
Node *tree_new_expression_statement(Tree *tree, Node *expression);
Node *tree_new_block_statement(Tree *tree, Array(Node*) statements);
Node *tree_new_import_statement(Tree *tree, String package);
Node *tree_new_assignment_statement(Tree *tree, Assignment assignment, Array(Node*) lhs, Array(Node*) rhs);
Node *tree_new_declaration_statement(Tree *tree, Node *type, Array(Node*) names, Array(Node*) values);
Node *tree_new_identifier(Tree *tree, String contents);
Node *tree_new_value(Tree *tree, Node *field, Node *val);
Node *tree_new_literal_value(Tree *tree, Literal literal, String value);
Node *tree_new_compound_literal(Tree *tree, Node *type, Array(Node*) elements);
Node *tree_new_field_list(Tree *tree, Array(Node*) list);

void tree_init(Tree *tree);
void tree_free(Tree *tree);

void tree_dump(Tree *tree);

#endif // CODIN_TREE_H