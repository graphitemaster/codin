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

typedef struct Statement Statement;
typedef struct EmptyStatement EmptyStatement;
typedef struct ImportStatement ImportStatement;
typedef struct ExpressionStatement ExpressionStatement;
typedef struct BlockStatement BlockStatement;
typedef struct AssignmentStatement AssignmentStatement;

typedef struct Identifier Identifier;
typedef struct Declaration Declaration;
typedef struct Value Value;
typedef struct LiteralValue LiteralValue;
typedef struct CompoundLiteral CompoundLiteral;

enum NodeKind {
	NODE_EXPRESSION,
	NODE_STATEMENT,
	NODE_IDENTIFIER,
	NODE_DECLARATION,
	NODE_VALUE,
	NODE_LITERAL_VALUE,
	NODE_COMPOUND_LITERAL,
};

enum ExpressionKind {
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_CAST,
	EXPRESSION_SELECTOR,
	EXPRESSION_CALL,
};

enum StatementKind {
	STATEMENT_EMPTY,
	STATEMENT_BLOCK,
	STATEMENT_IMPORT,
	STATEMENT_EXPRESSION,
	STATEMENT_ASSIGNMENT,
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

struct Expression {
	ExpressionKind kind;
	union {
		UnaryExpression    unary;
		BinaryExpression   binary;
		CastExpression     cast;
		SelectorExpression selector;
		CallExpression     call;
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

struct Statement {
	StatementKind kind;
	union {
		EmptyStatement      empty;
		ImportStatement     import;
		ExpressionStatement expression;
		BlockStatement      block;
		AssignmentStatement assignment;
	};
};

struct Identifier {
	String contents;
};

struct Declaration {
	Node *type;
	Array(Node*) names;
	Array(Node*) values;
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

struct Node {
	NodeKind kind;
	union {
		Expression      expression;
		Statement       statement;
		Identifier      identifier;
		Declaration     declaration;
		Value           value;
		LiteralValue    literal_value;
		CompoundLiteral compound_literal;
	};
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
Node *tree_new_empty_statement(Tree *tree);
Node *tree_new_import_statement(Tree *tree, String package);
Node *tree_new_expression_statement(Tree *tree, Node *expression);
Node *tree_new_block_statement(Tree *tree, Array(Node*) statements);
Node *tree_new_import_statement(Tree *tree, String package);
Node *tree_new_assignment_statement(Tree *tree, Assignment assignment, Array(Node*) lhs, Array(Node*) rhs);
Node *tree_new_identifier(Tree *tree, String contents);
Node *tree_new_declaration(Tree *tree, Node *type, Array(Node*) names, Array(Node*) values);
Node *tree_new_value(Tree *tree, Node *field, Node *val);
Node *tree_new_literal_value(Tree *tree, Literal literal, String value);
Node *tree_new_compound_literal(Tree *tree, Node *type, Array(Node*) elements);

void tree_init(Tree *tree);
void tree_free(Tree *tree);

void tree_dump(Tree *tree);

#endif // CODIN_TREE_H