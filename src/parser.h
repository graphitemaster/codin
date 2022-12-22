#ifndef CODIN_PARSER_H
#define CODIN_PARSER_H
#include "array.h"
#include "lexer.h"

typedef struct Tree Tree;
typedef struct Leaf Leaf;

typedef enum Node Node;

enum Node {
	NODE_UNARY_EXPRESSION,             // -expr, +expr, !expr
	NODE_BINARY_EXPRESSION,            // x op y
	NODE_CAST_EXPRESSION,              // cast(T)expr
	NODE_SELECTOR_EXPRESSION,          // a.b
	NODE_IMPLICIT_SELECTOR_EXPRESSION, // .A
	NODE_AUTO_CAST_EXPRESSION,         // auto_cast expr
	NODE_CALL_EXPRESSION,              // a()

	NODE_EMPTY_STATEMENT,              // _
	NODE_BLOCK_STATEMENT,              // {}
	NODE_IMPORT_STATEMENT,             // import
	NODE_EXPRESSION_STATEMENT,         // expression;
	NODE_ASSIGNMENT_STATEMENT,         // =, +=, ...

	NODE_IDENTIFIER,
	NODE_FIELD_VALUE,
	NODE_LITERAL,
	NODE_COMPOUND_LITERAL,
};

struct Tree {
	Array(Leaf*) leafs;
	Array(Leaf*) statements;
};

struct Leaf {
	Node node;
	union {
		struct {
			Operator op;
			Leaf *expression;
		} as_unary_expression;
		struct {
			Operator op;
			Leaf *lhs;
			Leaf *rhs;
		} as_binary_expression;
		struct {
			Leaf *type;
			Leaf *expression;
		} as_cast_expression;
		struct {
			Leaf *operand;
			Leaf *identifier;
		} as_selector_expression;
		struct {
			Leaf *identifier;
		} as_implicit_selector_expression;
		struct {
			Leaf *expression;
		} as_auto_cast_expression;
		struct {
			Leaf *expression;
		} as_expression_statement;
		struct {
			Array(Leaf*) statements;
		} as_block_statement;
		struct {
			Kind kind;
			Array(Leaf*) lhs;
			Array(Leaf*) rhs;
		} as_assignment_statement;
		struct {
			String import;
		} as_import_statement;
		struct {
			Leaf *operand;
			Array(Leaf*) args;
		} as_call_expression;
		struct {
			String contents;
		} as_identifier;
		struct {
			Leaf *field;
			Leaf *value;
		} as_field_value;
		struct {
			Literal literal;
			String contents;
		} as_literal;
		struct {
			Leaf *type;
			Array(Leaf*) elements;
		} as_compound_literal;
	};
};

Tree* parse(const char *filename);
void tree_free(Tree *tree);
void tree_print(Tree *tree);

#endif // CODIN_PARSER_H