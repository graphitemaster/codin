#ifndef CODIN_TREE_H
#define CODIN_TREE_H
#include "array.h"
#include "lexer.h"

typedef struct Context Context;

typedef struct Tree Tree;
typedef struct Node Node;

typedef struct Expression Expression;
typedef struct UnaryExpression UnaryExpression;
typedef struct BinaryExpression BinaryExpression;
typedef struct CastExpression CastExpression;
typedef struct SelectorExpression SelectorExpression;
typedef struct CallExpression CallExpression;
typedef struct AssertionExpression AssertionExpression;
typedef struct InExpression InExpression;
typedef struct DereferenceExpression DereferenceExpression;

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

typedef struct Type Type;
typedef struct SliceType SliceType;
typedef struct ArrayType ArrayType;
typedef struct DynamicArrayType DynamicArrayType;
typedef struct PointerType PointerType;
typedef struct MultiPointerType MultiPointerType;
typedef struct TypeIdType TypeIdType;

typedef struct Identifier Identifier;
typedef struct Value Value;
typedef struct LiteralValue LiteralValue;
typedef struct CompoundLiteral CompoundLiteral;
typedef struct Field Field;
typedef struct FieldList FieldList;
typedef struct Procedure Procedure;
typedef struct ProcedureType ProcedureType;
typedef struct Directive Directive;
typedef struct ProcedureGroup ProcedureGroup;

enum NodeKind {
	NODE_EXPRESSION,
	NODE_STATEMENT,
	NODE_IDENTIFIER,
	NODE_VALUE,
	NODE_LITERAL_VALUE,
	NODE_COMPOUND_LITERAL,
	NODE_FIELD,
	NODE_FIELD_LIST,
	NODE_PROCEDURE,
	NODE_PROCEDURE_GROUP,
	NODE_DIRECTIVE,
	NODE_TYPE,
};

enum ExpressionKind {
	EXPRESSION_UNARY,
	EXPRESSION_BINARY,
	EXPRESSION_CAST,
	EXPRESSION_SELECTOR,
	EXPRESSION_CALL,
	EXPRESSION_ASSERTION,
	EXPRESSION_IN,
	EXPRESSION_DEREFERENCE,
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

enum TypeKind {
	TYPE_PROCEDURE,      // proc
	TYPE_SLICE,          // []T
	TYPE_ARRAY,          // [N]T, or [?]T
	TYPE_DYNAMIC_ARRAY,  // [dynamic]T
	TYPE_POINTER,        // ^
	TYPE_MULTI_POINTER,  // [^]
	TYPE_TYPEID,         // typeid
};

typedef enum NodeKind NodeKind;
typedef enum ExpressionKind ExpressionKind;
typedef enum StatementKind StatementKind;
typedef enum TypeKind TypeKind;

struct UnaryExpression {
	OperatorKind operation;
	Node *operand;
};

struct BinaryExpression {
	OperatorKind operation;
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

struct InExpression {
	Array(Node) *lhs;
	Node *rhs;
};

struct DereferenceExpression {
	Node *operand;
};

struct Expression {
	ExpressionKind kind;
	union {
		UnaryExpression       unary;
		BinaryExpression      binary;
		CastExpression        cast;
		SelectorExpression    selector;
		CallExpression        call;
		AssertionExpression   assertion;
		InExpression          in;
		DereferenceExpression dereference;
	};
};

struct EmptyStatement {
	Sint32 : 32;
};

struct ImportStatement {
	String name;
	String package;
};

struct ExpressionStatement {
	Node *expression;
};

enum BlockFlag {
	BLOCK_FLAG_BOUNDS_CHECK = 1 << 0,
	BLOCK_FLAG_TYPE_ASSERT  = 1 << 1,
};

typedef enum BlockFlag BlockFlag;

struct BlockStatement {
	BlockFlag flags;
	Array(Node*) statements;
};

struct AssignmentStatement {
	AssignmentKind assignment;
	Array(Node*) lhs;
	Array(Node*) rhs;
};

struct DeclarationStatement {
	Node *type;
	Array(Node*) names;
	Array(Node*) values;
};

struct IfStatement {
	Node *init;
	Node *cond;
	Node *body;
	Node *elif;
};

struct ReturnStatement {
	Array(Node*) results;
};

struct ForStatement {
	Node *init;
	Node *cond;
	Node *body;
	Node *post;
};

struct DeferStatement {
	Node *statement;
};

struct BreakStatement {
	Uint32 : 32;
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
		IfStatement          if_;
		ReturnStatement      return_;
		ForStatement         for_;
		DeferStatement       defer;
		BreakStatement       break_;
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
	LiteralKind literal;
	String value;
};

struct CompoundLiteral {
	Node *type;
	Array(Node*) elements;
};

struct Field {
	Node *name;
	Node *type;
	// TODO(dweiler): default value, flags, etc.
};

struct FieldList {
	Array(Node*) fields;
};

enum ProcedureFlag {
	PROC_FLAG_DIVERGING                 = 1 << 0,
	PROC_FLAG_OPTIONAL_OK               = 1 << 1,
	PROC_FLAG_OPTIONAL_ALLOCATION_ERROR = 1 << 2,
	PROC_FLAG_BOUNDS_CHECK              = 1 << 3,
	PROC_FLAG_TYPE_ASSERT               = 1 << 4,
	PROC_FLAG_FORCE_INLINE              = 1 << 5,
};

typedef enum ProcedureFlag ProcedureFlag;

struct Procedure {
	ProcedureFlag flags;
	Node *type;
	Node *body;
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

struct ProcedureGroup {
	Array(Node*) procedures;
};

struct Directive {
	DirectiveKind kind;
};

struct ProcedureType {
	Node *params;  // FieldList
	Node *results; // FieldList
	Uint64 flags;
	CallingConvention convention;
};

struct SliceType {
	Node *type;
};

struct ArrayType {
	Node *count;
	Node *type;
};

struct DynamicArrayType {
	Node *type;
};

struct PointerType {
	Node *type;
};

struct MultiPointerType {
	Node *type;
};

struct TypeIdType {
	Sint32 : 32;
};

struct Type {
	TypeKind kind;
	union {
		ProcedureType    procedure;
		SliceType        slice;
		ArrayType        array;
		DynamicArrayType dynamic_array;
		PointerType      pointer;
		MultiPointerType multi_pointer;
		TypeIdType       typeid_;
	};
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
		Field           field;
		FieldList       field_list;
		Procedure       procedure;
		ProcedureGroup  procedure_group;
		Directive      	directive;
		Type            type;
	};
};

Bool node_is_literal(const Node *node);

FORCE_INLINE Bool node_is_kind(const Node *node, NodeKind kind) {
	return node && node->kind == kind;
}

FORCE_INLINE Bool node_is_statement(const Node *node, StatementKind kind) {
	return node_is_kind(node, NODE_STATEMENT) && node->statement.kind == kind;
}

FORCE_INLINE Bool node_is_expression(const Node *node, ExpressionKind kind) {
	return node_is_kind(node, NODE_EXPRESSION) && node->expression.kind == kind;
}

FORCE_INLINE Bool node_is_type(const Node *node, TypeKind kind) {
	return node_is_kind(node, NODE_TYPE) && node->type.kind == kind;
}

STATIC_ASSERT(sizeof(Node) <= 64, "Too big");

struct Tree {
	Context *context;
	String package;
	Array(Node*) nodes;
	Array(Node*) statements;
};

Node *tree_new_unary_expression(Tree *tree, OperatorKind operation, Node *operand);
Node *tree_new_binary_expression(Tree *tree, OperatorKind operation, Node *lhs, Node *rhs);
Node *tree_new_cast_expression(Tree *tree, Node *type, Node *expr);
Node *tree_new_selector_expression(Tree *tree, Node *operand, Node *identifier);
Node *tree_new_call_expression(Tree *tree, Node *operand, Array(Node*) arguments);
Node *tree_new_assertion_expression(Tree *tree, Node *operand, Node *type);
Node *tree_new_in_expression(Tree *tree, Array(Node*) lhs, Node *rhs);
Node *tree_new_dereference_expression(Tree *tree, Node *opernad);

Node *tree_new_empty_statement(Tree *tree);
Node *tree_new_import_statement(Tree *tree, String name, String package);
Node *tree_new_expression_statement(Tree *tree, Node *expression);
Node *tree_new_block_statement(Tree *tree, BlockFlag flags, Array(Node*) statements);
Node *tree_new_assignment_statement(Tree *tree, AssignmentKind assignment, Array(Node*) lhs, Array(Node*) rhs);
Node *tree_new_declaration_statement(Tree *tree, Node *type, Array(Node*) names, Array(Node*) values);
Node *tree_new_if_statement(Tree *tree, Node *init, Node *condition, Node *body, Node *elif);
Node *tree_new_for_statement(Tree *tree, Node *init, Node *cond, Node *body, Node *post);
Node *tree_new_return_statement(Tree *tree, Array(Node*) results);
Node *tree_new_defer_statement(Tree *tree, Node *statement);
Node *tree_new_break_statement(Tree *tree);

Node *tree_new_procedure_type(Tree *tree, Node *params, Node* results, Uint64 flags, CallingConvention convention);
Node *tree_new_slice_type(Tree *tree, Node *type);
Node *tree_new_array_type(Tree *tree, Node *count, Node *type);
Node *tree_new_dynamic_array_type(Tree *tree, Node *type);
Node *tree_new_pointer_type(Tree *tree, Node *type);
Node *tree_new_multi_pointer_type(Tree *tree, Node *type);
Node *tree_new_typeid_type(Tree *node);

Node *tree_new_identifier(Tree *tree, String contents);
Node *tree_new_value(Tree *tree, Node *field, Node *val);
Node *tree_new_literal_value(Tree *tree, LiteralKind literal, String value);
Node *tree_new_compound_literal(Tree *tree, Node *type, Array(Node*) elements);
Node *tree_new_field(Tree *tree, Node* name, Node *type);
Node *tree_new_field_list(Tree *tree, Array(Node*) list);

Node *tree_new_procedure(Tree *tree, ProcedureFlag flags, Node *type, Node *body);
Node *tree_new_procedure_group(Tree *tree, Array(Node*) procedures);
Node *tree_new_directive(Tree *tree, DirectiveKind directive);

void tree_init(Tree *tree, Context *context);

void tree_dump(Tree *tree);
void tree_dump_node(const Node *node, Sint32 depth);

#endif // CODIN_TREE_H