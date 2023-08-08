#ifndef CODIN_LEXER_H
#define CODIN_LEXER_H
#include "string.h"
#include "array.h"

typedef struct Context Context;

typedef struct Lexer Lexer;
typedef struct Token Token;
typedef struct Location Location;
typedef struct Source Source;
typedef struct Input Input;

#define KIND(kind, ...) KIND_ ## kind,
enum Kind {
	#include "lexemes.h"
	KIND_COUNT,
};
typedef enum Kind Kind;
String kind_to_string(Kind kind);

#define LITERAL(kind, ...) LITERAL_ ## kind,
enum LiteralKind {
	#include "lexemes.h"
	LITERAL_COUNT,
};
typedef enum LiteralKind LiteralKind;
String literal_to_string(LiteralKind literal);

#define OPERATOR(kind, ...) OPERATOR_ ## kind,
enum OperatorKind {
	#include "lexemes.h"
	OPERATOR_COUNT,
};
typedef enum OperatorKind OperatorKind;
String operator_to_string(OperatorKind op);

#define KEYWORD(kind, ...) KEYWORD_ ## kind,
enum KeywordKind {
	#include "lexemes.h"
	KEYWORD_COUNT,
};
typedef enum KeywordKind KeywordKind;
String keyword_to_string(KeywordKind keyword);

#define ASSIGNMENT(kind, ...) ASSIGNMENT_ ## kind,
enum AssignmentKind {
	#include "lexemes.h"
	ASSIGNMENT_COUNT,
};
typedef enum AssignmentKind AssignmentKind;
String assignment_to_string(AssignmentKind assignment);

#define DIRECTIVE(kind, ...) DIRECTIVE_ ## kind,
enum DirectiveKind {
	#include "lexemes.h"
	DIRECTIVE_COUNT,
};

typedef enum DirectiveKind DirectiveKind;

String directive_to_string(DirectiveKind directive);

struct Source {
	String name;
	String contents;
};

struct Location {
	int column;
	int line;
};

struct Token {
	Kind kind;
	Location location;
	String string; // comment, identifier
	union {
		LiteralKind as_literal;
		OperatorKind as_operator;
		KeywordKind as_keyword;
		AssignmentKind as_assignment;
		DirectiveKind as_directive;
	};
};

extern const Token TOKEN_NIL;

STATIC_ASSERT(sizeof(Token) <= 64, "Too big");

String token_to_string(Token token);

struct Input {
	const Source *source;
	Uint8 *cur;
	const Uint8 *end;
};

struct Lexer {
	Context *context;
	Input input;
	Location this_location; 
	Location last_location;
	const Uint8 *here;
	Rune rune;
	Bool asi;
	Array(Token) peek;
};

Bool lexer_init(Lexer *lexer, Context *context, const Source *source);
Token lexer_next(Lexer *lexer);
Token lexer_peek(Lexer *lexer);

#endif // CODIN_LEXER_H