#ifndef CODIN_LEXER_H
#define CODIN_LEXER_H
#include "string.h"

typedef struct Context Context;

typedef struct Lexer Lexer;
typedef struct Token Token;
typedef struct Location Location;
typedef struct Source Source;
typedef struct Input Input;

typedef enum Kind Kind;
typedef enum LiteralKind LiteralKind;
typedef enum OperatorKind OperatorKind;
typedef enum KeywordKind KeywordKind;
typedef enum AssignmentKind AssignmentKind;
typedef enum DirectiveKind DirectiveKind;

#define KIND(kind, ...) KIND_ ## kind,
enum Kind {
	#include "lexemes.h"
	KIND_COUNT,
};
String kind_to_string(Kind kind);

#define LITERAL(kind, ...) LITERAL_ ## kind,
enum LiteralKind {
	#include "lexemes.h"
	LITERAL_COUNT,
};
String literal_to_string(LiteralKind literal);

#define OPERATOR(kind, ...) OPERATOR_ ## kind,
enum OperatorKind {
	#include "lexemes.h"
	OPERATOR_COUNT,
};
String operator_to_string(OperatorKind op);

#define KEYWORD(kind, ...) KEYWORD_ ## kind,
enum KeywordKind {
	#include "lexemes.h"
	KEYWORD_COUNT,
};
String keyword_to_string(KeywordKind keyword);

#define ASSIGNMENT(kind, ...) ASSIGNMENT_ ## kind,
enum AssignmentKind {
	#include "lexemes.h"
	ASSIGNMENT_COUNT,
};

String assignment_to_string(AssignmentKind assignment);

#define DIRECTIVE(kind, ...) DIRECTIVE_ ## kind,
enum DirectiveKind {
	#include "lexemes.h"
	DIRECTIVE_COUNT,
};

String directive_to_string(DirectiveKind directive);

struct Source {
	String name;
	String contents;
};

extern const Source SOURCE_NIL;

struct Location {
	int column;
	int line;
};

#define LOCATION_NIL { 0, 0 }

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

_Static_assert(sizeof(Token) <= 64, "Too big");

String token_to_string(Token token);

struct Input {
	const Source *source;
	Uint8 *cur;
	const Uint8 *end;
};

struct Lexer {
	Context *context;
	Input input;
	Location location; 
	const Uint8 *here;
	Rune rune;
	Bool asi;
	Token peek;
};

Bool lexer_init(Lexer *lexer, Context *context, const Source *source);
Token lexer_next(Lexer *lexer);
Token lexer_peek(Lexer *lexer);

#endif // CODIN_LEXER_H