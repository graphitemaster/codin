#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "lexer.h"
#include "support.h"
#include "report.h"
#include "context.h"

const Token TOKEN_NIL = { KIND_INVALID, { 0, 0 }, { 0, 0 }, { CAST(LiteralKind, 0), } };
const Source SOURCE_NIL = { { 0, 0 }, { 0, 0 } };

#define LEX_ERROR(...) \
	do { \
		report_error(lexer->input.source, &lexer->location, __VA_ARGS__); \
		THROW(1); \
	} while (0)

// Searches for a keyword
static Bool keyword_find(String string, KeywordKind *result) {
	#define KEYWORD(ident, content, ...) \
		if (string_compare(string, SCLIT(content))) { \
			*result = KEYWORD_ ## ident; \
			return true; \
		}
	#include "lexemes.h"
	return false;
}

// Searches for a operator
static Bool operator_find(String string, OperatorKind *result) {
	#define OPERATOR(ident, content, ...) \
		if (string_compare(string, SCLIT(content))) { \
			*result = OPERATOR_ ## ident; \
			return true; \
		}
	#include "lexemes.h"
	return false;
}

static Bool directive_find(String string, DirectiveKind *result) {
	#define DIRECTIVE(ident, content, ...) \
		if (string_compare(string, SCLIT(content))) { \
			*result = DIRECTIVE_ ## ident; \
			return true; \
		}
	#include "lexemes.h"
	return false;
}

static Bool is_char(Rune ch) {
	if (ch < 0x80) {
		if (ch == '_') {
			return true;
		}
		return ((CAST(Uint32, ch) | 0x20) - 0x61) < 26; // [a-z][A-Z]
	}
	// TODO(dweiler): UTF-8.
	return false;
}

static Bool is_digit(Rune ch) {
	if (ch < 0x80) {
		return (CAST(Uint32, ch) - '0') < 10;
	}
	// TODO(dweiler): UTF-8.
	return false;
}

static Uint8 peekl(Lexer *lexer) {
	const Input *input = &lexer->input;
	if (input->cur < input->end) {
		return *input->cur;
	}
	return 0;
}

static void advancel(Lexer *lexer) {
	Context *context = lexer->context;
	if (lexer->rune == '\n') {
		lexer->location.column = 1;
		lexer->location.line++;
	}
	Input *input = &lexer->input;
	if (input->cur < input->end) {
		lexer->here = input->cur;
		const Rune rune = *input->cur;
		if (rune == 0) {
			LEX_ERROR("Unexpected EOF");
			input->cur++;
		} else if (rune & 0x80) {
			// TODO(dweiler): UTF-8.
			// input->cur += utf8_len(input->end - input->cur);
		} else {
			input->cur++;
		}
		lexer->rune = rune;
		lexer->location.column++;
	} else {
		lexer->here = input->end;
		lexer->rune = RUNE_EOF;
	}
}

static void skip_line(Lexer *lexer) {
	while (lexer->rune != '\n' && lexer->rune != RUNE_EOF) {
		advancel(lexer);
	}
}

static void skip_whitespace(Lexer *lexer, Bool newline) {
	for (;;) {
		const Rune rune = lexer->rune;
		if (rune == ' ' || rune == '\t' || rune == '\r' || (!newline && rune == '\n')) {
			advancel(lexer);
		} else {
			break;
		}
	}
}

static Sint32 numeric_base(Rune ch) {
	/**/ if (ch >= '0' && ch <= '9') return ch - '0';
	else if (ch >= 'a' && ch <= 'f') return ch - 'a' + 10;
	else if (ch >= 'A' && ch <= 'F') return ch - 'A' + 10;
	return 16;
}

static void scan(Lexer* lexer, Sint32 base) {
	while (numeric_base(lexer->rune) < base || lexer->rune == '_') {
		advancel(lexer);
	}
}

static Token scan_numeric(Lexer *lexer, Bool dot) {
	Token token;
	token.kind = KIND_LITERAL;
	token.string.contents = CCAST(Uint8*, lexer->here);
	token.string.length = 1;
	token.as_literal = LITERAL_INTEGER;
	token.location = lexer->location;

	if (dot) {
		token.as_literal = LITERAL_FLOAT;
		token.string.contents--;
		token.string.length++;
		token.location.column--;
		scan(lexer, 10);
		goto L_exponent;
	}

	if (lexer->rune == '0') {
		advancel(lexer);
		switch (lexer->rune) {
		case 'b':
			advancel(lexer);
			scan(lexer, 2);
			break;
		case 'o':
			advancel(lexer);
			scan(lexer, 8);
			break;
		case 'd':
			advancel(lexer);
			scan(lexer, 10);
			break;
		case 'z':
			advancel(lexer);
			scan(lexer, 12);
			break;
		case 'x':
			advancel(lexer);
			scan(lexer, 16);
			break;
		case 'h':
			advancel(lexer);
			scan(lexer, 16);
			break;
		default:
			break;
		}
	}

	scan(lexer, 10);

	if (lexer->rune == '.') {
		// .. is an operator
		if (peekl(lexer) == '.') {
			token.string.length = lexer->here - token.string.contents;
			return token;
		}

		advancel(lexer);
		token.as_literal = LITERAL_FLOAT;
		scan(lexer, 10);
	}

L_exponent:
	// Handle 'e' and 'E' exponents.
	if (lexer->rune == 'e' || lexer->rune == 'E') {
		advancel(lexer);
		token.as_literal = LITERAL_FLOAT;
		if (lexer->rune == '-' || lexer->rune == '+') {
			advancel(lexer);
		}
		scan(lexer, 10);
	}

	if (lexer->rune == 'i' || lexer->rune == 'j' || lexer->rune == 'k') {
		advancel(lexer);
		token.as_literal = LITERAL_IMAGINARY;
	}

	token.string.length = lexer->here - token.string.contents;

	return token;
}

Bool lexer_init(Lexer *lexer, Context *context, const Source *source) {
	const String *const string = &source->contents;
	if (string->length == 0) {
		return false;
	}

	lexer->context = context;

	lexer->input.source = source;
	lexer->input.cur = string->contents;
	lexer->input.end = string->contents + string->length;

	lexer->location.column = 1;
	lexer->location.line = 1;

	lexer->here = lexer->input.cur;
	lexer->rune = 0;
	lexer->asi = false;
	lexer->peek.kind = KIND_INVALID;

	advancel(lexer);
	if (lexer->rune == RUNE_BOM) {
		advancel(lexer);
	}

	return true;
}

static Bool unescape(Lexer *lexer) {
	// TODO(dweiler): Implement.
	(void)lexer;
	return false;
}

Token lexer_peek(Lexer *lexer) {
	ASSERT(lexer->peek.kind == KIND_INVALID);
	const Token peek = lexer_next(lexer);
	lexer->peek = peek;
	return peek;
}

Token lexer_tokenize(Lexer *lexer) {
	Context *context = lexer->context;
	if (lexer->peek.kind != KIND_INVALID) {
		const Token token = lexer->peek;
		lexer->peek.kind = KIND_INVALID;
		return token;
	}

	skip_whitespace(lexer, lexer->asi);

	Token token;
	token.kind = KIND_INVALID;

	token.string.contents = CCAST(Uint8*, lexer->here);
	token.string.length = 1; // One rune.

	token.location = lexer->location;

	const Rune rune = lexer->rune;
	if (is_char(rune)) {
		// Looks like an identifier.
		token.kind = KIND_IDENTIFIER;
		while (is_char(lexer->rune) || is_digit(lexer->rune)) {
			advancel(lexer);
		}
		token.string.length = lexer->here - token.string.contents;
		// Check if this token is actually a keyword.
		if (keyword_find(token.string, &token.as_keyword)) {
			token.kind = KIND_KEYWORD;
		}
		// Check if this token is actually a operator.
		if (operator_find(token.string, &token.as_operator)) {
			token.kind = KIND_OPERATOR;
		}
	} else if (rune >= '0' && rune <= '9') {
		token = scan_numeric(lexer, false);
	} else {
		advancel(lexer);
		switch (rune) {
		case RUNE_EOF:
			token.kind = KIND_EOF;
			if (lexer->asi) {
				lexer->asi = false;
				token.string = SCLIT("\n");
				token.kind = KIND_SEMICOLON;
				return token;
			}
			break;
		case '\n':
			lexer->asi = false;
			token.string = SCLIT("\n");
			token.kind = KIND_SEMICOLON;
			return token;
		case '\\':
			lexer->asi = false;
			// TODO(dweiler): Implement.
			break;
		case '\'':
			// Rune literal.
			token.kind = KIND_LITERAL;
			// TODO(dweiler): Implement.
			break;
		case '`':
			// Raw string literal
			FALLTHROUGH();
		case '"':
			{
				Rune quote = rune;
				for (;;) {
					const Rune r = lexer->rune;
					if ((rune == '"' && r == '\n') || r == EOF) {
						LEX_ERROR("Unterminated string literal");
						break;
					}
					advancel(lexer);
					if (r == quote) break;
					if (rune != '"' && r == '\\') {
						unescape(lexer);
					}
				}
				token.kind = KIND_LITERAL;
				token.as_literal = LITERAL_STRING;
				token.string.length = lexer->here - token.string.contents;
			}
			break;
		case '.':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_PERIOD;
			if (lexer->rune >= '0' && lexer->rune <= '9') {
				token = scan_numeric(lexer, true);
			} else if (lexer->rune == '.') {
				advancel(lexer);
				token.as_operator = OPERATOR_ELLIPSIS;
				if (lexer->rune == '<') {
					advancel(lexer);
					token.as_operator = OPERATOR_RANGEHALF;
				} else if (lexer->rune == '=') {
					advancel(lexer);
					token.as_operator = OPERATOR_RANGEFULL;
				}
				break;
			}
			break;
		case '@':
			token.kind = KIND_ATTRIBUTE;
			break;
		case '$':
			token.kind = KIND_CONST;
			break;
		case '?':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_QUESTION;
			break;
		case '^':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_POINTER;
			break;
		case ';':
			token.kind = KIND_SEMICOLON;
			break;
		case ',':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_COMMA;
			break;
		case ':':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_COLON;
			break;
		case '(':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_LPAREN;
			break;
		case ')':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_RPAREN;
			break;
		case '[':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_LBRACKET;
			break;
		case ']':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_RBRACKET;
			break;
		case '{':
			token.kind = KIND_LBRACE;
			break;
		case '}':
			token.kind = KIND_RBRACE;
			break;
		case '%':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_MOD;
			switch (lexer->rune) {
			case '=':
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_EQ;
				break;
			case '%':
				advancel(lexer);
				token.as_operator = OPERATOR_MODMOD;
				if (lexer->rune == '=') {
					advancel(lexer);
					token.kind = KIND_ASSIGNMENT;
					token.as_assignment = ASSIGNMENT_MODMODEQ;
				}
				break;
			}
			break;
		case '*':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_MUL;
			if (lexer->rune == '=') {
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_MULEQ;
			}
			break;
		case '=':
			token.kind = KIND_ASSIGNMENT;
			token.as_assignment = ASSIGNMENT_EQ;
			if (lexer->rune == '=') {
				advancel(lexer);
				token.kind = KIND_OPERATOR;
				token.as_operator = OPERATOR_CMPEQ;
			}
			break;
		case '~':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_XOR;
			if (lexer->rune == '=') {
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_XOREQ;
			}
			break;
		case '!':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_NOT;
			if (lexer->rune == '=') {
				advancel(lexer);
				token.as_operator = OPERATOR_NOTEQ;
			}
			break;
		case '+':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_ADD;
			if (lexer->rune == '=') {
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_ADDEQ;
			}
			break;
		case '-':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_SUB;
			switch (lexer->rune) {
			case '=':
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_SUBEQ;
				break;
			case '-':
				advancel(lexer);
				if (lexer->rune == '-') {
					advancel(lexer);
					token.kind = KIND_UNDEFINED;
				} else {
					LEX_ERROR("The decrement operator '--' does not exist");
				}
				break;
			case '>':
				advancel(lexer);
				token.as_operator = OPERATOR_ARROW;
				break;
			}
			break;
		case '#':
			while (is_char(lexer->rune)) {
				advancel(lexer);
			}
			token.string.contents += 1; // Skip '#'
			token.string.length = lexer->here - token.string.contents;
			if (directive_find(token.string, &token.as_directive)) {
				token.kind = KIND_DIRECTIVE;
			} else {
				token.kind = KIND_INVALID;
			}
			break;
		case '/':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_QUO;
			switch (lexer->rune) {
			case '=':
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_QUOEQ;
				break;
			// Line comment.
			case '/':
				token.kind = KIND_COMMENT;
				skip_line(lexer);
				break;
			// Block comment
			case '*':
				token.kind = KIND_COMMENT; 
				advancel(lexer);
				// Support nested block comments.
				for (int i = 1; i > 0; /**/) switch (lexer->rune) {
				case RUNE_EOF:
					i = 0;
					break;
				case '/':
					advancel(lexer);
					if (lexer->rune == '*') {
						advancel(lexer);
						i++;
					}
					break;
				case '*':
					advancel(lexer);
					if (lexer->rune == '/') {
						advancel(lexer);
						i--;
					}
					break;
				default:
					advancel(lexer);
					break;
				}
				break;
			}
			break;
		case '<':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_LT;
			switch (lexer->rune) {
			case '=':
				advancel(lexer);
				token.as_operator = OPERATOR_LTEQ;
				break;
			case '<':
				advancel(lexer);
				token.as_operator = OPERATOR_SHL;
				if (lexer->rune == '=') {
					advancel(lexer);
					token.kind = KIND_ASSIGNMENT;
					token.as_assignment = ASSIGNMENT_SHLEQ;
				}
				break;
			}
			break;
		case '>':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_GT;
			switch (lexer->rune) {
			case '=':
				advancel(lexer);
				token.as_operator = OPERATOR_GTEQ;
				break;
			case '>':
				advancel(lexer);
				token.as_operator = OPERATOR_SHR;
				if (lexer->rune == '=') {
					advancel(lexer);
					token.kind = KIND_ASSIGNMENT;
					token.as_assignment = ASSIGNMENT_SHREQ;
				}
				break;
			}
			break;
		case '&':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_AND;
			switch (lexer->rune) {
			case '~':
				advancel(lexer);
				token.as_operator = OPERATOR_ANDNOT;
				if (lexer->rune == '=') {
					advancel(lexer);
					token.kind = KIND_ASSIGNMENT;
					token.as_assignment = ASSIGNMENT_ANDNOTEQ;
				}
				break;
			case '=':
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_ANDEQ;
				break;
			case '&':
				advancel(lexer);
				token.as_operator = OPERATOR_CMPAND;
				if (lexer->rune == '=') {
					advancel(lexer);
					token.kind = KIND_ASSIGNMENT;
					token.as_assignment = ASSIGNMENT_ANDEQ;
				}
				break;
			}
			break;
		case '|':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_OR;
			switch (lexer->rune) {
			case '=':
				advancel(lexer);
				token.kind = KIND_ASSIGNMENT;
				token.as_assignment = ASSIGNMENT_OREQ;
				break;
			case '|':
				advancel(lexer);
				token.as_operator = OPERATOR_CMPOR;
				if (lexer->rune == '=') {
					advancel(lexer);
					token.kind = KIND_ASSIGNMENT;
					token.as_assignment = ASSIGNMENT_CMPOREQ;
				}
				break;
			}
			break;
		}
	}
	return token;
}

// Simple helper routines
String kind_to_string(Kind kind) {
	#define KIND(enumerator, name, ...) SLIT(name),
	static const String KINDS[] = {
		#include "lexemes.h"
	};
	return KINDS[kind];
}

String literal_to_string(LiteralKind literal) {
	#define LITERAL(enumerator, name) SLIT(name),
	static const String LITERALS[] = {
		#include "lexemes.h"
	};
	return LITERALS[literal];
}

String keyword_to_string(KeywordKind keyword) {
	#define KEYWORD(ident, string, ...) SLIT(string),
	static const String KEYWORDS[] = {
		#include "lexemes.h"
	};
	return KEYWORDS[keyword];
}

String operator_to_string(OperatorKind op) {
	#define OPERATOR(ident, string, ...) SLIT(string),
	static const String OPERATORS[] = {
		#include "lexemes.h"
	};
	return OPERATORS[op];
}

String assignment_to_string(AssignmentKind assignment) {
	#define ASSIGNMENT(ident, string) SLIT(string),
	static const String ASSIGNMENTS[] = {
		#include "lexemes.h"
	};
	return ASSIGNMENTS[assignment];
}

String directive_to_string(DirectiveKind directive) {
	#define DIRECTIVE(ident, string) SLIT(string),
	static const String DIRECTIVES[] = {
		#include "lexemes.h"
	};
	return DIRECTIVES[directive];
}

String token_to_string(Token token) {
	#define KIND(enumerator, kind, ...) SLIT(#enumerator),
	static const String STRINGS[] = {
		#include "lexemes.h"
	};
	switch (token.kind) {
	case KIND_COMMENT:
		FALLTHROUGH();
	case KIND_IDENTIFIER:
		return token.string;
	case KIND_KEYWORD:
		return keyword_to_string(token.as_keyword);
	case KIND_LITERAL:
		return token.string;
	case KIND_OPERATOR:
		return operator_to_string(token.as_operator);
	case KIND_ASSIGNMENT:
		return assignment_to_string(token.as_assignment);
	default:
		return STRINGS[token.kind];
	}
	UNREACHABLE();
}


Token lexer_next(Lexer *lexer) {
	Token token = lexer_tokenize(lexer);

	#define KIND(enum, name, asi) asi,
	static const Bool KIND_ASI[] = {
		#include "lexemes.h"
	};
	#undef KIND

	#define OPERATOR(enum, match, precedence, asi) asi,
	static const Bool OPERATOR_ASI[] = {
		#include "lexemes.h"
	};
	#undef OPERATOR

	#define KEYWORD(enum, match, asi) asi,
	static const Bool KEYWORD_ASI[] = {
		#include "lexemes.h"
	};
	#undef KEYWORD

	switch (token.kind) {
	case KIND_OPERATOR:
		lexer->asi = OPERATOR_ASI[token.as_operator];
		break;
	case KIND_KEYWORD:
		lexer->asi = KEYWORD_ASI[token.as_keyword];
		break;
	default:
		lexer->asi = KIND_ASI[token.kind];
		break;
	}

	return token;
}