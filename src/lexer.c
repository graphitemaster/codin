#include <string.h>
#include <stdio.h>

#include "lexer.h"
#include "support.h"

// Searches for a keyword
static Bool keyword_find(const String *string, Keyword *result) {
	#define KEYWORD(ident, content) \
		if (string_compare(string, &SLIT(content))) { \
			*result = KEYWORD_ ## ident; \
			return true; \
		}
	#include "lexemes.h"
	return false;
}

// Searches for a operator
static Bool operator_find(const String *string, Operator *result) {
	#define OPERATOR(ident, content, ...) \
		if (string_compare(string, &SLIT(content))) { \
			*result = OPERATOR_ ## ident; \
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

static void advance(Lexer *lexer) {
	if (lexer->rune == '\n') {
		lexer->location.column = 1;
		lexer->location.line++;
	}
	Input *input = &lexer->input;
	if (input->cur < input->end) {
		lexer->here = input->cur;
		const Rune rune = *input->cur;
		if (rune == 0) {
			// ERROR: Unexpected NUL.
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
		advance(lexer);
	}
}

static void skip_whitespace(Lexer *lexer, Bool newline) {
	for (;;) {
		const Rune rune = lexer->rune;
		if (rune == ' ' || rune == '\t' || rune == '\r' || (newline && rune == '\n')) {
			advance(lexer);
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
		advance(lexer);
	}
}

static Token scan_numeric(Lexer *lexer, Bool dot) {
	Token token;
	token.kind = KIND_LITERAL;
	token.string.data = CAST(Uint8*, lexer->here);
	token.string.size = 1;
	token.as_literal = LITERAL_INTEGER;
	token.location = lexer->location;

	if (dot) {
		token.as_literal = LITERAL_FLOAT;
		token.string.data--;
		token.string.size++;
		token.location.column--;
		scan(lexer, 10);
		goto L_exponent;
	}

	if (lexer->rune == '0') {
		advance(lexer);
		switch (lexer->rune) {
		case 'b':
			advance(lexer);
			scan(lexer, 2);
			break;
		case 'o':
			advance(lexer);
			scan(lexer, 8);
			break;
		case 'd':
			advance(lexer);
			scan(lexer, 10);
			break;
		case 'z':
			advance(lexer);
			scan(lexer, 12);
			break;
		case 'x':
			advance(lexer);
			scan(lexer, 16);
			break;
		case 'h':
			advance(lexer);
			scan(lexer, 16);
			break;
		default:
			break;
		}
	}

	scan(lexer, 10);

	if (lexer->rune == '.') {
		// TODO(dweiler): Check for '..' which is an ellipsis.
		advance(lexer);
		token.as_literal = LITERAL_FLOAT;
		scan(lexer, 10);
	}

L_exponent:
	// Handle 'e' and 'E' exponents.
	if (lexer->rune == 'e' || lexer->rune == 'E') {
		advance(lexer);
		token.as_literal = LITERAL_FLOAT;
		if (lexer->rune == '-' || lexer->rune == '+') {
			advance(lexer);
		}
		scan(lexer, 10);
	}

	if (lexer->rune == 'i' || lexer->rune == 'j' || lexer->rune == 'k') {
		advance(lexer);
		token.as_literal = LITERAL_IMAGINARY;
	}

	token.string.size = lexer->here - token.string.data;

	return token;
}

Bool lexer_init(Lexer *lexer, const Source *source) {
	const String *const contents = &source->contents;
	if (contents->size == 0) {
		return false;
	}

	lexer->input.source = source;
	lexer->input.cur = contents->data;
	lexer->input.end = contents->data + contents->size;

	lexer->location.column = 1;
	lexer->location.line = 1;

	lexer->here = lexer->input.cur;

	lexer->rune = 0;

	advance(lexer);
	if (lexer->rune == RUNE_BOM) {
		advance(lexer);
	}

	return true;
}

Token lexer_next(Lexer *lexer) {
	skip_whitespace(lexer, lexer->asi);

	Token token;
	token.kind = KIND_INVALID;

	token.string.data = CAST(Uint8*, lexer->here);
	token.string.size = 1; // One rune.

	token.location = lexer->location;

	const Rune rune = lexer->rune;
	if (is_char(rune)) {
		// Looks like an identifier.
		token.kind = KIND_IDENTIFIER;
		while (is_char(lexer->rune) || is_digit(lexer->rune)) {
			advance(lexer);
		}
		token.string.size = lexer->here - token.string.data;
		// Check if this token is actually a keyword.
		if (keyword_find(&token.string, &token.as_keyword)) {
			token.kind = KIND_KEYWORD;
		}
		// Check if this token is actually a operator.
		if (operator_find(&token.string, &token.as_operator)) {
			token.kind = KIND_OPERATOR;
		}
	} else if (rune >= '0' && rune <= '9') {
		token = scan_numeric(lexer, false);
	} else {
		advance(lexer);
		switch (rune) {
		case RUNE_EOF:
			token.kind = KIND_EOF;
			if (lexer->asi) {
				lexer->asi = false;
				token.kind = KIND_SEMICOLON;
				return token;
			}
			break;
		case '\n':
			lexer->asi = false;
			token.string = SLIT("\n");
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
			// Raw string literal
			// TODO(dweiler): Implement.
			break;
		case '.':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_PERIOD;
			if (lexer->rune >= '0' && rune <= '9') {
				token = scan_numeric(lexer, true);
			} else if (lexer->rune == '.') {
				advance(lexer);
				token.as_operator = OPERATOR_ELLIPSIS;
				if (lexer->rune == '<') {
					advance(lexer);
					token.as_operator = OPERATOR_RANGEHALF;
				} else if (lexer->rune == '=') {
					advance(lexer);
					token.as_operator = OPERATOR_RANGEFULL;
				}
				break;
			}
			break;
		case '@':
			token.kind = KIND_ATTRIBUTE;
			break;
		case '$':
			// TODO(dweiler): Not sure how to deal with this yet. Should we make it
			// a separate token so the parser consumes it and then an identifier or
			// should we just treat $ as a modifier on an existing identifier token
			// since it can only come before an identifier.
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
			token.as_operator = OPERATOR_OPENPAREN;
			break;
		case ')':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_CLOSEPAREN;
			break;
		case '[':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_OPENBRACKET;
			break;
		case ']':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_CLOSEBRACKET;
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
				advance(lexer);
				token.as_operator = OPERATOR_MODEQ;
				break;
			case '%':
				advance(lexer);
				token.as_operator = OPERATOR_MODMOD;
				if (lexer->rune == '=') {
					advance(lexer);
					token.as_operator = OPERATOR_MODMODEQ;
				}
				break;
			}
			break;
		case '*':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_MUL;
			if (lexer->rune == '=') {
				advance(lexer);
				token.as_operator = OPERATOR_MULEQ;
			}
			break;
		case '=':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_EQ;
			if (lexer->rune == '=') {
				advance(lexer);
				token.as_operator = OPERATOR_CMPEQ;
			}
			break;
		case '~':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_XOR;
			if (lexer->rune == '=') {
				advance(lexer);
				token.as_operator = OPERATOR_XOREQ;
			}
			break;
		case '!':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_NOT;
			if (lexer->rune == '=') {
				advance(lexer);
				token.as_operator = OPERATOR_NOTEQ;
			}
			break;
		case '+':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_ADD;
			if (lexer->rune == '=') {
				advance(lexer);
				token.as_operator = OPERATOR_ADDEQ;
			}
			break;
		case '-':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_SUB;
			switch (lexer->rune) {
			case '=':
				advance(lexer);
				token.as_operator = OPERATOR_SUBEQ;
				break;
			case '>':
				advance(lexer);
				token.as_operator = OPERATOR_ARROW;
				break;
			}
			break;
		case '#':
			token.kind = KIND_HASH;
			break;
		case '/':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_QUO;
			switch (lexer->rune) {
			case '=':
				advance(lexer);
				token.as_operator = OPERATOR_QUOEQ;
				break;
			// Line comment.
			case '/':
				token.kind = KIND_COMMENT;
				skip_line(lexer);
				break;
			// Block comment
			case '*':
				token.kind = KIND_COMMENT; 
				advance(lexer);
				// Support nested block comments.
				for (int i = 1; i > 0; /**/) switch (lexer->rune) {
				case RUNE_EOF:
					i = 0;
					break;
				case '/':
					advance(lexer);
					if (lexer->rune == '*') {
						advance(lexer);
						i++;
					}
					break;
				case '*':
					advance(lexer);
					if (lexer->rune == '/') {
						advance(lexer);
						i--;
					}
					break;
				default:
					advance(lexer);
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
				advance(lexer);
				token.as_operator = OPERATOR_LTEQ;
				break;
			case '<':
				advance(lexer);
				token.as_operator = OPERATOR_SHL;
				if (lexer->rune == '=') {
					advance(lexer);
					token.as_operator = OPERATOR_SHLEQ;
				}
				break;
			}
			break;
		case '>':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_GT;
			switch (lexer->rune) {
			case '=':
				advance(lexer);
				token.as_operator = OPERATOR_GTEQ;
				break;
			case '>':
				advance(lexer);
				token.as_operator = OPERATOR_SHR;
				if (lexer->rune == '=') {
					advance(lexer);
					token.as_operator = OPERATOR_SHREQ;
				}
				break;
			}
			break;
		case '&':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_AND;
			switch (lexer->rune) {
			case '~':
				advance(lexer);
				token.as_operator = OPERATOR_ANDNOT;
				if (lexer->rune == '=') {
					advance(lexer);
					token.as_operator = OPERATOR_ANDNOTEQ;
				}
				break;
			case '=':
				advance(lexer);
				token.as_operator = OPERATOR_ANDEQ;
				break;
			case '&':
				advance(lexer);
				token.as_operator = OPERATOR_CMPAND;
				if (lexer->rune == '=') {
					advance(lexer);
					token.as_operator = OPERATOR_CMPANDEQ;
				}
				break;
			}
			break;
		case '|':
			token.kind = KIND_OPERATOR;
			token.as_operator = OPERATOR_OR;
			switch (lexer->rune) {
			case '=':
				advance(lexer);
				token.as_operator = OPERATOR_OREQ;
				break;
			case '|':
				advance(lexer);
				token.as_operator = OPERATOR_CMPOR;
				if (lexer->rune == '=') {
					advance(lexer);
					token.as_operator = OPERATOR_CMPOREQ;
				}
				break;
			}
			break;
		}
	}
	return token;
}

// Simple helper routines
String keyword_to_string(Keyword keyword) {
	#define KEYWORD(ident, string) SLIT(string),
	static const String KEYWORDS[] = {
		#include "lexemes.h"
	};
	return KEYWORDS[keyword];
}

String operator_to_string(Operator op) {
	#define OPERATOR(ident, string, ...) SLIT(string),
	static const String OPERATORS[] = {
		#include "lexemes.h"
	};
	return OPERATORS[op];
}

String token_to_string(Token token) {
	switch (token.kind) {
	case KIND_COMMENT:
		return token.string;
	case KIND_EOF:
		return SLIT("EOF");
	case KIND_IDENTIFIER:
		return token.string;
	case KIND_INVALID:
		return SLIT("INVALID");
	case KIND_KEYWORD:
		return keyword_to_string(token.as_keyword);
	case KIND_LITERAL:
		return token.string;
	case KIND_OPERATOR:
		return operator_to_string(token.as_operator);
	case KIND_HASH:
		return SLIT("HASH");
	case KIND_ATTRIBUTE:
		return SLIT("ATTRIBUTE");
	case KIND_SEMICOLON:
		return SLIT("SEMICOLON");
	case KIND_LBRACE:
		return SLIT("LBRACE");
	case KIND_RBRACE:
		return SLIT("RBRACE");
	case KIND_COUNT:
		break;
	}
	UNREACHABLE();
}