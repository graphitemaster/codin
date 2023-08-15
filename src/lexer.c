#include "lexer.h"
#include "support.h"
#include "report.h"
#include "context.h"

const Token TOKEN_NIL = { KIND_INVALID, { 0, 0 }, { 0, 0 }, { CAST(LiteralKind, 0), } };

// Automatic semicolon insertion tables.
#define KIND(enum, name, asi) asi,
static const Bool KIND_ASI[] = {
	#include "lexemes.h"
};

#define OPERATOR(enum, match, precedence, named, asi) asi,
static const Bool OPERATOR_ASI[] = {
	#include "lexemes.h"
};

#define KEYWORD(enum, match, asi) asi,
static const Bool KEYWORD_ASI[] = {
	#include "lexemes.h"
};

// Keyword LUT
#define KEYWORD(ident, match, ...) SLIT(match),
static const String KEYWORDS[] = {
	#include "lexemes.h"
};

// Directive LUT
#define DIRECTIVE(ident, match) SLIT(match),
static const String DIRECTIVES[] = {
	#include "lexemes.h"
};

// Named operator LUT.
//
// We use a preprocessing trick here to extract the named operators
// out of the global table.
//
// Since this table will only contain named operators we need the
// OperatorKind enumerator to return in operator_find as the loop
// index will no longer match like it does for KEYWORDS.
#define OPERATOR_IS_NAMED_true(ident, match) \
	{ SLIT(match), OPERATOR_ ## ident },
#define OPERATOR_IS_NAMED_false(...)
#define OPERATOR(ident, match, prec, named, ...) \
	OPERATOR_IS_NAMED_ ## named(ident, match)
static const struct NamedOperator { String s; OperatorKind e; } NAMED_OPERATORS[] = {
	#include "lexemes.h"
};

#define LEX_ERROR(...) \
	do { \
		report_error(lexer->input.source, &lexer->last_location, __VA_ARGS__); \
		THROW(ERROR_LEX); \
	} while (0)

// Searches for a keyword
static Bool keyword_find(String string, KeywordKind *result) {
	// The KEYWORDS table matches the KeywordKind order, so we can just
	// return the result of the loop counter here as an optimization.
	for (Size i = 0; i < sizeof KEYWORDS / sizeof *KEYWORDS; i++) {
		if (string_compare(string, KEYWORDS[i])) {
			*result = CAST(KeywordKind, i);
			return true;
		}
	}
	return false;
}

// Searches for a operator
static Bool operator_find(String string, OperatorKind *result) {
	for (Size i = 0; i < sizeof NAMED_OPERATORS / sizeof *NAMED_OPERATORS; i++) {
		const struct NamedOperator *op = &NAMED_OPERATORS[i];
		if (string_compare(string, op->s)) {
			*result = op->e;
			return true;
		}
	}
	return false;
}

static Bool directive_find(String string, DirectiveKind *result) {
	for (Size i = 0; i < sizeof DIRECTIVES / sizeof *DIRECTIVES; i++) {
		if (string_compare(string, DIRECTIVES[i])) {
			*result = CAST(DirectiveKind, i);
			return true;
		}
	}
	return false;
}

static FORCE_INLINE Bool is_char(Rune ch) {
	if (ch < 0x80) {
		if (ch == '_') {
			return true;
		}
		return ((CAST(Uint32, ch) | 0x20) - 0x61) < 26; // [a-z][A-Z]
	}
	if (ch > 128) {
		return true;
	}
	// TODO(dweiler): UTF-8.
	return false;
}

static FORCE_INLINE Bool is_digit(Rune ch) {
	if (ch < 0x80) {
		return (CAST(Uint32, ch) - '0') < 10;
	}
	// TODO(dweiler): UTF-8.
	return false;
}

static Uint8 peekl(Lexer *lexer) {
	const Input *input = &lexer->input;
	return input->cur < input->end ? *input->cur : 0;
}

static Rune advancel(Lexer *lexer) {
	Context *context = lexer->context;
	if (lexer->rune == '\n') {
		lexer->this_location.column = 0;
		lexer->this_location.line++;
	}
	Input *input = &lexer->input;
	if (input->cur < input->end) {
		lexer->here = input->cur;
		Rune rune = *input->cur;
		if (rune == 0) {
			LEX_ERROR("Unexpected EOF");
			input->cur++;
		} else if (rune & 0x80) {
			Uint32 state = UTF8_ACCEPT;
			while (input->cur < input->end && *input->cur & 0x80) {
				utf8_decode(&state, &rune, *input->cur++);
			}
			if (state != UTF8_ACCEPT) {
				LEX_ERROR("Malformed UTF-8");
			}
		} else {
			input->cur++;
		}
		lexer->rune = rune;
		lexer->this_location.column++;
	} else {
		lexer->here = input->end;
		lexer->rune = RUNE_EOF;
	}
	return lexer->rune;
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

static FORCE_INLINE Token mkkind(const Lexer *lexer, Kind kind) {
	Token token = {0};
	token.kind = kind;
	token.location = lexer->last_location;
	return token;
}

static FORCE_INLINE Token mkop(const Lexer *lexer, OperatorKind kind) {
	Token token = mkkind(lexer, KIND_OPERATOR);
	token.as_operator = kind;
	return token;
}

static FORCE_INLINE Token mkassign(const Lexer *lexer, AssignmentKind kind) {
	Token token = mkkind(lexer, KIND_ASSIGNMENT);
	token.as_assignment = kind;
	return token;
}

static Token scan_numeric(Lexer *lexer, Bool dot) {
	Token token = mkkind(lexer, KIND_LITERAL);
	token.string.contents = CCAST(Uint8*, lexer->here);
	token.string.length = 1;
	token.as_literal = LITERAL_INTEGER;

	if (dot) {
		token.as_literal = LITERAL_FLOAT;
		token.string.contents--;
		token.string.length++;
		token.location.column--;
		scan(lexer, 10);
		goto L_exponent;
	}

	if (lexer->rune == '0') {
		switch (advancel(lexer)) {
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

static Bool scan_escape(Lexer *lexer) {
	Context *const context = lexer->context;
	switch (lexer->rune) {
	// \a, \b, \e, \f, \n, \r, \t, \v
	case 'a': case 'b': case 'e': case 'f': case 'n': case 'r': case 't': case 'v':
		advancel(lexer);
		return true;
	case '\\': case '\'': case '\"':
		advancel(lexer);
		return true;
	case 'x':
		advancel(lexer);
		for (Size i = 0; i < 2; i++) advancel(lexer); // "0x"
		return true;
	case 'u':
		advancel(lexer);
		for (Size i = 0; i < 4; i++) advancel(lexer); // u000
		return true;
	case 'U':
		advancel(lexer);
		for (Size i = 0; i < 8; i++) advancel(lexer); // U0000000
		return true;
	default:
		if (is_digit(lexer->rune)) {
			for (Size i = 0; i < 3; i++) advancel(lexer); // %d00
			return true;
		} else {
			if (lexer->rune < 0) {
				LEX_ERROR("Unterminated escape sequence");
			} else {
				LEX_ERROR("Unknown escape sequence %d", CAST(int, lexer->rune));
			}
		}
	}
	UNREACHABLE();
}

Bool lexer_init(Lexer *lexer, const Source *source, Context *context) {
	const String *const string = &source->contents;
	if (string->length == 0) {
		return false;
	}

	lexer->context = context;

	lexer->input.source = source;
	lexer->input.cur = string->contents;
	lexer->input.end = string->contents + string->length;

	lexer->this_location.column = 0;
	lexer->this_location.line = 1;

	lexer->here = lexer->input.cur;
	lexer->rune = 0;
	lexer->asi = false;
	lexer->peek = array_make(context);

	if (advancel(lexer) == RUNE_BOM) {
		advancel(lexer);
	}

	return true;
}

void lexer_fini(Lexer *lexer) {
	array_free(lexer->peek);
}

static Token lexer_raw_next(Lexer *lexer);

Token lexer_peek(Lexer *lexer) {
	const Token peek = lexer_raw_next(lexer);
	const Size index = array_size(lexer->peek);
	array_push(lexer->peek, peek);
	return index ? lexer->peek[index - 1] : peek;
}

static Token lexer_tokenize(Lexer *lexer) {
	Context *context = lexer->context;

	skip_whitespace(lexer, lexer->asi);

	lexer->last_location = lexer->this_location;

	Token token = mkkind(lexer, KIND_INVALID);
	token.string.contents = CCAST(Uint8*, lexer->here);
	token.string.length = 1; // One rune.

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
		return token;
	} else if (is_digit(rune)) {
		return scan_numeric(lexer, false);
	}

	// Single character dispatch
	advancel(lexer);

	// #define USE_COMPUTED_GOTO

	#if defined(USE_COMPUTED_GOTO)
	// Can we use a threaded dispatch table.
	static const void *DISPATCH_TABLE[] = {
		[CAST(Uint8, RUNE_EOF)] = &&L_EOF,
		['\n']                  = &&L_LF,
		['\\']                  = &&L_BSLASH,
		['\'']                  = &&L_SQUOTE,
		['"']                   = &&L_DQUOTE,
		['`']                   = &&L_GRAVE,
		['.']                   = &&L_PERIOD,
		['{']                   = &&L_LBRACE,
		['}']                   = &&L_RBRACE,
		[';']                   = &&L_SEMI,
		['@']                   = &&L_AT,
		['$']                   = &&L_DOLLAR,
		['?']                   = &&L_QUESTION,
		['^']                   = &&L_CARET,
		[',']                   = &&L_COMMA,
		[':']                   = &&L_COLON,
		['(']                   = &&L_LPAREN,
		[')']                   = &&L_RPAREN,
		['[']                   = &&L_LBRACKET,
		[']']                   = &&L_RBRACKET,
		['%']                   = &&L_PERCENT,
		['*']                   = &&L_STAR,
		['=']                   = &&L_EQUAL,
		['~']                   = &&L_TILDE,
		['!']                   = &&L_EXCLAIM,
		['+']                   = &&L_PLUS,
		['-']                   = &&L_MINUS,
		['#']                   = &&L_POUND,
		['/']                   = &&L_FSLASH,
		['<']                   = &&L_LT,
		['>']                   = &&L_GT,
		['&']                   = &&L_AND,
		['|']                   = &&L_OR,
	};
	#define CASE(_name, char) L ## _ ## _name
	#define SWITCH(code) goto *DISPATCH_TABLE[CAST(Uint8, code)];
	#else
	#define CASE(_name, char) case char
	#define SWITCH(code) switch (code)
	#endif

	SWITCH(rune) {
	CASE(EOF, RUNE_EOF):
		token.kind = KIND_EOF;
		if (lexer->asi) {
			lexer->asi = false;
			token.string = SCLIT("\n");
			token.kind = KIND_SEMICOLON;
			return token;
		}
		return token;
	CASE(LF, '\n'):
		lexer->asi = false;
		token.string = SCLIT("\n");
		token.kind = KIND_SEMICOLON;
		return token;
	CASE(BSLASH, '\\'):
		// Line continuation.
		lexer->asi = false;
		return lexer_tokenize(lexer);
	CASE(SQUOTE, '\''):
		// Rune literal.
		{
			token.kind = KIND_LITERAL;
			token.as_literal = LITERAL_RUNE;
			Rune quote = rune;
			for (;;) {
				Rune ch = lexer->rune;
				if (ch == '\n' || ch < 0) {
					LEX_ERROR("Unterminated rune literal");
				}
				advancel(lexer);
				if (ch == quote) {
					break;
				}
				if (ch == '\\' && !scan_escape(lexer)) {
					LEX_ERROR("Malformed rune literal");
				}
			}
			token.string.length = lexer->here - token.string.contents;
			return token;
		}
	CASE(GRAVE, '`'):
		// Raw string literal
		// FALLTHROUGH();
	CASE(DQUOTE, '"'):
		{
			const Rune quote = rune;
			for (;;) {
				const Rune ch = lexer->rune;
				if (ch == '\n' || ch < 0) {
					LEX_ERROR("Unterminated string literal");
					break;
				}
				advancel(lexer);
				if (ch == quote) {
					break;
				}
				if (rune == '"' && (ch == '\\' && !scan_escape(lexer))) {
					LEX_ERROR("Malformed string literal");
				}
			}
			token.kind = KIND_LITERAL;
			token.as_literal = LITERAL_STRING;
			token.string.length = lexer->here - token.string.contents;
			return token;
		}
	CASE(PERIOD, '.'):
		if (is_digit(lexer->rune)) {
			return scan_numeric(lexer, true);
		} else if (lexer->rune == '.') {
			switch (advancel(lexer)) {
			case '<':
				advancel(lexer);
				return mkop(lexer, OPERATOR_RANGEHALF);
			case '=':
				advancel(lexer);
				return mkop(lexer, OPERATOR_RANGEFULL);
			default:
				return mkop(lexer, OPERATOR_ELLIPSIS);
			}
		} else {
			return mkop(lexer, OPERATOR_PERIOD);
		}
	CASE(LBRACE,   '{'): return mkkind(lexer, KIND_LBRACE);
	CASE(RBRACE,   '}'): return mkkind(lexer, KIND_RBRACE);
	CASE(SEMI,     ';'): return mkkind(lexer, KIND_SEMICOLON);
	CASE(AT,       '@'): return mkkind(lexer, KIND_ATTRIBUTE);
	CASE(DOLLAR,   '$'): return mkkind(lexer, KIND_CONST);
	CASE(QUESTION, '?'): return mkop(lexer, OPERATOR_QUESTION);
	CASE(CARET,    '^'): return mkop(lexer, OPERATOR_POINTER);
	CASE(COMMA,    ','): return mkop(lexer, OPERATOR_COMMA);
	CASE(COLON,    ':'): return mkop(lexer, OPERATOR_COLON);
	CASE(LPAREN,   '('): return mkop(lexer, OPERATOR_LPAREN);
	CASE(RPAREN,   ')'): return mkop(lexer, OPERATOR_RPAREN);
	CASE(LBRACKET, '['): return mkop(lexer, OPERATOR_LBRACKET);
	CASE(RBRACKET, ']'): return mkop(lexer, OPERATOR_RBRACKET);
	CASE(PERCENT,  '%'):
		switch (lexer->rune) {
		case '=':
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_QUO);
		case '%':
			if (advancel(lexer) == '=') {
				advancel(lexer);
				return mkassign(lexer, ASSIGNMENT_REM);
			}
			return mkop(lexer, OPERATOR_MODMOD);
		default:
			return mkop(lexer, OPERATOR_MOD);
		}
	CASE(STAR, '*'):
		if (lexer->rune == '=') {
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_MUL);
		} else {
			return mkop(lexer, OPERATOR_MUL);
		}
	CASE(EQUAL, '='):
		if (lexer->rune == '=') {
			advancel(lexer);
			return mkop(lexer, OPERATOR_CMPEQ);
		} else {
			return mkassign(lexer, ASSIGNMENT_EQ);
		}
	CASE(TILDE, '~'):
		if (lexer->rune == '=') {
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_XOR);
		} else {
			return mkop(lexer, OPERATOR_XOR);
		}
	CASE(EXCLAIM, '!'):
		if (lexer->rune == '=') {
			advancel(lexer);
			return mkop(lexer, OPERATOR_NOTEQ);
		} else {
			return mkop(lexer, OPERATOR_NOT);
		}
	CASE(PLUS, '+'):
		switch (lexer->rune) {
		case '=':
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_ADD);
		case '+':
			LEX_ERROR("The increment operator '++' does not exist");
		default:
			return mkop(lexer, OPERATOR_ADD);
		}
	CASE(MINUS, '-'):
		switch (lexer->rune) {
		case '=':
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_SUB);
		case '-':
			if (advancel(lexer) == '-') {
				advancel(lexer);
				return mkkind(lexer, KIND_UNDEFINED);
			} else {
				LEX_ERROR("The decrement operator '--' does not exist");
			}
		case '>':
			advancel(lexer);
			return mkop(lexer, OPERATOR_ARROW);
		default:
			return mkop(lexer, OPERATOR_SUB);
		}
	CASE(POUND, '#'):
		while (is_char(lexer->rune)) {
			advancel(lexer);
		}
		token.string.contents += 1; // Skip '#'
		token.string.length = lexer->here - token.string.contents;
		if (directive_find(token.string, &token.as_directive)) {
			token.kind = KIND_DIRECTIVE;
		} else {
			LEX_ERROR("Unknown directive '%.*s'", SFMT(token.string));
		}
		return token;
	CASE(FSLASH, '/'):
		switch (lexer->rune) {
		case '=':
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_QUO);
		// Line comment.
		case '/':
			skip_line(lexer);
			return mkkind(lexer, KIND_COMMENT);
		// Block comment
		case '*':
			advancel(lexer);
			// Support nested block comments.
			for (int i = 1; i > 0; /**/) switch (lexer->rune) {
			case RUNE_EOF:
				return mkkind(lexer, KIND_EOF);
			case '/':
				if (advancel(lexer) == '*') {
					advancel(lexer);
					i++;
				}
				break;
			case '*':
				if (advancel(lexer) == '/') {
					advancel(lexer);
					i--;
				}
				break;
			default:
				advancel(lexer);
			}
			return mkkind(lexer, KIND_COMMENT);
		default:
			return mkop(lexer, OPERATOR_QUO);
		}
	CASE(LT, '<'):
		switch (lexer->rune) {
		case '=':
			advancel(lexer);
			return mkop(lexer, OPERATOR_LTEQ);
		case '<':
			if (advancel(lexer) == '=') {
				advancel(lexer);
				return mkassign(lexer, ASSIGNMENT_SHL);
			}
			return mkop(lexer, OPERATOR_SHL);
		default:
			return mkop(lexer, OPERATOR_LT);
		}
	CASE(GT, '>'):
		switch (lexer->rune) {
		case '=':
			advancel(lexer);
			return mkop(lexer, OPERATOR_GTEQ);
		case '>':
			if (advancel(lexer) == '=') {
				advancel(lexer);
				return mkassign(lexer, ASSIGNMENT_SHR);
			}
			return mkop(lexer, OPERATOR_SHR);
		default:
			return mkop(lexer, OPERATOR_GT);
		}
	CASE(AND, '&'):
		switch (lexer->rune) {
		case '~':
			if (advancel(lexer) == '=') {
				advancel(lexer);
				return mkassign(lexer, ASSIGNMENT_ANDNOT);
			}
			return mkop(lexer, OPERATOR_ANDNOT);
		case '=':
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_AND);
		case '&':
			if (advancel(lexer) == '=') {
				advancel(lexer);
				return mkassign(lexer, ASSIGNMENT_AND);
			}
			return mkop(lexer, OPERATOR_CMPAND);
		default:
			return mkop(lexer, OPERATOR_AND);
		}
	CASE(OR, '|'):
		switch (lexer->rune) {
		case '=':
			advancel(lexer);
			return mkassign(lexer, ASSIGNMENT_OR);
		case '|':
			if (advancel(lexer) == '=') {
				advancel(lexer);
				return mkassign(lexer, ASSIGNMENT_CMPOR);
			}
			return mkop(lexer, OPERATOR_CMPOR);
		default:
			return mkop(lexer, OPERATOR_OR);
		}
	}

	LEX_ERROR("Unexpected rune '%d'", rune);
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

static Token lexer_raw_next(Lexer *lexer) {
	const Token token = lexer_tokenize(lexer);
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

Token lexer_next(Lexer *lexer) {
	if (array_size(lexer->peek) != 0) {
		const Token token = lexer->peek[0];
		array_pop_front(lexer->peek);
		return token;
	}
	return lexer_raw_next(lexer);
}