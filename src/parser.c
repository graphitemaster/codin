#include <stdio.h>

#include "tree.h"
#include "context.h"
#include "strbuf.h"
#include "report.h"
#include "utility.h"
#include "allocator.h"

// #define TRACE

#define PARSE_ERROR(...) \
	do { \
		report_error(parser->lexer.input.source, &parser->this_token.location, __VA_ARGS__); \
		THROW(ERROR_PARSE); \
	} while (0)

#define ICE(...) \
	PARSE_ERROR(__VA_ARGS__)

// Simple operator precedence climbing.
#define OPERATOR(ident, match, prec, ...) (prec),
static const Uint8 PRECEDENCE[] = {
	#include "lexemes.h"
	0,
};

static Bool source_read(Source *source, Context *context) {
	Array(Uint8) const contents = readfile(source->name, context);
	if (!contents) {
		return false;
	}
	source->contents.contents = contents;
	source->contents.length = array_size(contents);
	return true;
}

typedef struct Parser Parser;

struct Parser {
	Context *context;
	Tree *tree;

	Lexer lexer;

	Token this_token; // This token is being processed
	Token last_token; // Last token processed

	ProcedureType *this_procedure;

	Sint32 trace_depth;
	Sint32 expression_depth;

	Bool allow_newline;
	Bool allow_type;
	Bool allow_in;
};

static FORCE_INLINE void record(Parser *parser) {
	tree_record_token(parser->tree, parser->this_token);
}

#if defined(TRACE)
static void parser_trace_enter(Parser *parser, const char *function) {
	if (parser->trace_depth) {
		printf("%*c", parser->trace_depth * 2, ' ');
	}
	puts(function);
	parser->trace_depth++;
}

static FORCE_INLINE void parser_trace_leave(Parser *parser) {
	parser->trace_depth--;
}

#define TRACE_ENTER() parser_trace_enter(parser, __FUNCTION__)
#define TRACE_LEAVE() parser_trace_leave(parser)
#else
#define TRACE_ENTER()
#define TRACE_LEAVE()
#endif

static Bool parser_init(Parser *parser, Tree *tree, Context *context) {
	parser->context = context;
	parser->tree = tree;

	if (!lexer_init(&parser->lexer, &tree->source, context)) {
		return false;
	}

	parser->this_token = TOKEN_NIL;
	parser->last_token = TOKEN_NIL;

	parser->this_procedure = 0;

	parser->trace_depth = 0;
	parser->expression_depth = 0;

	parser->allow_newline = false;
	parser->allow_type = false;
	parser->allow_in = false;

	return true;
}

void parser_fini(Parser *parser) {
	lexer_fini(&parser->lexer);
}

static FORCE_INLINE Bool is_kind(Token token, Kind kind) {
	return token.kind == kind;
}
static FORCE_INLINE Bool is_operator(Token token, OperatorKind op) {
	return is_kind(token, KIND_OPERATOR) && token.as_operator == op;
}
static FORCE_INLINE Bool is_keyword(Token token, KeywordKind keyword) {
	return is_kind(token, KIND_KEYWORD) && token.as_keyword == keyword;
}
static FORCE_INLINE Bool is_assignment(Token token, AssignmentKind assignment) {
	return is_kind(token, KIND_ASSIGNMENT) && token.as_assignment == assignment;
}
static FORCE_INLINE Bool is_literal(Token token, LiteralKind literal) {
	return is_kind(token, KIND_LITERAL) && token.as_literal == literal;
}
static FORCE_INLINE Bool is_newline(Token token) {
	return is_kind(token, KIND_SEMICOLON) && string_compare(token.string, SCLIT("\n"));
}

static Token advancep(Parser *parser);

static Bool accepted_operator(Parser *parser, OperatorKind op) {
	if (is_operator(parser->this_token, op)) {
		advancep(parser);
		return true;
	}
	return false;
}

static Bool accepted_assignment(Parser *parser, AssignmentKind kind) {
	if (is_assignment(parser->this_token, kind)) {
		advancep(parser);
		return true;
	}
	return false;
}

static Bool accepted_kind(Parser *parser, Kind kind) {
	if (is_kind(parser->this_token, kind)) {
		advancep(parser);
		return true;
	}
	return false;
}

static Bool accepted_keyword(Parser *parser, KeywordKind keyword) {
	if (is_keyword(parser->this_token, keyword)) {
		advancep(parser);
		return true;
	}
	return false;
}

static Token peekp(Parser *parser);
static Bool accepted_separator(Parser *parser) {
	Context *const context = parser->context;
	const Token token = parser->this_token;
	if (accepted_operator(parser, OPERATOR_COMMA)) {
		return true;
	}
	if (is_kind(token, KIND_SEMICOLON)) {
		if (is_newline(token)) {
			const Token peek = peekp(parser);
			if (is_kind(peek, KIND_RBRACE) || is_operator(peek, OPERATOR_RPAREN)) {
				advancep(parser);
				return true;
			}
		}
		PARSE_ERROR("Expected a comma");
	}
	return false;
}

// Same as above but for control statements
static Bool accepted_control_statement_separator(Parser *parser) {
	const Token token = peekp(parser);
	if (!is_kind(token, KIND_LBRACE)) {
		return accepted_kind(parser, KIND_SEMICOLON);
	}
	if (string_compare(parser->this_token.string, SCLIT(";"))) {
		return accepted_kind(parser, KIND_SEMICOLON);
	}
	return false;
}

static Bool ignore_newline(const Parser *parser) {
	return parser->expression_depth > 0;
}

static Token peekp(Parser *parser) {
	Token token = lexer_peek(&parser->lexer);
	while (is_kind(token, KIND_COMMENT)) {
		token = lexer_peek(&parser->lexer);
	}
	return token;
}

static Bool advance_possible_newline(Parser *parser) {
	if (is_newline(parser->this_token)) {
		advancep(parser);
		return true;
	}
	return false;
}

static Bool advance_possible_newline_within(Parser *parser) {
	const Token token = parser->this_token;
	if (!is_newline(token)) {
		return false;
	}
	const Token next = peekp(parser);
	if (token.location.line + 1 < next.location.line) {
		return false;
	}
	if (!is_kind(next, KIND_LBRACE) && !is_keyword(next, KEYWORD_ELSE) && !is_keyword(next, KEYWORD_WHERE)) {
		return false;
	}
	advancep(parser);
	return true;
}

static Token advancep(Parser *parser) {
	const Token last = parser->this_token;
	parser->last_token = last;
	parser->this_token = lexer_next(&parser->lexer);
	while (is_kind(parser->this_token, KIND_COMMENT)) {
		parser->this_token = lexer_next(&parser->lexer);
	}
	if (is_kind(parser->this_token, KIND_SEMICOLON)) {
		if (ignore_newline(parser) && string_compare(parser->this_token.string, SCLIT("\n"))) {
			advancep(parser);
		}
	}
	return last;
}

static Token expect_kind(Parser *parser, Kind kind) {
	Context *const context = parser->context;
	const Token token = parser->this_token;
	if (!is_kind(token, kind)) {
		const String want = kind_to_string(kind);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_operator(Parser *parser, OperatorKind op) {
	Context *const context = parser->context;
	const Token token = parser->this_token;
	if ((is_operator(token, OPERATOR_IN) || is_operator(token, OPERATOR_NOT_IN))
	  && (parser->expression_depth >= 0 || parser->allow_in))
	{
		// Nothing to do here.
	} else if (!is_operator(token, op)) {
		const String want = operator_to_string(op);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected operator '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_keyword(Parser* parser, KeywordKind keyword) {
	Context *const context = parser->context;
	const Token token = parser->this_token;
	if (!is_keyword(token, keyword)) {
		const String want = keyword_to_string(keyword);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected keyword '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_assignment(Parser *parser, AssignmentKind assignment) {
	Context *const context = parser->context;
	const Token token = parser->this_token;
	if (!is_assignment(token, assignment)) {
		const String want = assignment_to_string(assignment);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected assignment '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_literal(Parser *parser, LiteralKind literal) {
	Context *const context = parser->context;
	const Token token = parser->this_token;
	if (!is_literal(token, literal)) {
		const String want = literal_to_string(literal);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected literal '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static void expect_semicolon(Parser *parser) {
	Context *const context = parser->context;

	if (accepted_kind(parser, KIND_SEMICOLON)) {
		return;
	}

	const Token token = parser->this_token;
	if (is_kind(token, KIND_RBRACE) || is_operator(token, OPERATOR_RPAREN)) {
		if (token.location.line == parser->last_token.location.line) {
			return;
		}
	}

	if (is_kind(parser->last_token, KIND_SEMICOLON)) {
		return;
	}

	if (is_kind(parser->this_token, KIND_EOF)) {
		return;
	}

	if (token.location.line == parser->last_token.location.line) {
		const String got = token_to_string(token);
		PARSE_ERROR("Expected semicolon, got '%.*s'", SFMT(got));
	}
}

#define CCONVENTION(enumerator, string, ...) SLIT(string),
static const String CCONVENTIONS[] = {
	#include "lexemes.h"
};

static CallingConvention string_to_calling_convention(String input) {
	for (Size i = 0; i < sizeof CCONVENTIONS / sizeof *CCONVENTIONS; i++) {
		if (string_compare(input, CCONVENTIONS[i])) {
			return CAST(CallingConvention, i + 1);
		}
	}
	/**/ if (string_compare(input, SCLIT("c")))    return CCONV_CDECL;
	else if (string_compare(input, SCLIT("std")))  return CCONV_STDCALL;
	else if (string_compare(input, SCLIT("fast"))) return CCONV_FASTCALL;
	return CCONV_INVALID;
}

// Identifier = ['$'] Char { Char | Digit }
static Identifier *parse_identifier(Parser *parser, Bool poly) {
	Context *const context = parser->context;

	TRACE_ENTER();

	record(parser);

	Token token = parser->this_token;
	if (is_kind(token, KIND_IDENTIFIER)) {
		// NOTE(dweiler): Should write this a different way?
		advancep(parser);
	} else if (is_kind(token, KIND_CONST)) {
		advancep(parser); // KIND_CONST
		if (!accepted_kind(parser, KIND_IDENTIFIER)) {
			const String got = token_to_string(parser->this_token);
			PARSE_ERROR("Expected identifier, got '%.*s'", SFMT(got));
		}
		token = parser->this_token;
	} else {
		const String got = token_to_string(token);
		PARSE_ERROR("Expected identifier or '$', got '%.*s'", SFMT(got));
	}

	Identifier *identifier = tree_new_identifier(parser->tree, token.string, poly);

	TRACE_LEAVE();

	return identifier;
}

static Expression *parse_operand(Parser *parser, Bool lhs);
static Expression *parse_expression(Parser *parser, Bool lhs);
static Expression *parse_atom_expression(Parser *parser, Expression *operand, Bool lhs);

static Type *parse_type_or_identifier(Parser *parser) {
	TRACE_ENTER();

	const Sint32 depth = parser->expression_depth;
	const Bool allow_type = parser->allow_type;
	parser->expression_depth = -1;
	parser->allow_type = true;

	Expression *const operand = parse_operand(parser, true);
	Expression *const expression = parse_atom_expression(parser, operand, true);

	parser->expression_depth = depth;
	parser->allow_type = allow_type;

	if (expression) {
		// When TypeExpression we can strip the nesting.
		Type *type = 0;
		if (expression->kind == EXPRESSION_TYPE) {
			type = RCAST(TypeExpression *, expression)->type;
		} else {
			type = RCAST(Type *, tree_new_expression_type(parser->tree, expression));
		}
		TRACE_LEAVE();
		return type;
	}

	TRACE_LEAVE();
	return 0;
}

static Type *parse_type(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Type *const type = parse_type_or_identifier(parser);
	if (!type) {
		const String name = token_to_string(parser->this_token);
		PARSE_ERROR("Expected a type, got '%.*s' instead", SFMT(name));
	}

	TRACE_LEAVE();

	return type;
}

static CompoundLiteralExpression *parse_compound_literal_expression(Parser *parser, Type *type);

static Type *parse_variable_name_or_type(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	if (is_operator(parser->this_token, OPERATOR_ELLIPSIS)) {
		advancep(parser);
		Type *type = parse_type_or_identifier(parser);
		if (!type) {
			PARSE_ERROR("Missing type after '..'");
		}
		return type;
	} else if (is_keyword(parser->this_token, KEYWORD_TYPEID)) {
		expect_keyword(parser, KEYWORD_TYPEID);
		Type *specialization = 0;
		if (accepted_operator(parser, OPERATOR_QUO)) {
			specialization = parse_type(parser);
		}
		TypeidType *const type = tree_new_typeid_type(parser->tree, specialization);
		TRACE_LEAVE();
		return RCAST(Type *, type);
	}

	Type *const type = parse_type(parser);

	TRACE_LEAVE();

	return type;
}

static Identifier *evaluate_identifier_expression(const Expression *expression);

static Identifier *evaluate_identifier_type(const Type *type) {
	switch (type->kind) {
	case TYPE_EXPRESSION:
		return evaluate_identifier_expression(RCAST(const ExpressionType *, type)->expression);
	case TYPE_POLY:
		{
			const PolyType *const poly = RCAST(const PolyType *, type);
			if (!poly->specialization) {
				return evaluate_identifier_type(poly->type);
			}
			FALLTHROUGH();
		}
	default:
		return 0;
	}
	UNREACHABLE();
}

static Identifier *evaluate_identifier_expression(const Expression *expression) {
	switch (expression->kind) {
	case EXPRESSION_IDENTIFIER:
		return RCAST(const IdentifierExpression *, expression)->identifier;
	case EXPRESSION_TYPE:
		return evaluate_identifier_type(RCAST(const TypeExpression *, expression)->type);
	case EXPRESSION_SELECTOR:
		{
			const SelectorExpression *selector = RCAST(const SelectorExpression *, expression);
			if (!selector->operand) {
				// Only allowed for implicit selector expressions.
				return selector->identifier;
			}
		}
		return 0;
	default:
		return 0;
	}
	UNREACHABLE();
}

static FieldFlag parse_field_flags(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	FieldFlag flags = CAST(FieldFlag, 0);
	if (is_keyword(parser->this_token, KEYWORD_USING)) {
		advancep(parser);
		flags |= FIELD_FLAG_USING;
	}

	while (is_kind(parser->this_token, KIND_DIRECTIVE)) {
		const DirectiveKind directive = parser->this_token.as_directive;
		switch (directive) {
		case DIRECTIVE_ANY_INT:
			flags |= FIELD_FLAG_ANY_INT;
			break;
		case DIRECTIVE_C_VARARG:
			flags |= FIELD_FLAG_C_VARARG;
			break;
		case DIRECTIVE_NO_ALIAS:
			flags |= FIELD_FLAG_NO_ALIAS;
			break;
		case DIRECTIVE_SUBTYPE:
			flags |= FIELD_FLAG_SUBTYPE;
			break;
		case DIRECTIVE_CONST:
			flags |= FIELD_FLAG_CONST;
			break;
		default:
			{
				const String name = directive_to_string(directive);
				PARSE_ERROR("Unknown field directive '%.*s'", SFMT(name));
			}
		}
		advancep(parser);
	}

	TRACE_LEAVE();

	return flags;
}

static Array(Field*) parse_field_list(Parser *parser, Bool is_struct) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Array(Field*) fields = array_make(context);

	if (is_operator(parser->this_token, OPERATOR_RPAREN)) {
		TRACE_LEAVE();
		return fields;
	}

	const Bool allow_newline = parser->allow_newline;
	parser->allow_newline = true;

	// When parsing a field list we may have
	// 	Type0, Type1, Type2, ..., or
	//	Ident0, Ident1, Ident2, ... : Type0
	Array(Type*) list = array_make(context);
	Array(FieldFlag) field_flags = array_make(context);
	Type *type = 0;
	while ((is_struct
	          ? !is_kind(parser->this_token, KIND_RBRACE)
	          : !is_operator(parser->this_token, OPERATOR_COLON))
	    && !is_kind(parser->this_token, KIND_EOF))
	{
		const FieldFlag flags = parse_field_flags(parser);
		Type *const name_or_type = parse_variable_name_or_type(parser);
		array_push(list, name_or_type);
		array_push(field_flags, flags);
		if (!accepted_separator(parser)) {
			break;
		}
	}

	if (is_operator(parser->this_token, OPERATOR_COLON)) {
		// So everything we parsed before is actually an identifier.
		expect_operator(parser, OPERATOR_COLON);
		if (!is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			// With the following type.
			type = parse_variable_name_or_type(parser);
		}
	}

	// Handle optional default value now.
	Expression *value = 0;
	if (accepted_assignment(parser, ASSIGNMENT_EQ)) {
		value = parse_expression(parser, false);
	}

	String tag = STRING_NIL;
	if (type && !value && is_literal(parser->this_token, LITERAL_STRING)) {
		tag = parser->this_token.string;
		advancep(parser);
	}

	const Size n_elements = array_size(list);
	if (type) {
		// The list are identifiers which have type 'type'
		for (Size i = 0; i < n_elements; i++) {
			const Type *const name = list[i];
			const FieldFlag flags = field_flags[i];
			Identifier *const identifier = evaluate_identifier_type(name);
			if (!identifier) {
				PARSE_ERROR("Expected identifier in field list");
			}
			array_push(fields, tree_new_field(parser->tree, type, identifier, value, tag, flags));
		}
	} else {
		record(parser);

		// Generate identifiers with '__unnamed_%d'.
		for (Size i = 0; i < n_elements; i++) {
			Type *const type = list[i];
			const FieldFlag flags = field_flags[i];
			StrBuf buf;
			strbuf_init(&buf, context);
			strbuf_put_string(&buf, SCLIT("__unnamed_"));
			strbuf_put_int(&buf, CAST(Sint32, i), 10);
			Identifier *name = tree_new_identifier(parser->tree, strbuf_result(&buf), false);
			array_push(fields, tree_new_field(parser->tree, type, name, value, tag, flags));
		}
	}

	// Early out for trailing commas.
	if (!accepted_separator(parser)) {
		parser->allow_newline = allow_newline;
		TRACE_LEAVE();
		return fields;
	}

	while ((is_struct
	          ? !is_kind(parser->this_token, KIND_RBRACE)
	          : !is_operator(parser->this_token, OPERATOR_RPAREN))
	    && !is_kind(parser->this_token, KIND_EOF)
	    && !is_kind(parser->this_token, KIND_SEMICOLON))
	{
		const FieldFlag flags = parse_field_flags(parser);
		Type *type = 0;
		Array(Identifier*) names = array_make(context);
		for (;;) {
			Identifier *const name = parse_identifier(parser, false);
			array_push(names, name);
			const Token token = parser->this_token;
			if (!is_operator(token, OPERATOR_COMMA) || is_kind(token, KIND_EOF)) {
				break;
			}
			advancep(parser);
		}

		expect_operator(parser, OPERATOR_COLON);
		if (!is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			type = parse_variable_name_or_type(parser);
		}
		Expression *value = 0;
		if (accepted_assignment(parser, ASSIGNMENT_EQ)) {
			value = parse_expression(parser, false);
		}
		String tag = STRING_NIL;
		if (type && !value && is_literal(parser->this_token, LITERAL_STRING)) {
			tag = parser->this_token.string;
			advancep(parser);
		}
		const Size n_names = array_size(names);
		for (Size i = 0; i < n_names; i++) {
			Identifier *const name = names[i];
			array_push(fields, tree_new_field(parser->tree, type, name, value, tag, flags));
		}

		if (!accepted_separator(parser)) {
			break;
		}
	}

	parser->allow_newline = allow_newline;

	TRACE_LEAVE();

	return fields;
}

static Array(Field*) parse_procedure_results(Parser *parser, Bool *diverging) {
	Context *const context = parser->context;

	TRACE_ENTER();

	if (!accepted_operator(parser, OPERATOR_ARROW)) {
		TRACE_LEAVE();
		return 0;
	}

	if (accepted_operator(parser, OPERATOR_NOT)) {
		*diverging = true;
		TRACE_LEAVE();
		return 0;
	}
	
	Array(Field*) fields = array_make(context);

	if (!is_operator(parser->this_token, OPERATOR_LPAREN)) {
		Type *const type = parse_type(parser);
		record(parser);
		Identifier *const identifier = tree_new_identifier(parser->tree, SCLIT("__unnamed"), false);
		Field *const field = tree_new_field(parser->tree, type, identifier, 0, STRING_NIL, CAST(FieldFlag, 0));
		array_push(fields, field);
		TRACE_LEAVE();
		return fields;
	}

	expect_operator(parser, OPERATOR_LPAREN);

	fields = parse_field_list(parser, false);

	advance_possible_newline(parser);

	expect_operator(parser, OPERATOR_RPAREN);

	TRACE_LEAVE();

	return fields;
}

static ProcedureType *parse_procedure_type(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	CallingConvention convention = CCONV_ODIN;
	if (is_literal(parser->this_token, LITERAL_STRING)) {
		const Token token = expect_literal(parser, LITERAL_STRING);
		const String string = token.string;
		convention = string_to_calling_convention(string_unquote(string, "\"`"));
		if (convention == CCONV_INVALID) {
			PARSE_ERROR("Unknown calling convention '%.*s'", SFMT(string));
		}
	}

	expect_operator(parser, OPERATOR_LPAREN);

	Array(Field*) const fields = parse_field_list(parser, false);

	advance_possible_newline(parser);

	expect_operator(parser, OPERATOR_RPAREN);

	// Check if generic (needs to be monomorphized) procedure.
	Bool is_generic = false;
	Size n_fields = array_size(fields);
	for (Size i = 0; i < n_fields; i++) {
		const Field *field = fields[i];
		if (field->name->poly) {
			is_generic = true;
			break;
		}
		if (field->type && field->type->poly) {
			is_generic = true;
			break;
		}
	}

	Bool diverging = false;
	Array(Field*) const results = parse_procedure_results(parser, &diverging);

	// We start all procedure types with these flags by default.
	ProcedureFlag flags = CAST(ProcedureFlag, 0);
	flags |= PROC_FLAG_BOUNDS_CHECK;
	flags |= PROC_FLAG_TYPE_ASSERT;
	if (diverging) {
		flags |= PROC_FLAG_DIVERGING;
	}

	ProcedureType *type = 0;
	if (is_generic) {
		type = RCAST(ProcedureType *, tree_new_generic_procedure_type(parser->tree, fields, results, flags, convention));
	} else {
		type = RCAST(ProcedureType *, tree_new_concrete_procedure_type(parser->tree, fields, results, flags, convention));
	}

	TRACE_LEAVE();

	return type;
}

static BlockStatement *parse_body(Parser *parser, BlockFlag flags);
static ListExpression *parse_rhs_list_expression(Parser *parser);

static Expression *parse_procedure(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	ProcedureType *const type = parse_procedure_type(parser);

	advance_possible_newline_within(parser);

	ListExpression *where_clauses = 0;
	if (is_keyword(parser->this_token, KEYWORD_WHERE)) {
		expect_keyword(parser, KEYWORD_WHERE);
		const Sint32 expression_depth = parser->expression_depth;
		parser->expression_depth = -1;
		where_clauses = parse_rhs_list_expression(parser);
		parser->expression_depth = expression_depth;
	}

	// We start each procedure with the flags of the procedure type.
	ProcedureFlag flags = type->flags;

	// Then we add or remove optional flags.
	while (is_kind(parser->this_token, KIND_DIRECTIVE)) {
		const Token token = expect_kind(parser, KIND_DIRECTIVE);
		const String name = directive_to_string(token.as_directive);
		switch (token.as_directive) {
		case DIRECTIVE_OPTIONAL_OK:
			flags |= PROC_FLAG_OPTIONAL_OK;
			break;
		case DIRECTIVE_OPTIONAL_ALLOCATOR_ERROR:
			flags |= PROC_FLAG_OPTIONAL_ALLOCATION_ERROR;
			break;
		case DIRECTIVE_BOUNDS_CHECK:
			flags |= PROC_FLAG_BOUNDS_CHECK;
			break;
		case DIRECTIVE_NO_BOUNDS_CHECK:
			flags &= ~PROC_FLAG_BOUNDS_CHECK;
			break;
		case DIRECTIVE_TYPE_ASSERT:
			flags |= PROC_FLAG_TYPE_ASSERT;
			break;
		case DIRECTIVE_NO_TYPE_ASSERT:
			flags &= ~PROC_FLAG_TYPE_ASSERT;
			break;
		default:
			PARSE_ERROR("Cannot use directive '%.*s' on a procedure", SFMT(name));
		}
	}

	// Update the procedure type flags early.
	type->flags = flags;

	advance_possible_newline_within(parser);

	if (accepted_kind(parser, KIND_UNDEFINED)) {
		if (where_clauses) {
			PARSE_ERROR("Cannot use 'where' clause on procedures without bodies");
		}
		ProcedureExpression *const expression = tree_new_procedure_expression(parser->tree, type, 0, 0);
		TRACE_LEAVE();
		return RCAST(Expression *, expression);
	} else if (is_kind(parser->this_token, KIND_LBRACE)) {
		BlockFlag block_flags = CAST(BlockFlag, 0);
		if (flags & PROC_FLAG_BOUNDS_CHECK) block_flags |= BLOCK_FLAG_BOUNDS_CHECK;
		if (flags & PROC_FLAG_TYPE_ASSERT)  block_flags |= BLOCK_FLAG_TYPE_ASSERT;
		parser->this_procedure = type;
		BlockStatement *const body = parse_body(parser, block_flags);
		ProcedureExpression *const expression = tree_new_procedure_expression(parser->tree, type, where_clauses, body);
		TRACE_LEAVE();
		return RCAST(Expression *, expression);
	}

	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return RCAST(Expression *, expression);
}

static CallExpression *parse_call_expression(Parser *parser, Expression *operand);

static Statement *parse_statement(Parser *parser, BlockFlag block_flags);
static Expression *parse_unary_expression(Parser *parser, Bool lhs);

// Given a name generates a call statement to intrinsics.%s(...)
static Expression *parse_directive_call_expression(Parser *parser, String name) {
	record(parser);
	Identifier *intrinsics = tree_new_identifier(parser->tree, SCLIT("intrinsics"), false);
	Identifier *directive = tree_new_identifier(parser->tree, name, false);
	IdentifierExpression *identifier = tree_new_identifier_expression(parser->tree, intrinsics);
	SelectorExpression *selector = tree_new_selector_expression(parser->tree, RCAST(Expression *, identifier), directive);
	CallExpression *expression = parse_call_expression(parser, RCAST(Expression *, selector));
	return RCAST(Expression *, expression);
}

static Statement *parse_directive_call_statement(Parser *parser, String name) {
	Expression *expression = parse_directive_call_expression(parser, name);
	return RCAST(Statement *, tree_new_expression_statement(parser->tree, RCAST(Expression *, expression)));
}

static Statement *parse_directive_for_statement(Parser *parser, BlockFlag block_flags) {
	Context *const context = parser->context;

	TRACE_ENTER();

	const Token token = expect_kind(parser, KIND_DIRECTIVE);
	Statement *statement = 0;
	switch (token.as_directive) {
	case DIRECTIVE_BOUNDS_CHECK:
		statement = parse_statement(parser, block_flags | BLOCK_FLAG_BOUNDS_CHECK);
		break;
	case DIRECTIVE_NO_BOUNDS_CHECK:
		statement = parse_statement(parser, block_flags & ~BLOCK_FLAG_BOUNDS_CHECK);
		break;
	case DIRECTIVE_TYPE_ASSERT:
		statement = parse_statement(parser, block_flags | BLOCK_FLAG_TYPE_ASSERT);
		break;
	case DIRECTIVE_NO_TYPE_ASSERT:
		statement = parse_statement(parser, block_flags & ~BLOCK_FLAG_TYPE_ASSERT);
		break;
	case DIRECTIVE_PARTIAL:
		// Ignore the partial directive for now.
		statement = parse_statement(parser, block_flags);
		break;
	case DIRECTIVE_ASSERT:
		statement = parse_directive_call_statement(parser, SCLIT("assert"));
		break;
	case DIRECTIVE_PANIC:
		statement = parse_directive_call_statement(parser, SCLIT("panic"));
		break;
	case DIRECTIVE_LOAD:
		statement = parse_directive_call_statement(parser, SCLIT("load"));
		break;
	case DIRECTIVE_UNROLL:
		// Ignore the unroll directive for now.
		statement = parse_statement(parser, block_flags);
		break;
	case DIRECTIVE_FORCE_INLINE:
		FALLTHROUGH();
	case DIRECTIVE_FORCE_NO_INLINE:
		// Ignore the force_inline and force_no_inline directives for now.
		statement = parse_statement(parser, block_flags);
		break;
	default:
		{
			const String directive = directive_to_string(token.as_directive);
			PARSE_ERROR("Unsupported directive '%.*s' in statement", SFMT(directive));
		}
		break;
	}

	TRACE_LEAVE();

	return statement;
}

static Expression *parse_value(Parser *parser);

static Statement *parse_attributes_for_statement(Parser *parser, BlockFlag block_flags) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_kind(parser, KIND_ATTRIBUTE);

	Array(Field*) attributes = array_make(context);
	if (is_kind(parser->this_token, KIND_IDENTIFIER)) {
		Identifier *const identifier = parse_identifier(parser, false);
		Field *const field = tree_new_field(parser->tree, 0, identifier, 0, STRING_NIL, CAST(FieldFlag, 0));
		array_push(attributes, field);
	} else {
		expect_operator(parser, OPERATOR_LPAREN);
		parser->expression_depth++;
		while (!is_operator(parser->this_token, OPERATOR_RPAREN) &&
		       !is_kind(parser->this_token, KIND_EOF))
		{
			Field *field = 0;
			Identifier *const identifier = parse_identifier(parser, false);
			if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
				expect_assignment(parser, ASSIGNMENT_EQ);
				Expression *value = parse_value(parser);
				field = tree_new_field(parser->tree, 0, identifier, value, STRING_NIL, CAST(FieldFlag, 0));
			} else {
				field = tree_new_field(parser->tree, 0, identifier, 0, STRING_NIL, CAST(FieldFlag, 0));
			}
			array_push(attributes, field);
			if (!accepted_separator(parser)) {
				break;
			}
		}
		parser->expression_depth--;
		expect_operator(parser, OPERATOR_RPAREN);
	}

	advance_possible_newline(parser);

	Statement *const statement = parse_statement(parser, block_flags);
	switch (statement->kind) {
	case STATEMENT_DECLARATION:
		RCAST(DeclarationStatement *, statement)->attributes = attributes;
		break;
	case STATEMENT_FOREIGN_BLOCK:
		RCAST(ForeignBlockStatement *, statement)->attributes = attributes;
		break;
	case STATEMENT_FOREIGN_IMPORT:
		RCAST(ForeignImportStatement *, statement)->attributes = attributes;
		break;
	default:
		PARSE_ERROR("Unexpected statement after attribute");
	}

	TRACE_LEAVE();

	return statement;
}

static IdentifierExpression *parse_identifier_expression(Parser *parser) {
	TRACE_ENTER();

	Identifier *const identifier = parse_identifier(parser, false);
	IdentifierExpression *const expression = tree_new_identifier_expression(parser->tree, identifier);

	TRACE_LEAVE();

	return expression;
}

static LiteralExpression *parse_literal_expression(Parser *parser) {
	TRACE_ENTER();

	const Token token = advancep(parser);
	LiteralExpression *const expression = tree_new_literal_expression(parser->tree, token.as_literal, token.string);

	TRACE_LEAVE();

	return expression;
}

// bit_set[T; U]
static TypeExpression *parse_bit_set_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_BIT_SET);
	expect_operator(parser, OPERATOR_LBRACKET);
	Expression *const expression = parse_expression(parser, false);
	Type *underlying = 0;
	if (accepted_kind(parser, KIND_SEMICOLON)) {
		underlying = parse_type(parser);
	}
	expect_operator(parser, OPERATOR_RBRACKET);

	BitSetType *const type = tree_new_bit_set_type(parser->tree, expression, underlying);
	TypeExpression *const result = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return result;
}

// typeid
static TypeExpression *parse_typeid_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_TYPEID);

	TypeidType *const type = tree_new_typeid_type(parser->tree, 0);
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// map[K]V
static TypeExpression *parse_map_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_MAP);

	expect_operator(parser, OPERATOR_LBRACKET);
	Expression *const key_expression = parse_expression(parser, true);
	expect_operator(parser, OPERATOR_RBRACKET);
	Type *const value = parse_type(parser);
	ExpressionType *const key = tree_new_expression_type(parser->tree, key_expression);
	MapType *const type = tree_new_map_type(parser->tree, RCAST(Type *, key), value);
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// matrix[R,C]T
static TypeExpression *parse_matrix_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_MATRIX);

	expect_operator(parser, OPERATOR_LBRACKET);
	Expression *const rows = parse_expression(parser, true);
	expect_operator(parser, OPERATOR_COMMA);
	Expression *const columns = parse_expression(parser, true);
	expect_operator(parser, OPERATOR_RBRACKET);

	Type *const base_type = parse_type(parser);

	MatrixType *const type = tree_new_matrix_type(parser->tree, rows, columns, base_type);
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// ^T
static TypeExpression *parse_pointer_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_operator(parser, OPERATOR_POINTER);

	PointerType *const type = tree_new_pointer_type(parser->tree, parse_type(parser));
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// [^]T
static TypeExpression *parse_multi_pointer_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_operator(parser, OPERATOR_POINTER);
	expect_operator(parser, OPERATOR_RBRACKET);

	MultiPointerType *const type = tree_new_multi_pointer_type(parser->tree, parse_type(parser));
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// [?]T or [N]T
static TypeExpression *parse_array_type_expression(Parser *parser, Bool parse_count) {
	TRACE_ENTER();

	Expression *count = 0;
	if (parse_count) {
		parser->expression_depth++;
		count = parse_expression(parser, false);
		parser->expression_depth--;
	} else {
		expect_operator(parser, OPERATOR_QUESTION);
	}

	expect_operator(parser, OPERATOR_RBRACKET);

	ArrayType *const type = tree_new_array_type(parser->tree, parse_type(parser), count);
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// [dynamic]T
static TypeExpression *parse_dynamic_array_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_operator(parser, OPERATOR_RBRACKET);

	DynamicArrayType *const type = tree_new_dynamic_array_type(parser->tree, parse_type(parser));
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// []T
static TypeExpression *parse_slice_type_expression(Parser *parser) {
	TRACE_ENTER();

	SliceType *const type = tree_new_slice_type(parser->tree, parse_type(parser));
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// distinct T
static TypeExpression *parse_distinct_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_DISTINCT);

	DistinctType *const type = tree_new_distinct_type(parser->tree, parse_type(parser));
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// proc{...}
static ProcedureGroupExpression *parse_procedure_group_expression(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Array(Expression*) expressions = array_make(context);
	expect_kind(parser, KIND_LBRACE);
	while (!is_kind(parser->this_token, KIND_RBRACE)
	    && !is_kind(parser->this_token, KIND_EOF))
	{
		Expression *expression = parse_expression(parser, false);
		array_push(expressions, expression);
		if (!accepted_separator(parser)) {
			break;
		}
	}
	expect_kind(parser, KIND_RBRACE);

	ProcedureGroupExpression *expression = 
		tree_new_procedure_group_expression(parser->tree, expressions);

	TRACE_LEAVE();

	return expression;
}

// struct
static TypeExpression *parse_struct_type_expression(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_STRUCT);

	Array(Field*) parameters = array_make(context);
	if (accepted_operator(parser, OPERATOR_LPAREN)) {
		parameters = parse_field_list(parser, false);
		expect_operator(parser, OPERATOR_RPAREN);
	}

	Expression *align = 0;
	StructFlag flags = CAST(StructFlag, 0);
	while (accepted_kind(parser, KIND_DIRECTIVE)) {
		const DirectiveKind directive = parser->last_token.as_directive;
		switch (directive) {
		case DIRECTIVE_PACKED:
			flags |= STRUCT_FLAG_PACKED;
			break;
		case DIRECTIVE_ALIGN:
			{
				const Sint32 expression_depth = parser->expression_depth;
				parser->expression_depth = -1;
				// TODO(dweiler): Enable when Odin requires it.
				// expect_operator(parser, OPERATOR_LPAREN);
				align = parse_expression(parser, true);
				// expect_operator(parser, OPERATOR_RPAREN);
				parser->expression_depth = expression_depth;
				break;
			}
		case DIRECTIVE_RAW_UNION:
			flags |= STRUCT_FLAG_UNION;
			break;
		case DIRECTIVE_NO_COPY:
			flags |= STRUCT_FLAG_UNCOPYABLE;
			break;
		default:
			{
				const String name = directive_to_string(directive);
				PARSE_ERROR("Unexpected directive '%.*s' on structure", SFMT(name));
			}
		}
	}

	advance_possible_newline_within(parser);

	ListExpression *where_clauses = 0;
	if (accepted_keyword(parser, KEYWORD_WHERE)) {
		const Sint32 expression_depth = parser->expression_depth;
		parser->expression_depth = -1;
		where_clauses = parse_rhs_list_expression(parser);
		parser->expression_depth = expression_depth;
	}

	advance_possible_newline_within(parser);

	expect_kind(parser, KIND_LBRACE);
	Array(Field*) const fields = parse_field_list(parser, true);
	expect_kind(parser, KIND_RBRACE);

	Type *type = 0;
	if (array_size(parameters) == 0) {
		type = RCAST(Type *, tree_new_concrete_struct_type(parser->tree, flags, align, fields, where_clauses));
	} else {
		type = RCAST(Type *, tree_new_generic_struct_type(parser->tree, flags, align, parameters, fields, where_clauses));
	}

	TypeExpression *const expression = tree_new_type_expression(parser->tree, type);
	
	TRACE_LEAVE();

	return expression;
}

// union
static TypeExpression *parse_union_type_expression(Parser *parser) {
	Context *context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_UNION);

	Array(Field*) parameters = array_make(context);
	if (accepted_operator(parser, OPERATOR_LPAREN)) {
		parameters = parse_field_list(parser, false);
		expect_operator(parser, OPERATOR_RPAREN);
	}

	Expression *align = 0;
	UnionFlag flags = CAST(UnionFlag, 0);
	while (accepted_kind(parser, KIND_DIRECTIVE)) {
		const DirectiveKind directive = parser->last_token.as_directive;
		switch (directive) {
		case DIRECTIVE_ALIGN:
			{
				const Sint32 expression_depth = parser->expression_depth;
				parser->expression_depth = -1;
				align = parse_expression(parser, true);
				parser->expression_depth = expression_depth;
				break;
			}
		case DIRECTIVE_NO_NIL:
			flags |= UNION_FLAG_NO_NIL;
			break;
		case DIRECTIVE_SHARED_NIL:
			flags |= UNION_FLAG_SHARED_NIL;
			break;
		case DIRECTIVE_MAYBE:
			flags |= UNION_FLAG_MAYBE;
			break;
		default:
			{
				const String name = directive_to_string(directive);
				PARSE_ERROR("Unexpected directive '%.*s' on union", name);
			}
		}
	}

	advance_possible_newline_within(parser);

	ListExpression *where_clauses = 0;
	if (accepted_keyword(parser, KEYWORD_WHERE)) {
		const Sint32 expression_depth = parser->expression_depth;
		parser->expression_depth = -1;
		where_clauses = parse_rhs_list_expression(parser);
		parser->expression_depth = expression_depth;
	}

	advance_possible_newline_within(parser);

	expect_kind(parser, KIND_LBRACE);

	Array(Type*) variants = array_make(context);
	while (!is_kind(parser->this_token, KIND_RBRACE)
	   &&  !is_kind(parser->this_token, KIND_EOF))
	{
		Type *const type = parse_type(parser);
		array_push(variants, type);
		if (!accepted_separator(parser)) {
			break;
		}
	}

	expect_kind(parser, KIND_RBRACE);

	UnionType *type = 0;
	if (parameters) {
		type = RCAST(UnionType *, tree_new_generic_union_type(parser->tree, flags, align, parameters, variants, where_clauses));
	} else {
		type = RCAST(UnionType *, tree_new_concrete_union_type(parser->tree, flags, align, variants, where_clauses));
	}

	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

static Expression *parse_value(Parser *parser) {
	TRACE_ENTER();

	if (is_kind(parser->this_token, KIND_LBRACE)) {
		CompoundLiteralExpression *const expression = parse_compound_literal_expression(parser, 0);
		TRACE_LEAVE();
		return RCAST(Expression *, expression);
	}

	Expression *const value = parse_expression(parser, false);

	TRACE_LEAVE();

	return value;
}

static TypeExpression *parse_enum_type_expression(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_ENUM);

	Type *base_type = 0;
	if (!is_kind(parser->this_token, KIND_LBRACE)) {
		base_type = parse_type(parser);
	}

	advance_possible_newline_within(parser);

	Array(Field*) fields = array_make(context);
	expect_kind(parser, KIND_LBRACE);
	while (!is_kind(parser->this_token, KIND_RBRACE)
	    && !is_kind(parser->this_token, KIND_EOF))
	{
		Expression *name = parse_value(parser);
		if (name->kind != EXPRESSION_IDENTIFIER) {
			PARSE_ERROR("Expected identifier for enumerator");
		}

		Expression *value = 0;
		if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			expect_assignment(parser, ASSIGNMENT_EQ);
			value = parse_value(parser);
		}
	
		Identifier *const identifier = RCAST(IdentifierExpression *, name)->identifier;
	
		Field *const field = tree_new_field(parser->tree, 0, identifier, value, STRING_NIL, CAST(FieldFlag, 0));
		array_push(fields, field);
	
		if (!accepted_separator(parser)) {
			break;
		}
	}
	expect_kind(parser, KIND_RBRACE);
	
	EnumType *const type = tree_new_enum_type(parser->tree, base_type, fields);
	TypeExpression *const expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// $T or $T/$U
static TypeExpression *parse_poly_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_kind(parser, KIND_CONST);

	IdentifierExpression *identifier = tree_new_identifier_expression(parser->tree, parse_identifier(parser, true));
	ExpressionType *type = tree_new_expression_type(parser->tree, RCAST(Expression *, identifier));

	Type *specialization = 0;
	if (accepted_operator(parser, OPERATOR_QUO)) {
		specialization = parse_type(parser);
	}

	PolyType *poly = tree_new_poly_type(parser->tree, RCAST(Type *, type), specialization);
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, poly));

	TRACE_LEAVE();

	return expression;
}

// ---
static UndefinedExpression *parse_undefined_expression(Parser *parser) {
	TRACE_ENTER();

	expect_kind(parser, KIND_UNDEFINED);

	UndefinedExpression *expression = tree_new_undefined_expression(parser->tree);

	TRACE_LEAVE();

	return expression;
}

static Expression *parse_directive_prefix(Parser *parser, Bool lhs) {
	Context *const context = parser->context;

	TRACE_ENTER();

	const Token token = expect_kind(parser, KIND_DIRECTIVE);
	switch (token.as_directive) {
	case DIRECTIVE_TYPE:
		{
			Type *const type = parse_type(parser);
			TypeExpression *const expression = tree_new_type_expression(parser->tree, type);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_SIMD:
		{
			Type *const type = parse_type(parser);
			if (type->kind != TYPE_ARRAY) {
				PARSE_ERROR("Can only apply '#simd' directive to array type");
			}
			TypeExpression *const expression = tree_new_type_expression(parser->tree, type);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_SOA:
		FALLTHROUGH();
	case DIRECTIVE_SPARSE:
		{
			Type *const type = parse_type(parser);
			TypeExpression *const expression = tree_new_type_expression(parser->tree, type);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_PARTIAL:
		{
			Expression *const expression = parse_expression(parser, lhs);
			TRACE_LEAVE();
			return expression;
		}
	case DIRECTIVE_BOUNDS_CHECK:    FALLTHROUGH();
	case DIRECTIVE_NO_BOUNDS_CHECK: FALLTHROUGH();
	case DIRECTIVE_TYPE_ASSERT:     FALLTHROUGH();
	case DIRECTIVE_NO_TYPE_ASSERT:
		{
			Expression *const expression = parse_expression(parser, lhs);
			TRACE_LEAVE();
			return expression;
		}
	case DIRECTIVE_FORCE_INLINE:    FALLTHROUGH();
	case DIRECTIVE_FORCE_NO_INLINE:
		{
			Expression *const expression = parse_expression(parser, lhs);
			TRACE_LEAVE();
			return expression;
		}
	case DIRECTIVE_CONFIG:
		{
			Expression *const expression = parse_directive_call_expression(parser, SCLIT("config"));
			TRACE_LEAVE();
			return expression;
		}
	case DIRECTIVE_DEFINED:
		{
			Expression *const expression = parse_directive_call_expression(parser, SCLIT("defined"));
			TRACE_LEAVE();
			return expression;
		}
	case DIRECTIVE_LOAD:
		{
			Expression *const expression = parse_directive_call_expression(parser, SCLIT("load"));
			TRACE_LEAVE();
			return expression;
		}
	case DIRECTIVE_CALLER_LOCATION:
		{
			Identifier *intrinsics_identifier = tree_new_identifier(parser->tree, SCLIT("intrinsics"), false);
			Identifier *caller_location_identifier = tree_new_identifier(parser->tree, SCLIT("caller_location"), false);
			IdentifierExpression *operand = tree_new_identifier_expression(parser->tree, intrinsics_identifier);
			SelectorExpression *expression = tree_new_selector_expression(parser->tree, RCAST(Expression *, operand), caller_location_identifier);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_PROCEDURE:
		{
			Identifier *intrinsics_identifier = tree_new_identifier(parser->tree, SCLIT("intrinsics"), false);
			Identifier *procedure_identifier = tree_new_identifier(parser->tree, SCLIT("procedure"), false);
			IdentifierExpression *operand = tree_new_identifier_expression(parser->tree, intrinsics_identifier);
			SelectorExpression *expression = tree_new_selector_expression(parser->tree, RCAST(Expression *, operand), procedure_identifier);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_RELATIVE:
		{
			parse_directive_call_expression(parser, SCLIT("relative"));
			Type *const pointer_type = parse_type(parser);
			TRACE_LEAVE();
			return RCAST(Expression *, tree_new_type_expression(parser->tree, pointer_type));
		}

	default:
		{
			const String name = directive_to_string(token.as_directive);
			PARSE_ERROR("Unexpected directive '%.*s'", SFMT(name));
		}
		break;
	}

	TRACE_LEAVE();
	return 0;
}

static Expression *parse_operand(Parser *parser, Bool lhs) {
	Context *const context = parser->context;

	TRACE_ENTER();

	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_IDENTIFIER:
		{
			IdentifierExpression *const expression = parse_identifier_expression(parser);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case KIND_LITERAL:
		switch (token.as_literal) {
		case LITERAL_INTEGER:   FALLTHROUGH();
		case LITERAL_FLOAT:     FALLTHROUGH();
		case LITERAL_IMAGINARY: FALLTHROUGH();
		case LITERAL_RUNE:      FALLTHROUGH();
		case LITERAL_STRING:
			{
				LiteralExpression *const expression = parse_literal_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case LITERAL_COUNT:
			UNREACHABLE();
		}
		break;
	case KIND_LBRACE:
		if (!lhs) {
			CompoundLiteralExpression *const expression = parse_compound_literal_expression(parser, 0);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
		break;
	case KIND_DIRECTIVE:
		{
			Expression *const expression = parse_directive_prefix(parser, lhs);
			TRACE_LEAVE();
			return expression;
		}
	case KIND_CONST:
		{
			// $ident
			TypeExpression *const expression = parse_poly_type_expression(parser);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
		break;
	case KIND_UNDEFINED:
		{
			// ---
			UndefinedExpression *const expression = parse_undefined_expression(parser);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case KIND_KEYWORD:
		switch (token.as_keyword) {
		case KEYWORD_DISTINCT:
			{
				TypeExpression *const expression = parse_distinct_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_PROC:
			{
				Expression *expression = 0;
				expect_keyword(parser, KEYWORD_PROC);
				if (is_kind(parser->this_token, KIND_LBRACE)) {
					ProcedureGroupExpression *const expression = parse_procedure_group_expression(parser);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else {
					expression = parse_procedure(parser);
				}
				TRACE_LEAVE();
				return expression;
			}
		case KEYWORD_BIT_SET:
			{
				TypeExpression *const expression = parse_bit_set_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_TYPEID:
			{
				TypeExpression *const expression = parse_typeid_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_MAP:
			{
				TypeExpression *const expression = parse_map_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_MATRIX:
			{
				TypeExpression *const expression = parse_matrix_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_STRUCT:
			{
				TypeExpression *const expression = parse_struct_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_UNION:
			{
				TypeExpression *const expression = parse_union_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_ENUM:
			{
				TypeExpression *const expression = parse_enum_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_CONTEXT:
			{
				record(parser);
				expect_keyword(parser, KEYWORD_CONTEXT);
				Identifier *const identifier = tree_new_identifier(parser->tree, SCLIT("context"), false);
				IdentifierExpression *const expression = tree_new_identifier_expression(parser->tree, identifier);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		default:
			break;
		}
		break;
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_LPAREN:
			{
				expect_operator(parser, OPERATOR_LPAREN);
				if (is_operator(parser->last_token, OPERATOR_RPAREN)) {
					PARSE_ERROR("Empty parenthesized expression");
				}
				const Sint32 expression_depth = parser->expression_depth;
				const Bool allow_newline = parser->allow_newline;
				if (expression_depth < 0) {
					parser->allow_newline = false;
				}
				parser->expression_depth = (expression_depth < 0 ? 0 : expression_depth) + 1;
				Expression *const operand = parse_expression(parser, false);
				expect_operator(parser, OPERATOR_RPAREN);
				parser->expression_depth = expression_depth;
				parser->allow_newline = allow_newline;
				TRACE_LEAVE();
				return operand;
			}
		case OPERATOR_POINTER:
			{
				// ^T
				TypeExpression *const expression = parse_pointer_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case OPERATOR_LBRACKET:
			{
				// [^]T, [?]T, [dynamic]T, [N]T, []T

				expect_operator(parser, OPERATOR_LBRACKET);

				if (is_operator(parser->this_token, OPERATOR_POINTER)) {
					// [^]T
					TypeExpression *const expression = parse_multi_pointer_type_expression(parser);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (is_operator(parser->this_token, OPERATOR_QUESTION)) {
					// [?]T
					TypeExpression *const expression = parse_array_type_expression(parser, false);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (accepted_keyword(parser, KEYWORD_DYNAMIC)) {
					// [dynamic]T
					TypeExpression *const expression = parse_dynamic_array_type_expression(parser);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (!is_operator(parser->this_token, OPERATOR_RBRACKET)) {
					// [N]T
					TypeExpression *const expression = parse_array_type_expression(parser, true);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (accepted_operator(parser, OPERATOR_RBRACKET)) {
					// []T
					TypeExpression *const expression = parse_slice_type_expression(parser);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				}
			}
		default:
			break;
		}
		break;
	default:
		break;
	}

	TRACE_LEAVE();

	return 0;
}

static CallExpression *parse_call_expression(Parser *parser, Expression *operand) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Array(Field*) arguments = array_make(context);
	const Sint32 expression_depth = parser->expression_depth;
	const Bool allow_newline = parser->allow_newline;
	parser->expression_depth = 0;
	parser->allow_newline = true;

	expect_operator(parser, OPERATOR_LPAREN);

	Bool seen_ellipsis = false;
	while (!is_operator(parser->this_token, OPERATOR_RPAREN)
	    && !is_kind(parser->this_token, KIND_EOF))
	{
		if (is_operator(parser->this_token, OPERATOR_COMMA)) {
			PARSE_ERROR("Expected an expression COMMA");
		} else if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			PARSE_ERROR("Expected an expression EQ");
		}
	
		Bool has_ellipsis = false;
		if (is_operator(parser->this_token, OPERATOR_ELLIPSIS)) {
			has_ellipsis = true;
			expect_operator(parser, OPERATOR_ELLIPSIS);
		}

		Expression *const argument = parse_expression(parser, false);
		if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			expect_assignment(parser, ASSIGNMENT_EQ);
			if (has_ellipsis) {
				PARSE_ERROR("Cannot apply '..' to field");
			}
			// We need to specify the argument name somehow.
			Identifier *name = evaluate_identifier_expression(argument);
			if (name) {
				Expression *value = parse_value(parser);
				Field *field = tree_new_field(parser->tree, 0, name, value, STRING_NIL, CAST(FieldFlag, 0));
				array_push(arguments, field);
			} else {
				PARSE_ERROR("Expected identifier for named argument");
			}
		} else {
			if (seen_ellipsis) {
				PARSE_ERROR("Positional arguments not allowed after '..'");
			}
			Field *field = tree_new_field(parser->tree, 0, 0, argument, STRING_NIL, CAST(FieldFlag, 0));
			array_push(arguments, field);
		}

		if (has_ellipsis) {
			seen_ellipsis = true;
		}

		if (!accepted_separator(parser)) {
			break;
		}
	}
	expect_operator(parser, OPERATOR_RPAREN);

	parser->allow_newline = allow_newline;
	parser->expression_depth = expression_depth;

	CallExpression *const expression = tree_new_call_expression(parser->tree, operand, arguments);

	TRACE_LEAVE();

	return expression;
}

static Token expect_closing(Parser *parser, Kind kind) {
	Context *const context = parser->context;
	const Token token = parser->this_token;
	if (!is_kind(token, kind) && is_kind(token, KIND_SEMICOLON)
		&& (string_compare(token.string, SCLIT("\n")) || is_kind(token, KIND_EOF)))
	{
		if (parser->allow_newline) {
			PARSE_ERROR("Missing ',' before newline");
		}
		advancep(parser);
	}
	return expect_kind(parser, kind);
}

static CompoundLiteralExpression *parse_compound_literal_expression(Parser *parser, Type *type) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Array(Field*) fields = array_make(context);

	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;

	expect_kind(parser, KIND_LBRACE);
	while (!is_kind(parser->this_token, KIND_RBRACE) &&
				 !is_kind(parser->this_token, KIND_EOF))
	{
		Expression *element = parse_value(parser);
		Identifier *name = evaluate_identifier_expression(element);
		if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			expect_assignment(parser, ASSIGNMENT_EQ);
			Expression *const value = parse_value(parser);
			Field *const field = tree_new_field(parser->tree, 0, name, value, STRING_NIL, CAST(FieldFlag, 0));
			array_push(fields, field);
		} else {
			Field *const field = tree_new_field(parser->tree, 0, name, 0, STRING_NIL, CAST(FieldFlag, 0));
			array_push(fields, field);
		}
		if (!accepted_separator(parser)) {
			break;
		}
	}
	expect_closing(parser, KIND_RBRACE);

	parser->expression_depth = depth;
	CompoundLiteralExpression *const expression = tree_new_compound_literal_expression(parser->tree, type, fields);

	TRACE_LEAVE();

	return expression;
}

static Expression *parse_index_expression(Parser *parser, Expression *operand) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Expression *lhs = 0;
	Expression *rhs = 0;

	parser->expression_depth++;

	// [
	expect_operator(parser, OPERATOR_LBRACKET);

	// [lhs
	if (!is_operator(parser->this_token, OPERATOR_ELLIPSIS)
	 && !is_operator(parser->this_token, OPERATOR_RANGEFULL)
	 && !is_operator(parser->this_token, OPERATOR_RANGEHALF)
	 && !is_operator(parser->this_token, OPERATOR_COLON))
	{
		lhs = parse_expression(parser, false);
	}

	// Do not allow .., ..=, or ..< inside
	if (is_operator(parser->this_token, OPERATOR_ELLIPSIS)
	 || is_operator(parser->this_token, OPERATOR_RANGEFULL)
	 || is_operator(parser->this_token, OPERATOR_RANGEHALF))
	{
		PARSE_ERROR("Expected a colon in indexing expression");
	}

	// Handle [,rhs] or [:rhs]
	Token interval = TOKEN_NIL;
	if (is_operator(parser->this_token, OPERATOR_COMMA)
	 || is_operator(parser->this_token, OPERATOR_COLON))
	{
		interval = advancep(parser); 
		if (!is_operator(parser->this_token, OPERATOR_RBRACKET)
		 && !is_kind(parser->this_token, KIND_EOF))
		{
			rhs = parse_expression(parser, false);
		}
	}

	expect_operator(parser, OPERATOR_RBRACKET);

	parser->expression_depth--;

	Expression *expression = 0;
	if (interval.kind != KIND_INVALID) {
		if (is_operator(interval, OPERATOR_COMMA)) {
			expression = RCAST(Expression *, tree_new_index_expression(parser->tree, operand, lhs, rhs));
		} else {
			expression = RCAST(Expression *, tree_new_slice_expression(parser->tree, operand, lhs, rhs));
		}
	} else {
		expression = RCAST(Expression *, tree_new_index_expression(parser->tree, operand, lhs, rhs));
	}

	TRACE_LEAVE();

	return expression;
}

static Expression *parse_atom_expression(Parser *parser, Expression *operand, Bool lhs) {
	Context *const context = parser->context;

	TRACE_ENTER();

	if (!operand) {
		if (parser->allow_type) {
			TRACE_LEAVE();
			return 0;
		}
		const String got = token_to_string(parser->this_token);
		PARSE_ERROR("Expected an operand, got '%.*s'", SFMT(got));
	}

	Identifier *ident = 0;
	Type *type = 0;
	for (;;) {
		Token token = parser->this_token;
		switch (token.kind) {
		case KIND_OPERATOR:
			switch (token.as_operator) {
			// '('
			case OPERATOR_LPAREN:
				operand = RCAST(Expression *, parse_call_expression(parser, operand));
				break;
			// .x
			// .?
			// .(T)
			case OPERATOR_PERIOD:
				expect_operator(parser, OPERATOR_PERIOD);
				switch (parser->this_token.kind) {
				case KIND_IDENTIFIER:
					// .ident
					ident = parse_identifier(parser, false);
					operand = RCAST(Expression *, tree_new_selector_expression(parser->tree, operand, ident));
					break;
				case KIND_OPERATOR:
					switch (parser->this_token.as_operator) {
					case OPERATOR_LPAREN:
						// .(T)
						expect_operator(parser, OPERATOR_LPAREN);
						type = parse_type(parser);
						expect_operator(parser, OPERATOR_RPAREN);
						operand = RCAST(Expression *, tree_new_assertion_expression(parser->tree, operand, type));
						break;
					case OPERATOR_QUESTION:
						// .?
						expect_operator(parser, OPERATOR_QUESTION);
						operand = RCAST(Expression *, tree_new_assertion_expression(parser->tree, operand, 0));
						break;
					default:
						break;
					}
					break;
				default:
					PARSE_ERROR("Expected selector in selector expression");
					break;
				}
				break;
			case OPERATOR_ARROW:
				// ->ident
				expect_operator(parser, OPERATOR_ARROW);
				ident = parse_identifier(parser, false);
				operand = RCAST(Expression *, tree_new_selector_expression(parser->tree, operand, ident));
				break;
			case OPERATOR_LBRACKET:
				// [i], [:], [i:], [:i], [a:b], [c,r]
				operand = RCAST(Expression *, parse_index_expression(parser, operand));
				break;
			case OPERATOR_POINTER:
				FALLTHROUGH();
			case OPERATOR_OR_RETURN:
				expect_operator(parser, token.as_operator);
				operand = RCAST(Expression *, tree_new_unary_expression(parser->tree, token.as_operator, operand));
				break;
			default:
				goto L_exit;
			}
			break;
		case KIND_LBRACE:
			if (!lhs && parser->expression_depth >= 0) {
				Type *const type = RCAST(Type *, tree_new_expression_type(parser->tree, operand));
				CompoundLiteralExpression *const expression = parse_compound_literal_expression(parser, type);
				operand = RCAST(Expression *, expression);
			} else {
				goto L_exit;
			}
			break;
		default:
			goto L_exit;
		}

		// No longer left-hand side once one iteration through the loop.
		lhs = false;
	}

L_exit:
	TRACE_LEAVE();

	return operand;
}

static CastExpression *parse_cast_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	const Token token = advancep(parser);

	Type *type = 0;
	if (!is_operator(token, OPERATOR_AUTO_CAST)) {
		expect_operator(parser, OPERATOR_LPAREN);
		type = parse_type(parser);
		expect_operator(parser, OPERATOR_RPAREN);
	}

	Expression *const operand = parse_unary_expression(parser, lhs);
	CastExpression *const expression = tree_new_cast_expression(parser->tree, token.as_operator, type, operand);

	TRACE_LEAVE();

	return expression;
}

static UnaryExpression *parse_unary_stem_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	const Token token = advancep(parser);
	Expression *const operand = parse_unary_expression(parser, lhs);
	UnaryExpression *const expression = tree_new_unary_expression(parser->tree, token.as_operator, operand);

	TRACE_LEAVE();

	return expression;
}

static SelectorExpression *parse_implicit_selector_expression(Parser *parser) {
	TRACE_ENTER();

	expect_operator(parser, OPERATOR_PERIOD);
	Identifier *const identifier = parse_identifier(parser, false);
	SelectorExpression *const expression = tree_new_selector_expression(parser->tree, 0, identifier);

	TRACE_LEAVE();

	return expression;
}

static Expression *parse_unary_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_TRANSMUTE: FALLTHROUGH();
		case OPERATOR_AUTO_CAST: FALLTHROUGH();
		case OPERATOR_CAST:
			{
				CastExpression *const expression = parse_cast_expression(parser, lhs);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case OPERATOR_ADD: FALLTHROUGH();
		case OPERATOR_SUB: FALLTHROUGH();
		case OPERATOR_XOR: FALLTHROUGH();
		case OPERATOR_AND: FALLTHROUGH();
		case OPERATOR_NOT:
			{
				UnaryExpression *const expression = parse_unary_stem_expression(parser, lhs);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
			break;
		case OPERATOR_PERIOD:
			{
				SelectorExpression *const expression = parse_implicit_selector_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		default:
			break;
		}
		break;
	default:
		break;
	}

	Expression *const operand = parse_operand(parser, lhs);
	Expression *const expression = parse_atom_expression(parser, operand, lhs);

	TRACE_LEAVE();

	return expression;
}

static TernaryExpression *parse_ternary_expression(Parser *parser, Expression *expr, Bool lhs) {
	TRACE_ENTER();

	Expression *cond = 0;
	Expression *on_true = 0;
	KeywordKind kind = KEYWORD_IF;
	if (accepted_operator(parser, OPERATOR_QUESTION)) {
		cond = expr;
		on_true = parse_expression(parser, lhs);
		expect_operator(parser, OPERATOR_COLON);
	} else if (accepted_keyword(parser, KEYWORD_IF) || accepted_keyword(parser, KEYWORD_WHEN)) {
		kind = parser->last_token.as_keyword;
		cond = parse_expression(parser, lhs);
		on_true = expr;
		expect_keyword(parser, KEYWORD_ELSE);
	}

	Expression *const on_false = parse_expression(parser, lhs);
	TernaryExpression *const ternary = tree_new_ternary_expression(parser->tree, on_true, kind, cond, on_false);

	TRACE_LEAVE();

	return ternary;
}

static Expression *parse_binary_expression(Parser *parser, Bool lhs, Sint32 prec) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Expression *expr = parse_unary_expression(parser, lhs);
	for (;;) {
		const Token token = parser->this_token;
		const Token last = parser->last_token;
		Sint32 op_prec = 0;
		if (is_kind(token, KIND_OPERATOR)) {
			op_prec = PRECEDENCE[token.as_operator];
			// 'in' when used outside an expression behaves as a keyword
			if (is_operator(token, OPERATOR_IN) || is_operator(token, OPERATOR_NOT_IN)) {
				if (parser->expression_depth < 0 && !parser->allow_in) {
					op_prec = 0;
				}
			}
		} else if (is_keyword(token, KEYWORD_IF) || is_keyword(token, KEYWORD_WHEN)) {
			// The 'if' and 'when' keywords become ternary operators with highest precedence.
			op_prec = 1;
		}
		if (op_prec < prec) {
			break;
		}

		// Needs to be on the same line to be a ternary usage.
		if (is_keyword(token, KEYWORD_IF) || is_keyword(token, KEYWORD_WHEN)) {
			if (last.location.line < token.location.line) {
				TRACE_LEAVE();
				return expr;
			}
		}

		if (is_operator(token, OPERATOR_QUESTION)
		 || is_keyword(token, KEYWORD_IF)
		 || is_keyword(token, KEYWORD_WHEN))
		{
			TernaryExpression *const ternary = parse_ternary_expression(parser, expr, lhs);
			expr = RCAST(Expression *, ternary);
		} else {
			expect_kind(parser, KIND_OPERATOR);
			Expression *const rhs = parse_binary_expression(parser, false, op_prec + 1);
			if (!rhs) {
				PARSE_ERROR("Expected expression on the right-hand side");
			}
			expr = RCAST(Expression *, tree_new_binary_expression(parser->tree, token.as_operator, expr, rhs));
		}
		lhs = false;
	}

	TRACE_LEAVE();

	return expr;
}

static FORCE_INLINE Expression *parse_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	Expression *const expression = parse_binary_expression(parser, lhs, 1);

	TRACE_LEAVE();

	return expression;
}

static ListExpression *parse_list_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	Context *const context = parser->context;

	const Bool allow_newline = parser->allow_newline;
	parser->allow_newline = true;

	Array(Expression*) expressions = array_make(context);
	for (;;) {
		Expression *const expression = parse_expression(parser, lhs);
		array_push(expressions, expression);
		if (!is_operator(parser->this_token, OPERATOR_COMMA) || is_kind(parser->this_token, KIND_EOF)) {
			break;
		}
		advancep(parser);
	}

	ListExpression *const expression = tree_new_list_expression(parser->tree, expressions);

	parser->allow_newline = allow_newline;

	TRACE_LEAVE();

	return expression;
}

static FORCE_INLINE ListExpression *parse_lhs_list_expression(Parser *parser) {
	TRACE_ENTER();

	ListExpression *const result = parse_list_expression(parser, true);

	TRACE_LEAVE();

	return result;
}

static FORCE_INLINE ListExpression *parse_rhs_list_expression(Parser *parser) {
	TRACE_ENTER();

	ListExpression *const result = parse_list_expression(parser, false);

	TRACE_LEAVE();

	return result;
}

static Statement *parse_statement(Parser *parser, BlockFlag block_flags);

static Array(Statement*) parse_statement_list(Parser *parser, BlockFlag block_flags) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Array(Statement*) statements = array_make(context);

	// Stop parsing the statement when we encounter one of:
	//
	//	"case"
	//	"}"
	//	EOF
	while (!is_keyword(parser->this_token, KEYWORD_CASE) &&
	       !is_kind(parser->this_token, KIND_RBRACE) &&
	       !is_kind(parser->this_token, KIND_EOF))
	{
		Statement *const statement = parse_statement(parser, block_flags);
		if (statement && statement->kind != STATEMENT_EMPTY) {
			array_push(statements, statement);
		}
	}

	TRACE_LEAVE();

	return statements;
}

static BlockStatement *parse_body(Parser *parser, BlockFlag flags) {
	TRACE_ENTER();

	const Sint32 depth = parser->expression_depth;
	const Bool allow_newline = parser->allow_newline;
	parser->expression_depth = 0;
	parser->allow_newline = true;

	expect_kind(parser, KIND_LBRACE);
	Array(Statement*) const statements = parse_statement_list(parser, flags);
	expect_kind(parser, KIND_RBRACE);

	parser->expression_depth = depth;
	parser->allow_newline = allow_newline;

	BlockStatement *const statement = tree_new_block_statement(parser->tree, flags, statements);

	TRACE_LEAVE();

	return statement;
}

static BlockStatement *parse_block_statement(Parser *parser, BlockFlag flags, Bool when) {
	Context *const context = parser->context;

	TRACE_ENTER();

	advance_possible_newline_within(parser);

	if (!when && !parser->this_procedure) {
		PARSE_ERROR("Cannot use block statement at file scope");
	}

	BlockStatement *const body = parse_body(parser, flags);

	TRACE_LEAVE();

	return body;
}

static DeclarationStatement *parse_declaration_statement_tail(Parser *parser, Array(Identifier*) names, Bool is_using) {
	Context *const context = parser->context;

	// ':'
	TRACE_ENTER();

	ListExpression *values = 0;
	Type *const type = parse_type_or_identifier(parser);
	const Token token = parser->this_token;
	Bool constant = false;

	const Size n_names = array_size(names);
	// Check for '=' (:=)
	// Check for ':' (::)
	if (is_assignment(token, ASSIGNMENT_EQ) || is_operator(token, OPERATOR_COLON)) {
		const Token seperator = advancep(parser);
		constant = is_operator(seperator, OPERATOR_COLON);
		advance_possible_newline(parser); // hack?
		values = parse_rhs_list_expression(parser);
		const Size n_values = array_size(values->expressions);
		if (n_values > n_names) {
			PARSE_ERROR("Too many values on right-hand side of the declaration");
		} else if (n_values < n_names && constant) {
			PARSE_ERROR("All constant declarations must be defined");
		} else if (n_values == 0) {
			PARSE_ERROR("Expected an expression for this declaration");
		}
	}

	const Size n_values = values ? array_size(values->expressions) : 0;
	if (!type) {
		if (constant && n_values == 0) {
			PARSE_ERROR("Expected constant initializer");
		} else if (n_values == 0 && n_names > 0) {
			PARSE_ERROR("Missing constant value");
		}
	}

	if (parser->expression_depth >= 0) {
		const Token token = parser->this_token;
		if (is_kind(token, KIND_RBRACE)
			&& token.location.line == parser->last_token.location.line)
		{
			// Do nothing.
		} else {
			expect_semicolon(parser);
		}
	}

	if (!parser->this_procedure && n_values > 0 && n_names != n_values) {
		PARSE_ERROR("Expected %d expressions on the right-hand side, got %d instead", n_names, n_values);
	}

	// Synthesize initialization of values with empty compound literal {} for zero-initialization.
	// We do this at the syntatic level to eliminate unnecessary edge cases later on.
	if (type && n_values == 0) {
		Array(Expression*) expressions = array_make(context);
		for (Size i = 0; i < n_names; i++) {
			CompoundLiteralExpression *const expression = tree_new_compound_literal_expression(parser->tree, type, 0);
			array_push(expressions, RCAST(Expression *, expression));
		}
		values = tree_new_list_expression(parser->tree, expressions);
	}

	DeclarationStatement *const statement = tree_new_declaration_statement(parser->tree, type, names, values, is_using);

	TRACE_LEAVE();

	return statement;
}

static DeclarationStatement *parse_declaration_statement(Parser *parser, ListExpression *lhs, Bool is_using) {
	Context *const context = parser->context;

	TRACE_ENTER();

	// Everything inside 'lhs' should evaluate as an identifier.
	const Size n_names = array_size(lhs->expressions);
	Array(Identifier *) names = array_make(context);
	for (Size i = 0; i < n_names; i++) {
		const Expression *const expression = lhs->expressions[i];
		Identifier *const identifier = evaluate_identifier_expression(expression);
		if (!identifier) {
			PARSE_ERROR("Expected identifier in declaration");
		}
		array_push(names, identifier);
	}

	DeclarationStatement *const statement = parse_declaration_statement_tail(parser, names, is_using);

	TRACE_LEAVE();

	return statement;
}

static AssignmentStatement *parse_assignment_statement(Parser *parser, ListExpression *lhs) {
	Context *const context = parser->context;

	TRACE_ENTER();

	if (!parser->this_procedure) {
		PARSE_ERROR("Cannot use assignment statement at file scope");
	}

	const AssignmentKind kind = parser->this_token.as_assignment;

	advancep(parser);
	ListExpression *const rhs = parse_rhs_list_expression(parser);
	if (array_size(rhs->expressions) == 0) {
		PARSE_ERROR("Missing right-hand side in assignment");
	}

	AssignmentStatement *const statement = tree_new_assignment_statement(parser->tree, kind, lhs, rhs);

	TRACE_LEAVE();

	return statement;
}

static Statement *parse_simple_statement(Parser* parser, BlockFlag block_flags, Bool allow_in, Bool allow_label) {
	Context *const context = parser->context;

	TRACE_ENTER();

	ListExpression *const lhs = parse_lhs_list_expression(parser);
	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_ASSIGNMENT:
		{
			AssignmentStatement *const statement = parse_assignment_statement(parser, lhs);
			TRACE_LEAVE();
			return RCAST(Statement *, statement);
		}
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_IN:
			if (allow_in) {
				// [lhs] in <Expression>
				Bool allow_in = parser->allow_in;
				parser->allow_in = false;
				accepted_operator(parser, OPERATOR_IN);
				Expression *const rhs = parse_expression(parser, true);
				BinaryExpression *const expression = tree_new_binary_expression(parser->tree, OPERATOR_IN, RCAST(Expression *, lhs), rhs);
				ExpressionStatement *const statement = tree_new_expression_statement(parser->tree, RCAST(Expression *, expression));
				parser->allow_in = allow_in;
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
			break;
		case OPERATOR_COLON:
			{
				advancep(parser);
				// Check if label prefix.
				if (allow_label && array_size(lhs->expressions) == 1) {
					const Token token = parser->this_token;
					if (is_kind(token, KIND_LBRACE)
					 || is_keyword(token, KEYWORD_IF)
					 || is_keyword(token, KEYWORD_FOR)
					 || is_keyword(token, KEYWORD_SWITCH))
					{
						Expression *const name = lhs->expressions[0];
						if (name->kind != EXPRESSION_IDENTIFIER) {
							PARSE_ERROR("Expected identifier for label");
						}
						Identifier *const label = RCAST(IdentifierExpression *, name)->identifier;
						Statement *const statement = parse_statement(parser, block_flags);
						switch (statement->kind) {
						case STATEMENT_BLOCK:
							RCAST(BlockStatement *, statement)->label = label;
							break;
						case STATEMENT_IF:
							RCAST(IfStatement *, statement)->label = label;
							break;
						case STATEMENT_FOR:
							RCAST(ForStatement *, statement)->label = label;
							break;
						case STATEMENT_SWITCH:
							RCAST(SwitchStatement *, statement)->label = label;
							break;
						default:
							PARSE_ERROR("Cannot apply label to this statement");
						}
						TRACE_LEAVE();
						return statement;
					}
				}
				DeclarationStatement *const statement = parse_declaration_statement(parser, lhs, false);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		default:
			break;
		}
	default:
		break;
	}

	Array(Expression*) const expressions = lhs->expressions;
	if (array_size(expressions) == 0 || array_size(expressions) > 1) {
		PARSE_ERROR("Expected one expression on the left-hand side");
	}

	ExpressionStatement *const statement = tree_new_expression_statement(parser->tree, expressions[0]);

	TRACE_LEAVE();

	return RCAST(Statement *, statement);
}

static ImportStatement *parse_import_statement(Parser *parser, Bool is_using) {
	TRACE_ENTER();

	tree_record_token(parser->tree, expect_keyword(parser, KEYWORD_IMPORT));

	Token token = TOKEN_NIL;
	if (is_kind(parser->this_token, KIND_IDENTIFIER)) {
		token = advancep(parser);
	}

	const Token path = expect_literal(parser, LITERAL_STRING);

	expect_semicolon(parser);

	const String name = string_unquote(token.string, "\"");
	const String next = string_unquote(path.string, "\"");

	// Check for collection in package.
	ImportStatement *statement = 0;
	Size split = 0;
	if (string_find_first_byte(next, ':', &split)) {
		const String collection = string_slice(next, 0, split);
		const String pathname = string_slice(next, split + 1, next.length - (split + 1));
		statement = tree_new_import_statement(parser->tree, name, collection, pathname, is_using);
	} else {
		const String collection = STRING_NIL;
		const String pathname = next;
		statement = tree_new_import_statement(parser->tree, name, collection, pathname, is_using);
	}

	TRACE_LEAVE();

	return statement;
}

// Not a parser. Do not TRACE_(ENTER|LEAVE)
static BlockStatement *convert_statement_to_body(Parser *parser, BlockFlag flags, Statement *statement) {
	Context *const context = parser->context;
	const StatementKind kind = statement->kind;
	if (kind == STATEMENT_BLOCK || kind == STATEMENT_EMPTY) {
		PARSE_ERROR("Expected a regular statement");
	}
	Array(Statement*) statements = array_make(context);
	array_push(statements, statement);
	return tree_new_block_statement(parser->tree, flags, statements);
}

// Not a parser. Do not TRACE_(ENTER|LEAVE)
static Expression *convert_statement_to_expression(Parser *parser, Statement *statement) {
	if (!statement) {
		return 0;
	}
	if (statement->kind != STATEMENT_EXPRESSION) {
		Context *const context = parser->context;
		PARSE_ERROR("Expected a statement");
	}
	return RCAST(ExpressionStatement *, statement)->expression;
}

static BlockStatement *parse_do_body(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();

	const Sint32 expression_depth = parser->expression_depth;
	const Bool allow_newline = parser->allow_newline;
	parser->expression_depth = 0;
	parser->allow_newline = false;

	Statement *const statement = parse_statement(parser, block_flags);
	BlockStatement *const body = convert_statement_to_body(parser, block_flags, statement);
	parser->expression_depth = expression_depth;
	parser->allow_newline = allow_newline;

	TRACE_LEAVE();

	return body;
}

static IfStatement *parse_if_statement(Parser *parser, BlockFlag block_flags) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_IF);

	const Sint32 depth = parser->expression_depth;
	const Bool allow_in = parser->allow_in;
	parser->expression_depth = -1;
	parser->allow_in = true;

	Statement *init = parse_simple_statement(parser, block_flags, false, false);
	Expression *cond = 0;
	if (accepted_control_statement_separator(parser)) {
		cond = parse_expression(parser, false);
	} else {
		cond = convert_statement_to_expression(parser, init);
		init = 0;
	}

	parser->expression_depth = depth;
	parser->allow_in = allow_in;

	if (!cond) {
		PARSE_ERROR("Expected condition in if statement");
	}

	BlockStatement *body = 0;
	if (accepted_keyword(parser, KEYWORD_DO)) {
		body = parse_do_body(parser, block_flags);
	} else {
		body = parse_block_statement(parser, block_flags, false);
	}

	advance_possible_newline_within(parser);

	BlockStatement *elif = 0;
	if (is_keyword(parser->this_token, KEYWORD_ELSE)) {
		expect_keyword(parser, KEYWORD_ELSE);
		if (is_keyword(parser->this_token, KEYWORD_IF)) {
			IfStatement *const statement = parse_if_statement(parser, block_flags);
			Array(Statement) *statements = array_make(context);
			array_push(statements, RCAST(Statement *, statement));
			elif = tree_new_block_statement(parser->tree, block_flags, statements);
		} else if (is_kind(parser->this_token, KIND_LBRACE)) {
			elif = parse_block_statement(parser, block_flags, false);
		} else if (is_keyword(parser->this_token, KEYWORD_DO)) {
			expect_keyword(parser, KEYWORD_DO);
			elif = parse_do_body(parser, block_flags);
		} else {
			PARSE_ERROR("Expected block on 'else' statement");
		}
	}

	IfStatement *const statement = tree_new_if_statement(parser->tree, init, cond, body, elif);

	TRACE_LEAVE();

	return statement;
}

static WhenStatement *parse_when_statement(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_WHEN);

	const Sint32 expression_depth = parser->expression_depth;
	parser->expression_depth = -1;

	Expression *const cond = parse_expression(parser, false);

	parser->expression_depth = expression_depth;

	if (!cond) {
		PARSE_ERROR("Expected condition in 'when' statement");
	}

	BlockStatement *body = 0;
	const BlockFlag flags = CAST(BlockFlag, 0);
	if (accepted_keyword(parser, KEYWORD_DO)) {
		body = parse_do_body(parser, flags);
	} else {
		body = parse_block_statement(parser, flags, true);
	}

	advance_possible_newline_within(parser);

	BlockStatement *elif = 0;
	if (is_keyword(parser->this_token, KEYWORD_ELSE)) {
		expect_keyword(parser, KEYWORD_ELSE);
		if (is_keyword(parser->this_token, KEYWORD_WHEN)) {
			WhenStatement *const statement = parse_when_statement(parser);
			Array(Statement) *statements = array_make(context);
			array_push(statements, RCAST(Statement *, statement));
			elif = tree_new_block_statement(parser->tree, flags, statements);
		} else if (is_keyword(parser->this_token, KEYWORD_DO)) {
			expect_keyword(parser, KEYWORD_DO);
			elif = parse_do_body(parser, flags);
		} else if (is_kind(parser->this_token, KIND_LBRACE)) {
			elif = parse_block_statement(parser, flags, true);
		} else {
			PARSE_ERROR("Unexpected statement in 'when' else block");
		}
	}

	WhenStatement *const statement = tree_new_when_statement(parser->tree, cond, body, elif);

	TRACE_LEAVE();

	return statement;
}

static ForStatement *parse_for_statement(Parser *parser, BlockFlag block_flags) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Statement *init = 0;
	Expression *cond = 0;
	BlockStatement *body = 0;
	Statement *post = 0;

	expect_keyword(parser, KEYWORD_FOR);

	Bool range = false;

	const Token token = parser->this_token;
	if (!is_kind(token, KIND_LBRACE)
	 && !is_keyword(token, KEYWORD_DO))
	{
		const Sint32 depth = parser->expression_depth;
		parser->expression_depth = -1;
		if (is_operator(token, OPERATOR_IN)) {
			PARSE_ERROR("Use of 'for in' is not allowed. Use 'for _ in' instead.");
		}
		if (!is_kind(token, KIND_SEMICOLON)) {
			// for [...] in <Expression>
			Statement *const statement = parse_simple_statement(parser, block_flags, true, false);
			if (statement->kind == STATEMENT_EXPRESSION) {
				Expression *const expression = RCAST(ExpressionStatement *, statement)->expression;
				cond = expression;
				if (expression->kind == EXPRESSION_BINARY) {
					BinaryExpression *const bin = RCAST(BinaryExpression *, expression);
					if (bin->operation == OPERATOR_IN) {
						range = true;
					}
				}
			} else {
				// for <init>
				init = statement;
				cond = 0;
			}
		}
		if (!range && accepted_control_statement_separator(parser)) {
			const Token token = parser->this_token;
			if (is_kind(token, KIND_LBRACE) || is_keyword(token, KEYWORD_DO)) {
				PARSE_ERROR("Expected ';'");
			} else {
				if (!is_kind(token, KIND_SEMICOLON)) {
					// for [...] <cond>
					Statement *const statement = parse_simple_statement(parser, block_flags, false, false);
					cond = convert_statement_to_expression(parser, statement);
				}
				expect_semicolon(parser);
				if (!is_kind(parser->this_token, KIND_LBRACE)
				 && !is_keyword(parser->this_token, KEYWORD_DO))
				{
					// for [...] [...] <post>
					post = parse_simple_statement(parser, block_flags, false, false);
				}
			}
		}
		parser->expression_depth = depth;
	}

	if (accepted_keyword(parser, KEYWORD_DO)) {
		// for [...] do <Statement>
		body = parse_do_body(parser, block_flags);
	} else {
		// for [...] <BlockStatement>
		body = parse_block_statement(parser, block_flags, false);
	}

	ForStatement *const statement = tree_new_for_statement(parser->tree, init, cond, body, post);

	TRACE_LEAVE();

	return statement;
}

static CaseClause *parse_case_clause(Parser *parser, BlockFlag block_flags, Bool is_type) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_CASE);

	const Bool allow_in = parser->allow_in;
	parser->allow_in = !is_type;
	ListExpression *list = 0;
	if (!is_operator(parser->this_token, OPERATOR_COLON)) {
		list = parse_rhs_list_expression(parser);
	}
	parser->allow_in = allow_in;

	expect_operator(parser, OPERATOR_COLON);

	Array(Statement*) statements = parse_statement_list(parser, block_flags);
	CaseClause *clause = tree_new_case_clause(parser->tree, list, statements);

	TRACE_LEAVE();

	return clause;
}

static SwitchStatement *parse_switch_statement(Parser *parser, BlockFlag block_flags) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_SWITCH);

	Statement *init = 0;
	Expression *cond = 0;
	if (!is_kind(parser->this_token, KIND_LBRACE)) {
		const Sint32 expression_depth = parser->expression_depth;
		parser->expression_depth = -1;

		Statement *const statement = parse_simple_statement(parser, block_flags, true, false);
		if (statement->kind == STATEMENT_EXPRESSION) {
			cond = RCAST(ExpressionStatement *, statement)->expression;
		} else {
			init = statement;
			if (accepted_control_statement_separator(parser)) {
				cond = parse_expression(parser, false);
			} else {
				expect_semicolon(parser);
			}
		}
		parser->expression_depth = expression_depth;
	}

	const Bool is_type_switch
		= cond
				&& cond->kind == EXPRESSION_BINARY
					&& RCAST(BinaryExpression *, cond)->operation == OPERATOR_IN;

	Array(CaseClause*) clauses = array_make(context);
	advance_possible_newline(parser);
	expect_kind(parser, KIND_LBRACE);
	while (is_keyword(parser->this_token, KEYWORD_CASE)) {
		array_push(clauses, parse_case_clause(parser, block_flags, is_type_switch));
	}
	expect_kind(parser, KIND_RBRACE);

	SwitchStatement *statement = tree_new_switch_statement(parser->tree, init, cond, clauses);

	TRACE_LEAVE();

	return statement;
}

// 'defer' [Statement]
static DeferStatement *parse_defer_statement(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_DEFER);
	Statement *const statement = parse_statement(parser, block_flags);
	DeferStatement *const defer = tree_new_defer_statement(parser->tree, statement);

	TRACE_LEAVE();

	return defer;
}

// 'return' [Expression] (',' [Expression])+ [Semicolon]
static ReturnStatement *parse_return_statement(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_RETURN);

	if (parser->expression_depth > 0) {
		PARSE_ERROR("Cannot use return statement within expression");
	}

	Array(Expression*) results = array_make(context);
	while (!is_kind(parser->this_token, KIND_SEMICOLON) &&
	       !is_kind(parser->this_token, KIND_RBRACE) &&
	       !is_kind(parser->this_token, KIND_EOF))
	{
		Expression *const expression = parse_expression(parser, false);
		array_push(results, expression);
		if (!is_operator(parser->this_token, OPERATOR_COMMA) || is_kind(parser->this_token, KIND_EOF)) {
			break;
		}
		advancep(parser);
	}

	expect_semicolon(parser);

	ReturnStatement *const statement = tree_new_return_statement(parser->tree, results);

	TRACE_LEAVE();

	return statement;
}

// SimpleStatement , Semicolon
static Statement *parse_basic_simple_statement(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();

	Statement *const statement = parse_simple_statement(parser, block_flags, false, true);
	expect_semicolon(parser);

	TRACE_LEAVE();

	return statement;
}

// ForeignBlockStatement = Identifier? , '{' , Statement+ , '}'
static ForeignBlockStatement *parse_foreign_block_statement(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	Identifier *name = 0;
	if (is_kind(parser->this_token, KIND_IDENTIFIER)) {
		name = parse_identifier(parser, false);
	}

	Array(Statement*) statements = array_make(context);

	advance_possible_newline_within(parser);

	expect_kind(parser, KIND_LBRACE);
	while (!is_kind(parser->this_token, KIND_RBRACE)
	    && !is_kind(parser->this_token, KIND_EOF))
	{
		Statement *const statement = parse_statement(parser, CAST(BlockFlag, 0));
		array_push(statements, statement);
	}
	expect_kind(parser, KIND_RBRACE);

	BlockStatement *block = tree_new_block_statement(parser->tree, CAST(BlockFlag, 0), statements);
	ForeignBlockStatement *statement = tree_new_foreign_block_statement(parser->tree, name, block);

	expect_semicolon(parser);

	TRACE_LEAVE();

	return statement;
}

// ForeignImportStatement = 'import' , Identifier? , ()
static ForeignImportStatement *parse_foreign_import_statement(Parser *parser) {
	Context *const context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_IMPORT);

	Token name = TOKEN_NIL;
	if (is_kind(parser->this_token, KIND_IDENTIFIER)) {
		name = advancep(parser); // Consume the identifier
	}

	Array(String) sources = array_make(context);
	if (accepted_kind(parser, KIND_LBRACE)) {
		while (!is_kind(parser->this_token, KIND_RBRACE)
		    && !is_kind(parser->this_token, KIND_EOF))
		{
			const Token path = expect_literal(parser, LITERAL_STRING);
			array_push(sources, path.string);
			if (!accepted_separator(parser)) {
				break;
			}
		}
		expect_closing(parser, KIND_RBRACE);
	} else {
		const Token path = expect_literal(parser, LITERAL_STRING);
		array_push(sources, path.string);
	}

	ForeignImportStatement *statement = 
		tree_new_foreign_import_statement(parser->tree, name.string, sources);

	TRACE_LEAVE();

	return statement;
}

// ForeignDeclaration = 'foreign' , ( ForeignBlockStatement | ForeignImportStatement ) ;
static Statement *parse_foreign_declaration_statement(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_FOREIGN);
	if (is_kind(parser->this_token, KIND_IDENTIFIER)
	 || is_kind(parser->this_token, KIND_LBRACE))
	{
		ForeignBlockStatement *const statement = parse_foreign_block_statement(parser);
		TRACE_LEAVE();
		return RCAST(Statement *, statement);
	} else if (is_keyword(parser->this_token, KEYWORD_IMPORT)) {
		ForeignImportStatement *const statement = parse_foreign_import_statement(parser);
		TRACE_LEAVE();
		return RCAST(Statement *, statement);
	}

	TRACE_LEAVE();

	return 0;
}

static BranchStatement *parse_branch_statement(Parser *parser, KeywordKind kind) {
	TRACE_ENTER();

	advancep(parser);
	Identifier *label = 0;
	if (is_kind(parser->this_token, KIND_IDENTIFIER)) {
		label = parse_identifier(parser, false);
	}
	BranchStatement *const statement = tree_new_branch_statement(parser->tree, kind, label);

	TRACE_LEAVE();

	return statement;
}

static Statement *parse_using_statement(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_USING);

	if (is_keyword(parser->this_token, KEYWORD_IMPORT)) {
		ImportStatement *statement = parse_import_statement(parser, true);
		TRACE_LEAVE();
		return RCAST(Statement *, statement);
	}

	ListExpression *const list = parse_rhs_list_expression(parser);
	if (!is_operator(parser->this_token, OPERATOR_COLON)) {
		expect_semicolon(parser);
		UsingStatement *const statement = tree_new_using_statement(parser->tree, list);
		TRACE_LEAVE();
		return RCAST(Statement *, statement);
	}

	expect_operator(parser, OPERATOR_COLON);

	DeclarationStatement *const statement = parse_declaration_statement(parser, list, true);

	TRACE_LEAVE();

	return RCAST(Statement *, statement);
}

static PackageStatement *parse_package_statement(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_PACKAGE);
	const Token package = expect_kind(parser, KIND_IDENTIFIER);
	tree_record_token(parser->tree, package);
	
	PackageStatement *statement = tree_new_package_statement(parser->tree, package.string);

	TRACE_LEAVE();

	return statement;
}

static EmptyStatement *parse_empty_statement(Parser *parser) {
	TRACE_ENTER();

	EmptyStatement *const statement = tree_new_empty_statement(parser->tree);
	expect_semicolon(parser);

	TRACE_LEAVE();

	return statement;
}

static Statement *parse_statement(Parser *parser, BlockFlag block_flags) {
	Context *const context = parser->context;

	TRACE_ENTER();

	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_EOF:
		ICE("Unexpected EOF");
	case KIND_LITERAL:
		switch (token.as_literal) {
		case LITERAL_INTEGER:   FALLTHROUGH();
		case LITERAL_FLOAT:     FALLTHROUGH();
		case LITERAL_IMAGINARY: FALLTHROUGH();
		case LITERAL_RUNE:      FALLTHROUGH();
		case LITERAL_STRING:
			{
				Statement *const statement = parse_basic_simple_statement(parser, block_flags);
				TRACE_LEAVE();
				return statement;
			}
		default:
			ICE("Unexpected literal");
		}
		break;
	case KIND_KEYWORD:
		switch (token.as_keyword) {
		case KEYWORD_CONTEXT:
			// Needed to support assignment to context.
			FALLTHROUGH();
		case KEYWORD_PROC:
			{
				Statement *const statement = parse_basic_simple_statement(parser, block_flags);
				TRACE_LEAVE();
				return statement;
			}
		case KEYWORD_FOREIGN:
			{
				Statement *const statement = parse_foreign_declaration_statement(parser);
				TRACE_LEAVE();
				return statement;
			}
		case KEYWORD_IF:
			{
				IfStatement *const statement = parse_if_statement(parser, block_flags);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_WHEN:
			{
				WhenStatement *const statement = parse_when_statement(parser);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_IMPORT:
			{
				ImportStatement *const statement = parse_import_statement(parser, false);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_FOR:
			{
				ForStatement *const statement = parse_for_statement(parser, block_flags);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_SWITCH:
			{
				SwitchStatement *const statement = parse_switch_statement(parser, block_flags);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_DEFER:
			{
				DeferStatement *const statement = parse_defer_statement(parser, block_flags);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_RETURN:
			{
				ReturnStatement *const statement = parse_return_statement(parser);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_BREAK:       FALLTHROUGH();
		case KEYWORD_CONTINUE:    FALLTHROUGH();
		case KEYWORD_FALLTHROUGH:
			{
				BranchStatement *const statement = parse_branch_statement(parser, token.as_keyword);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_USING:
			{
				Statement *const statement = parse_using_statement(parser);
				TRACE_LEAVE();
				return statement;
			}
		case KEYWORD_PACKAGE:
			{
				PackageStatement *const statement = parse_package_statement(parser);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		default:
			{
				const String keyword = keyword_to_string(token.as_keyword);
				ICE("Unexpected keyword '%.*s' in statement", SFMT(keyword));
			}
		}
		break;
	case KIND_IDENTIFIER:
		{
			Statement *const statement = parse_basic_simple_statement(parser, block_flags);
			TRACE_LEAVE();
			return statement;
		}
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_LPAREN:  FALLTHROUGH();
		case OPERATOR_POINTER: FALLTHROUGH();
		case OPERATOR_ADD:     FALLTHROUGH();
		case OPERATOR_SUB:     FALLTHROUGH();
		case OPERATOR_XOR:     FALLTHROUGH();
		case OPERATOR_NOT:     FALLTHROUGH();
		case OPERATOR_AND:
			{
				Statement *const statement = parse_basic_simple_statement(parser, block_flags);
				TRACE_LEAVE();
				return statement;
			}
		default:
			break;
		}
		break;
	case KIND_ATTRIBUTE:
		{
			Statement *const statement = parse_attributes_for_statement(parser, block_flags);
			TRACE_LEAVE();
			return statement;
		}
	case KIND_DIRECTIVE:
		{
			Statement *const statement = parse_directive_for_statement(parser, block_flags);
			TRACE_LEAVE();
			return statement;
		}
	case KIND_LBRACE:
		{
			BlockStatement *const statement = parse_block_statement(parser, block_flags, false);
			TRACE_LEAVE();
			return RCAST(Statement *, statement);
		}
	case KIND_SEMICOLON:
		{
			EmptyStatement *const statement = parse_empty_statement(parser);
			TRACE_LEAVE();
			return RCAST(Statement *, statement);
		}
	default:
		ICE("Unexpected token in statement");
	}

	if (is_keyword(token, KEYWORD_ELSE)) {
		PARSE_ERROR("'else' unattached to an 'if' or 'when' statement");
	}

	PARSE_ERROR("Expected statement");

	TRACE_LEAVE(); // Unreachable.
}

Bool parse(Tree *tree, Context *context) {
	tree_init(tree, tree->source.name, context);

	if (!source_read(&tree->source, context)) {
		return false;
	}

	Parser parser;
	if (!parser_init(&parser, tree, context)) {
		return false;
	}

	// Read in the first token.
	advancep(&parser);

	// Read all the statements in the top-level.
	while (!is_kind(parser.this_token, KIND_EOF)) {
		Statement *const statement = parse_statement(&parser, CAST(BlockFlag, 0));
		if (statement->kind != STATEMENT_EMPTY) {
			array_push(tree->statements, statement);
		}
	}

	parser_fini(&parser);

	return true;
}
