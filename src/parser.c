#include <stdio.h>

#include "tree.h"
#include "context.h"
#include "strbuf.h"
#include "report.h"
#include "utility.h"

#define TRACE 0

#define PARSE_ERROR(...) \
	do { \
		report_error(&parser->source, &parser->lexer.location, __VA_ARGS__); \
		THROW(1); \
	} while (0)

#define ICE(...) \
	PARSE_ERROR(__VA_ARGS__)

#define UNIMPLEMENTED(what) \
	PARSE_ERROR("Unimplemented: %s", (what))

static Bool source_read(Source *source, String filename, Context *context) {
	Array(Uint8) contents = readfile(filename);
	if (!contents) {
		return false;
	}
	source->contents.contents = contents;
	source->contents.length = array_size(contents);
	source->name = filename;
	return true;
}

typedef struct Parser Parser;

struct Parser {
	Context *context;
	Source source;
	Lexer lexer;
	Tree *tree;
	Tree *generic;
	Token this_token; // This token is being processed
	Token last_token; // Last token processed
	ProcedureType *this_procedure;
	Sint32 trace_depth;
	Sint32 expression_depth;
	Bool allow_newline;
	Bool allow_type;
};

static void parser_trace_enter(Parser *parser, const char *function) {
	if (TRACE) {
		if (parser->trace_depth) {
			printf("%*c", parser->trace_depth * 2, ' ');
		}
		puts(function);
	}
	parser->trace_depth++;
}

static void parser_trace_leave(Parser *parser) {
	parser->trace_depth--;
}

#define TRACE_ENTER() parser_trace_enter(parser, __FUNCTION__)
#define TRACE_LEAVE() parser_trace_leave(parser)

static Bool parser_init(Parser *parser, String filename, Context *context) {
	if (!source_read(&parser->source, filename, context)) {
		return false;
	}

	if (!lexer_init(&parser->lexer, parser->context, &parser->source)) {
		return false;
	}

	parser->context = context;
	parser->this_token = TOKEN_NIL;
	parser->last_token = TOKEN_NIL;
	parser->trace_depth = 0;
	parser->expression_depth = 0;
	parser->allow_newline = false;
	parser->allow_type = false;

	return true;
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

static Bool accepted_separator(Parser *parser) {
	const Token token = parser->this_token;
	if (accepted_operator(parser, OPERATOR_COMMA)) {
		return true;
	}
	if (is_kind(token, KIND_SEMICOLON)) {
		// TODO(dweiler): Check that the next token is } or )
		advancep(parser);
		return true;
	}
	return false;
}

// Same as above but for control statements
static Token peekp(Parser *parser);
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
	const Token token = lexer_peek(&parser->lexer);
	if (is_kind(token, KIND_COMMENT)) {
		return peekp(parser);
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
	if (!is_kind(next, KIND_LBRACE) && !is_keyword(next, KEYWORD_ELSE) && is_keyword(next, KEYWORD_WHERE)) {
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
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_kind(token, kind)) {
		const String want = kind_to_string(kind);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected '%.*s', got '%.*s'\n", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_operator(Parser *parser, OperatorKind op) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_operator(token, op)) {
		const String want = operator_to_string(op);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected operator '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_keyword(Parser* parser, KeywordKind keyword) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_keyword(token, keyword)) {
		const String want = keyword_to_string(keyword);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected keyword '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_assignment(Parser *parser, AssignmentKind assignment) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_assignment(token, assignment)) {
		const String want = assignment_to_string(assignment);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected assignment '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_literal(Parser *parser, LiteralKind literal) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_literal(token, literal)) {
		const String want = literal_to_string(literal);
		const String have = token_to_string(token);
		PARSE_ERROR("Expected literal '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static void expect_semicolon(Parser *parser) {
	Context *context = parser->context;

	if (accepted_kind(parser, KIND_SEMICOLON)) {
		return;
	}

	const Token token = parser->this_token;
	if (is_kind(token, KIND_LBRACE) || is_kind(token, KIND_EOF)) {
		return;
	}

	if (token.location.line == parser->last_token.location.line) {
		PARSE_ERROR("Expected ';'");
	}
}

static CallingConvention string_to_calling_convention(String input) {
	/**/ if (string_compare(input, SCLIT("c")))    return CCONV_CDECL;
	else if (string_compare(input, SCLIT("std")))  return CCONV_STDCALL;
	else if (string_compare(input, SCLIT("fast"))) return CCONV_FASTCALL;
	#define CCONVENTION(string, enum) \
		else if (string_compare(input, SCLIT(string))) return CCONV_ ## enum;
	#include "lexemes.h"
	#undef CCONVENTION
	return CCONV_INVALID;
}

static Identifier *parse_identifier(Parser *parser, Bool poly) {
	TRACE_ENTER();
	const Token token = parser->this_token;
	if (is_kind(token, KIND_IDENTIFIER)) {
		// NOTE(dweiler): Should write this a different way?
		advancep(parser);
	} else {
		Context *context = parser->context;
		const String got = token_to_string(token);
		PARSE_ERROR("Expected identifier, got '%.*s'", SFMT(got));
	}
	Identifier *identifier = tree_new_identifier(parser->tree, token.string, poly);
	TRACE_LEAVE();
	return identifier;
}

static Expression *parse_operand(Parser *parser, Bool lhs);
static Expression *parse_expression(Parser *parser, Bool lhs);
static Expression *parse_atom_expression(Parser *parser, Expression *operand, Bool lhs);

// IdentifierType when parsing an identifier.
// ExpressionType when anything else.
static Type *parse_type_or_identifier(Parser *parser) {
	TRACE_ENTER();
	const Sint32 depth = parser->expression_depth;
	const Bool allow_type = parser->allow_type;
	parser->expression_depth = -1;
	parser->allow_type = true;
	Expression *operand = parse_operand(parser, true);
	Expression *expression = parse_atom_expression(parser, operand, true);
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
	TRACE_ENTER();
	Context *context = parser->context;
	Type *type = parse_type_or_identifier(parser);
	if (!type) {
		advancep(parser);
		PARSE_ERROR("Expected a type");
	}
	TRACE_LEAVE();
	return type;
}

static CompoundLiteralExpression *parse_compound_literal_expression(Parser *parser, Expression *expression);

static Type *parse_variable_name_or_type(Parser *parser) {
	Context *context = parser->context;
	TRACE_ENTER();
	if (is_operator(parser->this_token, OPERATOR_ELLIPSIS)) {
		advancep(parser);
		Type *type = parse_type_or_identifier(parser);
		if (!type) {
			PARSE_ERROR("Missing type after '..'");
		}
		// TODO(dweiler): Tag type with ellipsis.
	} else if (is_keyword(parser->this_token, KEYWORD_TYPEID)) {
		expect_keyword(parser, KEYWORD_TYPEID);
		Type *specialization = 0;
		if (accepted_operator(parser, OPERATOR_QUO)) {
			specialization = parse_type(parser);
		}
		TypeidType *type = tree_new_typeid_type(parser->tree, specialization);
		TRACE_LEAVE();
		return RCAST(Type *, type);
	}
	Type *type = parse_type(parser);
	TRACE_LEAVE();
	return type;
}

static Identifier *evaluate_identifier_expression(const Expression *expression);

static Identifier *evaluate_identifier_type(const Type *type) {
	switch (type->kind) {
	case TYPE_IDENTIFIER:
		return RCAST(const IdentifierType *, type)->identifier;
	case TYPE_EXPRESSION:
		return evaluate_identifier_expression(RCAST(const ExpressionType *, type)->expression);
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
	default:
		return 0;
	}
	UNREACHABLE();
}

static Array(Field*) parse_field_list(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	const Bool allow_newline = parser->allow_newline;
	parser->allow_newline = true;

	Array(Field*) fields = 0;

	// When parsing a field list we may have
	// 	Type0, Type1, Type2, ..., or
	//	Ident0, Ident1, Ident2, ... : Type0
	Array(Type*) list = 0;
	Type *type = 0;
	while (!is_operator(parser->this_token, OPERATOR_COLON)
			&& !is_operator(parser->this_token, OPERATOR_RPAREN)
			&& !is_kind(parser->this_token, KIND_EOF))
	{
		Type *name_or_type = parse_variable_name_or_type(parser);
		array_push(list, name_or_type);
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

	const Size n_elements = array_size(list);
	if (type) {
		// The list are identifiers which have type 'type'
		for (Size i = 0; i < n_elements; i++) {
			const Type *name = list[i];
			Identifier *identifier = evaluate_identifier_type(name);
			if (!identifier) {
				PARSE_ERROR("Expected identifier");
			}
			array_push(fields, tree_new_field(parser->tree, type, identifier, value));
		}
	} else {
		// Generate identifiers with '__unnamed_%d'.
		for (Size i = 0; i < n_elements; i++) {
			StrBuf buf;
			strbuf_init(&buf, context);
			strbuf_put_formatted(&buf, "__unnamed_%d", CAST(Sint32, array_size(fields)));
			Identifier *name = tree_new_identifier(parser->tree, strbuf_result(&buf), false);
			array_push(fields, tree_new_field(parser->tree, list[i], name, value));
		}
	}

	// Early out for trailing fields.
	if (!accepted_separator(parser)) {
		TRACE_LEAVE();
		parser->allow_newline = allow_newline;
		return fields;
	}

	while (!is_operator(parser->this_token, OPERATOR_RPAREN)
	    && !is_kind(parser->this_token, KIND_EOF)
			&& !is_kind(parser->this_token, KIND_SEMICOLON))
	{
		Type *type = 0;
		Array(Identifier*) names = 0;
		for (;;) {
			Identifier *name = parse_identifier(parser, false);
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
		const Size n_names = array_size(names);
		for (Size i = 0; i < n_names; i++) {
			array_push(fields, tree_new_field(parser->tree, type, names[i], value));
		}
		if (!accepted_separator(parser)) {
			break;
		}
	}

	parser->allow_newline = true;

	TRACE_LEAVE();

	return fields;
}

static Array(Field*) parse_procedure_results(Parser *parser, Bool *diverging) {
	Context *context = parser->context;

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
	
	Array(Field*) fields = 0;

	if (!is_operator(parser->this_token, OPERATOR_LPAREN)) {
		Type *type = parse_type(parser);
		Identifier *identifier = tree_new_identifier(parser->tree, SCLIT("__unnamed"), false);
		Field *field = tree_new_field(parser->tree, type, identifier, 0);
		array_push(fields, field);
		TRACE_LEAVE();
		return fields;
	}

	expect_operator(parser, OPERATOR_LPAREN);

	fields = parse_field_list(parser);

	advance_possible_newline(parser);

	expect_operator(parser, OPERATOR_RPAREN);

	TRACE_LEAVE();

	return fields;
}

static ProcedureType *parse_procedure_type(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;
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
	
	Array(Field*) fields = parse_field_list(parser);

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
	Array(Field*) results = parse_procedure_results(parser, &diverging);

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
	TRACE_ENTER();
	Context *context = parser->context;

	ProcedureType *type = parse_procedure_type(parser);

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

	if (is_kind(parser->this_token, KIND_LBRACE)) {
		BlockFlag block_flags = CAST(BlockFlag, 0);
		if (flags & PROC_FLAG_BOUNDS_CHECK) block_flags |= BLOCK_FLAG_BOUNDS_CHECK;
		if (flags & PROC_FLAG_TYPE_ASSERT)  block_flags |= BLOCK_FLAG_TYPE_ASSERT;
		parser->this_procedure = type;
		BlockStatement *body = parse_body(parser, block_flags);
		ProcedureExpression *expression = tree_new_procedure_expression(parser->tree, type, where_clauses, body);
		TRACE_LEAVE();
		return RCAST(Expression *, expression);
	}

	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return RCAST(Expression *, expression);
}

static CallExpression *parse_call_expression(Parser *parser, Expression *operand);

static Statement *parse_statement(Parser *parser, BlockFlag block_flags);
static Expression *parse_unary_expression(Parser *parser, Bool lhs);

static Statement *parse_directive_for_statement(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();

	Context *context = parser->context;

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
		statement = parse_statement(parser, block_flags);
		break;
	case DIRECTIVE_ASSERT:
		// Compile time assert.
		break;
	case DIRECTIVE_PANIC:
		// Compile time panic.
		break;
	case DIRECTIVE_UNROLL:
		UNIMPLEMENTED("#unroll");
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

static IdentifierExpression *parse_operand_identifier_expression(Parser *parser) {
	TRACE_ENTER();
	Identifier *identifier = parse_identifier(parser, false);
	IdentifierExpression *expression = tree_new_identifier_expression(parser->tree, identifier);
	TRACE_LEAVE();
	return expression;
}

static LiteralExpression *parse_operand_literal_expression(Parser *parser) {
	TRACE_ENTER();
	const Token token = advancep(parser);
	LiteralExpression *expression = tree_new_literal_expression(parser->tree, token.as_literal, token.string);
	TRACE_LEAVE();
	return expression;
}

// bit_set[T; U]
static TypeExpression *parse_operand_bit_set_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_BIT_SET);
	expect_operator(parser, OPERATOR_LBRACKET);
	Expression *expression = parse_expression(parser, false);
	Type *underlying = 0;
	if (accepted_kind(parser, KIND_SEMICOLON)) {
		underlying = parse_type(parser);
	}
	expect_operator(parser, OPERATOR_RBRACKET);

	BitSetType *type = tree_new_bit_set_type(parser->tree, expression, underlying);

	TypeExpression *result = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return result;
}

// typeid
static TypeExpression *parse_operand_typeid_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_TYPEID);

	TypeidType *type = tree_new_typeid_type(parser->tree, 0);
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// map[K]V
static TypeExpression *parse_operand_map_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_MAP);

	expect_operator(parser, OPERATOR_LBRACKET);
	Expression *key_expression = parse_expression(parser, true);
	expect_operator(parser, OPERATOR_RBRACKET);
	Type *value = parse_type(parser);
	ExpressionType *key = tree_new_expression_type(parser->tree, key_expression);
	MapType *type = tree_new_map_type(parser->tree, RCAST(Type *, key), value);
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// matrix[R,C]T
static TypeExpression *parse_operand_matrix_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_MATRIX);

	expect_operator(parser, OPERATOR_LBRACKET);
	Expression *rows = parse_expression(parser, true);
	expect_operator(parser, OPERATOR_COMMA);
	Expression *columns = parse_expression(parser, true);
	expect_operator(parser, OPERATOR_RBRACKET);

	Type *base_type = parse_type(parser);

	MatrixType *type = tree_new_matrix_type(parser->tree, rows, columns, base_type);
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// ^T
static TypeExpression *parse_operand_pointer_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_operator(parser, OPERATOR_POINTER);

	PointerType *type = tree_new_pointer_type(parser->tree, parse_type(parser));
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// [^]T
static TypeExpression *parse_operand_multi_pointer_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_operator(parser, OPERATOR_POINTER);
	expect_operator(parser, OPERATOR_RBRACKET);

	MultiPointerType *type = tree_new_multi_pointer_type(parser->tree, parse_type(parser));
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// [?]T or [N]T
static TypeExpression *parse_operand_array_type_expression(Parser *parser, Bool parse_count) {
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

	ArrayType *type = tree_new_array_type(parser->tree, parse_type(parser), count);
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// [dynamic]T
static TypeExpression *parse_operand_dynamic_array_type_expression(Parser *parser) {
	TRACE_ENTER();

	expect_operator(parser, OPERATOR_RBRACKET);

	DynamicArrayType *type = tree_new_dynamic_array_type(parser->tree, parse_type(parser));
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// []T
static TypeExpression *parse_operand_slice_type_expression(Parser *parser) {
	TRACE_ENTER();

	SliceType *type = tree_new_slice_type(parser->tree, parse_type(parser));
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// distinct T
static TypeExpression *parse_operand_distinct_type(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_DISTINCT);

	DistinctType *type = tree_new_distinct_type(parser->tree, parse_type(parser));
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

static Expression *parse_value(Parser *parser) {
	TRACE_ENTER();
	if (is_kind(parser->this_token, KIND_LBRACE)) {
		CompoundLiteralExpression *expression = parse_compound_literal_expression(parser, 0);
		TRACE_LEAVE();
		return RCAST(Expression *, expression);
	}
	Expression *value = parse_expression(parser, false);
	TRACE_LEAVE();
	return value;
}

static TypeExpression *parse_operand_enum_type_expression(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	expect_keyword(parser, KEYWORD_ENUM);

	Type *base_type = 0;
	if (!is_kind(parser->this_token, KIND_LBRACE)) {
		base_type = parse_type(parser);
	}

	advance_possible_newline_within(parser);

	Array(Field*) fields = 0;
	expect_kind(parser, KIND_LBRACE);
	while (!is_kind(parser->this_token, KIND_RBRACE) &&
	       !is_kind(parser->this_token, KIND_EOF))
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
	
		Identifier *identifier = RCAST(IdentifierExpression *, name)->identifier;
	
		Field *field = tree_new_field(parser->tree, 0, identifier, value);
		array_push(fields, field);
	
		if (!accepted_separator(parser)) {
			break;
		}
	}
	expect_kind(parser, KIND_RBRACE);
	
	EnumType *type = tree_new_enum_type(parser->tree, base_type, fields);
	TypeExpression *expression = tree_new_type_expression(parser->tree, RCAST(Type *, type));

	TRACE_LEAVE();

	return expression;
}

// $ident
static IdentifierExpression *parse_operand_const_identifier_expression(Parser *parser) {
	TRACE_ENTER();

	expect_kind(parser, KIND_CONST);

	Identifier *identifier = parse_identifier(parser, true);
	IdentifierExpression *expression = tree_new_identifier_expression(parser->tree, identifier);

	TRACE_LEAVE();

	return expression;
}

static Expression *parse_directive_prefix(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	Context *context = parser->context;

	const Token token = expect_kind(parser, KIND_DIRECTIVE);
	switch (token.as_directive) {
	case DIRECTIVE_TYPE:
		{
			Type *type = parse_type(parser);
			TypeExpression *expression = tree_new_type_expression(parser->tree, type);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_SIMD:
		{
			Type *type = parse_type(parser);
			if (type->kind != TYPE_ARRAY) {
				PARSE_ERROR("Can only apply '#simd' directive to array type");
			}
			TypeExpression *expression = tree_new_type_expression(parser->tree, type);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_SOA:
		FALLTHROUGH();
	case DIRECTIVE_SPARSE:
		{
			Type *type = parse_type(parser);
			TypeExpression *expression = tree_new_type_expression(parser->tree, type);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
	case DIRECTIVE_PARTIAL:
		{
			Expression *expression = parse_expression(parser, lhs);
			TRACE_LEAVE();
			return expression;
		}
	case DIRECTIVE_BOUNDS_CHECK:    FALLTHROUGH();
	case DIRECTIVE_NO_BOUNDS_CHECK: FALLTHROUGH();
	case DIRECTIVE_TYPE_ASSERT:     FALLTHROUGH();
	case DIRECTIVE_NO_TYPE_ASSERT:
		{
			Expression *expression = parse_expression(parser, lhs);
			TRACE_LEAVE();
			return expression;
		}
	default:
		break;
	}

	TRACE_LEAVE();
	return 0;
}

static Expression *parse_operand(Parser *parser, Bool lhs) {
	TRACE_ENTER();
	Context *context = parser->context;
	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_IDENTIFIER:
		{
			IdentifierExpression *expression = parse_operand_identifier_expression(parser);
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
				LiteralExpression *expression = parse_operand_literal_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case LITERAL_COUNT:
			UNREACHABLE();
		}
		break;
	case KIND_LBRACE:
		if (!lhs) {
			CompoundLiteralExpression *expression = parse_compound_literal_expression(parser, 0);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
		break;
	case KIND_DIRECTIVE:
		{
			Expression *expression = parse_directive_prefix(parser, lhs);
			TRACE_LEAVE();
			return expression;
		}
	case KIND_KEYWORD:
		switch (token.as_keyword) {
		case KEYWORD_DISTINCT:
			{
				TypeExpression *expression = parse_operand_distinct_type(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_PROC:
			{
				Expression *expression = 0;
				expect_keyword(parser, KEYWORD_PROC);
				if (is_kind(parser->this_token, KIND_LBRACE)) {
					UNIMPLEMENTED("Procedure groups");
				} else {
					expression = parse_procedure(parser);
				}
				TRACE_LEAVE();
				return expression;
			}
		case KEYWORD_BIT_SET:
			{
				TypeExpression *expression = parse_operand_bit_set_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_TYPEID:
			{
				TypeExpression *expression = parse_operand_typeid_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_MAP:
			{
				TypeExpression *expression = parse_operand_map_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_MATRIX:
			{
				TypeExpression *expression = parse_operand_matrix_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_STRUCT:
			UNIMPLEMENTED("struct");
		case KEYWORD_UNION:
			UNIMPLEMENTED("union");
		case KEYWORD_ENUM:
			{
				TypeExpression *expression = parse_operand_enum_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case KEYWORD_CONTEXT:
			{
				Identifier *identifier = tree_new_identifier(parser->tree, SCLIT("context"), false);
				IdentifierExpression *expression = tree_new_identifier_expression(parser->tree, identifier);
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
				Expression *operand = parse_expression(parser, false);
				expect_operator(parser, OPERATOR_RPAREN);
				parser->expression_depth = expression_depth;
				parser->allow_newline = allow_newline;
				TRACE_LEAVE();
				return operand;
			}
		case OPERATOR_POINTER:
			{
				// ^T
				TypeExpression *expression = parse_operand_pointer_type_expression(parser);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case OPERATOR_LBRACKET:
			{
				// [^]T, [?]T, [dynamic]T, [N]T, []T

				expect_operator(parser, OPERATOR_LBRACKET);

				if (is_operator(parser->this_token, OPERATOR_POINTER)) {
					// [^]T
					TypeExpression *expression = parse_operand_multi_pointer_type_expression(parser);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (is_operator(parser->this_token, OPERATOR_QUESTION)) {
					// [?]T
					TypeExpression *expression = parse_operand_array_type_expression(parser, false);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (accepted_keyword(parser, KEYWORD_DYNAMIC)) {
					// [dynamic]T
					TypeExpression *expression = parse_operand_dynamic_array_type_expression(parser);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (!is_operator(parser->this_token, OPERATOR_RBRACKET)) {
					// [N]T
					TypeExpression *expression = parse_operand_array_type_expression(parser, true);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				} else if (accepted_operator(parser, OPERATOR_RBRACKET)) {
					// []T
					TypeExpression *expression = parse_operand_slice_type_expression(parser);
					TRACE_LEAVE();
					return RCAST(Expression *, expression);
				}
			}
		default:
			break;
		}
		break;
	case KIND_CONST:
		{
			// $ident
			IdentifierExpression *expression = parse_operand_const_identifier_expression(parser);
			TRACE_LEAVE();
			return RCAST(Expression *, expression);
		}
		break;
	default:
		break;
	}

	TRACE_LEAVE();
	return 0;
}

static CallExpression *parse_call_expression(Parser *parser, Expression *operand) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Expression*) arguments = 0;
	const Sint32 expression_depth = parser->expression_depth;
	const Bool allow_newline = parser->allow_newline;
	parser->expression_depth = 0;
	parser->allow_newline = true;

	expect_operator(parser, OPERATOR_LPAREN);

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

		Expression *argument = parse_expression(parser, false);
		if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			expect_assignment(parser, ASSIGNMENT_EQ);
			if (has_ellipsis) {
				PARSE_ERROR("Cannot apply '..' to field");
			}

			// Value *value = CAST(Value *, parse_value(parser));
			// argument = 0; // tree_new_value(parser->tree, argument, value);
		} else if (has_ellipsis) {
			PARSE_ERROR("Position arguments not allowed after '..'");
		}

		array_push(arguments, argument);

		if (!accepted_separator(parser)) {
			break;
		}
	}
	parser->allow_newline = allow_newline;
	parser->expression_depth = expression_depth;
	expect_operator(parser, OPERATOR_RPAREN);
	CallExpression *expression = tree_new_call_expression(parser->tree, operand, arguments);
	TRACE_LEAVE();
	return expression;
}

static Token expect_closing(Parser *parser, Kind kind) {
	const Token token = parser->this_token;
	if (!is_kind(token, kind) && is_kind(token, KIND_SEMICOLON)
		&& (string_compare(token.string, SCLIT("\n")) || is_kind(token, KIND_EOF)))
	{
		advancep(parser);
	}
	return expect_kind(parser, kind);
}

static CompoundLiteralExpression *parse_compound_literal_expression(Parser *parser, Expression *expression) {
	Context *context = parser->context;
	TRACE_ENTER();
	Array(Expression*) elements = 0;
	expect_kind(parser, KIND_LBRACE);
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	if (!is_kind(parser->this_token, KIND_RBRACE)) {
		UNIMPLEMENTED("Compound literal expressions");
	}
	parser->expression_depth = depth;
	expect_closing(parser, KIND_RBRACE);
	CompoundLiteralExpression *expr = tree_new_compound_literal_expression(parser->tree, expression, elements);
	TRACE_LEAVE();
	return expr;
}

static Expression *parse_index_expression(Parser *parser, Expression *operand) {
	TRACE_ENTER();

	Context *context = parser->context;

	Expression *lhs = 0;
	Expression *rhs = 0;

	parser->expression_depth++;

	// [
	const Token token = expect_operator(parser, OPERATOR_LBRACKET);

	// [lhs
	if (!is_operator(token, OPERATOR_ELLIPSIS)
	 && !is_operator(token, OPERATOR_RANGEFULL)
	 && !is_operator(token, OPERATOR_RANGEHALF)
	 && !is_operator(token, OPERATOR_COLON))
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
		if (!is_operator(interval, OPERATOR_RBRACKET) && !is_kind(interval, KIND_EOF)) {
			rhs = parse_expression(parser, false);
		}
	}

	parser->expression_depth--;

	expect_operator(parser, OPERATOR_RBRACKET);

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
	TRACE_ENTER();

	Context *context = parser->context;

	if (!operand) {
		if (parser->allow_type) {
			TRACE_LEAVE();
			return 0;
		}
		PARSE_ERROR("Expected an operand");
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
				advancep(parser);
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
						ident = tree_new_identifier(parser->tree, SCLIT("?"), false);
						type = RCAST(Type *, tree_new_identifier_type(parser->tree, ident));
						operand = RCAST(Expression *, tree_new_assertion_expression(parser->tree, operand, type));
						break;
					default:
						break;
					}
					break;
				default:
					PARSE_ERROR("Expected selector");
					advancep(parser);
					break;
				}
				break;
			case OPERATOR_ARROW:
				// ->ident
				advancep(parser);
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
				CompoundLiteralExpression *expression = parse_compound_literal_expression(parser, operand);
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
	if (token.as_operator != OPERATOR_AUTO_CAST) {
		expect_operator(parser, OPERATOR_LPAREN);
		type = parse_type(parser);
		expect_operator(parser, OPERATOR_RPAREN);
	}
	Expression *operand = parse_unary_expression(parser, lhs);
	CastExpression *expression = tree_new_cast_expression(parser->tree, token.as_operator, type, operand);
	TRACE_LEAVE();
	return expression;
}

static UnaryExpression *parse_unary_stem_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();
	const Token token = advancep(parser);
	Expression *operand = parse_unary_expression(parser, lhs);
	UnaryExpression *expression = tree_new_unary_expression(parser->tree, token.as_operator, operand);
	TRACE_LEAVE();
	return expression;
}

static SelectorExpression *parse_implicit_selector_expression(Parser *parser) {
	TRACE_ENTER();
	expect_operator(parser, OPERATOR_PERIOD);
	Identifier *identifier = parse_identifier(parser, false);
	SelectorExpression *expression = tree_new_selector_expression(parser->tree, 0, identifier);
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
				CastExpression *expression = parse_cast_expression(parser, lhs);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
		case OPERATOR_ADD: FALLTHROUGH();
		case OPERATOR_SUB: FALLTHROUGH();
		case OPERATOR_XOR: FALLTHROUGH();
		case OPERATOR_AND: FALLTHROUGH();
		case OPERATOR_NOT:
			{
				UnaryExpression *expression = parse_unary_stem_expression(parser, lhs);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
			break;
		case OPERATOR_PERIOD:
			{
				SelectorExpression *expression = parse_implicit_selector_expression(parser);
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

	Expression *operand = parse_operand(parser, lhs);
	Expression *expression = parse_atom_expression(parser, operand, lhs);

	TRACE_LEAVE();

	return expression;
}

static TernaryExpression *parse_ternary_expression(Parser *parser, Expression *expr, Bool lhs) {
	TRACE_ENTER();

	Expression *cond = 0;
	Expression *on_true = 0;
	KeywordKind kind = KEYWORD_IF;
	if (is_operator(parser->last_token, OPERATOR_QUESTION)) {
		cond = expr;
		on_true = parse_expression(parser, lhs);
		expect_operator(parser, OPERATOR_COLON);
	} else {
		kind = parser->last_token.as_keyword;
		cond = parse_expression(parser, lhs);
		on_true = expr;
		expect_keyword(parser, KEYWORD_ELSE);
	}

	Expression *on_false = parse_expression(parser, lhs);
	TernaryExpression *ternary = tree_new_ternary_expression(parser->tree, on_true, kind, cond, on_false);

	TRACE_LEAVE();

	return ternary;
}

static Expression *parse_binary_expression(Parser *parser, Bool lhs, Sint32 prec) {
	TRACE_ENTER();

	Context *context = parser->context;

	Expression *expr = parse_unary_expression(parser, lhs);

	// Simple operator precedence climbing.
	#define OPERATOR(ident, match, prec, ...) (prec),
	static const int PRECEDENCE[] = {
		#include "lexemes.h"
		0,
	};

	for (;;) {
		const Token token = parser->this_token;
		const Token last = parser->last_token;
		Sint32 op_prec = 0;
		if (is_kind(token, KIND_OPERATOR)) {
			op_prec = PRECEDENCE[token.as_operator];
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
		
		// TODO(dweiler): More robust.
		advancep(parser);

		if (is_operator(token, OPERATOR_QUESTION) || is_keyword(token, KEYWORD_IF) || is_keyword(token, KEYWORD_WHEN)) {
			TernaryExpression *ternary = parse_ternary_expression(parser, expr, lhs);
			expr = RCAST(Expression *, ternary);
		} else {
			Expression *rhs = parse_binary_expression(parser, false, op_prec + 1);
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
	Expression *expression = parse_binary_expression(parser, lhs, 1);
	TRACE_LEAVE();
	return expression;
}

static ListExpression *parse_list_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	Context *context = parser->context;

	const Bool allow_newline = parser->allow_newline;

	Array(Expression*) expressions = 0;
	for (;;) {
		Expression *expression = parse_expression(parser, lhs);
		array_push(expressions, expression);
		if (!is_operator(parser->this_token, OPERATOR_COMMA) || is_kind(parser->this_token, KIND_EOF)) {
			break;
		}
		advancep(parser);
	}

	ListExpression *expression = tree_new_list_expression(parser->tree, expressions);

	parser->allow_newline = allow_newline;

	TRACE_LEAVE();

	return expression;
}

static FORCE_INLINE ListExpression *parse_lhs_list_expression(Parser *parser) {
	TRACE_ENTER();
	ListExpression *result = parse_list_expression(parser, true);
	TRACE_LEAVE();
	return result;
}

static FORCE_INLINE ListExpression *parse_rhs_list_expression(Parser *parser) {
	TRACE_ENTER();
	ListExpression *result = parse_list_expression(parser, false);
	TRACE_LEAVE();
	return result;
}

static Statement *parse_statement(Parser *parser, BlockFlag block_flags);

static Array(Statement*) parse_statement_list(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Statement*) statements = 0;

	// Stop parsing the statement when we encounter one of:
	//
	//		"case"
	//		"}"
	//		EOF
	while (!is_keyword(parser->this_token, KEYWORD_CASE) &&
	       !is_kind(parser->this_token, KIND_RBRACE) &&
	       !is_kind(parser->this_token, KIND_EOF))
	{
		Statement *statement = parse_statement(parser, block_flags);
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
	expect_kind(parser, KIND_LBRACE);
	Array(Statement*) statements = parse_statement_list(parser, flags);
	expect_kind(parser, KIND_RBRACE);
	parser->expression_depth = depth;
	parser->allow_newline = allow_newline;
	BlockStatement *statement = tree_new_block_statement(parser->tree, flags, statements);
	TRACE_LEAVE();
	return statement;
}

static BlockStatement *parse_block_statement(Parser *parser, BlockFlag flags, Bool when) {
	TRACE_ENTER();

	Context *context = parser->context;

	advance_possible_newline(parser);

	if (!when && !parser->this_procedure) {
		PARSE_ERROR("Cannot use block statement at file scope");
	}

	BlockStatement *body = parse_body(parser, flags);

	TRACE_LEAVE();

	return body;
}

static DeclarationStatement *parse_declaration_statement_tail(Parser *parser, Array(Identifier*) names) {
	// ':'
	TRACE_ENTER();

	Context *context = parser->context;

	ListExpression *values = 0;
	Type *type = parse_type_or_identifier(parser);
	const Token token = parser->this_token;
	Bool constant = false;

	const Size n_names = array_size(names);
	// Check for '=' (:=)
	// Check for ':' (::)
	if (is_assignment(token, ASSIGNMENT_EQ) || is_operator(token, OPERATOR_COLON)) {
		const Token seperator = advancep(parser);
		constant = is_operator(seperator, OPERATOR_COLON);
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
		if (is_kind(token, KIND_RBRACE) && token.location.line == parser->last_token.location.line) {
			// Do nothing.
		} else {
			expect_semicolon(parser);
		}
	}

	if (!parser->this_procedure && n_values > 0 && n_names != n_values) {
		PARSE_ERROR("Expected %d expressions on the right-hand side, got %d instead", n_names, n_values);
	}

	DeclarationStatement *statement = tree_new_declaration_statement(parser->tree, type, names, values);

	TRACE_LEAVE();

	return statement;
}

static DeclarationStatement *parse_declaration_statement(Parser *parser, ListExpression *lhs) {
	Context *context = parser->context;

	TRACE_ENTER();

	expect_operator(parser, OPERATOR_COLON);

	// Everything inside 'lhs' should evaluate as an identifier.
	const Size n_names = array_size(lhs->expressions);
	Array(Identifier *) names = 0;
	for (Size i = 0; i < n_names; i++) {
		const Expression *expression = lhs->expressions[i];
		Identifier *identifier = evaluate_identifier_expression(expression);
		if (!identifier) {
			PARSE_ERROR("Expected identifier");
		}
		array_push(names, identifier);
	}

	DeclarationStatement *statement = parse_declaration_statement_tail(parser, names);

	TRACE_LEAVE();

	return statement;
}

static AssignmentStatement *parse_assignment_statement(Parser *parser, ListExpression *lhs) {
	Context *context = parser->context;

	TRACE_ENTER();

	if (!parser->this_procedure) {
		PARSE_ERROR("Cannot use assignment statement at file scope");
	}

	const AssignmentKind kind = parser->this_token.as_assignment;

	advancep(parser);
	ListExpression *rhs = parse_rhs_list_expression(parser);
	if (array_size(rhs->expressions) == 0) {
		PARSE_ERROR("Missing right-hand side in assignment");
	}

	AssignmentStatement *statement = tree_new_assignment_statement(parser->tree, kind, lhs, rhs);

	TRACE_LEAVE();

	return statement;
}

static Statement *parse_simple_statement(Parser* parser, Bool allow_in) {
	TRACE_ENTER();

	Context *context = parser->context;

	ListExpression *lhs = parse_lhs_list_expression(parser);
	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_ASSIGNMENT:
		{
			AssignmentStatement *statement = parse_assignment_statement(parser, lhs);
			TRACE_LEAVE();
			return RCAST(Statement *, statement);
		}
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_IN:
			// TODO(dweiler): Does this actually belong here? Should be treated as a binary operator.
			if (allow_in) {
				// [lhs] in <Expression>
				accepted_operator(parser, OPERATOR_IN);
				Expression *rhs = parse_expression(parser, true);
				BinaryExpression *expression = tree_new_binary_expression(parser->tree, OPERATOR_IN, RCAST(Expression *, lhs), rhs);
				ExpressionStatement *statement = tree_new_expression_statement(parser->tree, RCAST(Expression *, expression));
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
			break;
		case OPERATOR_COLON:
			{
				DeclarationStatement *statement = parse_declaration_statement(parser, lhs);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		default:
			break;
		}
	default:
		break;
	}

	Array(Expression*) expressions = lhs->expressions;
	if (array_size(expressions) == 0 || array_size(expressions) > 1) {
		PARSE_ERROR("Expected one expression on the left-hand side");
	}

	ExpressionStatement *statement = tree_new_expression_statement(parser->tree, expressions[0]);

	TRACE_LEAVE();

	return RCAST(Statement *, statement);
}

static ImportStatement *parse_import_statement(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_IMPORT);

	Token token = TOKEN_NIL;
	switch (parser->this_token.kind) {
	case KIND_IDENTIFIER:
		token = advancep(parser);
		break;
	default:
		break;
	}

	const Token path = expect_literal(parser, LITERAL_STRING);

	expect_semicolon(parser);

	const String name = string_unquote(token.string, "\"");
	const String package = string_unquote(path.string, "\"");

	ImportStatement *statement = tree_new_import_statement(parser->tree, name, package);

	TRACE_LEAVE();

	return statement;
}

static BlockStatement *convert_statement_to_body(Parser *parser, BlockFlag flags, Statement *statement) {
	Context *context = parser->context;
	const StatementKind kind = statement->kind;
	if (kind == STATEMENT_BLOCK || kind == STATEMENT_EMPTY) {
		PARSE_ERROR("Expected a regular statement");
	}
	Array(Statement*) statements = 0;
	array_push(statements, statement);
	return tree_new_block_statement(parser->tree, flags, statements);
}

static Expression *convert_statement_to_expression(Parser *parser, Statement *statement) {
	if (!statement) {
		return 0;
	}
	if (statement->kind != STATEMENT_EXPRESSION) {
		Context *context = parser->context;
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
	Statement *statement = parse_statement(parser, block_flags);
	BlockStatement *body = convert_statement_to_body(parser, block_flags, statement);
	parser->expression_depth = expression_depth;
	parser->allow_newline = allow_newline;
	TRACE_LEAVE();
	return body;
}

static IfStatement *parse_if_statement(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();

	Context *context = parser->context;

	expect_keyword(parser, KEYWORD_IF);

	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = -1;

	Statement *init = parse_simple_statement(parser, false);
	Expression *cond = 0;
	if (accepted_control_statement_separator(parser)) {
		cond = parse_expression(parser, false);
	} else {
		cond = convert_statement_to_expression(parser, init);
		init = 0;
	}

	parser->expression_depth = depth;

	if (!cond) {
		PARSE_ERROR("Expected condition in if statement");
	}

	BlockStatement *body = 0;
	if (accepted_keyword(parser, KEYWORD_DO)) {
		body = parse_do_body(parser, block_flags);
	} else {
		body = parse_block_statement(parser, block_flags, false);
	}

	advance_possible_newline(parser);

	BlockStatement *elif = 0;
	if (is_keyword(parser->this_token, KEYWORD_ELSE)) {
		expect_keyword(parser, KEYWORD_ELSE);
		if (is_keyword(parser->this_token, KEYWORD_IF)) {
			IfStatement *statement = parse_if_statement(parser, block_flags);
			Array(Statement) *statements = 0;
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

	IfStatement *statement = tree_new_if_statement(parser->tree, init, cond, body, elif);

	TRACE_LEAVE();

	return statement;
}

static WhenStatement *parse_when_statement(Parser *parser) {
	Context *context = parser->context;

	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_WHEN);

	const Sint32 expression_depth = parser->expression_depth;
	parser->expression_depth = -1;

	Expression *cond = parse_expression(parser, false);

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
			WhenStatement *statement = parse_when_statement(parser);
			Array(Statement) *statements = 0;
			array_push(statements, RCAST(Statement *, statement));
			elif = tree_new_block_statement(parser->tree, flags, statements);
		} else if (is_keyword(parser->this_token, KEYWORD_DO)) {
			elif = parse_do_body(parser, flags);
		} else if (is_kind(parser->this_token, KIND_LBRACE)) {
			elif = parse_block_statement(parser, flags, true);
		} else {
			PARSE_ERROR("Unexpected statement in 'when' else block");
		}
	}

	WhenStatement *statement = tree_new_when_statement(parser->tree, cond, body, elif);

	TRACE_LEAVE();

	return statement;
}

static ForStatement *parse_for_statement(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();

	Context *context = parser->context;

	Statement *init = 0;
	Expression *cond = 0;
	BlockStatement *body = 0;
	Statement *post = 0;

	expect_keyword(parser, KEYWORD_FOR);

	Bool range = false;

	const Token token = parser->this_token;
	if (!is_kind(token, KIND_LBRACE) && !is_keyword(token, KEYWORD_DO)) {
		const Sint32 depth = parser->expression_depth;
		parser->expression_depth = -1;
		if (is_operator(token, OPERATOR_IN)) {
			// for in [...] do <Statement>
			// for in [...] <BlockStatement>
			expect_operator(parser, OPERATOR_IN);
			Expression *rhs = parse_expression(parser, false);
			if (accepted_keyword(parser, KEYWORD_DO)) {
				body = parse_do_body(parser, block_flags);
			} else {
				body = parse_block_statement(parser, block_flags, false);
			}
			parser->expression_depth = depth;
			cond = RCAST(Expression *, tree_new_unary_expression(parser->tree, OPERATOR_IN, rhs));
			ForStatement *statement = tree_new_for_statement(parser->tree, 0, cond, body, 0);
			TRACE_LEAVE();
			return statement;
		}

		if (!is_kind(token, KIND_SEMICOLON)) {
			// for [...] in <Expression>
			Statement *statement = parse_simple_statement(parser, true);
			if (statement->kind == STATEMENT_EXPRESSION) {
				Expression *expression = RCAST(ExpressionStatement *, statement)->expression;
				if (expression->kind == EXPRESSION_UNARY) {
					BinaryExpression *bin = RCAST(BinaryExpression *, expression);
					if (bin->operation == OPERATOR_IN) {
						// for <statement> in <expression>
						init = statement;
						range = true;
					}
				}
				cond = expression;
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
					Statement *statement = parse_simple_statement(parser, false);
					cond = convert_statement_to_expression(parser, statement);
				}
				if (!is_kind(parser->this_token, KIND_SEMICOLON)) {
					PARSE_ERROR("Expected ';'");
				} else {
					expect_kind(parser, KIND_SEMICOLON);
				}
				if (!is_kind(parser->this_token, KIND_LBRACE) && !is_keyword(parser->this_token, KEYWORD_DO)) {
					// for [...] [...] <post>
					post = parse_simple_statement(parser, false);
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

	ForStatement *statement = tree_new_for_statement(parser->tree, init, cond, body, post);

	TRACE_LEAVE();

	return statement;
}

static DeferStatement *parse_defer_statement(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();
	Context *context = parser->context;
	expect_keyword(parser, KEYWORD_DEFER);
	Statement *statement = parse_statement(parser, block_flags);
	switch (statement->kind) {
	case STATEMENT_EMPTY:
		PARSE_ERROR("Empty statement in defer");
	case STATEMENT_DEFER:
		PARSE_ERROR("Cannot defer a defer statement");
	case STATEMENT_RETURN:
		PARSE_ERROR("Cannot defer a return statement");
	case STATEMENT_DECLARATION:
		PARSE_ERROR("Cannot defer a declaration");
	default:
		break;
	}
	DeferStatement *defer = tree_new_defer_statement(parser->tree, statement);
	TRACE_LEAVE();
	return defer;
}

static ReturnStatement *parse_return_statement(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	expect_keyword(parser, KEYWORD_RETURN);

	if (parser->expression_depth > 0) {
		PARSE_ERROR("Cannot use return statement within expression");
	}

	Array(Expression*) results = 0;
	while (!is_kind(parser->this_token, KIND_SEMICOLON) &&
	       !is_kind(parser->this_token, KIND_RBRACE) &&
				 !is_kind(parser->this_token, KIND_EOF))
	{
		Expression *expression = parse_expression(parser, false);
		array_push(results, expression);
		if (!is_operator(parser->this_token, OPERATOR_COMMA) || is_kind(parser->this_token, KIND_EOF)) {
			break;
		}
		advancep(parser);
	}

	expect_semicolon(parser);

	ReturnStatement *statement = tree_new_return_statement(parser->tree, results);

	TRACE_LEAVE();

	return statement;
}

static Statement *parse_basic_simple_statement(Parser *parser, Bool allow_in) {
	TRACE_ENTER();
	Statement *statement = parse_simple_statement(parser, allow_in);
	expect_semicolon(parser);
	TRACE_LEAVE();
	return statement;
}

static BranchStatement *parse_branch_statement(Parser *parser, KeywordKind kind) {
	TRACE_ENTER();
	const Token token = advancep(parser);
	Identifier *label = 0;
	if (!is_keyword(token, KEYWORD_FALLTHROUGH) && is_kind(parser->this_token, KIND_IDENTIFIER)) {
		label = parse_identifier(parser, false);
	}
	BranchStatement *statement = tree_new_branch_statement(parser->tree, kind, label);
	TRACE_LEAVE();
	return statement;
}

static EmptyStatement *parse_empty_statement(Parser *parser) {
	TRACE_ENTER();
	EmptyStatement *statement = tree_new_empty_statement(parser->tree);
	expect_semicolon(parser);
	TRACE_LEAVE();
	return statement;
}

static Statement *parse_statement(Parser *parser, BlockFlag block_flags) {
	TRACE_ENTER();
	Context *context = parser->context;
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
				Statement *statement = parse_basic_simple_statement(parser, false);
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
				Statement *statement = parse_basic_simple_statement(parser, false);
				TRACE_LEAVE();
				return statement;
			}
		case KEYWORD_FOREIGN:
			UNIMPLEMENTED("foreign declaration");
		case KEYWORD_IF:
			{
				IfStatement *statement = parse_if_statement(parser, block_flags);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_WHEN:
			{
				WhenStatement *statement = parse_when_statement(parser);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_IMPORT:
			{
				ImportStatement *statement = parse_import_statement(parser);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_FOR:
			{
				ForStatement *statement = parse_for_statement(parser, block_flags);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_SWITCH:
			UNIMPLEMENTED("switch statement");
		case KEYWORD_DEFER:
			{
				DeferStatement *statement = parse_defer_statement(parser, block_flags);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_RETURN:
			{
				ReturnStatement *statement = parse_return_statement(parser);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_BREAK:       FALLTHROUGH();
		case KEYWORD_CONTINUE:    FALLTHROUGH();
		case KEYWORD_FALLTHROUGH:
			{
				BranchStatement *statement = parse_branch_statement(parser, token.as_keyword);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
			}
		case KEYWORD_USING:
			UNIMPLEMENTED("using statement");
		default:
			ICE("Unexpected keyword in statement");
		}
		break;
	case KIND_IDENTIFIER:
		{
			Statement *statement = parse_basic_simple_statement(parser, false);
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
				Statement *statement = parse_basic_simple_statement(parser, false);
				TRACE_LEAVE();
				return statement;
			}
		default:
			break;
		}
		break;
	case KIND_ATTRIBUTE:
		UNIMPLEMENTED("attribute");
	case KIND_DIRECTIVE:
		{
			Statement *statement = parse_directive_for_statement(parser, block_flags);
			TRACE_LEAVE();
			return statement;
		}
	case KIND_LBRACE:
		{
			BlockStatement *statement = parse_block_statement(parser, block_flags, false);
			TRACE_LEAVE();
			return RCAST(Statement *, statement);
		}
	case KIND_SEMICOLON:
		{
			EmptyStatement *statement = parse_empty_statement(parser);
			TRACE_LEAVE();
			return RCAST(Statement *, statement);
		}
	default:
		ICE("Unexpected token in statement");
	}

	if (is_keyword(token, KEYWORD_ELSE)) {
		PARSE_ERROR("'else' unattached to an 'if' or 'when' statement");
	}

	ICE("Expected statement");

	TRACE_LEAVE();

	return 0;
}

Tree *parse(String filename, Context *context) {
	Parser parse;
	Parser *parser = &parse;
	if (!parser_init(parser, filename, context)) {
		return 0;
	}

	Allocator *allocator = context->allocator;
	Tree *tree = CAST(Tree *, allocator->allocate(allocator, sizeof *tree));
	if (!tree) {
		return 0;
	}

	tree_init(tree, context);

	parser->tree = tree;

	advancep(parser);

	// Every Odin source file must begin with the package it's part of.
	if (!is_keyword(parser->this_token, KEYWORD_PACKAGE)) {
		PARSE_ERROR("Expected a package declaration at the beginning of the file");
	}

	static const String RESERVED_PACKAGES[] = {
		SLIT("builtin"),
		SLIT("runtime"),
		SLIT("intrinsics")
	};

	// "package <ident>"
	expect_keyword(parser, KEYWORD_PACKAGE);
	const Token package = expect_kind(parser, KIND_IDENTIFIER);
	if (string_compare(package.string, SCLIT("_"))) {
		PARSE_ERROR("Cannot name package '_'");
	}

	for (Size i = 0; i < sizeof(RESERVED_PACKAGES)/sizeof *RESERVED_PACKAGES; i++) {
		const String name = RESERVED_PACKAGES[i];
		if (string_compare(package.string, name)) {
			PARSE_ERROR("Use of reserved package name '%.*s'", SFMT(name));
		}
	}

	tree->package_name = package.string;
	tree->file_name = string_copy(filename);

	while (!is_kind(parser->this_token, KIND_EOF)) {
		Statement *statement = parse_statement(parser, CAST(BlockFlag, 0));
		if (statement && statement->kind != STATEMENT_EMPTY) {
			array_push(tree->statements, statement);
		}
	}

	TRACE_LEAVE();

	return tree;
}
