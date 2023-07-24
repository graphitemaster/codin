#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "parser.h"
#include "lexer.h"
#include "report.h"
#include "tree.h"
#include "context.h"
#include "utility.h"

#define TRACE 0

#ifdef ERROR
#undef ERROR
#endif

#define ERROR(...) \
	do { \
		report_error(&parser->source, &parser->lexer.location, __VA_ARGS__); \
		THROW(1); \
		exit(1); \
	} while (0)

#define ICE(...) \
	ERROR(__VA_ARGS__)

#define UNIMPLEMENTED(what) \
	ERROR("Unimplemented: %s", (what))

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
	Token this_token; // This token is being processed
	Token last_token; // Last token processed
	Node *this_procedure;
	Sint32 trace_depth;
	Sint32 expression_depth;
	Uint64 unique_id;
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
	parser->unique_id = 0;

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
	Context *context = parser->context;
	if (accepted_operator(parser, OPERATOR_COMMA)) {
		return true;
	}
	if (is_kind(token, KIND_SEMICOLON) && !string_compare(token.string, SCLIT("\n"))) {
		ERROR("Expected a comma");
	}
	return false;
}

static Bool ignore_newline(const Parser *parser) {
	// BUG(dweiler): Determine why this has to be >= and not > in the case of
	// procedure groups.
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
	if (!(is_kind(next, KIND_LBRACE) || is_keyword(next, KEYWORD_ELSE) || is_keyword(next, KEYWORD_WHERE))) {
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
		ERROR("Expected '%.*s', got '%.*s'\n", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_operator(Parser *parser, OperatorKind op) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_operator(token, op)) {
		const String want = operator_to_string(op);
		const String have = token_to_string(token);
		ERROR("Expected operator '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_keyword(Parser* parser, KeywordKind keyword) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_keyword(token, keyword)) {
		const String want = keyword_to_string(keyword);
		const String have = token_to_string(token);
		ERROR("Expected keyword '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_assignment(Parser *parser, AssignmentKind assignment) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_assignment(token, assignment)) {
		const String want = assignment_to_string(assignment);
		const String have = token_to_string(token);
		ERROR("Expected assignment '%.*s', got '%.*s'", SFMT(want), SFMT(have));
	}
	return advancep(parser);
}

static Token expect_literal(Parser *parser, LiteralKind literal) {
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (!is_literal(token, literal)) {
		const String want = literal_to_string(literal);
		const String have = token_to_string(token);
		ERROR("Expected literal '%.*s', got '%.*s'", SFMT(want), SFMT(have));
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
		ERROR("Expected ';'");
	}
}

static CallingConvention string_to_calling_convention(String string) {
	/**/ if (string_compare(string, SCLIT("odin")))        return CCONV_ODIN;
	else if (string_compare(string, SCLIT("contextless"))) return CCONV_CONTEXTLESS;
	else if (string_compare(string, SCLIT("cdecl")))       return CCONV_CDECL;
	else if (string_compare(string, SCLIT("c")))           return CCONV_CDECL;
	else if (string_compare(string, SCLIT("stdcall")))     return CCONV_STDCALL;
	else if (string_compare(string, SCLIT("std")))         return CCONV_STDCALL;
	else if (string_compare(string, SCLIT("fastcall")))    return CCONV_FASTCALL;
	else if (string_compare(string, SCLIT("fast")))        return CCONV_FASTCALL;
	else if (string_compare(string, SCLIT("none")))        return CCONV_NONE;
	else if (string_compare(string, SCLIT("naked")))       return CCONV_NAKED;
	else if (string_compare(string, SCLIT("win64")))       return CCONV_STDCALL;
	else if (string_compare(string, SCLIT("sysv")))        return CCONV_CDECL;
	return CCONV_INVALID;
}

static Node *parse_identifier(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (is_kind(token, KIND_IDENTIFIER) || is_keyword(token, KEYWORD_TYPEID)) {
		// NOTE(dweiler): Should write this a different way?
		advancep(parser);
	} else {
		ERROR("Expected identifier or 'typeid'");
	}
	Node *identifier = tree_new_identifier(parser->tree, token.string);
	TRACE_LEAVE();
	return identifier;
}

static Node *parse_operand(Parser *parser, Bool lhs);
static Node *parse_expression(Parser *parser, Bool lhs);
static Node *parse_atom_expression(Parser *parser, Node *operand, Bool lhs);

static Node *parse_type_or_identifier(Parser *parser) {
	TRACE_ENTER();
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = -1;
	Node *operand = parse_operand(parser, true);
	Node *type = parse_atom_expression(parser, operand, true);
	parser->expression_depth = depth;
	TRACE_LEAVE();
	return type;
}

static Node *parse_type(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	Node *type = parse_type_or_identifier(parser);
	if (!type) {
		advancep(parser);
		ERROR("Expected a type");
	}
	TRACE_LEAVE();
	return type;
}

static Node *parse_literal_value(Parser *parser, Node *type);

enum FieldFlag {
	FIELD_FLAG_INVALID   = 0,
	FIELD_FLAG_USING     = 1 << 0,
	FIELD_FLAG_AUTO_CAST = 1 << 1,
	FIELD_FLAG_NO_ALIAS  = 1 << 2,
	FIELD_FLAG_C_VARARG  = 1 << 3,
	FIELD_FLAG_CONST     = 1 << 4,
	FIELD_FLAG_ANY_INT   = 1 << 5,
	FIELD_FLAG_SUBTYPE   = 1 << 6,
	FIELD_FLAG_BY_PTR    = 1 << 7,
};

typedef enum FieldFlag FieldFlag;

static FieldFlag parse_field_flag(Parser *parser) {
	const Token token = parser->this_token;
	Context *context = parser->context;
	if (is_keyword(token, KEYWORD_USING)) {
		return FIELD_FLAG_USING;
	} else if (is_operator(token, OPERATOR_AUTO_CAST)) {
		return FIELD_FLAG_AUTO_CAST;
	} else if (is_kind(token, KIND_DIRECTIVE)) {
		switch (token.as_directive) {
		case DIRECTIVE_NO_ALIAS:
			return FIELD_FLAG_NO_ALIAS;
		case DIRECTIVE_C_VARARG:
			return FIELD_FLAG_C_VARARG;
		case DIRECTIVE_CONST:
			return FIELD_FLAG_CONST;
		case DIRECTIVE_ANY_INT:
			return FIELD_FLAG_ANY_INT;
		case DIRECTIVE_SUBTYPE:
			return FIELD_FLAG_SUBTYPE;
		case DIRECTIVE_BY_PTR:
			return FIELD_FLAG_BY_PTR;
		default:
			ERROR("Unsupported directive in field");
		}
	}
	return FIELD_FLAG_INVALID;
}

static FieldFlag parse_field_flags(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	FieldFlag flags = FIELD_FLAG_INVALID;
	for (;;) {
		const FieldFlag flag = parse_field_flag(parser);
		if (flag == FIELD_FLAG_INVALID) {
			break;
		}
		if (flags & flag) {
			ERROR("Duplicate in field list");
		}
		flags |= flag;
		advancep(parser);
	}
	TRACE_LEAVE();
	return flags;
}

static Node *parse_result_list(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	UNIMPLEMENTED("parse_result_list");
	TRACE_LEAVE();
	return 0;
}

static Node *parse_results(Parser *parser, Bool *diverging) {
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

	const Sint32 depth = parser->expression_depth;

	Node *list = 0;
	if (is_operator(parser->this_token, OPERATOR_OPENPAREN)) {
		// Multiple results '(...)'.
		expect_operator(parser, OPERATOR_OPENPAREN);
		list = parse_result_list(parser);
		expect_operator(parser, OPERATOR_CLOSEPAREN);
	} else {
		// Single result
		Node *type = parse_type(parser);
		// NOTE(dweiler): Should Field and FieldList be separate?
		Array(Node*) fields = 0;
		array_push(fields, type);
		list = tree_new_field_list(parser->tree, fields);
	}
	parser->expression_depth = depth;

	TRACE_LEAVE();
	ASSERT(list);
	return list;
}

static Node *parse_parameter_list(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Node*) fields = 0;
	Array(Node*) names = 0;
	while (!is_operator(parser->this_token, OPERATOR_CLOSEPAREN) &&
	       !is_kind(parser->this_token, KIND_EOF))
	{
		Uint64 flags = parse_field_flags(parser);
		(void)flags;
		Node *identifier = parse_type_or_identifier(parser);
		array_push(names, identifier);
		if (is_operator(parser->this_token, OPERATOR_COLON)) {
			expect_operator(parser, OPERATOR_COLON);
			Node *type = parse_type_or_identifier(parser);
			const Size n_names = array_size(names);
			for (Size i = 0; i < n_names; i++) {
				Node *field = tree_new_field(parser->tree, names[i], type);
				array_push(fields, field);
			}
			array_clear(names);
		}
		if (!accepted_separator(parser)) {
			break;
		}
	}
	if (array_size(names) != 0) {
		ERROR("Expected type");
	}
	array_free(names);
	Node *node = tree_new_field_list(parser->tree, fields);
	TRACE_LEAVE();
	return node;
}

static Node *parse_procedure_type(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	CallingConvention convention = CCONV_INVALID;
	if (is_literal(parser->this_token, LITERAL_STRING)) {
		const Token token = expect_literal(parser, LITERAL_STRING);
		const String string = token.string;
		convention = string_to_calling_convention(string_unquote(string, "\"`"));
		if (convention == CCONV_INVALID) {
			ERROR("Unknown calling convention '%.*s'", SFMT(string));
			TRACE_LEAVE();
			return 0;
		}
	} else {
		convention = CCONV_CDECL;
	}

	expect_operator(parser, OPERATOR_OPENPAREN);
	Node *params = parse_parameter_list(parser);
	expect_operator(parser, OPERATOR_CLOSEPAREN);

	Uint64 flags = 0;
	Bool diverging = false;
	Node *results = parse_results(parser, &diverging);
	if (diverging) {
		flags |= PROC_FLAG_DIVERGING;
	}

	Node *type = tree_new_procedure_type(parser->tree, params, results, flags, convention);
	TRACE_LEAVE();
	return type;
}

static Node *parse_body(Parser *parser);

static Node *parse_procedure(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	Node *type = parse_procedure_type(parser);

	advance_possible_newline_within(parser);

	if (is_keyword(parser->this_token, KEYWORD_WHERE)) {
		expect_keyword(parser, KEYWORD_WHERE);
		UNIMPLEMENTED("where specialization for procedures");
	}

	// We start each procedure with these flags.
	ProcedureFlag flags = CAST(ProcedureFlag, 0);
	flags |= PROC_FLAG_BOUNDS_CHECK;
	flags |= PROC_FLAG_TYPE_ASSERT;

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
			ERROR("Cannot use directive '%.*s' on a procedure", SFMT(name));
		}
	}

	advance_possible_newline_within(parser);

	if (accepted_kind(parser, KIND_UNDEFINED)) {
		UNIMPLEMENTED("Undefined procedure literal");
	} else if (is_kind(parser->this_token, KIND_LBRACE)) {
		Node *last_procedure = parser->this_procedure;
		parser->this_procedure = type;
		Node *body = parse_body(parser);
		parser->this_procedure = last_procedure;
		ASSERT(type);
		Node *procedure = tree_new_procedure(parser->tree, flags, type, body);
		TRACE_LEAVE();
		return procedure;
	}

	ICE("Unexpected situation in procedure parsing");
	TRACE_LEAVE();
	return type;
}

static Node *parse_procedure_group(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	expect_kind(parser, KIND_LBRACE);
	Array(Node*) procedures = 0;
	while (!is_kind(parser->this_token, KIND_RBRACE) &&
	       !is_kind(parser->this_token, KIND_EOF))
	{
		Node *element = parse_expression(parser, false);
		if (!element) {
			break;
		}
		array_push(procedures, element);
		if (!accepted_separator(parser)) {
			break;
		}
	}
	if (array_size(procedures) == 0) {
		ERROR("Expected at least one procedure in procedure group");
	}
	expect_kind(parser, KIND_RBRACE);
	Node *group = tree_new_procedure_group(parser->tree, procedures);
	TRACE_LEAVE();
	return group;
}

static Node *parse_call_expression(Parser *parser, Node *operand);

static Node *parse_statement(Parser *parser);
static Node *parse_unary_expression(Parser *parser, Bool lhs);
static Node *parse_directive_for_operand(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	Context *context = parser->context;

	const Token token = expect_kind(parser, KIND_DIRECTIVE);
	Node *node = 0;
	switch (token.as_directive) {
	case DIRECTIVE_TYPE:
		FALLTHROUGH();
	case DIRECTIVE_SIMD:
		FALLTHROUGH();
	case DIRECTIVE_SOA:
		FALLTHROUGH();
	case DIRECTIVE_PARTIAL:
		FALLTHROUGH();
	case DIRECTIVE_SPARSE:
		node = parse_type(parser);
		break;
	case DIRECTIVE_RELATIVE:
		node = parse_type(parser);
		break;
	case DIRECTIVE_BOUNDS_CHECK:
		node = parse_expression(parser, lhs);
		ASSERT(node->kind == NODE_STATEMENT);
		break;
	case DIRECTIVE_NO_BOUNDS_CHECK:
		node = parse_expression(parser, lhs);
		ASSERT(node->kind == NODE_STATEMENT);
		break;
	case DIRECTIVE_TYPE_ASSERT:
		node = parse_expression(parser, lhs);
		ASSERT(node->kind == NODE_STATEMENT);
		break;
	case DIRECTIVE_NO_TYPE_ASSERT:
		node = parse_expression(parser, lhs);
		ASSERT(node->kind == NODE_STATEMENT);
		break;
	case DIRECTIVE_FORCE_INLINE:
		FALLTHROUGH();
	case DIRECTIVE_FORCE_NO_INLINE:
		node = parse_unary_expression(parser, false);
		if (node->kind != NODE_PROCEDURE && !node_is_expression(node, EXPRESSION_CALL)) {
			ERROR("Directive '%.*s' must be followed by a procedure literal or call expression");
		}
		switch (token.as_directive) {
		case DIRECTIVE_FORCE_INLINE:
			node->procedure.flags |= PROC_FLAG_FORCE_INLINE;
			break;
		case DIRECTIVE_FORCE_NO_INLINE:
			node->procedure.flags &= ~PROC_FLAG_FORCE_INLINE;
			break;
		default:
			break;
		}
		return node;
	default:
		{
			const String directive = directive_to_string(token.as_directive);
			ERROR("Cannot use directive '%.*s' on a expression", SFMT(directive));
		}
	}

	TRACE_LEAVE();
	return 0;
}

static Node *parse_directive_for_statement(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	const Token token = expect_kind(parser, KIND_DIRECTIVE);
	Node *statement = 0;
	switch (token.as_directive) {
	case DIRECTIVE_BOUNDS_CHECK:
		statement = parse_statement(parser);
		break;
	case DIRECTIVE_NO_BOUNDS_CHECK:
		statement = parse_statement(parser);
		break;
	case DIRECTIVE_TYPE_ASSERT:
		statement = parse_statement(parser);
		break;
	case DIRECTIVE_NO_TYPE_ASSERT:
		statement = parse_statement(parser);
		break;
	case DIRECTIVE_PARTIAL:
		UNIMPLEMENTED("#partial");
	case DIRECTIVE_ASSERT:
		FALLTHROUGH();
	case DIRECTIVE_PANIC:
		statement = tree_new_directive(parser->tree, token.as_directive);
		statement = tree_new_expression_statement(parser->tree, parse_call_expression(parser, statement));
		expect_semicolon(parser);
		break;
	case DIRECTIVE_UNROLL:
		UNIMPLEMENTED("#unroll");
	default:
		{
			const String directive = directive_to_string(token.as_directive);
			ERROR("Unsupported directive '%.*s' in statement", SFMT(directive));
		}
		break;
	}

	TRACE_LEAVE();
	return statement;
}

static Node *parse_operand(Parser *parser, Bool lhs) {
	TRACE_ENTER();
	Context *context = parser->context;
	const Token token = parser->this_token;
	Node *node = 0;
	switch (token.kind) {
	case KIND_IDENTIFIER:
		node = parse_identifier(parser);
		TRACE_LEAVE();
		return node;
	case KIND_LITERAL:
		switch (token.as_literal) {
		case LITERAL_INTEGER:
			FALLTHROUGH();
		case LITERAL_FLOAT:
			FALLTHROUGH();
		case LITERAL_IMAGINARY:
			FALLTHROUGH();
		case LITERAL_RUNE:
			FALLTHROUGH();
		case LITERAL_STRING:
			{
				const Token token = advancep(parser);
				node = tree_new_literal_value(parser->tree, token.as_literal, token.string);
				TRACE_LEAVE();
				return node;
			}
		case LITERAL_COUNT:
			UNREACHABLE();
		}
		break;
	case KIND_LBRACE:
		if (!lhs) {
			node = parse_literal_value(parser, 0);
			TRACE_LEAVE();
			return node;
		}
		break;
	case KIND_DIRECTIVE:
		node = parse_directive_for_operand(parser, lhs);
		TRACE_LEAVE();
		return node;
	case KIND_KEYWORD:
		switch (token.as_keyword) {
		case KEYWORD_DISTINCT:
			UNIMPLEMENTED("distinct");
		case KEYWORD_PROC:
			expect_keyword(parser, KEYWORD_PROC);
			if (is_kind(parser->this_token, KIND_LBRACE)) {
				node = parse_procedure_group(parser);
			} else {
				node = parse_procedure(parser);
			}
			TRACE_LEAVE();
			return node;
		case KEYWORD_TYPEID:
			expect_keyword(parser, KEYWORD_TYPEID);
			node = tree_new_typeid_type(parser->tree);
			TRACE_LEAVE();
			return node;
		case KEYWORD_MAP:
			UNIMPLEMENTED("map");
		case KEYWORD_MATRIX:
			UNIMPLEMENTED("matrix");
		case KEYWORD_STRUCT:
			UNIMPLEMENTED("struct");
		case KEYWORD_UNION:
			UNIMPLEMENTED("union");
		case KEYWORD_ENUM:
			UNIMPLEMENTED("enum");
		case KEYWORD_BIT_SET:
			UNIMPLEMENTED("bit_set");
		case KEYWORD_ASM:
			UNIMPLEMENTED("asm");
		case KEYWORD_CONTEXT:
			UNIMPLEMENTED("context");
		default:
			ICE("Unexpected keyword in operand");
		}
		UNIMPLEMENTED("Keyword");
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_OPENPAREN:
			{
				expect_operator(parser, OPERATOR_OPENPAREN);
				if (is_operator(parser->last_token, OPERATOR_CLOSEPAREN)) {
					ERROR("Empty parenthesized expression");
				}
				const Sint32 depth = parser->expression_depth;
				parser->expression_depth = (depth > 0 ? depth : 0) + 1;
				node = parse_expression(parser, false);
				parser->expression_depth = depth;
				expect_operator(parser, OPERATOR_CLOSEPAREN);
				TRACE_LEAVE();
				return node;
			}
		case OPERATOR_POINTER:
			{
				expect_operator(parser, OPERATOR_POINTER);
				Node *type = parse_type(parser);
				node = tree_new_pointer_type(parser->tree, type);
				TRACE_LEAVE();
				return node;
			}
		case OPERATOR_OPENBRACKET:
			{
				expect_operator(parser, OPERATOR_OPENBRACKET);
				Node *count = 0;
				if (is_operator(parser->this_token, OPERATOR_POINTER)) {
					expect_operator(parser, OPERATOR_POINTER);
					expect_operator(parser, OPERATOR_CLOSEBRACKET);
					Node *type = parse_type(parser);
					node = tree_new_multi_pointer_type(parser->tree, type);
					TRACE_LEAVE();
					return node;
				} else if (is_operator(parser->this_token, OPERATOR_QUESTION)) {
					expect_operator(parser, OPERATOR_QUESTION);
					count = tree_new_unary_expression(parser->tree, OPERATOR_QUESTION, 0);
				} else if (accepted_keyword(parser, KEYWORD_DYNAMIC)) {
					expect_operator(parser, OPERATOR_CLOSEBRACKET);
					Node *type = parse_type(parser);
					node = tree_new_dynamic_array_type(parser->tree, type);
					TRACE_LEAVE();
					return node;
				} else if (!is_operator(parser->this_token, OPERATOR_CLOSEBRACKET)) {
					parser->expression_depth++;
					count = parse_expression(parser, false);
					parser->expression_depth--;
				}
				expect_operator(parser, OPERATOR_CLOSEBRACKET);
				Node *type = parse_type(parser);
				if (count) {
					node = tree_new_array_type(parser->tree, count, type);
				} else {
					node = tree_new_slice_type(parser->tree, type);
				}
				TRACE_LEAVE();
				return node;
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

static Node *parse_value(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	if (is_kind(parser->this_token, KIND_LBRACE)) {
		UNIMPLEMENTED("Literal value");
	}
	Node *node = parse_expression(parser, false);
	TRACE_LEAVE();
	return node;
}

static Node *parse_call_expression(Parser *parser, Node *operand) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Node*) arguments = 0;
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	expect_operator(parser, OPERATOR_OPENPAREN);
	while (!is_operator(parser->this_token, OPERATOR_CLOSEPAREN) && !is_kind(parser->this_token, KIND_EOF)) {
		if (is_operator(parser->this_token, OPERATOR_COMMA)) {
			ERROR("Expected an expression COMMA");
		} else if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			ERROR("Expected an expression EQ");
		}
	
		Bool has_ellipsis = false;
		if (is_operator(parser->this_token, OPERATOR_ELLIPSIS)) {
			has_ellipsis = true;
			expect_operator(parser, OPERATOR_ELLIPSIS);
		}

		Node *argument = parse_expression(parser, false);
		if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			expect_assignment(parser, ASSIGNMENT_EQ);
			if (has_ellipsis) {
				ERROR("Cannot apply '..' to field");
			}

			Node *value = parse_value(parser);
			argument = tree_new_value(parser->tree, argument, value);
		}
		array_push(arguments, argument);

		if (accepted_operator(parser, OPERATOR_COMMA)) {
			// ...
		}
	}
	parser->expression_depth = depth;
	expect_operator(parser, OPERATOR_CLOSEPAREN);
	Node *node = tree_new_call_expression(parser->tree, operand, arguments);
	// TODO(dweiler): Handle selector call expressions.
	TRACE_LEAVE();
	return node;
}

static Array(Node*) parse_element_list(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Node*) elements = 0;
	while (!is_kind(parser->this_token, KIND_RBRACE) && !is_kind(parser->this_token, KIND_EOF)) {
		Node *element = parse_value(parser);
		if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			expect_assignment(parser, ASSIGNMENT_EQ);
			Node *value = parse_value(parser);
			element = tree_new_value(parser->tree, element, value);
		}
		ASSERT(element);
		array_push(elements, element);
		if (!accepted_separator(parser)) {
			break;
		}
	}
	TRACE_LEAVE();
	return elements;
}

static Token expect_closing(Parser *parser, Kind kind) {
	const Token token = parser->this_token;
	if (!is_kind(token, kind) && is_kind(token, KIND_SEMICOLON) &&
		(string_compare(token.string, SCLIT("\n"))))
	{
		advancep(parser);
	}
	return expect_kind(parser, kind);
}

static Node *parse_literal_value(Parser *parser, Node *type) {
	TRACE_ENTER();
	Array(Node*) elements = 0;
	expect_kind(parser, KIND_LBRACE);
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	if (!is_kind(parser->this_token, KIND_RBRACE)) {
		elements = parse_element_list(parser);
	}
	parser->expression_depth = depth;
	expect_closing(parser, KIND_RBRACE);
	Node *node = tree_new_compound_literal(parser->tree, type, elements);
	TRACE_LEAVE();
	return node;
}

static Node *parse_atom_expression(Parser *parser, Node *operand, Bool lhs) {
	TRACE_ENTER();

	Context *context = parser->context;

	if (!operand) {
		// if (allow_type) return 0
		// ERROR("Expected an operand");
		TRACE_LEAVE();
		return 0;
	}
	// ERROR("Unimplemented atom expression");
	Node *node = 0;
	Node *type = 0;
	for (;;) {
		Token token = parser->this_token;
		switch (token.kind) {
		case KIND_OPERATOR:
			switch (token.as_operator) {
			case OPERATOR_OPENPAREN:
				operand = parse_call_expression(parser, operand);
				break;

			// .x
			// .?
			// .(T)
			case OPERATOR_PERIOD:
				advancep(parser);
				switch (parser->this_token.kind) {
				case KIND_IDENTIFIER:
					node = parse_identifier(parser);
					operand = tree_new_selector_expression(parser->tree, operand, node);
					break;
				case KIND_OPERATOR:
					switch (parser->this_token.as_operator) {
					case OPERATOR_OPENPAREN:
						expect_operator(parser, OPERATOR_OPENPAREN);
						type = parse_type(parser);
						expect_operator(parser, OPERATOR_CLOSEPAREN);
						operand = tree_new_assertion_expression(parser->tree, operand, type);
						break;
					case OPERATOR_QUESTION:
						expect_operator(parser, OPERATOR_QUESTION);
						type = tree_new_unary_expression(parser->tree, OPERATOR_QUESTION, 0);
						operand = tree_new_assertion_expression(parser->tree, operand, type);
						break;
					default:
						break;
					}
					break;
				default:
					ERROR("Expected selector");
					advancep(parser);
					break;
				}
				break;
			case OPERATOR_ARROW:
				advancep(parser);
				node = parse_identifier(parser);
				operand = tree_new_selector_expression(parser->tree, operand, node);
				break;
			case OPERATOR_OPENBRACKET:
				UNIMPLEMENTED("Bracket");
			case OPERATOR_POINTER:
				expect_operator(parser, OPERATOR_POINTER);
				operand = tree_new_dereference_expression(parser->tree, operand);
				break;
			case OPERATOR_OR_RETURN:
				UNIMPLEMENTED("or_return");
			default:
				TRACE_LEAVE();
				return operand;
			}
			break;
		case KIND_LBRACE:
			if (!lhs && node_is_literal(operand) && parser->expression_depth >= 0) {
				operand = parse_literal_value(parser, operand);
			} else {
				TRACE_LEAVE();
				return operand;
			}
			break;
		default:
			TRACE_LEAVE();
			return operand;
		}

		// No longer left-hand side once one iteration through the loop.
		lhs = false;
	}

	TRACE_LEAVE();
	return operand;
}

static Node *parse_unary_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();
	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_TRANSMUTE:
			FALLTHROUGH();
		case OPERATOR_CAST:
			{
				advancep(parser);
				expect_operator(parser, OPERATOR_OPENPAREN);
				Node *type = parse_type(parser);
				expect_operator(parser, OPERATOR_CLOSEPAREN);
				Node *expr = parse_unary_expression(parser, lhs);
				Node *node = tree_new_cast_expression(parser->tree, type, expr);
				TRACE_LEAVE();
				return node;
			}
			break;
		case OPERATOR_AUTO_CAST:
			{
				advancep(parser);
				Node *expr = parse_unary_expression(parser, lhs);
				Node *node = tree_new_cast_expression(parser->tree, 0, expr);
				TRACE_LEAVE();
				return node;
			}
			break;
		case OPERATOR_ADD:
			FALLTHROUGH();
		case OPERATOR_SUB:
			FALLTHROUGH();
		case OPERATOR_XOR:
			FALLTHROUGH();
		case OPERATOR_AND:
			FALLTHROUGH();
		case OPERATOR_NOT:
			{
				const Token token = advancep(parser);
				Node *expr = parse_unary_expression(parser, lhs);
				Node *node = tree_new_unary_expression(parser->tree, token.as_operator, expr);
				TRACE_LEAVE();
				return node;
			}
			break;
		case OPERATOR_PERIOD:
			{
				expect_operator(parser, OPERATOR_PERIOD);
				Node *ident = parse_identifier(parser);
				Node *node = tree_new_selector_expression(parser->tree, 0, ident);
				TRACE_LEAVE();
				return node;
			}
		default:
			break;
		}
		break;
	default:
		break;
	}

	Node *operand = parse_operand(parser, lhs);
	Node *node = parse_atom_expression(parser, operand, lhs);
	TRACE_LEAVE();
	return node;
}

static Node *parse_binary_expression(Parser *parser, Bool lhs, Sint32 prec) {
	TRACE_ENTER();

	Context *context = parser->context;

	Node *expr = parse_unary_expression(parser, lhs);

	// Simple operator precedence climbing.

	#define OPERATOR(ident, match, prec) (prec),
	static const int PRECEDENCE[] = {
		#include "lexemes.h"
		0,
	};

	for (;;) {
		const Token token = parser->this_token;
		Sint32 op_prec = 0;
		if (is_kind(token, KIND_OPERATOR)) {
			if ((op_prec = PRECEDENCE[token.as_operator]) < prec) {
				break;
			}
		} 
		const Token last = parser->last_token;
		if (is_keyword(token, KEYWORD_IF) || is_keyword(token, KEYWORD_WHEN)) {
			// Needs to be on the same line to be a ternary usage.
			if (last.location.line < token.location.line) {
				return expr;
			}
			ERROR("Unimplemented ternary if or when expression");
			break;
		} else if (!is_kind(token, KIND_OPERATOR)) {
			break;
		}

		advancep(parser);

		// Expect operator or if/when keywords.
		Node *rhs = parse_binary_expression(parser, false, op_prec + 1);
		if (!rhs) {
			ERROR("Expected expression on the right-hand side");
		}

		if (is_operator(token, OPERATOR_OR_ELSE)) {
			UNIMPLEMENTED("or_else");
		} else {
			expr = tree_new_binary_expression(parser->tree, token.as_operator, expr, rhs);
		}

		lhs = false;
	}

	TRACE_LEAVE();
	return expr;
}

static FORCE_INLINE Node *parse_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();
	Node *node = parse_binary_expression(parser, lhs, 1);
	TRACE_LEAVE();
	return node;
}

static Array(Node*) parse_expression_list(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Node*) list = 0;
	for (;;) {
		Node *expr = parse_expression(parser, lhs);
		if (expr) {
			array_push(list, expr);
		}
		if (is_kind(parser->this_token, KIND_EOF) || !is_operator(parser->this_token, OPERATOR_COMMA)) {
			break;
		}
		advancep(parser);
	}
	TRACE_LEAVE();
	return list;
}

static FORCE_INLINE Array(Node*) parse_lhs_expression_list(Parser *parser) {
	TRACE_ENTER();
	Array(Node*) result = parse_expression_list(parser, true);
	TRACE_LEAVE();
	return result;
}

static FORCE_INLINE Array(Node*) parse_rhs_expression_list(Parser *parser) {
	TRACE_ENTER();
	Array(Node*) result = parse_expression_list(parser, false);
	TRACE_LEAVE();
	return result;
}

static Node *parse_statement(Parser *parser);

static Array(Node*) parse_statement_list(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Node*) statements = 0;

	// Stop parsing the statement when we encounter one of:
	//
	//		"case"
	//		"}"
	//		EOF
	while (!is_keyword(parser->this_token, KEYWORD_CASE) &&
	       !is_kind(parser->this_token, KIND_RBRACE) &&
	       !is_kind(parser->this_token, KIND_EOF))
	{
		Node *statement = parse_statement(parser);
		if (statement && statement->statement.kind != STATEMENT_EMPTY) {
			array_push(statements, statement);
			// TODO(dweiler): More robust.
		}
	}

	TRACE_LEAVE();
	return statements;
}

static Node *parse_body(Parser *parser) {
	TRACE_ENTER();
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	expect_kind(parser, KIND_LBRACE);
	Array(Node*) statements = parse_statement_list(parser);
	expect_kind(parser, KIND_RBRACE);
	parser->expression_depth = depth;
	// NOTE(dweiler): We should be inheriting the flags from the parent block
	// but we don't have access to that here.
	BlockFlag flags = CAST(BlockFlag, 0);
	// Convert the procedure flags to block flags.
	const ProcedureFlag proc_flags = parser->this_procedure->procedure.flags;
	if (proc_flags & PROC_FLAG_BOUNDS_CHECK) flags |= BLOCK_FLAG_BOUNDS_CHECK;
	if (proc_flags & PROC_FLAG_TYPE_ASSERT)  flags |= BLOCK_FLAG_TYPE_ASSERT;
	Node *node = tree_new_block_statement(parser->tree, flags, statements);
	TRACE_LEAVE();
	return node;
}

static Node *parse_block_statement(Parser *parser, Bool when) {
	// The block statement may be part of a compile-time when statement.
	TRACE_ENTER();

	Context *context = parser->context;

	advance_possible_newline(parser);

	if (when) {
		UNIMPLEMENTED("when block");
	}

	if (!when && !parser->this_procedure) {
		ERROR("Cannot use block statement at file scope");
	}

	Node *body = parse_body(parser);

	TRACE_LEAVE();

	return body;
}

static Node *parse_declaration_statement(Parser *parser, Array(Node*) names) {
	// ':'
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Node*) values = 0;
	Node *type = parse_type_or_identifier(parser);
	const Token token = parser->this_token;
	Bool constant = false;
	// Check for '=' (:=)
	// Check for ':' (::)
	if (is_assignment(token, ASSIGNMENT_EQ) || is_operator(token, OPERATOR_COLON)) {
		const Token seperator = advancep(parser);
		constant = is_operator(seperator, OPERATOR_COLON);
		values = parse_rhs_expression_list(parser);
		const Size n_names = array_size(names);
		const Size n_values = array_size(values);
		if (n_values != n_names) {
			ERROR("Expected %d values on the right-hand side of this declaration", CAST(Sint32, n_names));
		}
	}

	if (constant && array_size(values) == 0) {
		ERROR("Expected constant initializer");
	}

	if (parser->expression_depth >= 0) {
		expect_semicolon(parser);
	}

	Node *node = tree_new_declaration_statement(parser->tree, type, names, values);
	TRACE_LEAVE();
	return node;
}

static Node *parse_simple_statement(Parser* parser, Bool allow_in) {
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Node*) lhs = parse_lhs_expression_list(parser);
	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_ASSIGNMENT:
		{
			if (!parser->this_procedure) {
				ERROR("Cannot use assignment statement at file scope");
			}
			advancep(parser);
			Array(Node*) rhs = parse_rhs_expression_list(parser);
			if (array_size(rhs) == 0) {
				ERROR("Missing right-hand side in assignment");
				return 0;
			}
			Node *node = tree_new_assignment_statement(parser->tree, token.as_assignment, lhs, rhs);
			TRACE_LEAVE();
			return node;
		}
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_IN:
			if (allow_in) {
				// [lhs] in <Expression>
				accepted_operator(parser, OPERATOR_IN);
				Node *rhs = parse_expression(parser, true);
				Node *node = tree_new_in_expression(parser->tree, lhs, rhs);
				TRACE_LEAVE();
				return node;
			}
			break;
		case OPERATOR_COLON:
			{
				expect_operator(parser, OPERATOR_COLON);
				// TODO(dweiler): Label check.
				Node *node = parse_declaration_statement(parser, lhs);
				TRACE_LEAVE();
				return node;
			}
		default:
			{
				const String string = operator_to_string(token.as_operator);
				ICE("Unexpected operator '%.*s' in statement", SFMT(string));
			}
		}
		break;
	default:
		// const String string = token_to_string(token);
		// ERROR("Unexpected '%.*s'",
		// 	CAST(int,          string.size),
		// 	CAST(const char *, string.data));
		break;
	}

	if (array_size(lhs) == 0 || array_size(lhs) > 1) {
		ERROR("Expected one expression on the left-hand side");
		return 0;
	}

	Node *lhs0 = lhs[0];

	array_free(lhs);

	Node *node = tree_new_expression_statement(parser->tree, lhs0);

	TRACE_LEAVE();
	return node;
}

static Node *parse_import_declaration(Parser *parser) {
	TRACE_ENTER();

	expect_keyword(parser, KEYWORD_IMPORT);

	Token name = TOKEN_NIL;
	switch (parser->this_token.kind) {
	case KIND_IDENTIFIER:
		name = advancep(parser);
		break;
	default:
		break;
	}

	const Token path = expect_literal(parser, LITERAL_STRING);

	expect_semicolon(parser);
	Node *result = tree_new_import_statement(parser->tree, string_unquote(name.string, "\""), string_unquote(path.string, "\""));
	TRACE_LEAVE();
	return result;
}

static Node *convert_statement_to_body(Parser *parser, Node *statement) {
	ASSERT(statement->kind == NODE_STATEMENT);

	Context *context = parser->context;

	const StatementKind kind = statement->statement.kind;
	if (kind == STATEMENT_BLOCK || kind == STATEMENT_EMPTY) {
		ERROR("Expected a regular statement");
	}
	Array(Node*) statements = 0;
	array_push(statements, statement);
	return tree_new_block_statement(parser->tree, CAST(BlockFlag, 0), statements);
}

static Node *convert_statement_to_expression(Parser *parser, Node *statement) {
	Context *context = parser->context;

	if (!statement) {
		return 0;
	}
	const StatementKind kind = statement->statement.kind;
	if (kind == STATEMENT_EXPRESSION) {
		return statement->statement.expression.expression;
	}
	ERROR("Expected a statement");
	return 0;
}

static Node *parse_do_body(Parser *parser) {
	TRACE_ENTER();
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	Node *statement = parse_statement(parser);
	Node *body = convert_statement_to_body(parser, statement);
	parser->expression_depth = depth;
	TRACE_LEAVE();
	return body;
}

static Bool accepted_control_statement_separator(Parser *parser) {
	const Token token = peekp(parser);
	if (!is_kind(token, KIND_LBRACE)) {
		return accepted_kind(parser, KIND_SEMICOLON);
	}
	if (string_compare(parser->this_token.string, SCLIT("\n"))) {
		return accepted_kind(parser, KIND_SEMICOLON);
	}
	return false;
}

static Node *parse_if_statement(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	expect_keyword(parser, KEYWORD_IF);

	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = -1;

	Node *init = parse_simple_statement(parser, false);
	Node *cond = 0;
	if (accepted_control_statement_separator(parser)) {
		cond = parse_expression(parser, false);
	} else {
		cond = convert_statement_to_expression(parser, init);
		init = 0;
	}

	parser->expression_depth = depth;

	if (!cond) {
		ERROR("Expected condition in if statement");
	}

	Node *body = 0;
	if (accepted_keyword(parser, KEYWORD_DO)) {
		body = parse_do_body(parser);
	} else {
		body = parse_block_statement(parser, false);
	}

	advance_possible_newline(parser);

	Node *elif = 0;
	if (is_keyword(parser->this_token, KEYWORD_ELSE)) {
		expect_keyword(parser, KEYWORD_ELSE);
		if (is_keyword(parser->this_token, KEYWORD_IF)) {
			elif = parse_if_statement(parser);
		} else if (is_kind(parser->this_token, KIND_LBRACE)) {
			elif = parse_block_statement(parser, false);
		} else if (is_keyword(parser->this_token, KEYWORD_DO)) {
			expect_keyword(parser, KEYWORD_DO);
			elif = parse_do_body(parser);
		} else {
			ERROR("Expected block on 'else' statement");
		}
	}

	Node *node = tree_new_if_statement(parser->tree, init, cond, body, elif);
	TRACE_LEAVE();
	return node;
}

static Node *parse_for_statement(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	Node *init = 0;
	Node *cond = 0;
	Node *body = 0;
	Node *post = 0;

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
			Node *rhs = parse_expression(parser, false);
			if (accepted_keyword(parser, KEYWORD_DO)) {
				body = parse_do_body(parser);
			} else {
				body = parse_block_statement(parser, false);
			}
			parser->expression_depth = depth;
			cond = tree_new_in_expression(parser->tree, 0, rhs);
			Node *node = tree_new_for_statement(parser->tree, 0, cond, body, 0);
			TRACE_LEAVE();
			return node;
		}

		if (!is_kind(token, KIND_SEMICOLON)) {
			cond = parse_simple_statement(parser, true);
			range = node_is_expression(cond, EXPRESSION_IN);
			// for [...] in <Expression>
		}

		if (!range && accepted_control_statement_separator(parser)) {
			init = cond;
			cond = 0;
		
			const Token token = parser->this_token;
		
			if (is_kind(token, KIND_LBRACE) || is_keyword(token, KEYWORD_DO)) {
				ERROR("Expected ';'");
			} else {
				if (!is_kind(token, KIND_SEMICOLON)) {
					// for [...] <Statement>
					cond = parse_simple_statement(parser, false);
				}
				if (!is_kind(parser->this_token, KIND_SEMICOLON)) {
					ERROR("Expected ';'");
				} else {
					expect_kind(parser, KIND_SEMICOLON);
				}
				if (!is_kind(parser->this_token, KIND_LBRACE) && !is_keyword(parser->this_token, KEYWORD_DO)) {
					// for [...] [...] <Statement>
					post = parse_simple_statement(parser, false);
				}
			}
		}
		parser->expression_depth = depth;
	}

	if (accepted_keyword(parser, KEYWORD_DO)) {
		// for [...] do <Statement>
		body = parse_do_body(parser);
	} else {
		// for [...] <BlockStatement>
		body = parse_block_statement(parser, false);
	}

	if (!range) {
		cond = convert_statement_to_expression(parser, cond);
	}

	cond = tree_new_for_statement(parser->tree, init, cond, body, post);

	TRACE_LEAVE();

	return cond;
}

static Node *parse_defer_statement(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	expect_keyword(parser, KEYWORD_DEFER);
	Node *node = parse_statement(parser);
	switch (node->statement.kind) {
	case STATEMENT_EMPTY:
		ERROR("Empty statement in defer");
	case STATEMENT_DEFER:
		ERROR("Cannot defer a defer statement");
	case STATEMENT_RETURN:
		ERROR("Cannot defer a return statement");
	default:
		break;
	}
	node = tree_new_defer_statement(parser->tree, node);
	TRACE_LEAVE();
	return node;
}

static Node *parse_return_statement(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	expect_keyword(parser, KEYWORD_RETURN);

	if (parser->expression_depth > 0) {
		ERROR("Cannot use return statement within expression");
	}

	Array(Node*) results = 0;
	while (!is_kind(parser->this_token, KIND_SEMICOLON) &&
	       !is_kind(parser->this_token, KIND_RBRACE))
	{
		Node *arg = parse_expression(parser, false);
		array_push(results, arg);
		if (!is_operator(parser->this_token, OPERATOR_COMMA) || is_kind(parser->this_token, KIND_EOF)) {
			break;
		}
		advancep(parser);
	}

	expect_semicolon(parser);

	Node *result = tree_new_return_statement(parser->tree, results);

	TRACE_LEAVE();
	return result;
}

static Node *parse_statement(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	Node *node = 0;
	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_EOF:
		ICE("Unexpected EOF");
	case KIND_LITERAL:
		switch (token.as_literal) {
		case LITERAL_INTEGER:
			FALLTHROUGH();
		case LITERAL_FLOAT:
			FALLTHROUGH();
		case LITERAL_IMAGINARY:
			FALLTHROUGH();
		case LITERAL_RUNE:
			FALLTHROUGH();
		case LITERAL_STRING:
			node = parse_simple_statement(parser, false);
			expect_semicolon(parser);
			TRACE_LEAVE();
			return node;
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
			node = parse_simple_statement(parser, false);
			expect_semicolon(parser);
			TRACE_LEAVE();
			return node;
		case KEYWORD_FOREIGN:
			UNIMPLEMENTED("Foreign declaration");
		case KEYWORD_IMPORT:
			node = parse_import_declaration(parser);
			TRACE_LEAVE();
			return node;
		case KEYWORD_IF:
			node = parse_if_statement(parser);
			TRACE_LEAVE();
			return node;
		case KEYWORD_WHEN:
			UNIMPLEMENTED("When statement");
		case KEYWORD_FOR:
			node = parse_for_statement(parser);
			TRACE_LEAVE();
			return node;
		case KEYWORD_SWITCH:
			UNIMPLEMENTED("Switch statement");
		case KEYWORD_DEFER:
			node = parse_defer_statement(parser);
			TRACE_LEAVE();
			return node;
		case KEYWORD_RETURN:
			node = parse_return_statement(parser);
			TRACE_LEAVE();
			return node;
		case KEYWORD_BREAK:
			FALLTHROUGH();
		case KEYWORD_CONTINUE:
			FALLTHROUGH();
		case KEYWORD_FALLTHROUGH:
			UNIMPLEMENTED("Break/continue/fallthrough statement");
		case KEYWORD_USING:
			UNIMPLEMENTED("Using statement");
		default:
			ICE("Unexpected keyword in statement");
		}
		break;
	case KIND_IDENTIFIER:
		node = parse_simple_statement(parser, false);
		expect_semicolon(parser);
		TRACE_LEAVE();
		return node;
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_OPENPAREN:
			FALLTHROUGH();
		case OPERATOR_POINTER:
			FALLTHROUGH();
		case OPERATOR_ADD:
			FALLTHROUGH();
		case OPERATOR_SUB:
			FALLTHROUGH();
		case OPERATOR_XOR:
			FALLTHROUGH();
		case OPERATOR_NOT:
			FALLTHROUGH();
		case OPERATOR_AND:
			node = parse_simple_statement(parser, false);
			expect_semicolon(parser);
			TRACE_LEAVE();
			return node;
		default:
			{
				const String string = operator_to_string(token.as_operator);
				ICE("Unexpected operator '%.*s' in statement", SFMT(string));
			}
			break;
		}
		break;
	case KIND_ATTRIBUTE:
		UNIMPLEMENTED("Attribute");
	case KIND_DIRECTIVE:
		node = parse_directive_for_statement(parser);
		TRACE_LEAVE();
		return node;
	case KIND_LBRACE:
		node = parse_block_statement(parser, false);
		TRACE_LEAVE();
		return node;
	case KIND_SEMICOLON:
		node = tree_new_empty_statement(parser->tree);
		expect_semicolon(parser);
		TRACE_LEAVE();
		return node;
	default:
		ICE("Unexpected token in statement");
	}

	if (is_keyword(token, KEYWORD_ELSE)) {
		ERROR("'else' unattached to an 'if' or 'when' statement");
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
	Tree *tree = CAST(Tree*, allocator->allocate(allocator, sizeof *tree));
	if (!tree) {
		return 0;
	}

	tree_init(tree, context);

	parser->tree = tree;
	advancep(parser);

	// Every Odin source file must begin with the package it's part of.
	if (!is_keyword(parser->this_token, KEYWORD_PACKAGE)) {
		ERROR("Expected a package declaration at the beginning of the file");
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
		ERROR("Cannot name package '_'");
	}

	for (Size i = 0; i < sizeof(RESERVED_PACKAGES)/sizeof *RESERVED_PACKAGES; i++) {
		const String name = RESERVED_PACKAGES[i];
		if (string_compare(package.string, name)) {
			ERROR("Use of reserved package name '%.*s'", SFMT(name));
		}
	}

	tree->package = package.string;

	while (!is_kind(parser->this_token, KIND_EOF)) {
		Node *statement = parse_statement(parser);
		if (statement) {
			ASSERT(statement->kind = NODE_STATEMENT);
			if (statement->statement.kind != STATEMENT_EMPTY) {
				array_push(tree->statements, statement);
			}
		}
	}

	TRACE_LEAVE();

	return tree;
}