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

#define PARSE_ERROR(...) \
	do { \
		report_error(&parser->source, &parser->lexer.location, __VA_ARGS__); \
		THROW(1); \
		exit(1); \
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
	Token this_token; // This token is being processed
	Token last_token; // Last token processed
	ProcedureType *this_procedure;
	BlockFlag this_block_flags;
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
	if (0) {}
	#define CCONVENTION(string, enum) \
		else if (string_compare(input, SCLIT(string))) return CCONV_ ## enum;
	#include "lexemes.h"
	#undef CCONVENTION
	else if (string_compare(input, SCLIT("c")))    return CCONV_CDECL;
	else if (string_compare(input, SCLIT("std")))  return CCONV_STDCALL;
	else if (string_compare(input, SCLIT("fast"))) return CCONV_FASTCALL;
	return CCONV_INVALID;
}

static Identifier *parse_identifier(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	const Token token = parser->this_token;
	if (is_kind(token, KIND_IDENTIFIER) || is_keyword(token, KEYWORD_TYPEID)) {
		// NOTE(dweiler): Should write this a different way?
		advancep(parser);
	} else {
		PARSE_ERROR("Expected identifier or 'typeid'");
	}
	Identifier *identifier = tree_new_identifier(parser->tree, token.string);
	TRACE_LEAVE();
	return identifier;
}

static Expression *parse_operand(Parser *parser, Bool lhs);
static Expression *parse_expression(Parser *parser, Bool lhs);
static Expression *parse_atom_expression(Parser *parser, Expression *operand, Bool lhs);

static Identifier *parse_type_or_identifier(Parser *parser) {
	TRACE_ENTER();
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = -1;
	Expression *operand = parse_operand(parser, true);
	Expression *type = parse_atom_expression(parser, operand, true);
	parser->expression_depth = depth;
	if (type) {
		ASSERT(type->kind == EXPRESSION_VALUE);
		Value *value = RCAST(ValueExpression *, type)->value;
		ASSERT(value->kind == VALUE_IDENTIFIER);
		Identifier *identifier = RCAST(IdentifierValue *, value)->identifier;
		TRACE_LEAVE();
		return identifier;
	}
	TRACE_LEAVE();
	return 0;
}

static Identifier *parse_type(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	Identifier *type = parse_type_or_identifier(parser);
	if (!type) {
		advancep(parser);
		PARSE_ERROR("Expected a type");
	}
	TRACE_LEAVE();
	return type;
}

static CompoundLiteralValue *parse_compound_literal_value(Parser *parser, Expression *expression);

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
			TRACE_LEAVE();
			return 0;
		}
	}

	expect_operator(parser, OPERATOR_OPENPAREN);
	expect_operator(parser, OPERATOR_CLOSEPAREN);

	// We start all procedure types with these flags.
	ProcedureFlag flags = CAST(ProcedureFlag, 0);
	flags |= PROC_FLAG_BOUNDS_CHECK;
	flags |= PROC_FLAG_TYPE_ASSERT;
	Bool diverging = false;
	if (diverging) {
		flags |= PROC_FLAG_DIVERGING;
	}

	ProcedureType *type = tree_new_procedure_type(parser->tree, flags, convention);

	TRACE_LEAVE();

	return type;
}

static BlockStatement *parse_body(Parser *parser);

static ProcedureExpression *parse_procedure(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;

	ProcedureType *type = parse_procedure_type(parser);

	advance_possible_newline_within(parser);

	if (is_keyword(parser->this_token, KEYWORD_WHERE)) {
		expect_keyword(parser, KEYWORD_WHERE);
		UNIMPLEMENTED("where specialization for procedures");
	}

	// We start each procedure with the flags of the procedure type.
	ProcedureFlag flags = type->flags;

	// Then we add optional flags that cannot be provided by the type
	// and are part of the procedure body.
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

	advance_possible_newline_within(parser);

	if (accepted_kind(parser, KIND_UNDEFINED)) {
		UNIMPLEMENTED("Undefined procedure literal");
	} else if (is_kind(parser->this_token, KIND_LBRACE)) {
		ProcedureType *last_procedure = parser->this_procedure;
		parser->this_procedure = type;
		// Convert procedure flags to block flags.
		BlockFlag block_flags = CAST(BlockFlag, 0);
		if (flags & PROC_FLAG_BOUNDS_CHECK) block_flags |= BLOCK_FLAG_BOUNDS_CHECK;
		if (flags & PROC_FLAG_TYPE_ASSERT)  block_flags |= BLOCK_FLAG_TYPE_ASSERT;
		parser->this_block_flags = block_flags;
		BlockStatement *body = parse_body(parser);
		parser->this_procedure = last_procedure;
		ASSERT(type);
		ProcedureExpression *expression = tree_new_procedure_expression(parser->tree, flags, type, body);
		TRACE_LEAVE();
		return expression;
	}

	ICE("Unexpected situation in procedure parsing");

	TRACE_LEAVE();

	return 0;
}

static CallExpression *parse_call_expression(Parser *parser, Expression *operand);

static Statement *parse_statement(Parser *parser);
static Expression *parse_unary_expression(Parser *parser, Bool lhs);

static Statement *parse_directive_for_statement(Parser *parser) {
	TRACE_ENTER();

	Context *context = parser->context;

	const Token token = expect_kind(parser, KIND_DIRECTIVE);
	Statement *statement = 0;
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
		// statement = tree_new_directive(parser->tree, token.as_directive);
		// statement = tree_new_expression_statement(parser->tree, parse_call_expression(parser, statement));
		// expect_semicolon(parser);
		// break;
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

static Expression *parse_operand(Parser *parser, Bool lhs) {
	TRACE_ENTER();
	Context *context = parser->context;
	const Token token = parser->this_token;
	Expression *expression = 0;
	switch (token.kind) {
	case KIND_IDENTIFIER:
		{
			Identifier *identifier = parse_identifier(parser);
			IdentifierValue *value = tree_new_identifier_value(parser->tree, identifier);
			expression = RCAST(Expression *, tree_new_value_expression(parser->tree, RCAST(Value *, value)));
			TRACE_LEAVE();
			return expression;
		}
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
				LiteralValue *value = tree_new_literal_value(parser->tree, token.as_literal, token.string);
				expression = RCAST(Expression *, tree_new_value_expression(parser->tree, RCAST(Value *, value)));
				TRACE_LEAVE();
				return expression;
			}
		case LITERAL_COUNT:
			UNREACHABLE();
		}
		break;
	case KIND_LBRACE:
		if (!lhs) {
			CompoundLiteralValue *value = parse_compound_literal_value(parser, 0);
			expression = RCAST(Expression *, tree_new_value_expression(parser->tree, RCAST(Value *, value)));
			TRACE_LEAVE();
			return expression;
		}
		break;
	case KIND_DIRECTIVE:
		UNIMPLEMENTED("Directive");
		TRACE_LEAVE();
		return expression;
	case KIND_KEYWORD:
		switch (token.as_keyword) {
		case KEYWORD_DISTINCT:
			UNIMPLEMENTED("distinct");
		case KEYWORD_PROC:
			expect_keyword(parser, KEYWORD_PROC);
			if (is_kind(parser->this_token, KIND_LBRACE)) {
				UNIMPLEMENTED("Procedure groups");
			} else {
				expression = RCAST(Expression *, parse_procedure(parser));
			}
			TRACE_LEAVE();
			return expression;
		/*
		case KEYWORD_TYPEID:
			expect_keyword(parser, KEYWORD_TYPEID);
			expression = tree_new_typeid_type(parser->tree);
			TRACE_LEAVE();
			return expression;
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
		*/
		default:
			ICE("Unexpected keyword in operand");
		}
		UNIMPLEMENTED("Keyword");
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_OPENPAREN:
			UNIMPLEMENTED("()");
		case OPERATOR_POINTER:
			UNIMPLEMENTED("^");
		case OPERATOR_OPENBRACKET:
			UNIMPLEMENTED("[]");
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
	TRACE_ENTER();

	Context *context = parser->context;

	Array(Expression*) arguments = 0;
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	expect_operator(parser, OPERATOR_OPENPAREN);
	while (!is_operator(parser->this_token, OPERATOR_CLOSEPAREN) && !is_kind(parser->this_token, KIND_EOF)) {
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
		}
		array_push(arguments, argument);

		if (accepted_operator(parser, OPERATOR_COMMA)) {
			// ...
		}
	}
	parser->expression_depth = depth;
	expect_operator(parser, OPERATOR_CLOSEPAREN);
	CallExpression *expression = tree_new_call_expression(parser->tree, operand, arguments);
	TRACE_LEAVE();
	return expression;
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

static CompoundLiteralValue *parse_compound_literal_value(Parser *parser, Expression *expression) {
	Context *context = parser->context;
	TRACE_ENTER();
	Array(Expression*) elements = 0;
	expect_kind(parser, KIND_LBRACE);
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	if (!is_kind(parser->this_token, KIND_RBRACE)) {
		UNIMPLEMENTED("Compound literal expressions");
		// elements = parse_element_list(parser);
	}
	parser->expression_depth = depth;
	expect_closing(parser, KIND_RBRACE);
	CompoundLiteralValue *value = tree_new_compound_literal_value(parser->tree, expression, elements);
	TRACE_LEAVE();
	return value;
}

static Expression *parse_atom_expression(Parser *parser, Expression *operand, Bool lhs) {
	TRACE_ENTER();

	Context *context = parser->context;

	if (!operand) {
		// if (allow_type) return 0
		// PARSE_ERROR("Expected an operand");
		TRACE_LEAVE();
		return 0;
	}

	// PARSE_ERROR("Unimplemented atom expression");
	Identifier *ident = 0;
	Identifier *type = 0;
	for (;;) {
		Token token = parser->this_token;
		switch (token.kind) {
		case KIND_OPERATOR:
			switch (token.as_operator) {
			case OPERATOR_OPENPAREN:
				operand = RCAST(Expression *, parse_call_expression(parser, operand));
				break;

			// .x
			// .?
			// .(T)
			case OPERATOR_PERIOD:
				advancep(parser);
				switch (parser->this_token.kind) {
				case KIND_IDENTIFIER:
					ident = parse_identifier(parser);
					operand = RCAST(Expression *, tree_new_selector_expression(parser->tree, operand, ident));
					break;
				case KIND_OPERATOR:
					switch (parser->this_token.as_operator) {
					case OPERATOR_OPENPAREN:
						expect_operator(parser, OPERATOR_OPENPAREN);
						type = parse_type(parser);
						expect_operator(parser, OPERATOR_CLOSEPAREN);
						operand = RCAST(Expression *, tree_new_assertion_expression(parser->tree, operand, type));
						break;
					case OPERATOR_QUESTION:
						expect_operator(parser, OPERATOR_QUESTION);
						type = tree_new_identifier(parser->tree, SCLIT("?"));
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
				advancep(parser);
				ident = parse_identifier(parser);
				operand = RCAST(Expression *, tree_new_selector_expression(parser->tree, operand, ident));
				break;
			case OPERATOR_OPENBRACKET:
				UNIMPLEMENTED("Bracket");
			case OPERATOR_POINTER:
				expect_operator(parser, OPERATOR_POINTER);
				operand = RCAST(Expression *, tree_new_unary_expression(parser->tree, OPERATOR_POINTER, operand));
				break;
			case OPERATOR_OR_RETURN:
				UNIMPLEMENTED("or_return");
			default:
				TRACE_LEAVE();
				return operand;
			}
			break;
		case KIND_LBRACE:
			if (!lhs && parser->expression_depth >= 0) {
				CompoundLiteralValue *value = parse_compound_literal_value(parser, operand);
				operand = RCAST(Expression *, tree_new_value_expression(parser->tree, RCAST(Value *, value)));
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

static Expression *parse_unary_expression(Parser *parser, Bool lhs) {
	TRACE_ENTER();

	Context *context = parser->context;

	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_TRANSMUTE:
			UNIMPLEMENTED("transmute");
		case OPERATOR_CAST:
			UNIMPLEMENTED("cast");
		case OPERATOR_AUTO_CAST:
			UNIMPLEMENTED("auto_cast");
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
				Expression *operand = parse_unary_expression(parser, lhs);
				UnaryExpression *expression = tree_new_unary_expression(parser->tree, token.as_operator, operand);
				TRACE_LEAVE();
				return RCAST(Expression *, expression);
			}
			break;
		case OPERATOR_PERIOD:
			UNIMPLEMENTED(".");
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

static Expression *parse_binary_expression(Parser *parser, Bool lhs, Sint32 prec) {
	TRACE_ENTER();

	Context *context = parser->context;

	Expression *expr = parse_unary_expression(parser, lhs);

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
			PARSE_ERROR("Unimplemented ternary if or when expression");
			break;
		} else if (!is_kind(token, KIND_OPERATOR)) {
			break;
		}

		advancep(parser);

		// Expect operator or if/when keywords.
		Expression *rhs = parse_binary_expression(parser, false, op_prec + 1);
		if (!rhs) {
			PARSE_ERROR("Expected expression on the right-hand side");
		}

		if (is_operator(token, OPERATOR_OR_ELSE)) {
			UNIMPLEMENTED("or_else");
		} else {
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

	Array(Expression*) expressions = 0;
	for (;;) {
		Expression *expr = parse_expression(parser, lhs);
		if (expr) {
			array_push(expressions, expr);
		}
		if (is_kind(parser->this_token, KIND_EOF) || !is_operator(parser->this_token, OPERATOR_COMMA)) {
			break;
		}
		advancep(parser);
	}

	ListExpression *expression = tree_new_list_expression(parser->tree, expressions);

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

static Statement *parse_statement(Parser *parser);

static Array(Statement*) parse_statement_list(Parser *parser) {
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
		Statement *statement = parse_statement(parser);
		if (statement && statement->kind != STATEMENT_EMPTY) {
			array_push(statements, statement);
			// TODO(dweiler): More robust.
		}
	}

	TRACE_LEAVE();

	return statements;
}

static BlockStatement *parse_body(Parser *parser) {
	TRACE_ENTER();
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	expect_kind(parser, KIND_LBRACE);
	Array(Statement*) statements = parse_statement_list(parser);
	expect_kind(parser, KIND_RBRACE);
	parser->expression_depth = depth;
	BlockFlag flags = parser->this_block_flags;
	BlockStatement *statement = tree_new_block_statement(parser->tree, flags, statements);
	TRACE_LEAVE();
	return statement;
}

static BlockStatement *parse_block_statement(Parser *parser, Bool when) {
	// The block statement may be part of a compile-time when statement.
	TRACE_ENTER();

	Context *context = parser->context;

	advance_possible_newline(parser);

	if (when) {
		UNIMPLEMENTED("when block");
	}

	if (!when && !parser->this_procedure) {
		PARSE_ERROR("Cannot use block statement at file scope");
	}

	BlockStatement *body = parse_body(parser);

	TRACE_LEAVE();

	return body;
}

static DeclarationStatement *parse_declaration_statement(Parser *parser, Array(Identifier*) names) {
	// ':'
	TRACE_ENTER();

	Context *context = parser->context;

	ListExpression *values = 0;
	Identifier *type = parse_type_or_identifier(parser);
	const Token token = parser->this_token;
	Bool constant = false;
	// Check for '=' (:=)
	// Check for ':' (::)
	if (is_assignment(token, ASSIGNMENT_EQ) || is_operator(token, OPERATOR_COLON)) {
		const Token seperator = advancep(parser);
		constant = is_operator(seperator, OPERATOR_COLON);
		values = parse_rhs_list_expression(parser);
		const Size n_names = array_size(names);
		const Size n_values = array_size(values->expressions);
		if (n_values != n_names) {
			PARSE_ERROR("Expected %d values on the right-hand side of this declaration", CAST(Sint32, n_names));
		}
	}

	if (constant && array_size(values) == 0) {
		PARSE_ERROR("Expected constant initializer");
	}

	if (parser->expression_depth >= 0) {
		expect_semicolon(parser);
	}

	DeclarationStatement *statement = tree_new_declaration_statement(parser->tree, type, names, values);

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
			if (!parser->this_procedure) {
				PARSE_ERROR("Cannot use assignment statement at file scope");
			}
			advancep(parser);
			ListExpression *rhs = parse_rhs_list_expression(parser);
			if (array_size(rhs->expressions) == 0) {
				PARSE_ERROR("Missing right-hand side in assignment");
				return 0;
			}
			AssignmentStatement *statement = tree_new_assignment_statement(parser->tree, token.as_assignment, lhs, rhs);
			TRACE_LEAVE();
			return RCAST(Statement *, statement);
		}
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_IN:
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
				expect_operator(parser, OPERATOR_COLON);
				// Everything inside 'lhs' should be an ValueExpression of type IdentifierValue
				const Uint64 n_names = array_size(lhs->expressions);
				Array(Identifier *) names = 0;
				for (Uint64 i = 0; i < n_names; i++) {
					const Expression *expression = lhs->expressions[i];
					if (expression->kind != EXPRESSION_VALUE) {
						PARSE_ERROR("Expected identifier");
					}
					const Value *value = RCAST(const ValueExpression *, expression)->value;
					if (value->kind != VALUE_IDENTIFIER) {
						PARSE_ERROR("Expected identifier");
					}
					array_push(names, RCAST(const IdentifierValue *, value)->identifier);
				}
				DeclarationStatement *statement = parse_declaration_statement(parser, names);
				TRACE_LEAVE();
				return RCAST(Statement *, statement);
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
		// PARSE_ERROR("Unexpected '%.*s'",
		// 	CAST(int,          string.size),
		// 	CAST(const char *, string.data));
		break;
	}

	Array(Expression*) expressions = lhs->expressions;
	if (array_size(expressions) == 0 || array_size(expressions) > 1) {
		PARSE_ERROR("Expected one expression on the left-hand side");
		return 0;
	}

	ExpressionStatement *statement = tree_new_expression_statement(parser->tree, expressions[0]);

	TRACE_LEAVE();

	return RCAST(Statement *, statement);
}

static ImportStatement *parse_import_declaration(Parser *parser) {
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

static BlockStatement *convert_statement_to_body(Parser *parser, Statement *statement) {
	Context *context = parser->context;

	const StatementKind kind = statement->kind;
	if (kind == STATEMENT_BLOCK || kind == STATEMENT_EMPTY) {
		PARSE_ERROR("Expected a regular statement");
	}
	Array(Statement*) statements = 0;
	array_push(statements, statement);
	return tree_new_block_statement(parser->tree, parser->this_block_flags, statements);
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

static BlockStatement *parse_do_body(Parser *parser) {
	TRACE_ENTER();
	const Sint32 depth = parser->expression_depth;
	parser->expression_depth = 0;
	Statement *statement = parse_statement(parser);
	BlockStatement *body = convert_statement_to_body(parser, statement);
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

static IfStatement *parse_if_statement(Parser *parser) {
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
		body = parse_do_body(parser);
	} else {
		body = parse_block_statement(parser, false);
	}

	advance_possible_newline(parser);

	BlockStatement *elif = 0;
	if (is_keyword(parser->this_token, KEYWORD_ELSE)) {
		expect_keyword(parser, KEYWORD_ELSE);
		if (is_keyword(parser->this_token, KEYWORD_IF)) {
			IfStatement *statement = parse_if_statement(parser);
			Array(Statement) *statements = 0;
			array_push(statements, RCAST(Statement *, statement));
			elif = tree_new_block_statement(parser->tree, parser->this_block_flags, statements);
		} else if (is_kind(parser->this_token, KIND_LBRACE)) {
			elif = parse_block_statement(parser, false);
		} else if (is_keyword(parser->this_token, KEYWORD_DO)) {
			expect_keyword(parser, KEYWORD_DO);
			elif = parse_do_body(parser);
		} else {
			PARSE_ERROR("Expected block on 'else' statement");
		}
	}

	IfStatement *statement = tree_new_if_statement(parser->tree, init, cond, body, elif);

	TRACE_LEAVE();

	return statement;
}

static ForStatement *parse_for_statement(Parser *parser) {
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
				body = parse_do_body(parser);
			} else {
				body = parse_block_statement(parser, false);
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
		body = parse_do_body(parser);
	} else {
		// for [...] <BlockStatement>
		body = parse_block_statement(parser, false);
	}

	ForStatement *statement = tree_new_for_statement(parser->tree, init, cond, body, post);

	TRACE_LEAVE();

	return statement;
}

static DeferStatement *parse_defer_statement(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	expect_keyword(parser, KEYWORD_DEFER);
	Statement *statement = parse_statement(parser);
	switch (statement->kind) {
	case STATEMENT_EMPTY:
		PARSE_ERROR("Empty statement in defer");
	case STATEMENT_DEFER:
		PARSE_ERROR("Cannot defer a defer statement");
	case STATEMENT_RETURN:
		PARSE_ERROR("Cannot defer a return statement");
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
	       !is_kind(parser->this_token, KIND_RBRACE))
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

static Statement *parse_statement(Parser *parser) {
	TRACE_ENTER();
	Context *context = parser->context;
	Statement *statement = 0;
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
			statement = parse_simple_statement(parser, false);
			expect_semicolon(parser);
			TRACE_LEAVE();
			return statement;
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
			statement = parse_simple_statement(parser, false);
			expect_semicolon(parser);
			TRACE_LEAVE();
			return statement;
		case KEYWORD_FOREIGN:
			UNIMPLEMENTED("Foreign declaration");
		case KEYWORD_IMPORT:
			statement = RCAST(Statement *, parse_import_declaration(parser));
			TRACE_LEAVE();
			return statement;
		case KEYWORD_IF:
			statement = RCAST(Statement *, parse_if_statement(parser));
			TRACE_LEAVE();
			return statement;
		case KEYWORD_WHEN:
			UNIMPLEMENTED("When statement");
		case KEYWORD_FOR:
			statement = RCAST(Statement *, parse_for_statement(parser));
			TRACE_LEAVE();
			return statement;
		case KEYWORD_SWITCH:
			UNIMPLEMENTED("Switch statement");
		case KEYWORD_DEFER:
			statement = RCAST(Statement *, parse_defer_statement(parser));
			TRACE_LEAVE();
			return statement;
		case KEYWORD_RETURN:
			statement = RCAST(Statement *, parse_return_statement(parser));
			TRACE_LEAVE();
			return statement;
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
		statement = parse_simple_statement(parser, false);
		expect_semicolon(parser);
		TRACE_LEAVE();
		return statement;
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
			statement = parse_simple_statement(parser, false);
			expect_semicolon(parser);
			TRACE_LEAVE();
			return statement;
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
		statement = RCAST(Statement *, parse_directive_for_statement(parser));
		TRACE_LEAVE();
		return statement;
	case KIND_LBRACE:
		statement = RCAST(Statement *, parse_block_statement(parser, false));
		TRACE_LEAVE();
		return statement;
	case KIND_SEMICOLON:
		statement = RCAST(Statement *, tree_new_empty_statement(parser->tree));
		expect_semicolon(parser);
		TRACE_LEAVE();
		return statement;
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
	Tree *tree = CAST(Tree*, allocator->allocate(allocator, sizeof *tree));
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

	tree->package = package.string;

	while (!is_kind(parser->this_token, KIND_EOF)) {
		Statement *statement = parse_statement(parser);
		if (statement && statement->kind != STATEMENT_EMPTY) {
			array_push(tree->statements, statement);
		}
	}

	TRACE_LEAVE();

	return tree;
}