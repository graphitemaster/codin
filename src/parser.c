#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "parser.h"
#include "lexer.h"
#include "report.h"
#include "tree.h"

#ifdef ERROR
#undef ERROR
#endif

#define ERROR(...) \
	report_error(&parser->source, &parser->lexer.location, __VA_ARGS__); \
	exit(1)

static int s_trace_level = 0;
#define TRACE_ENTER(name) \
	do { \
		break; \
		s_trace_level++; \
		for (int i = 0; i < s_trace_level; i++) printf(" "); \
		printf("%s\n", (name)); \
	} while (0)

#define TRACE_LEAVE() \
	do { \
		s_trace_level--; \
	} while (0)

// Internal compiler error.
#define ICE(...) \
	do { \
		ERROR(__VA_ARGS__); \
		exit(1); \
	} while (0)

#define UNIMPLEMENTED(what) \
	do { \
		ERROR("Unimplemented: %s", what); \
		exit(1); \
	} while (0)

static void source_free(Source *source) {
	string_free(&source->contents);
	string_free(&source->name);
}

static Bool source_read(Source *source, const char *filename) {
	source->name.data = 0;
	if (!string_assign(&source->name, filename)) return false;
	String *contents = &source->contents;
	FILE *fp = fopen(filename, "rb");
	if (!fp) goto L_error;
	if (fseek(fp, 0, SEEK_END) != 0) goto L_error;
	contents->size = ftell(fp);
	if (fseek(fp, 0, SEEK_SET) != 0) goto L_error;
	contents->data = malloc(contents->size);
	if (!contents->data) goto L_error;
	if (fread(contents->data, contents->size, 1, fp) != 1) goto L_error;
	fclose(fp);
	return true;
L_error:
	if (fp) fclose(fp);
	source_free(source);
	return false;
}

typedef struct Parser Parser;

struct Parser {
	Source source;
	Lexer lexer;
	Tree *tree;
	Token this_token; // This token is being processed
	Token last_token; // Last token processed
	Node *this_procedure;
};

void parser_free(Parser *parser) {
	source_free(&parser->source);
}

static Bool parser_init(Parser *parser, const char *filename) {
	if (source_read(&parser->source, filename)) {
		if (lexer_init(&parser->lexer, &parser->source)) {
			parser->this_token.kind = KIND_INVALID;
			parser->last_token.kind = KIND_INVALID;
			return true;
		}
	}
	parser_free(parser);
	return false;
}

static FORCE_INLINE Bool is_kind(Token token, Kind kind) {
	return token.kind == kind;
}
static FORCE_INLINE Bool is_operator(Token token, Operator op) {
	return is_kind(token, KIND_OPERATOR) && token.as_operator == op;
}
static FORCE_INLINE Bool is_keyword(Token token, Keyword keyword) {
	return is_kind(token, KIND_KEYWORD) && token.as_keyword == keyword;
}
static FORCE_INLINE Bool is_assignment(Token token, Assignment assignment) {
	return is_kind(token, KIND_ASSIGNMENT) && token.as_assignment == assignment;
}
static FORCE_INLINE Bool is_literal(Token token, Literal literal) {
	return is_kind(token, KIND_LITERAL) && token.as_literal == literal;
}

static Token advancep(Parser *parser) {
	const Token last = parser->this_token;
	parser->last_token = last;
	parser->this_token = lexer_next(&parser->lexer);
	while (is_kind(parser->this_token, KIND_COMMENT)) {
		parser->this_token = lexer_next(&parser->lexer);
	}
	return last;
}

static Token expect_kind(Parser *parser, Kind kind) {
	const Token token = parser->this_token;
	if (!is_kind(token, kind)) {
		const String want = kind_to_string(kind);
		const String have = token_to_string(token);
		ERROR("Expected '%.*s', got '%.*s'\n",
			CAST(int,         want.size),
			CAST(const char*, want.data),
			CAST(int,         have.size),
			CAST(const char*, have.data));
	}
	return advancep(parser);
}

static Token expect_operator(Parser *parser, Operator op) {
	const Token token = parser->this_token;
	if (!is_operator(token, op)) {
		const String want = operator_to_string(op);
		const String have = token_to_string(token);
		ERROR("Expected operator '%.*s', got '%.*s'",
			CAST(int,         want.size),
			CAST(const char*, want.data),
			CAST(int,         have.size),
			CAST(const char*, have.data));
	}
	return advancep(parser);
}

static Token expect_keyword(Parser* parser, Keyword keyword) {
	const Token token = parser->this_token;
	if (!is_keyword(token, keyword)) {
		const String want = keyword_to_string(keyword);
		const String have = token_to_string(token);
		ERROR("Expected keyword '%.*s', got '%.*s'",
			CAST(int,         want.size),
			CAST(const char*, want.data),
			CAST(int,         have.size),
			CAST(const char*, have.data));
	}
	return advancep(parser);
}

static Token expect_assignment(Parser *parser, Assignment assignment) {
	const Token token = parser->this_token;
	if (!is_assignment(token, assignment)) {
		const String want = assignment_to_string(assignment);
		const String have = token_to_string(token);
		ERROR("Expected assignment '%.*s', got '%.*s'",
			CAST(int,         want.size),
			CAST(const char*, want.data),
			CAST(int,         have.size),
			CAST(const char*, have.data));
	}
	return advancep(parser);
}

static Token expect_literal(Parser *parser, Literal literal) {
	const Token token = parser->this_token;
	if (!is_literal(token, literal)) {
		const String want = literal_to_string(literal);
		const String have = token_to_string(token);
		ERROR("Expected literal '%.*s', got '%.*s'",
			CAST(int,         want.size),
			CAST(const char*, want.data),
			CAST(int,         have.size),
			CAST(const char*, have.data));
	}
	return advancep(parser);
}

static FORCE_INLINE Token expect_semicolon(Parser *parser) {
	// TODO(dweiler): Odin's ridicolous ASI
	if (is_kind(parser->this_token, KIND_SEMICOLON)) {
		return advancep(parser);
	}
	return (Token){ .kind = KIND_INVALID };
}

static String unquote(const String string) {
	const Rune quote = string.data[0];
	if (quote == '\"' || quote == '`') {
		return (String) { .data = &string.data[1], .size = string.size - 2 };
	}
	return string;
}

static CallingConvention string_to_calling_convention(String string) {
	/**/ if (string_compare(&string, &SLIT("odin")))        return CCONV_ODIN;
	else if (string_compare(&string, &SLIT("contextless"))) return CCONV_CONTEXTLESS;
	else if (string_compare(&string, &SLIT("cdecl")))       return CCONV_CDECL;
	else if (string_compare(&string, &SLIT("c")))           return CCONV_CDECL;
	else if (string_compare(&string, &SLIT("stdcall")))     return CCONV_STDCALL;
	else if (string_compare(&string, &SLIT("std")))         return CCONV_STDCALL;
	else if (string_compare(&string, &SLIT("fastcall")))    return CCONV_FASTCALL;
	else if (string_compare(&string, &SLIT("fast")))        return CCONV_FASTCALL;
	else if (string_compare(&string, &SLIT("none")))        return CCONV_NONE;
	else if (string_compare(&string, &SLIT("naked")))       return CCONV_NAKED;
	else if (string_compare(&string, &SLIT("win64")))       return CCONV_STDCALL;
	else if (string_compare(&string, &SLIT("sysv")))        return CCONV_CDECL;
	return CCONV_INVALID;
}

static Node *parse_identifier(Parser *parser) {
	const Token token = parser->this_token;
	if (is_kind(token, KIND_IDENTIFIER)) {
		advancep(parser);
	}
	return tree_new_identifier(parser->tree, token.string);
}

static Node *parse_operand(Parser *parser, Bool lhs);
static Node *parse_atom_expression(Parser *parser, Node *operand, Bool lhs);

static Node *parse_type_or_identifier(Parser *parser) {
	TRACE_ENTER("parse_type_or_identifier");
	Node *operand = parse_operand(parser, true);
	Node *type = parse_atom_expression(parser, operand, true);
	TRACE_LEAVE();
	return type;
}

static Node *parse_type(Parser *parser) {
	Node *type = parse_type_or_identifier(parser);
	if (!type) {
		advancep(parser);
		ERROR("Expected a type");
	}
	return type;
}

static Bool accepted_operator(Parser *parser, Operator op) {
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

static Bool accepted_keyword(Parser *parser, Keyword keyword) {
	if (is_keyword(parser->this_token, keyword)) {
		advancep(parser);
		return true;
	}
	return false;
}

static Node *parse_literal_value(Parser *parser, Node *type);

static Node *parse_result_list(Parser *parser) {
	(void)parser;
	TRACE_ENTER("parse_result_list");
	UNIMPLEMENTED("parse_result_list not implemented");
	TRACE_LEAVE();
	return 0;
}

static Node *parse_results(Parser *parser) {
	TRACE_ENTER("parse_results");
	if (!accepted_operator(parser, OPERATOR_ARROW)) {
		TRACE_LEAVE();
		ASSERT(false && "no arrow");
		return 0;
	}

	if (accepted_operator(parser, OPERATOR_NOT)) {
		UNIMPLEMENTED("Diverging procedures");
	}

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

	TRACE_LEAVE();
	ASSERT(list);
	return list;
}

static Node *parse_procedure_type(Parser *parser) {
	TRACE_ENTER("parse_procedure_type");
	CallingConvention convention = CCONV_INVALID;
	if (is_literal(parser->this_token, LITERAL_STRING)) {
		const Token token = expect_literal(parser, LITERAL_STRING);
		const String string = token.string;
		convention = string_to_calling_convention(unquote(string));
		if (convention == CCONV_INVALID) {
			ERROR("Unknown calling convention '%.*s'",
				CAST(Sint32,       string.size),
				CAST(const char *, string.data));
			return 0;
		}
	} else {
		convention = CCONV_CDECL;
	}

	expect_operator(parser, OPERATOR_OPENPAREN);
	// Node *params = parse_parameters(parser);
	expect_operator(parser, OPERATOR_CLOSEPAREN);

	Node *results = parse_results(parser);

	TRACE_LEAVE();
	ASSERT(results);
	return results;
}

static Node *parse_body(Parser *parser);

static Node *parse_procedure(Parser *parser) {
	Node *type = parse_procedure_type(parser);
	if (is_keyword(parser->this_token, KEYWORD_WHERE)) {
		expect_keyword(parser, KEYWORD_WHERE);
		UNIMPLEMENTED("where specialization for procedures");
	}

	// TODO(dweiler): Check for procedure directives like "#no_bounds_check"

	if (accepted_kind(parser, KIND_UNDEFINED)) {
		UNIMPLEMENTED("Undefined procedure literal");
	} else if (is_kind(parser->this_token, KIND_LBRACE)) {
		Node *last_procedure = parser->this_procedure;
		parser->this_procedure = type;
		Node *body = parse_body(parser);
		parser->this_procedure = last_procedure;
		ASSERT(type);
		return tree_new_procedure(parser->tree, type, body);
	}

	ASSERT("WTF!?");
	return type;
}

static Node *parse_procedure_group(Parser *parser) {
	TRACE_ENTER("parse_procedure_group");
	UNIMPLEMENTED("parse_procedure_group");
	TRACE_LEAVE();
	return 0;
}

static Node *parse_operand(Parser *parser, Bool lhs) {
	TRACE_ENTER("parse_operand");
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
	case KIND_HASH:
		UNIMPLEMENTED("hash");
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
			UNIMPLEMENTED("typeid");
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
			UNIMPLEMENTED("(");
		case OPERATOR_POINTER:
			UNIMPLEMENTED("^");
		case OPERATOR_OPENBRACKET:
			UNIMPLEMENTED("[");
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

static Node *parse_expression(Parser *parser, Bool lhs);

static Node *parse_value(Parser *parser) {
	TRACE_ENTER("parse_value");
	if (is_kind(parser->this_token, KIND_LBRACE)) {
		UNIMPLEMENTED("Literal value");
	}
	Node *node = parse_expression(parser, false);
	TRACE_LEAVE();
	return node;
}

static Node *parse_call_expression(Parser *parser, Node *operand) {
	TRACE_ENTER("parse_call_expression");
	Array(Node*) arguments = 0;
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
	expect_operator(parser, OPERATOR_CLOSEPAREN);

	Node *node = tree_new_call_expression(parser->tree, operand, arguments);
	TRACE_LEAVE();
	return node;
}

static Array(Node*) parse_element_list(Parser *parser) {
	TRACE_ENTER("parse_element_list");
	Array(Node*) elements = 0;
	while (!is_kind(parser->this_token, KIND_RBRACE) && !is_kind(parser->this_token, KIND_EOF)) {
		Node *element = parse_value(parser);
		if (is_assignment(parser->this_token, ASSIGNMENT_EQ)) {
			expect_assignment(parser, ASSIGNMENT_EQ);
			Node *value = parse_value(parser);
			element = tree_new_value(parser->tree, element, value);
		}
		array_push(elements, element);
		// TODO(dweiler): Handle field separators.
	}
	TRACE_LEAVE();
	return elements;
}

static Node *parse_literal_value(Parser *parser, Node *type) {
	TRACE_ENTER("parse_literal_value");
	Array(Node*) elements = 0;
	expect_kind(parser, KIND_LBRACE);
	if (!is_kind(parser->this_token, KIND_RBRACE)) {
		elements = parse_element_list(parser);
	}
	expect_kind(parser, KIND_RBRACE);
	TRACE_LEAVE();
	return tree_new_compound_literal(parser->tree, type, elements);
}

static Node *parse_atom_expression(Parser *parser, Node *operand, Bool lhs) {
	TRACE_ENTER("parse_atom_expression");
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
				UNIMPLEMENTED("Dereference");
			case OPERATOR_OR_RETURN:
				UNIMPLEMENTED("or_return");
			default:
				TRACE_LEAVE();
				return operand;
			}
			break;
		case KIND_LBRACE:
			if (!lhs) {
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
	TRACE_ENTER("parse_unary_expression");
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
	TRACE_ENTER("parse_binary_expression");

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
		if (is_keyword(token, KEYWORD_IF) || is_keyword(token, KEYWORD_WHEN)) {
			// TODO(dweiler): Ternary if and when expressions.
			break;
			UNIMPLEMENTED("if when ternary");
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
	TRACE_ENTER("parse_expression");
	Node *node = parse_binary_expression(parser, lhs, 1);
	TRACE_LEAVE();
	return node;
}

static Array(Node*) parse_expression_list(Parser *parser, Bool lhs) {
	TRACE_ENTER("parse_expression_list");
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
	TRACE_ENTER("parse_lhs_expression_list");
	Array(Node*) result = parse_expression_list(parser, true);
	TRACE_LEAVE();
	return result;
}

static FORCE_INLINE Array(Node*) parse_rhs_expression_list(Parser *parser) {
	TRACE_ENTER("parse_rhs_expression_list");
	Array(Node*) result = parse_expression_list(parser, false);
	TRACE_LEAVE();
	return result;
}

static Node *parse_statement(Parser *parser);

static Array(Node*) parse_statement_list(Parser *parser) {
	TRACE_ENTER("parse_statement_list");

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
	TRACE_ENTER("parse_body");
	expect_kind(parser, KIND_LBRACE);
	Array(Node*) statements = parse_statement_list(parser);
	expect_kind(parser, KIND_RBRACE);
	Node *node = tree_new_block_statement(parser->tree, statements);
	TRACE_LEAVE();
	return node;
}

static Node *parse_block_statement(Parser *parser, Bool when) {
	// The block statement may be part of a compile-time when statement.
	TRACE_ENTER("parse_block_statement");
	if (when) {
		UNIMPLEMENTED("when block");
	}
	// TODO(dweiler) Check that we're inside a procedure.
	Node *body = parse_body(parser);
	TRACE_LEAVE();
	return body;
}

static Node *parse_declaration_statement(Parser *parser, Array(Node*) names) {
	// ':'
	TRACE_ENTER("parse_value_declaration");
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
		const Uint64 n_names = array_size(names);
		const Uint64 n_values = array_size(values);
		if (n_values != n_names) {
			ERROR("Expected %d values on the right-hand side of this declaration", CAST(Sint32, n_names));
		}
	}
	(void)constant;
	// TODO(dweiler): Robustness.
	Node *node = tree_new_declaration_statement(parser->tree, type, names, values);
	TRACE_LEAVE();
	return node;
}

static Node *parse_simple_statement(Parser* parser) {
	TRACE_ENTER("parse_simple_statement");
	Array(Node*) lhs = parse_lhs_expression_list(parser);
	const Token token = parser->this_token;
	switch (token.kind) {
	case KIND_ASSIGNMENT:
		// TODO(dweiler): Ensure we're inside a procedure.
		advancep(parser);
		Array(Node*) rhs = parse_rhs_expression_list(parser);
		if (array_size(rhs) == 0) {
			ERROR("Missing right-hand side in assignment");
			return 0;
		}
		Node *node = tree_new_assignment_statement(parser->tree, token.as_assignment, lhs, rhs);
		TRACE_LEAVE();
		return node;
	case KIND_OPERATOR:
		switch (token.as_operator) {
		case OPERATOR_IN:
			UNIMPLEMENTED("in");
		case OPERATOR_COLON:
			expect_operator(parser, OPERATOR_COLON);
			// TODO(dweiler): Label check.
			TRACE_LEAVE();
			return parse_declaration_statement(parser, lhs);
		default:
			{
				const String string = operator_to_string(token.as_operator);
				ICE("Unexpected operator '%.*s' in statement",
					CAST(int,         string.size),
					CAST(const char*, string.data));
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

	if (array_size(lhs) > 1) {
		ERROR("Expected one expression on the left-hand side");
		return 0;
	}

	Node *node = tree_new_expression_statement(parser->tree, lhs[0]);
	TRACE_LEAVE();
	return node;
}

static Node *parse_import_declaration(Parser *parser) {
	expect_keyword(parser, KEYWORD_IMPORT);
	Token name;
	switch (parser->this_token.kind) {
	case KIND_IDENTIFIER:
		name = advancep(parser);
		break;
	default:
		break;
	}
	expect_semicolon(parser);
	return tree_new_import_statement(parser->tree, name.string);
}

static Node *convert_statement_to_body(Parser *parser, Node *statement) {
	if (statement->kind != NODE_STATEMENT) {
		ICE("Expected a statement");
	}
	const StatementKind kind = statement->statement.kind;
	if (kind == STATEMENT_BLOCK || kind == STATEMENT_EMPTY) {
		ERROR("Expected a regular statement");
	}
	Array(Node*) statements = 0;
	array_push(statements, statement);
	return tree_new_block_statement(parser->tree, statements);
}

static Node *convert_statement_to_expression(Parser *parser, Node *statement) {
	if (statement->kind != NODE_STATEMENT) {
		ICE("Expected a statement");
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
	TRACE_ENTER("parse_do_body");
	Node *statement = parse_statement(parser);
	Node *body = convert_statement_to_body(parser, statement);
	TRACE_LEAVE();
	return body;
}

static Node *parse_if_statement(Parser *parser) {
	TRACE_ENTER("parse_if_statement");

	expect_keyword(parser, KEYWORD_IF);

	// TODO(dweiler): init; cond
	// TODO(dweiler): init; cond; post
	Node *init = parse_simple_statement(parser);
	Node *condition = convert_statement_to_expression(parser, init);
	// Node *condition = parse_expression(parser, false);

	if (!condition) {
		ERROR("Expected condition in if statement");
	}

	Node *body = 0;
	if (accepted_keyword(parser, KEYWORD_DO)) {
		body = parse_do_body(parser);
	} else {
		body = parse_block_statement(parser, false);
	}

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

	Node *node = tree_new_if_statement(parser->tree, condition, body, elif);
	TRACE_LEAVE();
	return node;
}

static Node *parse_statement(Parser *parser) {
	TRACE_ENTER("parse_statement");
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
			node = parse_simple_statement(parser);
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
			node = parse_simple_statement(parser);
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
			UNIMPLEMENTED("For statement");
		case KEYWORD_SWITCH:
			UNIMPLEMENTED("Switch statement");
		case KEYWORD_DEFER:
			UNIMPLEMENTED("Defer statement");
		case KEYWORD_RETURN:
			UNIMPLEMENTED("Return statement");
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
		node = parse_simple_statement(parser);
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
			node = parse_simple_statement(parser);
			expect_semicolon(parser);
			TRACE_LEAVE();
			return node;
		default:
			{
				const String string = operator_to_string(token.as_operator);
				ICE("Unexpected operator '%.*s' in statement",
					CAST(int,         string.size),
					CAST(const char*, string.data));
			}
			break;
		}
		break;
	case KIND_ATTRIBUTE:
		UNIMPLEMENTED("Attribute");
	case KIND_HASH:
		UNIMPLEMENTED("Directive");
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
		expect_keyword(parser, KEYWORD_ELSE);
		// printf("'else' unattached to an 'if' or 'when' statement\n");
		switch (parser->this_token.kind) {
		case KIND_KEYWORD:
			switch (parser->this_token.as_keyword) {
			case KEYWORD_IF:
				UNIMPLEMENTED("If");
			case KEYWORD_WHEN:
				UNIMPLEMENTED("When");
			case KEYWORD_DO:
				expect_keyword(parser, KEYWORD_DO);
				UNIMPLEMENTED("Do");
				break;
			default:
				break;
			}
			break;
		case KIND_LBRACE:
			{
				Node *block = parse_block_statement(parser, false);
				TRACE_LEAVE();
				return block;
			}
		default:
			break;
		}
	}

	ICE("Expected statement");

	TRACE_LEAVE();
	return 0;
}

Tree *parse(const char *filename) {
	Parser parser;
	if (!parser_init(&parser, filename)) {
		return 0;
	}

	Tree *tree = malloc(sizeof *tree);
	tree_init(tree);

	parser.tree = tree;
	advancep(&parser);
	while (!is_kind(parser.this_token, KIND_EOF)) {
		Node *statement = parse_statement(&parser);
		if (statement) {
			ASSERT(statement->kind = NODE_STATEMENT);
			if (statement->statement.kind != STATEMENT_EMPTY) {
				array_push(tree->statements, statement);
			}
		}
	}

	return tree;
}