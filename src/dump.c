#include <stdio.h>
#include <assert.h>

#include "dump.h"

static void pad(int depth) {
	for (int i = 0; i < depth; i++) printf("..");
}

static void dump_leaf(const Leaf *leaf, int depth, Bool last);

static void dump_unary_expression(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	const String op = operator_to_string(leaf->as_unary_expression.op);
	printf("(unary '%.*s' ",
		CAST(int,         op.size),
		CAST(const char*, op.data));
	dump_leaf(leaf->as_unary_expression.expression, 0, true);
	printf(")");
}

static void dump_binary_expression(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	const String op = operator_to_string(leaf->as_binary_expression.op);
	printf("(binary '%.*s' ",
		CAST(int,         op.size),
		CAST(const char*, op.data));
	dump_leaf(leaf->as_binary_expression.lhs, 0, true);
	printf(" ");
	dump_leaf(leaf->as_binary_expression.rhs, 0, true);
	printf(")");
}

static void dump_cast_expression(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	printf("(cast ");
	dump_leaf(leaf->as_cast_expression.expression, 0, true);
	printf(")");
}

static void dump_selector_expression(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	printf("(selector ");
	dump_leaf(leaf->as_selector_expression.operand, 0, true);
	printf(" ");
	dump_leaf(leaf->as_selector_expression.identifier, 0, true);
	printf(")");
}

static void dump_implicit_selector_expression(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	printf("(implicit ");
	dump_leaf(leaf->as_selector_expression.operand, 0, true);
	printf(")");
}

static void dump_auto_cast_expression(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	printf("(autocast ");
	dump_leaf(leaf->as_auto_cast_expression.expression, depth, true);
	printf(")");
}

static void dump_call_expression(const Leaf *leaf, int depth) {
	if (!leaf) return;
	printf("(call ");
	dump_leaf(leaf->as_call_expression.operand, 0, false);
	pad(depth);
	const Uint64 n_args = array_size(leaf->as_call_expression.args);
	for (Uint64 i = 0; i < n_args; i++) {
		const Leaf *arg = leaf->as_call_expression.args[i];
		dump_leaf(arg, depth + 1, i == n_args - 1);
	}
	printf(")");
}


static void dump_empty_statement(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	printf("(empty)");
}

static void dump_block_statement(const Leaf *leaf, int depth) {
	if (!leaf) return;
	const Uint64 n_statements = array_size(leaf->as_block_statement.statements);
	if (n_statements == 0) {
		printf("(block <empty>)");
		return;
	}
	printf("(block\n");
	for (Uint64 i = 0; i < n_statements; i++) {
		const Leaf *next = leaf->as_block_statement.statements[i];
		dump_leaf(next, depth + 1, i == n_statements - 1);
	}
	printf(")");
}

static void dump_import_statement(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	const String import = leaf->as_import_statement.import;
	printf("(import '%.*s')",
		CAST(int,         import.size),
		CAST(const char*, import.data));
}

static void dump_expression_statement(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	printf("(expression ");
	dump_leaf(leaf->as_expression_statement.expression, 0, true);
	printf(")");
}

static void dump_assignment_statement(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	const String string = kind_to_string(leaf->as_assignment_statement.kind);
	const Uint64 n_lhs = array_size(leaf->as_assignment_statement.lhs);
	const Uint64 n_rhs = array_size(leaf->as_assignment_statement.rhs);
	assert(n_lhs == n_rhs);
	printf("(assignments\n");
	for (Uint64 i = 0; i < n_lhs; i++) {
		const Leaf *lhs = leaf->as_assignment_statement.lhs[i];
		const Leaf *rhs = leaf->as_assignment_statement.rhs[i];
		pad(depth);
		printf("(%.*s ",
			CAST(int,          string.size),
			CAST(const char *, string.data));
		dump_leaf(lhs, 0, true);
		printf(" ");
		dump_leaf(rhs, 0, i == n_lhs - 1);
	}
	printf(")");
}

static void dump_identifier(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	const String ident = leaf->as_identifier.contents;
	printf("(ident '%.*s')",
		CAST(int,         ident.size),
		CAST(const char*, ident.data));
}

static void dump_field_value(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	Leaf *field = leaf->as_field_value.field;
	Leaf *value = leaf->as_field_value.value;
	(void)field;
	(void)value;
	// printf("(field ")
}

static void dump_literal(const Leaf *leaf, int MAYBE_UNUSED depth) {
	if (!leaf) return;
	const String contents = leaf->as_literal.contents;
	switch (leaf->as_literal.literal) {
	case LITERAL_FLOAT:
		printf("(float '%.*s')",
			CAST(int,          contents.size),
			CAST(const char *, contents.data));
		break;
	case LITERAL_IMAGINARY:
		// TODO
	case LITERAL_INTEGER:
		printf("(int '%.*s')",
			CAST(int,         contents.size),
			CAST(const char*, contents.data));
		break;
	case LITERAL_RUNE:
		printf("(rune '%.*s')",
			CAST(int,         contents.size),
			CAST(const char*, contents.data));
		break;
	case LITERAL_STRING:
			printf("(string '%.*s')",
			CAST(int,         contents.size),
			CAST(const char*, contents.data));
		break;
	case LITERAL_COUNT:
		UNREACHABLE();
	}
}

static void dump_compound_literal(const Leaf *leaf, int depth) {
	if (!leaf) return;
	printf("(compound ");
	dump_leaf(leaf->as_compound_literal.type, depth + 1, true);
	const Uint64 n_elements = array_size(leaf->as_compound_literal.elements);
	for (Uint64 i = 0; i < n_elements; i++) {
		const Leaf *element = leaf->as_compound_literal.elements[i];
		dump_leaf(element, depth + 1, i == n_elements - 1);
	}
	printf(")");
}

static void dump_leaf(const Leaf *leaf, int depth, Bool last) {
	if (!leaf) return;
	pad(depth);
	switch (leaf->node) {
	case NODE_UNARY_EXPRESSION:
		dump_unary_expression(leaf, depth + 1);
		break;
	case NODE_BINARY_EXPRESSION:
		dump_binary_expression(leaf, depth + 1);
		break;
	case NODE_CAST_EXPRESSION:
		dump_cast_expression(leaf, depth + 1);
		break;
	case NODE_SELECTOR_EXPRESSION:
		dump_selector_expression(leaf, depth + 1);
		break;
	case NODE_IMPLICIT_SELECTOR_EXPRESSION:
		dump_implicit_selector_expression(leaf, depth + 1);
		break;
	case NODE_AUTO_CAST_EXPRESSION:
		dump_auto_cast_expression(leaf, depth + 1);
		break;
	case NODE_CALL_EXPRESSION:
		dump_call_expression(leaf, depth + 1);
		break;
	case NODE_EMPTY_STATEMENT:
		dump_empty_statement(leaf, depth + 1);
		break;
	case NODE_BLOCK_STATEMENT:
		dump_block_statement(leaf, depth + 1);
		break;
	case NODE_IMPORT_STATEMENT:
		dump_import_statement(leaf, depth + 1);
		break;
	case NODE_EXPRESSION_STATEMENT:
		dump_expression_statement(leaf, depth + 1);
		break;
	case NODE_ASSIGNMENT_STATEMENT:
		dump_assignment_statement(leaf, depth + 1);
		break;
	case NODE_IDENTIFIER:
		dump_identifier(leaf, depth + 1);
		break;
	case NODE_FIELD_VALUE:
		dump_field_value(leaf, depth + 1);
		break;
	case NODE_LITERAL:
		dump_literal(leaf, depth + 1);
		break;
	case NODE_COMPOUND_LITERAL:
		dump_compound_literal(leaf, depth + 1);
		break;
	}
	if (!last) {
		printf("\n");
	}
}

void tree_dump(Tree *tree) {
	printf("(tree\n");
	if (tree) {
		const Uint64 n_statements = array_size(tree->statements);
		for (Uint64 i = 0; i < n_statements; i++) {
			Leaf *leaf = tree->statements[i];
			dump_leaf(leaf, 1, i == n_statements - 1);
		}
	}
	printf(")\n");
}