#ifndef KIND
#define KIND(...)
#endif

#ifndef LITERAL
#define LITERAL(...)
#endif

#ifndef OPERATOR
#define OPERATOR(...)
#endif

#ifndef KEYWORD
#define KEYWORD(...)
#endif

// Token kinds
//   ENUM,         NAME
KIND(INVALID,      "invalid")
KIND(EOF,          "end of file")
KIND(COMMENT,      "comment")
KIND(IDENTIFIER,   "identifier")
KIND(LITERAL,      "literal")
KIND(OPERATOR,     "operator")
KIND(KEYWORD,      "keyword") 
KIND(HASH,         "directive")   // '#' is a directive (not an operator)
KIND(ATTRIBUTE,    "attribute")   // '@' is a attribute (not an operator)
KIND(SEMICOLON,    "semicolon")   // ';' is a terminator
KIND(LBRACE,       "left brace")  // '{' is not an operator
KIND(RBRACE,       "right brace") // '}' is not an operator

// Literal kinds
//      ENUM,      NAME
LITERAL(INTEGER,   "integer")
LITERAL(FLOAT,     "float")
LITERAL(IMAGINARY, "imaginary")
LITERAL(RUNE,      "rune")
LITERAL(STRING,    "string")

// Operators
//       ENUM,         MATCH, PRECEDENCE
//
// Should '=' be an operator? Maybe make it a NODE_ASSIGNMENT_STATEMENT since
// it does not produce a value.
OPERATOR(EQ,           "=",   0)
OPERATOR(NOT,          "!",   0)
OPERATOR(POINTER,      "^",   0)
OPERATOR(QUESTION,     "?",   1)
OPERATOR(ELLIPSIS,     "..",  2)
OPERATOR(RANGEFULL,    "..=", 2)
OPERATOR(RANGEHALF,    "..<", 2)
OPERATOR(CMPOR,        "||",  3)
OPERATOR(CMPAND,       "&&",  4)
OPERATOR(CMPEQ,        "==",  5)
OPERATOR(NOTEQ,        "!=",  5)
OPERATOR(LT,           "<",   5)
OPERATOR(GT,           ">",   5)
OPERATOR(LTEQ,         "<=",  5)
OPERATOR(GTEQ,         ">=",  5)
OPERATOR(ADD,          "+",   6)
OPERATOR(SUB,          "-",   6)
OPERATOR(OR,           "|",   6)
OPERATOR(XOR,          "~",   6)
OPERATOR(MUL,          "*",   7)
OPERATOR(QUO,          "/",   7)
OPERATOR(MOD,          "%",   7)
OPERATOR(MODMOD,       "%%",  7)
OPERATOR(AND,          "&",   7)
OPERATOR(ANDNOT,       "&~",  7)
OPERATOR(SHL,          "<<",  7)
OPERATOR(SHR,          ">>",  7)
// Compound operator precedence doesn't matter since these do not produce values,
// should these be made into a special NODE_COMPONENT_ASSIGNMENT_STATEMENT.
OPERATOR(ADDEQ,        "+=",        0)
OPERATOR(SUBEQ,        "-=",        0)
OPERATOR(MULEQ,        "*=",        0)
OPERATOR(QUOEQ,        "/=",        0)
OPERATOR(MODEQ,        "%=",        0)
OPERATOR(MODMODEQ,     "%%=",       0)
OPERATOR(ANDEQ,        "&=",        0)
OPERATOR(OREQ,         "|=",        0)
OPERATOR(XOREQ,        "~=",        0)
OPERATOR(ANDNOTEQ,     "&~=",       0)
OPERATOR(SHLEQ,        "<<=",       0)
OPERATOR(SHREQ,        ">>=",       0)
OPERATOR(CMPANDEQ,     "&&=",       0)
OPERATOR(CMPOREQ,      "||=",       0)
// These should all have precedence 0.
OPERATOR(ARROW,        "->",        0)
OPERATOR(OPENPAREN,    "(",         0)
OPERATOR(CLOSEPAREN,   ")",         0)
OPERATOR(OPENBRACKET,  "[",         0)
OPERATOR(CLOSEBRACKET, "]",         0)
OPERATOR(COLON,        ":",         0)
OPERATOR(PERIOD,       ".",         0)
OPERATOR(COMMA,        ",",         0)
OPERATOR(IN,           "in",        0) // Produces a value, therefor an operator.
OPERATOR(NOT_IN,       "not_in",    0) // Produces a value, therefor an operator.
OPERATOR(AUTO_CAST,    "auto_cast", 0) // Produces a value, therefor an operator.
OPERATOR(CAST,         "cast",      0) // Produces a value, therefor an operator.
OPERATOR(TRANSMUTE,    "transmute", 0) // Produces a value, therefor an operator.
OPERATOR(OR_ELSE,      "or_else",   0) // Produces a value, therefor an operator.
OPERATOR(OR_RETURN,    "or_return", 0) // Produces a value, therefor an operator.

// Keywords
//      ENUM,        MATCH
KEYWORD(IMPORT,      "import")
KEYWORD(FOREIGN,     "foreign")
KEYWORD(PACKAGE,     "package")
KEYWORD(TYPEID,      "typeid")
KEYWORD(WHERE,       "where") 
KEYWORD(WHEN,        "when")   // Can be an operator in (x when y else z)
KEYWORD(IF,          "if")     // Can be an operator in (x if y else z)
KEYWORD(ELSE,        "else")   // Can be an operator in above examples.
KEYWORD(FOR,         "for")
KEYWORD(SWITCH,      "switch")
KEYWORD(DO,          "do")
KEYWORD(CASE,        "case")
KEYWORD(BREAK,       "break")
KEYWORD(CONTINUE,    "continue")
KEYWORD(FALLTHROUGH, "fallthrough")
KEYWORD(DEFER,       "defer")
KEYWORD(RETURN,      "return")
KEYWORD(PROC,        "proc")
KEYWORD(STRUCT,      "struct")
KEYWORD(UNION,       "union")
KEYWORD(ENUM,        "enum")
KEYWORD(BIT_SET,     "bit_set")
KEYWORD(MAP,         "map")
KEYWORD(DYNAMIC,     "dynamic")
KEYWORD(DISTINCT,    "distinct")
KEYWORD(USING,       "using")
KEYWORD(CONTEXT,     "context")
KEYWORD(ASM,         "asm")
KEYWORD(MATRIX,      "matrix")

// NOTE(dweiler): Actual types are not keywords because in Odin a type can be
// replaced within a package. This makes types regular identifiers.

#undef KEYWORD
#undef OPERATOR
#undef LITERAL
#undef KIND