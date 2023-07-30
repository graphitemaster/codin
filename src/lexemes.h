#ifndef KIND
#define KIND(...)
#endif

#ifndef ASSIGNMENT
#define ASSIGNMENT(...)
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

// Directives begin with '#'
#ifndef DIRECTIVE
#define DIRECTIVE(...)
#endif

// Attributes begin with '@'
#ifndef ATTRIBUTE
#define ATTRIBUTE(...)
#endif

#ifndef CCONVENTION
#define CCONVENTION(...)
#endif

// Token kinds
//   ENUM,         NAME,          ASI
KIND(INVALID,      "invalid",     false)
KIND(EOF,          "end of file", false)
KIND(COMMENT,      "comment",     false)
KIND(IDENTIFIER,   "identifier",  true)
KIND(LITERAL,      "literal",     true)
KIND(OPERATOR,     "operator",    false) // ASI handled by OPERATOR ASI column
KIND(KEYWORD,      "keyword",     false) // ASI handled by KEYWORD ASI column
KIND(ASSIGNMENT,   "assignment",  true)
KIND(DIRECTIVE,    "directive",   false) // '#' is a directive (not an operator)
KIND(ATTRIBUTE,    "attribute",   false) // '@' is a attribute (not an operator)
KIND(CONST,        "const",       false) // '$' is a constant (not an operator)
KIND(SEMICOLON,    "semicolon",   false) // ';' is a terminator
KIND(LBRACE,       "left brace",  false) // '{' is not an operator
KIND(RBRACE,       "right brace", true)  // '}' is not an operator
KIND(UNDEFINED,    "undefined",   true)  // '---' is not an operator

// Assignment tokens.
//
// These are different from operators because assignments are statements.
//
//   ENUM,         NAME
ASSIGNMENT(EQ,     "eq")     // '='
ASSIGNMENT(ADD,    "add")    // '+='
ASSIGNMENT(SUB,    "sub")    // '-='
ASSIGNMENT(MUL,    "mul")    // '*='
ASSIGNMENT(QUO,    "quot")   // '/='
ASSIGNMENT(MOD,    "mod")    // '%='
ASSIGNMENT(REM,    "rem")    // '%%='
ASSIGNMENT(AND,    "and")    // '&='
ASSIGNMENT(OR,     "or")     // '|='
ASSIGNMENT(XOR,    "xor")    // '~='
ASSIGNMENT(ANDNOT, "andnot") // '&~='
ASSIGNMENT(SHL,    "shl")    // '<<='
ASSIGNMENT(SHR,    "shr")    // '>>='
ASSIGNMENT(CMPAND, "and")    // '&&='
ASSIGNMENT(CMPOR,  "or")     // '||='

// Literal kinds
//      ENUM,      NAME
LITERAL(INTEGER,   "integer")
LITERAL(FLOAT,     "float")
LITERAL(IMAGINARY, "imaginary")
LITERAL(RUNE,      "rune")
LITERAL(STRING,    "string")

// Operators
//       ENUM,         MATCH,       PRECEDENCE,    NAMED, ASI
OPERATOR(NOT,          "!",         0,             false, false)
OPERATOR(POINTER,      "^",         0,             false, true)
OPERATOR(ARROW,        "->",        0,             false, false)
OPERATOR(LPAREN,       "(",         0,             false, false)
OPERATOR(RPAREN,       ")",         0,             false, true)
OPERATOR(LBRACKET,     "[",         0,             false, false)
OPERATOR(RBRACKET,     "]",         0,             false, true)
OPERATOR(COLON,        ":",         0,             false, false)
OPERATOR(PERIOD,       ".",         0,             false, false)
OPERATOR(COMMA,        ",",         0,             false, false)
OPERATOR(IN,           "in",        0,             true,  false)  // Produces a value, therefore an operator.
OPERATOR(NOT_IN,       "not_in",    0,             true,  false)  // Produces a value, therefore an operator.
OPERATOR(AUTO_CAST,    "auto_cast", 0,             true,  false)  // Produces a value, therefore an operator.
OPERATOR(CAST,         "cast",      0,             true,  false)  // Produces a value, therefore an operator.
OPERATOR(TRANSMUTE,    "transmute", 0,             true,  false)  // Produces a value, therefore an operator.
OPERATOR(OR_ELSE,      "or_else",   1,             true,  false)  // Produces a value, therefore an operator.
OPERATOR(OR_RETURN,    "or_return", 1,             true,  true)   // Produces a value, therefore an operator.
OPERATOR(QUESTION,     "?",         1,             false, true)
OPERATOR(ELLIPSIS,     "..",        2,             false, false)
OPERATOR(RANGEFULL,    "..=",       2,             false, false)
OPERATOR(RANGEHALF,    "..<",       2,             false, false)
OPERATOR(CMPOR,        "||",        3,             false, false)
OPERATOR(CMPAND,       "&&",        4,             false, false)
OPERATOR(CMPEQ,        "==",        5,             false, false)
OPERATOR(NOTEQ,        "!=",        5,             false, false)
OPERATOR(LT,           "<",         5,             false, false)
OPERATOR(GT,           ">",         5,             false, false)
OPERATOR(LTEQ,         "<=",        5,             false, false)
OPERATOR(GTEQ,         ">=",        5,             false, false)
OPERATOR(ADD,          "+",         6,             false, false)
OPERATOR(SUB,          "-",         6,             false, false)
OPERATOR(OR,           "|",         6,             false, false)
OPERATOR(XOR,          "~",         6,             false, false)
OPERATOR(QUO,          "/",         7,             false, false)
OPERATOR(MUL,          "*",         7,             false, false)
OPERATOR(MOD,          "%",         7,             false, false)
OPERATOR(MODMOD,       "%%",        7,             false, false)
OPERATOR(AND,          "&",         7,             false, false)
OPERATOR(ANDNOT,       "&~",        7,             false, false)
OPERATOR(SHL,          "<<",        7,             false, false)
OPERATOR(SHR,          ">>",        7,             false, false)

// Keywords
//      ENUM,        MATCH,            ASI
KEYWORD(IMPORT,      "import",         false)
KEYWORD(FOREIGN,     "foreign",        false)
KEYWORD(PACKAGE,     "package",        false)
KEYWORD(TYPEID,      "typeid",         true)
KEYWORD(WHERE,       "where",          false) 
KEYWORD(WHEN,        "when",           false) // Can be an operator in (x when y else z)
KEYWORD(IF,          "if",             false) // Can be an operator in (x if y else z)
KEYWORD(ELSE,        "else",           false)
KEYWORD(FOR,         "for",            false)
KEYWORD(SWITCH,      "switch",         false)
KEYWORD(DO,          "do",             false)
KEYWORD(CASE,        "case",           false)
KEYWORD(BREAK,       "break",          true)
KEYWORD(CONTINUE,    "continue",       true)
KEYWORD(FALLTHROUGH, "fallthrough",    true)
KEYWORD(DEFER,       "defer",          false)
KEYWORD(RETURN,      "return",         true)
KEYWORD(PROC,        "proc",           false)
KEYWORD(STRUCT,      "struct",         false)
KEYWORD(UNION,       "union",          false)
KEYWORD(ENUM,        "enum",           false)
KEYWORD(BIT_SET,     "bit_set",        false)
KEYWORD(MAP,         "map",            false)
KEYWORD(DYNAMIC,     "dynamic",        false)
KEYWORD(DISTINCT,    "distinct",       false)
KEYWORD(USING,       "using",          false)
KEYWORD(CONTEXT,     "context",        false)
KEYWORD(ASM,         "asm",            false)
KEYWORD(MATRIX,      "matrix",         false)

// Direcitves
//        ENUM,                     MATCH
DIRECTIVE(OPTIONAL_OK,              "optional_ok")
DIRECTIVE(OPTIONAL_ALLOCATOR_ERROR, "optional_allocator_error")
DIRECTIVE(BOUNDS_CHECK,             "bounds_check")
DIRECTIVE(NO_BOUNDS_CHECK,          "no_bounds_check")
DIRECTIVE(TYPE_ASSERT,              "type_assert")
DIRECTIVE(NO_TYPE_ASSERT,           "no_type_assert")
DIRECTIVE(ALIGN,                    "align")
DIRECTIVE(RAW_UNION,                "raw_union")
DIRECTIVE(PACKED,                   "packed")
DIRECTIVE(TYPE,                     "type")
DIRECTIVE(SIMD,                     "simd")
DIRECTIVE(SOA,                      "soa")
DIRECTIVE(PARTIAL,                  "partial")
DIRECTIVE(SPARSE,                   "sparse")
DIRECTIVE(RELATIVE,                 "relative")
DIRECTIVE(FORCE_INLINE,             "force_inline")
DIRECTIVE(FORCE_NO_INLINE,          "force_no_inline")
DIRECTIVE(NO_NIL,                   "no_nil")
DIRECTIVE(SHARED_NIL,               "shared_nil")
DIRECTIVE(NO_ALIAS,                 "no_alias")
DIRECTIVE(C_VARARG,                 "c_vararg")
DIRECTIVE(ANY_INT,                  "any_int")
DIRECTIVE(SUBTYPE,                  "subtype")
DIRECTIVE(BY_PTR,                   "by_ptr")
DIRECTIVE(ASSERT,                   "assert")
DIRECTIVE(PANIC,                    "panic")
DIRECTIVE(UNROLL,                   "unroll")
DIRECTIVE(LOCATION,                 "location")
DIRECTIVE(LOAD,                     "load")
DIRECTIVE(LOAD_HASH,                "load_hash")
DIRECTIVE(DEFINED,                  "defined")
DIRECTIVE(CONFIG,                   "config")
DIRECTIVE(MAYBE,                    "maybe")

/*
// This is actually impossible to lex within the Odin language because @ can
// introduce a single attribute (easy to lex), or it can introduce an attribute
// list which is a comma-separated list of attributes inside parenthesis. We
// just lex '@' to KIND_ATTRIBUTE and it's up to the parser to parse the list.
//
//        ENUM,                     MATCH,                    WHERE
ATTRIBUTE(TEST,                     "test",                   ATTRIBUTE_PROC)
ATTRIBUTE(EXPORT,                   "export",                 ATTRIBUTE_PROC | ATTRIBUTE_VAR)
ATTRIBUTE(LINKAGE,                  "linkage",                ATTRIBUTE_PROC | ATTRIBUTE_VAR)
ATTRIBUTE(REQUIRE,                  "require",                ATTRIBUTE_PROC | ATTRIBUTE_VAR)
ATTRIBUTE(INIT,                     "init",                   ATTRIBUTE_PROC)
ATTRIBUTE(DEFERRED,                 "deferred",               ATTRIBUTE_PROC)
ATTRIBUTE(DEFERRED_NONE,            "deferred_none",          ATTRIBUTE_PROC)
ATTRIBUTE(DEFERRED_IN,              "deferred_in",            ATTRIBUTE_PROC)
ATTRIBUTE(DEFERRED_OUT,             "deferred_out",           ATTRIBUTE_PROC)
ATTRIBUTE(DEFERRED_IN_OUT,          "deferred_in_out",        ATTRIBUTE_PROC)
ATTRIBUTE(LINK_NAME,                "link_name",              ATTRIBUTE_PROC | ATTRIBUTE_VAR)
ATTRIBUTE(LINK_PREFIX,              "link_prefix",            ATTRIBUTE_PROC)
ATTRIBUTE(DEPRECATED,               "deprecated",             ATTRIBUTE_PROC)
ATTRIBUTE(WARNING,                  "warning",                ATTRIBUTE_PROC)
ATTRIBUTE(REQUIRE_RESULTS,          "require_results",        ATTRIBUTE_PROC)
ATTRIBUTE(DISABLED,                 "disabled",               ATTRIBUTE_PROC)
ATTRIBUTE(COLD,                     "cold",                   ATTRIBUTE_PROC)
ATTRIBUTE(OPTIMIZATION_MODE,        "optimization_mode",      ATTRIBUTE_PROC)
ATTRIBUTE(OBJC_NAME,                "objc_name",              ATTRIBUTE_PROC)
ATTRIBUTE(OBJC_IS_CLASS_METHOD,     "objc_is_class_method",   ATTRIBUTE_PROC)
ATTRIBUTE(OBJC_TYPE,                "objc_type",              ATTRIBUTE_PROC)
ATTRIBUTE(REQUIRE_TARGET_FEATURE,   "require_target_feature", ATTRIBUTE_PROC)
ATTRIBUTE(ENABLE_TARGET_FEATURE,    "enable_target_feature",  ATTRIBUTE_PROC)
ATTRIBUTE(STATIC,                   "static",                 ATTRIBUTE_VAR)
ATTRIBUTE(THREAD_LOCAL,             "thread_local",           ATTRIBUTE_VAR)
ATTRIBUTE(LINK_SECTION,             "link_section",           ATTRIBUTE_VAR)
ATTRIBUTE(PRIVATE,                  "private",                ATTRIBUTE_PROC | ATTRIBUTE_CONST | ATTRIBUTE_TYPE)
ATTRIBUTE(OBJC_CLASS,               "objc_class",             ATTRIBUTE_TYPE)
*/

// Calling conventions
CCONVENTION("odin",        ODIN)
CCONVENTION("contextless", CONTEXTLESS)
CCONVENTION("cdecl",       CDECL)
CCONVENTION("stdcall",     STDCALL)
CCONVENTION("fastcall",    FASTCALL)
CCONVENTION("none",        NONE)
CCONVENTION("naked",       NAKED)
CCONVENTION("win64",       WIN64)
CCONVENTION("sysv",        SYSV)

// NOTE(dweiler): Actual types are not keywords because in Odin a type can be
// replaced within a package. This makes types regular identifiers.

#undef ATTRIBUTE
#undef DIRECTIVE
#undef KEYWORD
#undef OPERATOR
#undef LITERAL
#undef ASSIGNMENT
#undef KIND
#undef CCONVENTION