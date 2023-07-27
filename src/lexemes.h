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
//   ENUM,         NAME
KIND(INVALID,      "invalid")
KIND(EOF,          "end of file")
KIND(COMMENT,      "comment")
KIND(IDENTIFIER,   "identifier")
KIND(LITERAL,      "literal")
KIND(OPERATOR,     "operator")
KIND(KEYWORD,      "keyword")
KIND(ASSIGNMENT,   "assignment")
KIND(DIRECTIVE,    "directive")     // '#' is a directive (not an operator)
KIND(ATTRIBUTE,    "attribute")     // '@' is a attribute (not an operator)
KIND(SEMICOLON,    "semicolon")     // ';' is a terminator
KIND(LBRACE,       "left brace")    // '{' is not an operator
KIND(RBRACE,       "right brace")   // '}' is not an operator
KIND(UNDEFINED,    "undefined")     // '---' is not an operator

// Assignment tokens.
//
// These are different from operators because assignments are statements.
//
//   ENUM,           NAME
ASSIGNMENT(EQ,       "eq")     // '=' is n ot an operator
ASSIGNMENT(ADDEQ,    "add")    // '+=' is not an operator
ASSIGNMENT(SUBEQ,    "sub")    // '-=' is not an operator
ASSIGNMENT(MULEQ,    "mul")    // '*=' is not an operator
ASSIGNMENT(QUOEQ,    "quot")   // '/=' is not an operator
ASSIGNMENT(MODEQ,    "mod")    // '%=' is not an operator
ASSIGNMENT(MODMODEQ, "rem")    // '%%=' is not an operator
ASSIGNMENT(ANDEQ,    "and")    // '&=' is not an operator
ASSIGNMENT(OREQ,     "or")     // '|=' is not an operator
ASSIGNMENT(XOREQ,    "xor")    // '~=' is not an operator
ASSIGNMENT(ANDNOTEQ, "andnot") // '&~=' is not an operator
ASSIGNMENT(SHLEQ,    "lshift") // '<<=' is not an operator
ASSIGNMENT(SHREQ,    "rshift") // '>>=' is not an operator
ASSIGNMENT(CMPANDEQ, "andeq")  // '&&=' is not an operator
ASSIGNMENT(CMPOREQ,  "oreq")   // '||=' is not an operator

// Literal kinds
//      ENUM,      NAME
LITERAL(INTEGER,   "integer")
LITERAL(FLOAT,     "float")
LITERAL(IMAGINARY, "imaginary")
LITERAL(RUNE,      "rune")
LITERAL(STRING,    "string")

// Operators
//       ENUM,         MATCH,       PRECEDENCE
OPERATOR(NOT,          "!",         0)
OPERATOR(POINTER,      "^",         0)
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
OPERATOR(QUESTION,     "?",         1)
OPERATOR(ELLIPSIS,     "..",        2)
OPERATOR(RANGEFULL,    "..=",       2)
OPERATOR(RANGEHALF,    "..<",       2)
OPERATOR(CMPOR,        "||",        3)
OPERATOR(CMPAND,       "&&",        4)
OPERATOR(CMPEQ,        "==",        5)
OPERATOR(NOTEQ,        "!=",        5)
OPERATOR(LT,           "<",         5)
OPERATOR(GT,           ">",         5)
OPERATOR(LTEQ,         "<=",        5)
OPERATOR(GTEQ,         ">=",        5)
OPERATOR(ADD,          "+",         6)
OPERATOR(SUB,          "-",         6)
OPERATOR(OR,           "|",         6)
OPERATOR(XOR,          "~",         6)
OPERATOR(QUO,          "/",         7)
OPERATOR(MUL,          "*",         7)
OPERATOR(MOD,          "%",         7)
OPERATOR(MODMOD,       "%%",        7)
OPERATOR(AND,          "&",         7)
OPERATOR(ANDNOT,       "&~",        7)
OPERATOR(SHL,          "<<",        7)
OPERATOR(SHR,          ">>",        7)

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
DIRECTIVE(MAYBE,                    "maybe")
DIRECTIVE(NO_ALIAS,                 "no_alias")
DIRECTIVE(C_VARARG,                 "c_vararg")
DIRECTIVE(CONST,                    "const")
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