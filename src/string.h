#ifndef CODIN_STRING_H
#define CODIN_STRING_H
#include "support.h"

typedef struct Context Context;
typedef struct String String;

// This string isn't NUL terminated.
struct String {
	Uint8* contents;
	Size length;
};

// Define a literal initializer for a String. This does not produce a compound-
// literal though, use SCLIT for that. The reason for this distinction has to
// do with static initializers, the use of a compound-literal is not allowed in
// a static initializer because a compound-literal isn't considered a constant
// expression, while an aggregate initializer-list of a static storage String is.
//
// Use this for static String or string arrays.
#define SINIT(content) \
	RCAST(Uint8*, CCAST(char*, content)), sizeof(content) - 1
#define SLIT(content) \
	{ SINIT(content) }

// Use this anywhere SLIT won't work, like taking the address of a String or
// passing a String to a function.
#define SCLIT(content) \
	LIT(String, SINIT(content))

#define SFMT(string) \
	CAST(Sint32, string.length), RCAST(const char *, string.contents)

extern const String STRING_NIL;

String string_copy_from_data(const Uint8 *data, Size size, Context *context);
String string_copy_from_null(const char *string, Context *context);

String string_from_null(const char *string);

String string_copy(String string, Context *context);
Bool string_compare(String lhs, String rhs);
String string_unquote(String string, const char *quote_set);
void string_free(String string, Context *context);
char* string_to_null(String string, Context *context);
Bool string_starts_with(String string, String prefix);
Bool string_ends_with(String string, String suffix);

Bool string_find_first_byte(String String, Uint8 byte, Size *index);
Bool string_find_last_byte(String string, Uint8 byte, Size *index);

String string_slice(String string, Size from, Size len);

void utf8_to_utf16(const char *source, Uint16 **const destination, Context *context);
void utf16_to_utf8(const Uint16 *source, char **const destination, Context *context);

#define UTF8_ACCEPT 0
#define UTF8_REJECT 12

Uint32 utf8_decode(Uint32 *state, Rune *codep, Uint32 byte);

#endif // CODIN_STRING_H