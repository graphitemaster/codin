#ifndef CODIN_STRING_H
#define CODIN_STRING_H
#include "support.h"

typedef struct String String;

// This string isn't NUL terminated.
struct String {
	Uint8* contents;
	Uint64 length;
};

// Define a literal initializer for a String. This does not produce a compound-
// literal though, use SCLIT for that. The reason for this distinction has to
// do with static initializers, the use of a compound-literal is not allowed in
// a static initializer because a compound-literal isn't considered a constant
// expression, while an aggregate initializer-list of a static storage String is.
//
// Use this for static String or string arrays.
#define SLIT(content) \
	{ CAST(Uint8*, content), sizeof(content) - 1 }

// Use this anywhere SLIT won't work, like taking the address of a String or
// passing a String to a function.
#define SCLIT(content) \
	((String) SLIT(content))

#define SFMT(string) \
	CAST(Sint32, string.length), CAST(const char *, string.contents)

extern const String STRING_NIL;

String string_from_data(const Uint8 *data, Uint64 size);
String string_from_null(const char *string);
String string_copy(String string);
Bool string_compare(String lhs, String rhs);
String string_unquote(String string, const char *quote_set);
void string_free(String string);
char* string_to_null(String string);

Bool utf8_to_utf16(const char *source, Uint16 **const destination);

#endif // CODIN_STRING_H