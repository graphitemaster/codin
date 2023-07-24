#ifndef CODIN_STRING_H
#define CODIN_STRING_H
#include "support.h"

typedef struct Context Context;
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
	{ RCAST(Uint8*, CCAST(char*, content)), sizeof(content) - 1 }

// Use this anywhere SLIT won't work, like taking the address of a String or
// passing a String to a function.
#define SCLIT(content) \
	((String) SLIT(content))

#define SFMT(string) \
	CAST(Sint32, string.length), RCAST(const char *, string.contents)

extern const String STRING_NIL;

String _string_copy_from_data(const Uint8 *data, Uint64 size, Context *context);
String _string_copy_from_null(const char *string, Context *context);

String string_from_null(const char *string);

String _string_copy(String string, Context *context);
Bool string_compare(String lhs, String rhs);
String string_unquote(String string, const char *quote_set);
void _string_free(String string, Context *context);
char* _string_to_null(String string, Context *context);
Bool string_starts_with(String string, String prefix);
Bool string_ends_with(String string, String suffix);

Bool string_find_first_byte(String String, Uint8 byte, Uint64 *index);
Bool string_find_last_byte(String string, Uint8 byte, Uint64 *index);

String string_slice(String string, Uint64 from, Uint64 to);

Bool _utf8_to_utf16(const char *source, Uint16 **const destination, Context *context);

#define utf8_to_utf16(source, destination) \
	_utf8_to_utf16((source), (destination), context)

#define string_copy(string) \
	_string_copy((string), context)

#define string_copy_from_data(data, size) \
	_string_copy_from_data((data), (size), context)

#define string_copy_from_null(string) \
	_string_copy_from_null((string), context)

#define string_free(string) \
	_string_free((string), context)

#define string_to_null(string) \
	_string_to_null((string), context)

#endif // CODIN_STRING_H