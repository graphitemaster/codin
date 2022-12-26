#include <string.h> // memcpy, strlen, memcmp
#include <stdlib.h> // malloc, free

#include "string.h"

const String STRING_NIL = { 0, 0 };

String string_from_data(const Uint8 *data, Uint64 length) {
	if (length == 0) {
		return STRING_NIL;
	}
	Uint8 *storage = malloc(length);
	if (!storage) {
		return STRING_NIL;
	}
	memcpy(storage, data, length);	
	return (String) { storage, length };
}

String string_from_null(const char *string) {
	if (string == 0) {
		return STRING_NIL;
	}
	const Uint64 length = strlen(string);
	return string_from_data(CAST(const Uint8 *, string), length);
}

String string_copy(String string) {
	return string_from_data(string.contents, string.length);
}

Bool string_compare(String lhs, String rhs) {
	return lhs.length == rhs.length &&
		memcmp(lhs.contents, rhs.contents, lhs.length) == 0;
}

String string_unquote(String string, const char *quote_set) {
	if (string.length == 0) {
		return STRING_NIL;
	}
	const char *ch = strchr(quote_set, string.contents[0]);
	if (ch && string.contents[string.length - 1] == *ch) {
		return (String) { string.contents + 1, string.length - 2 };
	}
	return string;
}

void string_free(String string) {
	free(string.contents);
}

char* string_to_null(String string) {
	const Uint64 length = string.length;
	char *result = malloc(length + 1);
	if (result) {
		memcpy(result, string.contents, length);
		result[length] = '\0';
		return result;
	}
	return 0;
}

static void utf8_to_utf16_core(const char *const source, Uint16 *destination, Uint64 *const length) {
	Uint32 code_point = 0;
	Uint64 elements = 0;
	for (const char *element = source; *element; element++) {
		const Uint8 ch = CAST(Uint8, *element);
		if (ch <= 0x7f) {
			code_point = CAST(Uint16, ch);
		} else if (ch <= 0xbf) {
			code_point = (code_point << 6) | (ch & 0x3f);
		} else if (ch <= 0xdf) {
			code_point = ch & 0x1f;
		} else if (ch <= 0xef) {
			code_point = ch & 0x0f;
		} else {
			code_point = ch & 0x07;
		}
		if (((*element & 0xc0) != 0x80) && code_point <= 0x10ffff) {
			if (code_point > 0xffff) {
				elements += 2;
				if (destination) {
					*destination++ = CAST(Uint16, 0xd800 + (code_point >> 10));
					*destination++ = CAST(Uint16, 0xdc00 + (code_point & 0x03ff));
				}
			} else if (code_point < 0xd800 || code_point >= 0xe000) {
				elements += 1;
				if (destination) {
					*destination++ = CAST(Uint16, code_point);
				}
			}
		}
	}
	if (length) {
		*length = elements;
	}
}

Bool utf8_to_utf16(const char *source, Uint16 **const destination) {
	Uint64 length = 0;
	utf8_to_utf16_core(source, 0, &length);

	Uint16 *dest = malloc((length + 1) * sizeof(Uint16));
	if (!dest) {
		return false;
	}

	utf8_to_utf16_core(source, dest, 0);
	dest[length] = 0;
	*destination = dest;

	return true;
}