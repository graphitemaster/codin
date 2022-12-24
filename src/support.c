#include <string.h> // memcpy, strlen
#include <stdlib.h> // malloc, free

#include "support.h"

Bool string_compare(const String *lhs, const String *rhs) {
	return lhs->size == rhs->size && memcmp(lhs->data, rhs->data, lhs->size) == 0;
}

Bool string_assign(String *string, const char *source) {
	const Uint64 length = strlen(source);
	void *data = malloc(length);
	if (!data) return false;
	memcpy(data, source, length);
	string->data = CAST(Uint8*, data);
	string->size = length;
	return true;
}

void string_free(String *string) {
	free(string->data);
	string->data = 0;
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