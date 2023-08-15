#include <string.h> // memcpy, strlen, memcmp
#include <stdlib.h> // malloc, free

#include "string.h"
#include "context.h"
#include "allocator.h"

static void *our_memrrchr(const void *m, int c, size_t n) {
	const unsigned char *s = CAST(const unsigned char *, m);
	c = CAST(unsigned char, c);
	while (n--) if (s[n] == c) return CCAST(unsigned char *, s + n);
	return 0;
}

const String STRING_NIL = { 0, 0 };

String string_copy_from_data(const Uint8 *data, Size length, Context *context) {
	if (length == 0) {
		return STRING_NIL;
	}
	Allocator *const allocator = &context->allocator;
	Uint8 *const storage = allocator_allocate(allocator, length);
	if (!storage) {
		THROW(ERROR_OOM);
	}
	memcpy(storage, data, length);
	return LIT(String, storage, length);
}

String string_copy_from_null(const char *string, Context *context) {
	if (string == 0) {
		return STRING_NIL;
	}
	const Size length = strlen(string);
	return string_copy_from_data(RCAST(const Uint8 *, string), length, context);
}

String string_from_null(const char *string) {
	return LIT(String, RCAST(Uint8*, CCAST(char*, string)), strlen(string));
}

String string_copy(String string, Context *context) {
	return string_copy_from_data(string.contents, string.length, context);
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
		return LIT(String, string.contents + 1, string.length - 2);
	}
	return string;
}

void string_free(String string, Context *context) {
	Allocator *const allocator = &context->allocator;
	allocator_deallocate(allocator, string.contents);
}

char *string_to_null(String string, Context *context) {
	Allocator *const allocator = &context->allocator;
	const Size length = string.length;
	char *const result = allocator_allocate(allocator, length + 1);
	if (!result) {
		THROW(ERROR_OOM);
	}
	memcpy(result, string.contents, length);
	result[length] = '\0';
	return result;
}

Bool string_starts_with(String string, String prefix) {
	return string.length >= prefix.length &&
		memcmp(string.contents, prefix.contents, prefix.length) == 0;
}

Bool string_ends_with(String string, String suffix) {
	return string.length >= suffix.length &&
		memcmp(string.contents + (string.length - suffix.length), suffix.contents, suffix.length) == 0;
}

Bool string_find_first_byte(String string, Uint8 byte, Size *index) {
	const Uint8 *const find = CAST(const Uint8*, memchr(string.contents, byte, string.length));
	if (find) {
		*index = find - string.contents;
		return true;
	}
	return false;
}

Bool string_find_last_byte(String string, Uint8 byte, Size *index) {
	const Uint8 *const find = CAST(const Uint8*, our_memrrchr(string.contents, byte, string.length));
	if (find) {
		*index = find - string.contents;
		return true;
	}
	return false;
}

String string_slice(String string, Size from, Size len) {
	return LIT(String, string.contents + from, len);
}

static void utf8_to_utf16_core(const char *const source, Uint16 *destination, Size *const length) {
	Uint32 code_point = 0;
	Size elements = 0;
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

void utf8_to_utf16(const char *source, Uint16 **const destination, Context *context) {
	Size length = 0;
	utf8_to_utf16_core(source, 0, &length);

	Allocator *allocator = &context->allocator;
	Uint16 *dest = allocator_allocate(allocator, (length + 1) * sizeof *dest);
	if (!dest) {
		THROW(ERROR_OOM);
	}

	utf8_to_utf16_core(source, dest, 0);
	dest[length] = 0;
	*destination = dest;
}

static void utf16_to_utf8_core(const Uint16 *const source, char *destination, Size *const length) {
	Size elements = 0;
	Uint32 code_point = 0;
	for (const Uint16 *element = source; *element; element++) {
		if (*element >= 0xd800 && *element <= 0xdbff) {
			code_point = ((*element - 0xd800) << 10) + 0x10000;
		} else {
			if (*element >= 0xdc00 && *element <= 0xdfff) {
				code_point |= *element - 0xdc00;
			} else {
				code_point = *element;
			}
			if (code_point < 0x7f) {
				elements += 1;
				if (destination) {
					*destination++ = CAST(char, code_point);
				}
			} else if (code_point <= 0x7ff) {
				elements += 2;
				if (destination) {
					*destination++ = CAST(char, 0xc0 | ((code_point >> 6) & 0x1f));
					*destination++ = CAST(char, 0x80 | (code_point & 0x3f));
				}
			} else if (code_point <= 0xffff) {
				elements += 3;
				if (destination) {
					*destination++ = CAST(char, 0xe0 | ((code_point >> 12) & 0x0f));
					*destination++ = CAST(char, 0x80 | ((code_point >> 6) & 0x3f));
					*destination++ = CAST(char, 0x80 | (code_point & 0x3f));
				}
			} else {
				elements += 4;
				if (destination) {
					*destination++ = CAST(char, 0xf0 | ((code_point >> 18) & 0x07));
					*destination++ = CAST(char, 0x80 | ((code_point >> 12) & 0x3f));
					*destination++ = CAST(char, 0x80 | ((code_point >> 6) & 0x3f));
					*destination++ = CAST(char, 0x80 | (code_point & 0x3f));
				}
			}
			code_point = 0;
		}
	}
	if (length) {
		*length = elements;
	}
}

void utf16_to_utf8(const Uint16 *source, char **destination, Context *context) {
	Size length = 0;
	utf16_to_utf8_core(source, 0, &length);

	Allocator *const allocator = &context->allocator;
	char *dest = allocator_allocate(allocator, (length + 1) * sizeof *dest);
	if (!dest) {
		THROW(ERROR_OOM);
	}

	utf16_to_utf8_core(source, dest, 0);
	dest[length] = 0;
	*destination = dest;
}

// Copyright (c) 2008-2010 Bjoern Hoehrmann <bjoern@hoehrmann.de>
// See http://bjoern.hoehrmann.de/utf-8/decoder/dfa/ for details.
static const Uint8 UTF8_DECODE_LUT[] = {
	// The first part of the table maps bytes to character classes that
	// to reduce the size of the transition table and create bitmasks.
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
	 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
	 7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,  7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
	 8,8,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
	10,3,3,3,3,3,3,3,3,3,3,3,3,4,3,3, 11,6,6,6,5,8,8,8,8,8,8,8,8,8,8,8,

	// The second part is a transition table that maps a combination
	// of a state of the automaton and a character class to a state.
	 0,12,24,36,60,96,84,12,12,12,48,72, 12,12,12,12,12,12,12,12,12,12,12,12,
	12, 0,12,12,12,12,12, 0,12, 0,12,12, 12,24,12,12,12,12,12,24,12,24,12,12,
	12,12,12,12,12,12,12,24,12,12,12,12, 12,24,12,12,12,12,12,12,12,24,12,12,
	12,12,12,12,12,12,12,36,12,36,12,12, 12,36,12,12,12,12,12,36,12,36,12,12,
	12,36,12,12,12,12,12,12,12,12,12,12, 
};

Uint32 utf8_decode(Uint32 *state, Rune *codep, Uint32 byte) {
	const Uint32 type = UTF8_DECODE_LUT[byte];
	*codep = (*state != UTF8_ACCEPT) ?
	  (byte & 0x3fu) | (*codep << 6) :
	  (0xff >> type) & byte;
	*state = UTF8_DECODE_LUT[256 + *state + type];
	return *state;
}