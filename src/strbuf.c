#include <string.h> // memcpy
#include <stdio.h>
#include <stdarg.h> 

#include "strbuf.h"

void strbuf_init(StrBuf *strbuf, Context *context) {
	strbuf->context = context;
	strbuf->contents = array_make(context);
}

void strbuf_fini(StrBuf *strbuf) {
	array_free(strbuf->contents);
}

void strbuf_clear(StrBuf *strbuf) {
	array_clear(strbuf->contents);
}

void strbuf_put_byte(StrBuf *strbuf, Uint8 byte) {
	array_push(strbuf->contents, byte);
}

void strbuf_put_rune(StrBuf *strbuf, Rune ch) {
	// Decode Rune into individual UTF-8 bytes.
	if (ch <= 0x7f) {
		strbuf_put_byte(strbuf, ch);
	} else if (ch <= 0x7ff) {
		strbuf_put_byte(strbuf, (ch >> 6) & 0x1f);
		strbuf_put_byte(strbuf, ch & 0x3f);
	} else if (ch <= 0xffff) {
		strbuf_put_byte(strbuf, ((ch >> 12) & 0x0f));
		strbuf_put_byte(strbuf, ((ch >> 6) & 0x3f));
		strbuf_put_byte(strbuf, ch & 0x3f);
	} else {
		strbuf_put_byte(strbuf, ((ch >> 18) & 0x07));
		strbuf_put_byte(strbuf, ((ch >> 12) & 0x3f));
		strbuf_put_byte(strbuf, ((ch >> 6) & 0x3f));
		strbuf_put_byte(strbuf, ch & 0x3f);
	}
	UNREACHABLE();
}

void strbuf_put_string(StrBuf *strbuf, String string) {
	const Size size = array_size(strbuf->contents);
	array_expand(strbuf->contents, string.length);
	memcpy(&strbuf->contents[size], string.contents, string.length);
}

void strbuf_put_int(StrBuf *strbuf, Sint32 i, Sint32 base) {
	char buffer[sizeof(Sint32) * 8 + 1 + 1];
	static const char DIGITS[37] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	// Start filling from the end
	char* p = &buffer[sizeof buffer - 1];
	*p = '\0';

	// Work with negative `int`
	int an = i < 0 ? i : -i;
	do {
		*(--p) = DIGITS[-(an % base)];
		an /= base;
	} while (an);

	if (i < 0) {
		*(--p) = '-';
	}

	const Size used = &buffer[sizeof(buffer)] - p;
	const Size index = array_size(strbuf->contents);

	array_expand(strbuf->contents, used);

	memcpy(&strbuf->contents[index], p, used);
}

void strbuf_put_fmtv(StrBuf *strbuf, const char *fmt, va_list va) {
	const long bytes = vsnprintf(0, 0, fmt, va) + 1;

	const Size offset = array_size(strbuf->contents);
	array_expand(strbuf->contents, bytes);

	vsnprintf(RCAST(char *, &strbuf->contents[offset]), bytes, fmt, va);

	array_meta(strbuf->contents)->size--;
}

void strbuf_put_fmt(StrBuf *strbuf, const char *fmt, ...) {
	va_list va;
	va_start(va, fmt);
	strbuf_put_fmtv(strbuf, fmt, va);
	va_end(va);
}

String strbuf_result(StrBuf *strbuf) {
	Array(Uint8) contents = strbuf->contents;
	return LIT(String, contents, array_size(contents));
}