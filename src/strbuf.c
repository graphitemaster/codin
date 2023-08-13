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

Bool strbuf_put_byte(StrBuf *strbuf, Uint8 byte) {
	return array_push(strbuf->contents, byte);
}

Bool strbuf_put_rune(StrBuf *strbuf, Rune ch) {
	// Decode Rune into individual UTF-8 bytes.
	if (ch <= 0x7f) {
		return strbuf_put_byte(strbuf, ch);
	} else if (ch <= 0x7ff) {
		return strbuf_put_byte(strbuf, (ch >> 6) & 0x1f)
		    && strbuf_put_byte(strbuf, ch & 0x3f);
	} else if (ch <= 0xffff) {
		return strbuf_put_byte(strbuf, ((ch >> 12) & 0x0f))
		    && strbuf_put_byte(strbuf, ((ch >> 6) & 0x3f))
		    && strbuf_put_byte(strbuf, ch & 0x3f);
	} else {
		return strbuf_put_byte(strbuf, ((ch >> 18) & 0x07))
		    && strbuf_put_byte(strbuf, ((ch >> 12) & 0x3f))
		    && strbuf_put_byte(strbuf, ((ch >> 6) & 0x3f))
		    && strbuf_put_byte(strbuf, ch & 0x3f);
	}
	UNREACHABLE();
}

Bool strbuf_put_string(StrBuf *strbuf, String string) {
	const Size size = array_size(strbuf->contents);
	if (array_expand(strbuf->contents, string.length)) {
		memcpy(&strbuf->contents[size], string.contents, string.length);
		return true;
	}
	return false;
}

void strbuf_put_formatted(StrBuf *strbuf, const char *fmt, ...) {
	va_list va;
	va_start(va, fmt);
	const long bytes = vsnprintf(0, 0, fmt, va);
	va_end(va);
	const Size size = array_size(strbuf->contents);
	array_expand(strbuf->contents, bytes + 1);
	va_list ap;
	va_start(ap, fmt);
	vsnprintf(RCAST(char *, &strbuf->contents[size]), bytes + 1, fmt, ap);
	array_meta(strbuf->contents)->size--; // Remove NUL from vsnprintf.
	va_end(ap);
}

String strbuf_result(StrBuf *strbuf) {
	Array(Uint8) contents = strbuf->contents;
	return LIT(String, contents, array_size(contents));
}