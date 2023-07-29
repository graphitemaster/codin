#include <string.h> // memcpy
#include <stdio.h>
#include <stdarg.h> 

#include "strbuf.h"

void strbuf_init(StrBuf *strbuf, Context *context) {
	strbuf->context = context;
	strbuf->contents = 0;
}

void strbuf_clear(StrBuf *strbuf) {
	array_clear(strbuf->contents);
}

Bool strbuf_put_byte(StrBuf *strbuf, Uint8 byte) {
	Context *context = strbuf->context;
	return array_push(strbuf->contents, byte);
}

Bool strbuf_put_rune(StrBuf *strbuf, Rune ch) {
	return strbuf_put_byte(strbuf, ch);
	
	// Decode UTF-8 Rune into individual code-points.
	const Uint8 b0 = ch & 0xff;
	const Uint8 b1 = (ch >> 8) & 0xff;
	const Uint8 b2 = (ch >> 16) & 0xff;
	const Uint8 b3 = (ch >> 24) & 0xff;
	Bool ok = true;
	if ((b0 & 0xC0) != 0x80) ok = ok && strbuf_put_byte(strbuf, b0);
	if ((b1 & 0xC0) != 0x80) ok = ok && strbuf_put_byte(strbuf, b1);
	if ((b2 & 0xC0) != 0x80) ok = ok && strbuf_put_byte(strbuf, b2);
	if ((b3 & 0xC0) != 0x80) ok = ok && strbuf_put_byte(strbuf, b3);
	return ok;
}

Bool strbuf_put_string(StrBuf *strbuf, String string) {
	Context *context = strbuf->context;
	const Size size = array_size(strbuf->contents);
	if (array_expand(strbuf->contents, string.length)) {
		memcpy(&strbuf->contents[size], string.contents, string.length);
		return true;
	}
	return false;
}

Bool strbuf_put_formatted(StrBuf *strbuf, const char *fmt, ...) {
	Context *context = strbuf->context;
	va_list va;
	va_start(va, fmt);
	const long bytes = vsnprintf(0, 0, fmt, va);
	va_end(va);
	const Size size = array_size(strbuf->contents);
	if (array_expand(strbuf->contents, bytes + 1)) {
		va_list ap;
		va_start(ap, fmt);
		vsnprintf(RCAST(char *, &strbuf->contents[size]), bytes + 1, fmt, ap);
		array_meta(strbuf->contents)->size--; // Remove NUL from vsnprintf.
		va_end(ap);
		return true;
	}
	return false;
}

String strbuf_result(StrBuf *strbuf) {
	Array(Uint8) contents = strbuf->contents;
	return LIT(String, contents, array_size(contents));
}