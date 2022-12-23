#include <string.h> // memcpy

#include "strbuf.h"

void strbuf_init(StrBuf *strbuf) {
	strbuf->contents = 0;
}

void strbuf_free(StrBuf *strbuf) {
	if (!strbuf) return;
	array_free(strbuf->contents);
}

Bool strbuf_put_byte(StrBuf *strbuf, Uint8 byte) {
	return array_push(strbuf->contents, byte);
}

Bool strbuf_put_rune(StrBuf *strbuf, Rune ch) {
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
	const Uint64 size = array_size(strbuf->contents);
	if (array_expand(strbuf->contents, string.size)) {
		memcpy(&strbuf->contents[size], string.data, string.size);
		return true;
	}
	return false;
}

String strbuf_result(StrBuf *strbuf) {
	return (String) {
		.data = strbuf->contents,
		.size = array_size(strbuf->contents)
	};
}