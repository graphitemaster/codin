#ifndef CODIN_STRBUF_H
#define CODIN_STRBUF_H
#include <stdarg.h>

#include "string.h"
#include "array.h"

typedef struct Context Context;
typedef struct StrBuf StrBuf;

struct StrBuf {
	Context *context;
	Array(Uint8) contents;
};

void strbuf_init(StrBuf *strbuf, Context *context);
void strbuf_fini(StrBuf *strbuf);
void strbuf_clear(StrBuf *strbuf);
void strbuf_put_rune(StrBuf *strbuf, Rune ch);
void strbuf_put_string(StrBuf *strbuf, String string);
void strbuf_put_int(StrBuf *strbuf, Sint32 i, Sint32 base);
void strbuf_put_fmt(StrBuf *strbuf, const char *fmt, ...);
void strbuf_put_fmtv(StrBuf *strbuf, const char *fmt, va_list va);
String strbuf_result(StrBuf *strbuf);

#endif // CODIN_STRBUF