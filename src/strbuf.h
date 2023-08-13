#ifndef CODIN_STRBUF_H
#define CODIN_STRBUF_H
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
Bool strbuf_put_rune(StrBuf *strbuf, Rune ch);
Bool strbuf_put_string(StrBuf *strbuf, String string);
String strbuf_result(StrBuf *strbuf);
void strbuf_put_formatted(StrBuf *strbuf, const char *fmt, ...);

#endif // CODIN_STRBUF