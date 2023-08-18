#include <stdarg.h> // va_list, va_start, va_end
#include <stdlib.h> // abort
#include <stdio.h>  // fprintf, fputc

#include "report.h"
#include "support.h"
#include "string.h"
#include "lexer.h"
#include "strbuf.h"

NORETURN void report_assertion(const char *expression, const char *file, int line) {
	fprintf(stderr, "Assertion failed: %s:%d: %s\n", file, line, expression);
	abort();
}

void report_error(const Source *source, const Location *location, Context *context, const char *fmt, ...) {
	StrBuf buf;
	strbuf_init(&buf, context);
	strbuf_put_string(&buf, source->name);    // %.*s
	strbuf_put_rune(&buf, ':');               // :
	if (location) {
		strbuf_put_int(&buf, location->line);   // %d
		strbuf_put_int(&buf, location->column); // %d
	}
	strbuf_put_string(&buf, SCLIT(": ERROR "));
	va_list va;
	va_start(va, fmt);
	strbuf_put_fmtv(&buf, fmt, va);
	strbuf_put_rune(&buf, '\n');
	const String result = strbuf_result(&buf);
	fprintf(stderr, "%.*s", SFMT(result));
	strbuf_fini(&buf);
}

void report_warning(const Source *source, const Location *location, Context *context, const char *fmt, ...) {
	StrBuf buf;
	strbuf_init(&buf, context);
	strbuf_put_string(&buf, source->name);    // %.*s
	strbuf_put_rune(&buf, ':');               // :
	if (location) {
		strbuf_put_int(&buf, location->line);   // %d
		strbuf_put_int(&buf, location->column); // %d
	}
	strbuf_put_string(&buf, SCLIT(": WARNING "));
	va_list va;
	va_start(va, fmt);
	strbuf_put_fmtv(&buf, fmt, va);
	strbuf_put_rune(&buf, '\n');
	const String result = strbuf_result(&buf);
	fprintf(stderr, "%.*s", SFMT(result));
	strbuf_fini(&buf);
}