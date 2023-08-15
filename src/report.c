#include <stdarg.h> // va_list, va_start, va_end
#include <stdlib.h> // abort
#include <stdio.h>  // fprintf, fputc

#include "report.h"
#include "support.h"
#include "string.h"
#include "lexer.h"

NORETURN void report_assertion(const char *expression, const char *file, int line) {
	fprintf(stderr, "Assertion failed: %s:%d: %s\n", file, line, expression);
	abort();
}

void report_error(const Source *source, const Location *location, const char *fmt, ...) {
	if (location) {
		fprintf(stderr, "%.*s:%d:%d: ERROR ", SFMT(source->name), location->line, location->column);
	} else {
		fprintf(stderr, "%.*s: ERROR ", SFMT(source->name));
	}
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fputc('\n', stderr);
}

void report_warning(const Source *source, const Location *location, const char *fmt, ...) {
	if (location) {
		fprintf(stderr, "%.*s:%d:%d: WARNING ", SFMT(source->name), location->line, location->column);
	} else {
		fprintf(stderr, "%.*s: WARNING ", SFMT(source->name));
	}
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fputc('\n', stderr);
}