#include <stdarg.h>
#include <stdio.h>

#include "report.h"

void report_error(const Source *source, const Location *location, const char *fmt, ...) {
	fprintf(stderr, "%.*s:%d:%d: ERROR ",
		CAST(int,         source->name.size),
		CAST(const char*, source->name.data),
		location->line,
		location->column);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fputc('\n', stderr);
}

void report_warning(const Source *source, const Location *location, const char *fmt, ...) {
	fprintf(stderr, "%.*s:%d:%d: WARNING ",
		CAST(int,         source->name.size),
		CAST(const char*, source->name.data),
		location->line,
		location->column);
	va_list va;
	va_start(va, fmt);
	vfprintf(stderr, fmt, va);
	va_end(va);
	fputc('\n', stderr);
}