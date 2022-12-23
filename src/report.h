#ifndef CODIN_REPORT_H
#define CODIN_REPORT_H
#include <stdnoreturn.h>
#include "lexer.h"

void report_error(const Source *source, const Location *location, const char *fmt, ...);
void report_warning(const Source *source, const Location *location, const char *fmt, ...);

#endif // CODIN_REPORT_H