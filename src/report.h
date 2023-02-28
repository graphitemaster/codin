#ifndef CODIN_REPORT_H
#define CODIN_REPORT_H

_Noreturn void report_assertion(const char *expression, const char *file, int line);
void report_error(const Source *source, const Location *location, const char *fmt, ...);
void report_warning(const Source *source, const Location *location, const char *fmt, ...);

#endif // CODIN_REPORT_H