#ifndef CODIN_REPORT_H
#define CODIN_REPORT_H

typedef struct Source Source;
typedef struct Location Location;
typedef struct Context Context;

void report_error(const Source *source, const Location *location, Context *context, const char *fmt, ...);
void report_warning(const Source *source, const Location *location, Context *context, const char *fmt, ...);

#endif // CODIN_REPORT_H