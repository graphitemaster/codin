#ifndef CODIN_PATH_H
#define CODIN_PATH_H
#include "array.h"
#include "string.h"

typedef struct Context Context;

Bool _path_mkdir(const char *pathname, Context *context);

Array(String) _path_list(String path, Context *context);

#endif // CODIN_PATH_H