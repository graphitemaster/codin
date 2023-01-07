#ifndef CODIN_PATH_H
#define CODIN_PATH_H
#include "array.h"
#include "string.h"

typedef struct Context Context;

Bool path_mkdir(const char *pathname);

Array(String) _path_list(String path, Context *context);

#define path_list(path) \
	_path_list((path), context)

#endif // CODIN_PATH_H