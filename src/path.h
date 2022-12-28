#ifndef CODIN_PATH_H
#define CODIN_PATH_H
#include "array.h"
#include "string.h"

Bool path_mkdir(const char *pathname);

Array(String) path_list(String path);

#endif // CODIN_PATH_H