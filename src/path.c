#include <stdlib.h> // free
#include <string.h> // strcmp

#if defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/stat.h> // mkdir
#include <dirent.h> // DIR, dirent, readdir
#endif

#include <stdio.h> //
#include "path.h"
#include "context.h"

Bool path_mkdir(const char *pathname) {
#if defined(OS_WINDOWS)
	Uint16 *pathname_utf16 = 0;
	if (utf8_to_utf16(pathname, &pathname_utf16)) {
		const Bool result = CreateDirectoryW(pathname_utf16, 0);
		free(pathname_utf16);
		return result;
	} else {
		return false;
	}
#elif defined(OS_LINUX)
	return mkdir(pathname, 0777) == 0;
#else
	return false;
#endif
}

Array(String) _path_list(String path, Context *context) {
	Array(String) results = 0;
#if defined(OS_WINDOWS)
	// TODO(dweiler): Implement.
#elif defined(OS_LINUX)
	char *name = string_to_null(path);
	DIR *dp = opendir(name);
	context->allocator->deallocate(context->allocator, name);
	if (!dp) {
		return 0;
	}
	for (struct dirent *de = readdir(dp); de; de = readdir(dp)) {
		const char *name = de->d_name;
		if (!strcmp(name, ".") || !strcmp(name, "..")) {
			continue;
		}
		if (!array_push(results, string_copy_from_null(name))) {
			goto L_error;
		}
	}
	closedir(dp);
	return results;

L_error:
	closedir(dp);
	array_free(results);
#endif
	return 0;
}