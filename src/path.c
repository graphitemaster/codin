#include <stdlib.h> // free
#include <string.h> // strcmp

#include "path.h"
#include "context.h"

#if defined(OS_POSIX)
#include <sys/stat.h> // mkdir
#include <dirent.h> // DIR, dirent, readdir
#elif defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

Bool _path_mkdir(const char *pathname, Context *context) {
#if defined(OS_POSIX)
	(void)context;
	return mkdir(pathname, 0777) == 0;
#elif defined(OS_WINDOWS)
	Uint16 *pathname_utf16 = 0;
	if (utf8_to_utf16(pathname, &pathname_utf16)) {
		return CreateDirectoryW(RCAST(LPCWSTR, pathname_utf16), 0);
	} else {
		return false;
	}
#elif
	return false;
#endif
}

Array(String) _path_list(String path, Context *context) {
	Array(String) results = 0;
#if defined(OS_POSIX)
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
#elif defined(OS_WINDOWS)
	(void)path;
	(void)context;
	// TODO(dweiler): Implement.
#endif
	return results;
}