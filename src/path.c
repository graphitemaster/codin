#include "support.h"
#include "path.h"
#include "context.h"

#include <stdlib.h> // free
#include <string.h> // strcmp

#if defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/stat.h> // mkdir
#include <dirent.h> // DIR, dirent, readdir
#endif // #if defined(OS_WINDOWS)

Bool _path_mkdir(const char *pathname, Context *context) {
#if defined(OS_WINDOWS)
	Uint16 *pathname_utf16 = 0;
	if (utf8_to_utf16(pathname, &pathname_utf16)) {
		const Bool result = CreateDirectoryW(pathname_utf16, 0);
		free(pathname_utf16);
		return result;
	} else {
		return false;
	}
#elif defined(OS_LINUX) || defined(OS_APPLE)
	return mkdir(pathname, 0777) == 0;
#else
	return false;
#endif
}

Array(String) _path_list(String path, Context *context) {
	Array(String) results = 0;
#if defined(OS_WINDOWS)
	// null terminated wildcard mask
	Allocator *allocator = context->allocator;
	char *mask = allocator->allocate(allocator, path.length + 3);
	if (!mask) {
		return 0;
	}
	memcpy(mask, path.contents, path.length);
	mask[path.length + 0] = '\\';
	mask[path.length + 1] = '*';
	mask[path.length + 2] = '\0';

	WIN32_FIND_DATA fd;
	HANDLE find = FindFirstFile(mask, &fd);
	context->allocator->deallocate(context->allocator, mask);
	if (find == INVALID_HANDLE_VALUE) {
		return 0;
	}

	do {
		const char *name = fd.cFileName;
		if (!strcmp(name, ".") || !strcmp(name, "..")) {
			continue;
		}
		if (!array_push(results, string_copy_from_null(name))) {
			goto L_error;
		}
	} while (FindNextFile(find, &fd));
L_return:
	FindClose(find);
	return results;
L_error:
	array_free(results);
	goto L_return;
#elif defined(OS_LINUX) || defined(OS_APPLE)
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
