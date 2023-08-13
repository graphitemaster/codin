#include <stdlib.h> // free
#include <string.h> // strcmp

#include "path.h"
#include "context.h"
#include "strbuf.h"

#if defined(OS_POSIX)
#include <sys/stat.h> // mkdir
#include <dirent.h> // DIR, dirent, readdir
#elif defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

Bool path_mkdir(const char *pathname, Context *context) {
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

Bool dir_list_r_impl(String path, Array(String) *results, Context *context) {
	DIR *dp = opendir(string_to_null(path));
	if (!dp) {
		return false;
	}
	for (struct dirent *de = readdir(dp); de; de = readdir(dp)) {
		const char *name = de->d_name;
		if (!strcmp(name, ".") || !strcmp(name, "..") || de->d_type != DT_DIR) {
			continue;
		}
		StrBuf buf;
		strbuf_init(&buf, context);
		strbuf_put_string(&buf, path);
		strbuf_put_rune(&buf, '/');
		strbuf_put_string(&buf, string_from_null(name));
		const String result = strbuf_result(&buf);
		array_push(*results, result);
		if (!dir_list_r_impl(result, results, context)) {
			goto L_error;
		}
	}

	closedir(dp);
	return true;

L_error:
	closedir(dp);
	return false;
}

Array(String) dir_list_r(String path, Context *context) {
	Array(String) results = array_make(context);
	if (dir_list_r_impl(path, &results, context)) {
		return results;
	}
	array_free(results);
	return 0;
}

Array(String) path_list(String path, Context *context) {
	Array(String) results = array_make(context);
#if defined(OS_POSIX)
	DIR *dp = opendir(string_to_null(path));
	if (!dp) {
		return 0;
	}
	for (struct dirent *de = readdir(dp); de; de = readdir(dp)) {
		const char *name = de->d_name;
		if (!strcmp(name, ".") || !strcmp(name, "..") || de->d_type == DT_DIR) {
			continue;
		}
		array_push(results, string_copy_from_null(name));
	}
	closedir(dp);
	return results;
#elif defined(OS_WINDOWS)
	(void)path;
	(void)context;
	// TODO(dweiler): Implement.
#endif
	return results;
}