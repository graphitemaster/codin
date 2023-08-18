#include "path.h"
#include "context.h"
#include "strbuf.h"

#if defined(OS_POSIX)
#include <stdlib.h> // free, realpath
#include <sys/stat.h> // mkdir
#include <dirent.h> // DIR, dirent, readdir
#elif defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#endif

Bool path_mkdir(String pathname, Context *context) {
	char *terminated = string_to_null(pathname, context);
#if defined(OS_POSIX)
	(void)context;
	return mkdir(terminated, 0777) == 0;
#elif defined(OS_WINDOWS)
	Uint16 *pathname_utf16 = 0;
	utf8_to_utf16(terminated, &pathname_utf16, context);
	allocator_deallocate(&context->allocator, terminated);
	const Bool result = CreateDirectoryW(RCAST(LPCWSTR, pathname_utf16), 0);
	allocator_deallocate(&context->allocator, pathname_utf16);
	return result;
#else
	#error Missing implementation
#endif
	allocator_deallocate(&context->allocator, terminated);
	return false;
}

Bool path_directory_exists(String pathname, Context *context) {
	char *terminated = string_to_null(pathname, context);
#if defined(OS_POSIX)
	struct stat s = {};
	if (stat(terminated, &s) == 0) {
		allocator_deallocate(&context->allocator, terminated);
		return (s.st_mode & S_IFDIR);
	}
#elif defined(OS_WINDOWS)
	Uint16 *pathname_utf16 = 0;
	utf8_to_utf16(terminated, &pathname_utf16, context);
	allocator_deallocate(&context->allocator, terminated);
	const DWORD attributes = GetFileAttributesW(RCAST(LPCWSTR, pathname_utf16));
	allocator_deallocate(&context->allocator, pathname_utf16);
	return attributes != INVALID_FILE_ATTRIBUTES && (attributes & FILE_ATTRIBUTE_DIRECTORY);
#else
	#error Missing implementation
#endif
	allocator_deallocate(&context->allocator, terminated);
	return false;
}

Array(String) path_list(String path, Context *context) {
	Array(String) results = array_make(context);
#if defined(OS_POSIX)
	char *terminated = string_to_null(path, context);
	DIR *dp = opendir(terminated);
	allocator_deallocate(&context->allocator, terminated);
	if (!dp) {
		goto L_error;
	}
	for (struct dirent *de = readdir(dp); de; de = readdir(dp)) {
		const char *name = de->d_name;
		if (de->d_type == DT_DIR) {
			continue;
		}
		array_push(results, string_copy_from_null(name, context));
	}
	closedir(dp);
	return results;
#elif defined(OS_WINDOWS)
	const char WILDCARD[] = "\\*";
	char *terminated = allocator_allocate(&context->allocator, path.length + sizeof WILDCARD);
	memcpy(terminated, path.contents, path.length);
	memcpy(terminated + path.length, WILDCARD, sizeof WILDCARD);
	Uint16 *pathname_utf16 = 0;
	utf8_to_utf16(terminated, &pathname_utf16, context);
	allocator_deallocate(&context->allocator, terminated);
	WIN32_FIND_DATAW ent;
	HANDLE handle = FindFirstFileW(RCAST(LPCWSTR, pathname_utf16), &ent);
	allocator_deallocate(&context->allocator, pathname_utf16);
	if (handle == INVALID_HANDLE_VALUE) {
		goto L_error;
	}
	do {
		if (ent.dwFileAttributes & FILE_ATTRIBUTE_DIRECTORY) {
			continue;
		}
		char *pathname_utf8 = 0;
		utf16_to_utf8(RCAST(const Uint16 *, ent.cFileName), &pathname_utf8, context);
		array_push(results, string_from_null(pathname_utf8));
	} while (FindNextFileW(handle, &ent));
	FindClose(handle);
#else
	#error Missing implementation
#endif
	return results;

L_error:
	array_free(results);
	return 0;
}

String path_cat(String pathname, String filename, Context *context) {
	if (string_ends_with(pathname, SCLIT("/")) ||
	    string_ends_with(pathname, SCLIT("\\")))
	{
		pathname.length--;
	}

	const Size length = pathname.length + filename.length + 1;
	Uint8* data = allocator_allocate(&context->allocator, length);
	if (pathname.contents) {
		memcpy(data, pathname.contents, pathname.length);
	}
	data[pathname.length] = '/';
	memcpy(&data[pathname.length + 1], filename.contents, filename.length);
	return LIT(String, data, length);
}

String path_canonicalize(String pathname, Context *context) {
	char *terminated = string_to_null(pathname, context);
#if defined(OS_POSIX)
	char *resolved = realpath(terminated, 0);
	String result = string_copy_from_null(resolved, context);
	free(resolved);
	return result;
#elif defined(OS_WINDOWS)
	Allocator *const allocator = &context->allocator;
	// Convert pathname to UTF-16.
	Uint16 *pathname_utf16 = 0;
	utf8_to_utf16(terminated, &pathname_utf16, context);
	ULONG length = GetFullPathNameW(RCAST(LPCWSTR, pathname_utf16), 0, 0, 0);
	PWSTR buffer = allocator_allocate(allocator, length * sizeof(WCHAR));
	if (GetFullPathNameW(RCAST(LPCWSTR, pathname_utf16), length, buffer, 0)) {
		allocator_deallocate(allocator, pathname_utf16);
		char *pathname_utf8 = 0;
		utf16_to_utf8(RCAST(const Uint16 *, buffer), &pathname_utf8, context);
		allocator_deallocate(allocator, buffer);
		return string_from_null(pathname_utf8);
	}
	allocator_deallocate(allocator, pathname_utf16);
	allocator_deallocate(allocator, buffer);
#else
	#error Missing implementation
#endif
	return STRING_NIL;
}