#include "path.h"

#if defined(OS_WINDOWS)
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#else
#include <sys/stat.h> // mkdir
#endif

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