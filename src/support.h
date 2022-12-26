#ifndef CODIN_SUPPORT_H
#define CODIN_SUPPORT_H
#include <stdint.h>
#include <stdbool.h>
#include <stdnoreturn.h>

#if defined(_WIN32)
	#define OS_WINDOWS
#else
	#define OS_LINUX
#endif

#if defined(_MSC_VER)
	#define FORCE_INLINE __forceinline
#else
	#define FORCE_INLINE __attribute__((always_inline)) inline
#endif

#if defined(_MSC_VER)
	#define UNREACHABLE() __assume(0)
#else
	#define UNREACHABLE() __builtin_unreachable()
#endif

#if defined(__cplusplus)
	#define FALLTHROUGH() [[fallthrough]]
#elif defined(_MSC_VER)
	#define FALLTHROUGH()
#else
	#define FALLTHROUGH() __attribute__((fallthrough))
#endif

typedef int8_t Sint8;
typedef uint8_t Uint8;
typedef int16_t Sint16;
typedef uint16_t Uint16;
typedef int32_t Sint32;
typedef uint32_t Uint32;
typedef int64_t Sint64;
typedef uint64_t Uint64;

typedef bool Bool;

typedef int32_t Rune; // Unicode codepoint.

// Easier to search for.
#define CAST(T, expr) ((T)(expr))

#define RUNE_MAX CAST(Rune, 0x0010ffff)
#define RUNE_BOM CAST(Rune, 0xfeff)
#define RUNE_EOF CAST(Rune, -1)

noreturn void report_assertion(const char *expression, const char *file, int line);

#define ASSERT(expression) \
  ((void)((expression) ? (void)0 : report_assertion(#expression, __FILE__, __LINE__)))

#endif // CODIN_SUPPORT_H