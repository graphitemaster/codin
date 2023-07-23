#ifndef CODIN_SUPPORT_H
#define CODIN_SUPPORT_H
#include <stdint.h>
#include <stdbool.h>

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
	#define NORETURN      [[noreturn]]
#elif defined(_MSC_VER)
	#define FALLTHROUGH()
	#define NORETURN      __declspec(noreturn)
#else
	#define FALLTHROUGH() __attribute__((__fallthrough__))
	#define NORETURN      __attribute__((__noreturn__))
#endif

typedef int8_t Sint8;
typedef uint8_t Uint8;
typedef int16_t Sint16;
typedef uint16_t Uint16;
typedef int32_t Sint32;
typedef uint32_t Uint32;
typedef int64_t Sint64;
typedef uint64_t Uint64;

typedef Uint16 Float16;
typedef float Float32;
typedef double Float64;

typedef bool Bool;
typedef size_t Size;

typedef int32_t Rune; // Unicode codepoint.

// Easier to search for.
#define CAST(T, expr) ((T)(expr))

#define RUNE_MAX CAST(Rune, 0x0010ffff)
#define RUNE_BOM CAST(Rune, 0xfeff)
#define RUNE_EOF CAST(Rune, -1)

NORETURN void report_assertion(const char *expression, const char *file, int line);

#define ASSERT(expression) \
  ((void)((expression) ? (void)0 : report_assertion(#expression, __FILE__, __LINE__)))

#endif // CODIN_SUPPORT_H