#ifndef CODIN_SUPPORT_H
#define CODIN_SUPPORT_H
#include <stdint.h>
#include <stdbool.h>

#if defined(_WIN32)
	#define OS_WINDOWS
#elif defined(__APPLE__)
	#define OS_APPLE
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

#define STRINGIFY(s) #s

#if defined(__GNUC__) || defined(__clang__)
#define GCC_CLANG_DIAGNOSTICS_PUSH() _Pragma("GCC diagnostic push")
#define GCC_CLANG_DIAGNOSTICS_POP() _Pragma("GCC diagnostic pop")
#define GCC_CLANG_DIAGNOSTICS_IGNORED(warning) \
	_Pragma(STRINGIFY(GCC diagnostic ignored warning))
#else
#define GCC_CLANG_DIAGNOSTICS_PUSH()
#define GCC_CLANG_DIAGNOSTICS_POP()
#define GCC_CLANG_DIAGNOSTICS_IGNORED(warning)
#endif // #if defined(__GNUC__) || defined(__clang__)

#ifdef __clang__
#define NULLABLE \
	GCC_CLANG_DIAGNOSTICS_PUSH() \
	GCC_CLANG_DIAGNOSTICS_IGNORED("-Wnullability-extension") \
	_Nullable \
	GCC_CLANG_DIAGNOSTICS_POP()

#define NONNULL \
	GCC_CLANG_DIAGNOSTICS_PUSH() \
	GCC_CLANG_DIAGNOSTICS_IGNORED("-Wnullability-extension") \
	_Nonnull \
	GCC_CLANG_DIAGNOSTICS_POP()
#else

#define NULLABLE
#define NONNULL

#endif // #ifdef __clang__

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

extern _Noreturn void report_assertion(const char *expression, const char *file, int line);

#define ASSERT(expression) \
	((void)((expression) ? (void)0 : report_assertion(#expression, __FILE__, __LINE__)))

#endif // CODIN_SUPPORT_H
