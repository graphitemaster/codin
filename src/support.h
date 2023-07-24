#ifndef CODIN_SUPPORT_H
#define CODIN_SUPPORT_H
#include <stdint.h>
#include <stddef.h>

#if defined(_WIN32)
	#define OS_WINDOWS
#else
	#define OS_POSIX
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

#if defined(__cplusplus)
	#define STATIC_ASSERT(expr, message) static_assert((expr), message)
#else
	#define STATIC_ASSERT(expr, message) _Static_assert((expr), message)
#endif

// Easier to search for.
#if defined(__cplusplus)
	#define CAST(T, expr) static_cast<T>(expr)
	#define RCAST(T, expr) reinterpret_cast<T>(expr)
	#define CCAST(T, expr) const_cast<T>(expr)
#else
	#define CAST(T, expr) ((T)(expr))
	#define RCAST(T, expr) ((T)(expr))
	#define CCAST(T, expr) ((T)(expr))
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

#if defined(__cplusplus)
typedef bool Bool;
#else
typedef _Bool Bool;
#define true  CAST(Bool, 1)
#define false CAST(Bool, 0)
#endif

typedef size_t Size;

typedef int32_t Rune; // Unicode codepoint.

#define RUNE_MAX CAST(Rune, 0x0010ffff)
#define RUNE_BOM CAST(Rune, 0xfeff)
#define RUNE_EOF CAST(Rune, -1)

NORETURN void report_assertion(const char *expression, const char *file, int line);

#define ASSERT(expression) \
  ((void)((expression) ? (void)0 : report_assertion(#expression, __FILE__, __LINE__)))

// Support bitwise operators on enumerators in C++ like C.
#if defined(__cplusplus)
template<typename T>
constexpr T operator~ (T a) noexcept {
	return CAST(T, ~CAST(int, a));
}
template<typename T>
constexpr T operator| (T a, T b) noexcept { 
	return CAST(T, CAST(int, a) | CAST(int, b));
}
template<typename T>
constexpr T operator& (T a, T b) noexcept { 
	return CAST(T, CAST(int, a) & CAST(int, b));
}
template<typename T>
constexpr T operator^ (T a, T b) noexcept { 
	return CAST(T, CAST(int, a) ^ CAST(int, b));
}
template<typename T>
constexpr T& operator|= (T& a, T b) noexcept {
	return RCAST(T&, RCAST(int&, a) |= CAST(int, b));
}
template<typename T>
constexpr T& operator&= (T& a, T b) noexcept {
	return RCAST(T&, RCAST(int&, a) &= CAST(int, b));
}
template<typename T>
constexpr T& operator^= (T& a, T b) noexcept {
	return RCAST(T&, RCAST(int&, a) ^= CAST(int, b));
}
#endif

#endif // CODIN_SUPPORT_H