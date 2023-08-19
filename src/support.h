#ifndef CODIN_SUPPORT_H
#define CODIN_SUPPORT_H
#include <stdint.h>
#include <stddef.h>

// OS_{}
#if defined(_WIN32)
	#define OS_WINDOWS
#elif defined(__linux__)
	#define OS_POSIX
	#define OS_LINUX
#else
	#error Unsupported platform
#endif

// COMPILER_{}
#if defined(__clang__)
	#define COMPILER_CLANG
#elif defined(__GNUC__) || defined(__GNUG__)
	#define COMPILER_GCC
#elif defined(_MSC_VER)
	#define COMPILER_MSVC
#else
	#error Unsupported compiler
#endif

// ISA_{}
#if defined(__x86_64__) || defined(_M_X64)
	#define ISA_AMD64
#elif defined(__aarch64__)
	#define ISA_AARCH64
#else
	#error Unsupported architecture
#endif

// FORCE_INLINE
#if defined(COMPILER_MSVC)
	#define FORCE_INLINE __forceinline
#else
	#define FORCE_INLINE __attribute__((always_inline)) inline
#endif

// UNREACHABLE()
#if defined(COMPILER_MSVC)
	#define UNREACHABLE() __assume(0)
#else
	#define UNREACHABLE() __builtin_unreachable()
#endif

// FALLTHROUGH()
#if defined(COMPILER_MSVC)
	#define FALLTHROUGH()
#elif defined(__cplusplus)
	#define FALLTHROUGH() [[fallthrough]]
#else
	#define FALLTHROUGH() __attribute__((__fallthrough__))
#endif

// NORETURN
#if defined(COMPILER_MSVC)
	#define NORETURN      __declspec(noreturn)
#elif defined(__cplusplus)
	#define NORETURN      [[noreturn]]
#else
	#define NORETURN      __attribute__((__noreturn__))
#endif

// ALIGN(n)
#if defined(COMPILER_MSVC)
	#define ALIGN(n)      __declspec(align(n))
#elif defined(__cplusplus)
	#define ALIGN(n)      alignas(n)
#else
	#define ALIGN(n)      __attribute__((__aligned__(n)))
#endif

// LIT
#if defined(__cplusplus)
#define LIT(T, ...) (T { __VA_ARGS__ })
#else
#define LIT(T, ...) ((T) { __VA_ARGS__ })
#endif

// STATIC_ASSERT(expr, message)
#if defined(__cplusplus)
	#define STATIC_ASSERT(expr, message) \
		static_assert(expr, message)
#else
	#if __STDC_VERSION__ >= 201112L
		#define STATIC_ASSERT(expr, message) \
			_Static_assert(expr, message)
	#else
		#define STATIC_ASSERT(expr, ...) \
			typedef int static_assert_ ## __LINE__ [(expr) ? 1 : -1]
	#endif
#endif

// CAST(T, expr)
// RCAST(T, expr)
// CCAST(T, expr)
#if defined(__cplusplus)
	#define CAST(T, expr) static_cast<T>(expr)
	#define RCAST(T, expr) reinterpret_cast<T>(expr)
	#define CCAST(T, expr) const_cast<T>(expr)
#else
	#define CAST(T, expr) ((T)(expr))
	#define RCAST(T, expr) ((T)(expr))
	#define CCAST(T, expr) ((T)(expr))
#endif

// Integer types
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

typedef size_t Size;

// Bool
#if defined(__cplusplus)
typedef bool Bool;
#else
typedef _Bool Bool;
#define true  CAST(Bool, 1)
#define false CAST(Bool, 0)
#endif

// Rune
typedef int32_t Rune; // Unicode codepoint.
#define RUNE_MAX CAST(Rune, 0x0010ffff)
#define RUNE_BOM CAST(Rune, 0xfeff)
#define RUNE_EOF CAST(Rune, -1)

// ASSERT
NORETURN void report_assertion(const char *expression, const char *file, int line);
#define ASSERT(expression) \
  CAST(void, (expression) ? CAST(void, 0) : report_assertion(#expression, __FILE__, __LINE__))

// Ptr
#if defined(__cplusplus)
struct Ptr {
	constexpr Ptr(void *p = 0) noexcept : p{p} {}
	template <typename T> operator T*() const noexcept { return CAST(T*, p); };
	constexpr operator bool() const noexcept { return p; }
	void *p = 0;
};
#else
typedef void *Ptr;
#endif

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

#if defined(__clang__) && defined(__cplusplus)
#define THREAD_ATTRIBUTE(x) __attribute__((x))
#else
#define THREAD_ATTRIBUTE(x)
#endif

#define THREAD_GUARDED(...)     THREAD_ATTRIBUTE(guarded_by(__VA_ARGS__))
#define THREAD_CAPABILITY(...)  THREAD_ATTRIBUTE(capability(__VA_ARGS__))
#define THREAD_ACQUIRES(...)    THREAD_ATTRIBUTE(acquire_capability(__VA_ARGS__))
#define THREAD_RELEASES(...)    THREAD_ATTRIBUTE(release_capability(__VA_ARGS__))
#define THREAD_TRY_ACQUIRE(...) THREAD_ATTRIBUTE(try_acquire_capability(__VA_ARGS__))
#define THREAD_EXCLUDES(...)    THREAD_ATTRIBUTE(locks_excluded(__VA_ARGS__))
#define THREAD_REQUIRES(...)    THREAD_ATTRIBUTE(requires_capability(__VA_ARGS__))
#define THREAD_INTERNAL         THREAD_ATTRIBUTE(no_thread_safety_analysis)

#endif // CODIN_SUPPORT_H