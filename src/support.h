#ifndef CODIN_SUPPORT_H
#define CODIN_SUPPORT_H
#include <stdint.h>
#include <stdbool.h>

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
	#define MAYBE_UNUSED  [[maybe_unused]]
#elif defined(_MSC_VER)
	#define FALLTHROUGH()
	#define MAYBE_UNUSED
#else
	#define FALLTHROUGH() __attribute__((fallthrough))
	#define MAYBE_UNUSED  __attribute__((unused))
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

typedef struct String String;

// Non-null terminated string.
struct String {
	Uint8 *data;
	Uint64 size;
};

// Create a String from a string literal.
#define SLIT(content) \
	((const String){ .data = CAST(Uint8 *, content), .size = sizeof(content) - 1 })

Bool string_assign(String *string, const char *source);
void string_free(String *string);
Bool string_compare(const String *lhs, const String *rhs);

// Easier to search for.
#define CAST(T, expr) ((T)(expr))

#define RUNE_MAX CAST(Rune, 0x0010ffff)
#define RUNE_BOM CAST(Rune, 0xfeff)
#define RUNE_EOF CAST(Rune, -1)

#endif // CODIN_SUPPORT_H