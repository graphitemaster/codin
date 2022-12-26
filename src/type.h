#ifndef CODIN_TYPE_H
#define CODIN_TYPE_H
#include "support.h"

typedef enum TypeKind TypeKind;
typedef enum TypeFlag TypeFlag;

typedef struct TypeTrait TypeTrait;

enum TypeKind {
	TYPE_KIND_BOOL,       // b{8,16,32,64}, bool
	TYPE_KIND_INT,        // {i,u}{8,16,32,64,128}
	TYPE_KIND_FLOAT,      // f{16,32,64}
	TYPE_KIND_COMPLEX,    // complex{32,64,128}
	TYPE_KIND_QUATERNION, // quaternion{64,128,256}
	TYPE_KIND_MATRIX,     // matrix[0..3, 0..3]f{16,32,64}
	TYPE_KIND_STRING,     // string
	TYPE_KIND_RUNE,       // rune
	TYPE_KIND_POINTER,    // ^T, or [^]T, or rawptr
	TYPE_KIND_ARRAY,      // T[N], or T[enum], or [dynamic]T
	TYPE_KIND_SLICE,      // T[]
	TYPE_KIND_MAP,        // map[K,V]
	TYPE_KIND_STRUCT,     // struct
	TYPE_KIND_UNION,      // union
	TYPE_KIND_ENUM,       // enum
	TYPE_KIND_PROC,       // proc
	TYPE_KIND_BITSET,     // bit_set
};

enum TypeFlag {
	TYPE_FLAG_NONE     = 0,
	TYPE_FLAG_LE       = 1 << 0, // for TYPE_KIND_{BOOL,INT,FLOAT} only
	TYPE_FLAG_BE       = 1 << 1, // for TYPE_KIND_{BOOL,INT,FLOAT} only
	TYPE_FLAG_SIGNED   = 1 << 2, // for TYPE_KIND_INT only
	TYPE_FLAG_UNSIGNED = 1 << 3, // for TYPE_KIND_INT only
};

struct TypeTrait {
	String identifier;
	TypeKind kind;
	TypeFlag flags;
	int size;
};

static const TypeTrait TYPE_TRAITS[] = {
	{ SLIT("b8"),   TYPE_KIND_BOOL, TYPE_FLAG_NONE,     8   },
	{ SLIT("b16"),  TYPE_KIND_BOOL, TYPE_FLAG_NONE,     16  },
	{ SLIT("b32"),  TYPE_KIND_BOOL, TYPE_FLAG_NONE,     32  },
	{ SLIT("b64"),  TYPE_KIND_BOOL, TYPE_FLAG_NONE,     64  },

	{ SLIT("i8"),   TYPE_KIND_INT,  TYPE_FLAG_SIGNED,   8   },
	{ SLIT("i16"),  TYPE_KIND_INT,  TYPE_FLAG_SIGNED,   16  },
	{ SLIT("i32"),  TYPE_KIND_INT,  TYPE_FLAG_SIGNED,   32  },
	{ SLIT("i64"),  TYPE_KIND_INT,  TYPE_FLAG_SIGNED,   64  },
	{ SLIT("i128"), TYPE_KIND_INT,  TYPE_FLAG_SIGNED,   128 },

	{ SLIT("u8"),   TYPE_KIND_INT,  TYPE_FLAG_UNSIGNED, 8   },
	{ SLIT("u16"),  TYPE_KIND_INT,  TYPE_FLAG_UNSIGNED, 16  },
	{ SLIT("u32"),  TYPE_KIND_INT,  TYPE_FLAG_UNSIGNED, 32  },
	{ SLIT("u64"),  TYPE_KIND_INT,  TYPE_FLAG_UNSIGNED, 64  },
	{ SLIT("u128"), TYPE_KIND_INT,  TYPE_FLAG_UNSIGNED, 128 },

	{ SLIT("f16"),  TYPE_KIND_FLOAT, TYPE_FLAG_NONE,    16  },
	{ SLIT("f32"),  TYPE_KIND_FLOAT, TYPE_FLAG_NONE,    32  },
	{ SLIT("f64"),  TYPE_KIND_FLOAT, TYPE_FLAG_NONE,    64  },
};

// int, uint, bool, rune, byte

#endif // CODIN_TYPE_H