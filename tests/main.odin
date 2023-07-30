package main

// type declarations, we can add these to a symbol table!
A :: struct($T: typeid) #align 16 { x: int, }
B :: enum { C, }
C :: bit_set[B]
D :: map[B]C
E :: [B]D
F :: distinct E
G :: proc() -> int
H :: []F
I :: matrix[1, 1]f32
J :: union {
  int,
}