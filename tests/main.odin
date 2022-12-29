package foo

import "core:fmt"

triangle :: #force_inline proc(n: i32) {
	for y: i32 = n - 1; y >= 0; y -= 1 {
		defer fmt.printf("\n")
		for i: i32 = 0; i < y; i += 1 do fmt.printf(" ")
		for x: i32 = 0; x + y < n; x += 1 {
			if (x & y) != 0 {
				fmt.printf("  ")
			} else {
				fmt.printf("* ")
			}
		}
	}
}
main :: proc() {
	triangle(16)
}
