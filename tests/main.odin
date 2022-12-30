package main

import "core:fmt"

triangle :: #force_inline proc "contextless" (n: i64) {
	for m in 0..<n {
		y: i64 = n - 1 - m
		defer fmt.printf("\n")
		for in 0..<y do fmt.printf(" ")
		for x in 0..<n {
			if x & y != 0 do fmt.printf("  ")
			else          do fmt.printf("* ")
		}
	}
}

main :: proc() {
	triangle(16)
}
