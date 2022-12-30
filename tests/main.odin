package main

import "core:fmt"

main :: proc() {
	for x in 0..=10 do fmt.printf("%d, ", x)
	fmt.printf("\n")
	for x in 0..<10 do fmt.printf("%d, ", x)
	fmt.printf("\n")
	for x: i64 = 0; x <= 10; x += 1 do fmt.printf("%d, ", x)
	fmt.printf("\n")
	for x: i64 = 0; x < 10; x += 1 do fmt.printf("%d, ", x)
	fmt.printf("\n")
	n: i32 = 16
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
