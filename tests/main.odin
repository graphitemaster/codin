package main

import "core:fmt"

main :: proc() {
	defer fmt.printf("The end\n");
	defer {
		fmt.printf("woah\n");
		fmt.printf("nice\n");
	}
	for x: i32 = 0; x < 10; x += 1 {
		fmt.printf("%d\n", x);
		if x > 1 do return;
	}
	defer fmt.printf("hi\n");
}
