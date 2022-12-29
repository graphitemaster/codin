package main

import "core:fmt"

foo :: proc() {
	fmt.printf("Hello ");
	bar();
}

bar :: proc() {
	fmt.printf("world\n");
}

main :: proc() {
	foo();
}