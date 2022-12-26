main :: proc() -> i32 {
	x: f16 = 10;
	y: f16 = 10;
	z: f16 = x + y;
	fmt.printf("%f %s\n", f32(z), #load("tests/main.odin", cstring));
	return 0;
}