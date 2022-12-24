# codin

An Odin to C compiler written in C.

### Example
```odin
main :: proc() -> i32 {
  x, y, z: i32 = 10, 20, 30;
  s: string = "world";
  fmt.printf("Hello %s %d\n", s, -x + y * z - 1);
}
```

Generates the following C0.

```c
typedef int i32;
typedef const char *string;

#if defined(_MSC_VER)
        #define FORCE_INLINE __forceinline
#else
        #define FORCE_INLINE __attribute__((always_inline)) inline
#endif

FORCE_INLINE i32 negi32(i32 value) {
        return -value;
}
FORCE_INLINE i32 addi32(i32 lhs, i32 rhs) {
        return lhs + rhs;
}
FORCE_INLINE i32 subi32(i32 lhs, i32 rhs) {
        return lhs - rhs;
}
FORCE_INLINE i32 muli32(i32 lhs, i32 rhs) {
        return lhs * rhs;
}
i32 main() {
        i32 x = 10;
        i32 y = 20;
        i32 z = 30;
        string s = "world";
        printf("Hello %s %d\n", s, subi32(addi32(negi32(x), muli32(y, z)), 1));
}

```

### Building

To build on Linux
```
$ make
```

> On Linux you have some additional build options provided by the Makefile. Check the documentation at the top of the `Makefile` for them.

To build on Windows
```
$ cl.exe unity.c
```

### Running
```
$ ./codin run tests/main.odin -file
```

### Documentation
Before contributing please read the [C0.md](doc/C0.md) and [STYLE.md](doc/STYLE.md) documents in the `doc` directory.

### License
Licensed under the MIT license. View copyright information [here](doc/LICENSE.md)