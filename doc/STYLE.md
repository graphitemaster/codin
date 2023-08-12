# Style

This is the style guide of the c-odin compiler.

* C11
* Types `PascalCase`
* Functions `snake_case`
* Constants `TITLECASE`
* Soft 80-column limit
* Indent with tabs
* Align with spaces
* Assume allocations can fail and handle accordingly
* Put space between `if`, `switch`, and `for` and the expression
* The bracing style looks like (1TBS)
    ```c
    if (x) {

    } else {

    }
    ```
* Put case statements on the same column of switch
    ```c
    switch (x) {
    case A:
        foo();
        break;
    case B:
        bar();
        break;
    }
    ```
* Every statement should have `{}`
* Do not put `()` on `sizeof` operator unless needed
* Do not put `()` on `return`
* No dependencies