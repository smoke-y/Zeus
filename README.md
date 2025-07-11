# The Zeus Programming Language

<img src="logo.jpeg">

```
swap :: proc(x,y :u32) -> (u32, u32){
    return y, x
}

compTimeProc :: proc #comptime (){
    //This will print the license while compiling
    license("LICENSE.txt")

    log("This is being printed from a procedure being executed at compile time")
    log("The VM executing the bytecodes is bootstrapped and written in Zeus!")
}

#link "libc"

printf :: proc_decl(^char, ...)

main :: proc(){
    printf("Hello, World")
    defer printf("Bye, World!")

    for "outer-loop" g:=0...3{
        x := 0
        for {
            printf("%d %d\n", g, x)
                if x == 3{
                    g = g + 1
                    continue "outer-loop"
                }
            x = x + 1
        }
    }
}
```

read about it: http://smoke-y.github.io/articles/zeus.html
