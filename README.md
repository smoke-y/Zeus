# The Zeus Programming Language

<img src="logo.jpeg">

```
swap :: proc(x,y :u32) -> (u32, u32){
    return y, x
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
