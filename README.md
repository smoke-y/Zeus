# The Zeus Programming Language

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

### About

A systems programming language aimed to fix shortcomings of C. Zeus supports <br>
<ul>
<li>compile time execution(limited)</li>
<li>named loops</li>
<li>defer</li>
<li>multiple return</li>
</ul>
and many more. read about it: http://smoke-y.github.io/articles/zeus.html

### Installation
Clone the repo and run the build script. Since the compiler is written from scratch, it has almost no dependencies. The compiler
directly outputs LLVM IR, hence you dont need to build LLVM. All you need is the clang compiler which will compile the Zeus compiler
and lower LLMV IR output by Zeus compiler.<br>

If you want compile time execution, you will have to compile https://github.com/smoke-y/Aetos using the Zeus compiler(pass the
dynamic library flag) and you can load the VM on your next compilation to execute Zeus bytecodes.
