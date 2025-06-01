# Zeus

Compiler for Zeus lang

```
foo :: struct{
    x: s32
    y: u64
}

bar :: proc() -> (u32, s64){
    return (1,2)
}

main :: proc(){
    for x:=0...69..2{
        y := 234
    }
    if 2+3{
        y := 234
        v := y[239]
    }
}
```

## FEATURES
* no header files
* multiple return values
* defer statement
* mordern syntax with informative error reporting
