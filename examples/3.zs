#import "lib/str.zs"
#import "lib/io.zs"

foo :: struct{
    x: u32
    y: u32
    z: ^char
}

main :: proc(){
    f: foo
    f.y = 3
    printf("%d\n", f.y)

    x := 6.9
    y := 0.1
    printf("%f\n", x+y)

    z: u32  = 65 
    f.z = $ &z

    mainProcPtr := #proc_ptr main
    v := f.z

    printf("%p\n%c\n", mainProcPtr, v[0])

    str:String = #fill "Hello, World"
    printf("%.*s", str.len, str.mem)
}
