#import "lib/str.zs"
#import "lib/io.zs"

foo :: struct{
    x: u32
    y: u32
    z: ^char
}
days :: enum{
    MONDAY,
    SUNDAY,
    TUESDAY,
}
macroExample :: macro{
    hehe := 32
    printf("printf from an AST level macro\n")
}

trial :: proc(f: ^foo){
    f@x = 3
    printf("f@x: %d\n", f@x)
}

main :: proc(){
    f: foo
    f.y = 3
    printf("%d\n", f.y)

    x := 6.9
    y := 0.1
    printf("%f\n", x+y)

    z: u32  = 65  //A
    f.z = $ &z

    mainProcPtr := #proc_ptr main
    v := f.z

    printf("%p\n%c\n", mainProcPtr, v[0])

    str:String = #fill "Hello, World"
    printf("%.*s\n", str.len, str.mem)

    trial(&f)
    printf("f.x: %d\n", f.x)

    printf("days\\SUNDAY -> %d\n", days\SUNDAY)

    #macro macroExample
    printf("%d", hehe)
}
