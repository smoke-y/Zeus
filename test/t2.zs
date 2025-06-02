#import "test/t3.zs"
#import "lib/stdio.zs"

foo :: struct{
    x: s32
    y: s32
}
goo :: struct{
    f: foo
}

x: u32 = 2

main :: proc(){
    defer{
        g : u32 = 234 
        printf("DEFER statement baby %d :) ", g)
    }
    y: u32 = 4

    //from test/t3.zs
    f := gg(y)

    //from lib/stdio.zs
    printf("f: %d ", f)
}
