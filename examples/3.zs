#import "lib/io.zs"

foo :: struct{
    x: u32
    y: u32
}

main :: proc(){
    f: foo
    f.y = 3
    printf("%d\n", f.y)
}
