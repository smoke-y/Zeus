#import "test/t3.zs"

foo :: struct{
    x: s32
    y: s32
}
goo :: struct{
    f: foo
}

x: u32 = 2

main :: proc(x: s32) -> u32{
    y: u32 = 4
    return y
}
