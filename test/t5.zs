#import "lib/stdio.zs"

swap :: proc(x,y: u32) -> (u32, u32){
    return y, x
}

main :: proc(){
    x: u32  = 9
    y: u32 = 6
    z,v := swap(x,y)
    printf("%d%d", z, v)
}
