#import "lib/stdio.zs"

main :: proc(){
    x: u32 = 4
    y: u32  = 4
    z: u32 = x+y
    printf("%d\n", z)
}
