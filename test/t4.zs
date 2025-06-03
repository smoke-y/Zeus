#import "lib/stdio.zs"

foo :: proc() -> (u32, u32){
    x: u32 = 6
    y: u32 = 9
    return x,y
}

main :: proc(){
    x,y := foo()
    printf("%d%d", x, y)
}
