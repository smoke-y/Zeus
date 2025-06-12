#import "lib/stdio.zs"

main :: proc(){
    x: []u32 = {0,1,2,3,4}
    y :=  x[1]
    z :=  x[0]
    printf("%d  %d", z,y)
}
