#import "lib/stdio.zs"

main :: proc(){
    x := 34
    y := &x
    z := &y
    a := &z
    v := a^^^
    printf("%p -> %d", a, v)
}
