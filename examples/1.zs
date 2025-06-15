/*
    Zeus is the king of the Olympian gods in ancient Greek religion and mythology,
    considered the supreme deity and the ruler of the sky, thunder, and lightning.
    /*
        nested comment: Also a horny dude
    */
*/

//Zeus is a language that aims to be C, but without C's obvious disadvantages such as header files, syntax, etc...
//any other feature implemented is ALWAYS a zero-cost abstraction(defer, multiple return, etc...).

//we can specify what we want to link against in the source file
#link "libc"

/*
    @NOTE: the syntax is copied from odin(https://odin-lang.org/news/declaration-syntax/) which
           in turn has been inspired from Jonathan Blow's JAI(https://youtu.be/TH9VCN6UkyQ?si=JC9oNHfAD-Oc0iV9)
*/
//a procedure decleration
printf :: proc_decl(^char, ...)

//a procedure defenition
swap :: proc(x,y :u32) -> (u32, u32){
    //swapping without 3rd variable
    return y, x
}

main :: proc(){
    //no need for ';'
    printf("Hello, World\n")

    //defer statements are 0 cost abstraction. The compiler inserts the ast
    //tree at the end of all return statements including default return
    defer printf("Bye, World\n")
    defer {
        printf("Another defer statement")
    }

    x :u32 = 420
    z : ^char = "loop"
    //the compiler will figure out the type
    y := 69

    if x==4{
        printf("x is 4\n")
        //you can add brackets around expression if you want
    }else if(x == 3){
        printf("x is 3\n")
    } else {
        printf("i don't know what x is\n")
    }

    //c-style for loops
    printf("[%s %d]\n", z, 1)
    //start from 0 and go upto 4-1=3
    for a:=0...4{
        printf("%d\n", a)
    }

    //c-style for loops with increment
    printf("[loop 2]\n")
    y = 2
    for a:=0...4..y{
        printf("%d\n", a)
    }

    //c-style while loop
    printf("[loop 3]\n")
    x = 0
    for x != 4{
        printf("%d\n", x)
        x = x + 1
    }

    //named-loops for complex control flow
    printf("[loop 4]\n")
    x = 0
    for "outer-loop" g:=0...3{
        for {
            printf("%d\n", x)
                if x == 3{
                    break "outer-loop"
                }
            x = x + 1
        }
    }

    printf("[loop 5]\n")
    for "outer-loop" g:=0...3{
        x = 0
        for {
            printf("%d %d\n", g, x)
                if x == 3{
                    g = g + 1
                    continue "outer-loop"
                }
            x = x + 1
        }
    }

    printf("[loop 6]\n")
    g: ^u32 = {10,9,8,7,6,5}
    for a:=0...6{
        printf("%d\n", g[a])
    }

    x = 69 
    pointerToX := &x
    pointerToPointerToX := &pointerToX
    //pointer resolution: right side
    //pointer type declaration: left side
    pointerToPointerToPointerToX : ^^^u32 = &pointerToPointerToX
    printf("%p -> %d\n", pointerToX, pointerToPointerToPointerToX^^^)

    //multilpe return
    a,b := swap(1,2)
    printf("%d%d\n", a,b)
}

