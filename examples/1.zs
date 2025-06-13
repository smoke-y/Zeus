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

    //c-style for loops
    printf("[%s %d]\n", z, 1)
    //start from 0 and go upto 4-1=3
    for a:=0...4{
        printf("%d\n", a)
    }

    printf("[loop 02]\n")
    //start from 0 and increment by 2
    y = 2
    for a:=0...4..y{
        printf("%d\n", a)
    }
}
