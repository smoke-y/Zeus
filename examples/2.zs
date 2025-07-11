#import "lib/io.zs"
#import "lib/mem.zs"

compTimeProc :: proc #comptime (){
    //This will print the license while compiling
    license("LICENSE.txt")

    log("This is being printed from a procedure being executed at compile time")
    log("The VM executing the bytecodes is bootstrapped and written in Zeus!")
}

main :: proc(){
    x: u32
    printf("enter an integer: ")
    scanf("%d", &x)
    printf("you entered: %d\n", x)

    fileName: ^char = "examples/2.zs"
    file := fopen(fileName, "rb")
    if file == 0{
        printf("File does not exist")
        return
    }
    defer fclose(file)

    fseek(file, 0, SEEK_END)
    len := ftell(file)
    fseek(file, 0, SEEK_SET)

    printf("%s(%p) is of length %d\n", fileName, file, len)

    buff : ^char = malloc(len)
    defer free(buff)

    fread(buff, len, 1, file)
    printf("contents:\n%s", buff)
}
