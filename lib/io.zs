#link "libc"

printf :: proc_decl(^char, ...)
scanf  :: proc_decl(^char, ...) -> s32

fopen  :: proc_decl(^char, ^char) -> ^s64
fclose :: proc_decl(^s64) -> s32
fseek  :: proc_decl(^s64, u64, u32) -> s32
ftell  :: proc_decl(^s64) -> s64
