#link "libc"

SEEK_SET :u32 = 0
SEEK_CUR :u32 = 1
SEEK_END :u32 = 2

printf :: proc_decl(^char, ...)
scanf  :: proc_decl(^char, ...) -> s32

fopen  :: proc_decl(^char, ^char) -> ptr
fclose :: proc_decl(ptr) -> s32
fseek  :: proc_decl(ptr, u64, u32) -> s32
ftell  :: proc_decl(ptr) -> s64
fread  :: proc_decl(ptr, s64, s64, ptr) -> s64
