char *TypeToString[] ={
    "invalid",
    "i8",
    "double",
    "i64",
    "i64",
    "float",
    "i32",
    "i32",
    "i16",
    "i16",
    "i8",
    "i8",
    "i8",
    "i64",
    "i64",
    "invalid",
};

void lowerToLLVM(char *outputPath, DynamicArray<ASTBase*> &globals){
#if(WIN)
    HANDLE file = CreateFile(outputPath, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    DEFER(CloseHandle(file));
#elif(LIN)
    int file = open(outputPath, O_RDWR|O_CREAT);
    DEFER(close(file));
#endif
    const u32 BUFF_SIZE = 1024;
    char buff[BUFF_SIZE];
    u32 cursor = 0;
    u32 temp;
    for(u32 x=0,i=0; x<stringToId.count;){
        if(stringToId.status[i]){
DUMP_STRINGS:
            const String str = stringToId.keys[i];
            temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@.str.%d = private unnamed_addr constant [%d x i8] c\"%.*s\\00\"\n", x, str.len+1, str.len, str.mem);
            if(temp+cursor > BUFF_SIZE){
                WRITE(file, buff, cursor);
                cursor = 0;
                goto DUMP_STRINGS;
            };
            cursor += temp;
            x++;
        };
        i++;
    };
    for(u32 x=0; x<globals.count; x++){
        ASTAssDecl *assdecl = (ASTAssDecl*)globals[x];
        ASTVariable *var = (ASTVariable*)assdecl->lhs[0];
GLOBAL_WRITE_LLVM_TO_BUFF:
        switch(assdecl->rhs->type){
            case ASTType::STRING:{
                ASTString *str = (ASTString*)assdecl->rhs;
                u32 off;
                stringToId.getValue(str->str, &off);
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@%.*s = dso_local global ptr @.str.%d\n", var->name.len, var->name.mem, off);
            }break;
            case ASTType::CHARACTER:
            case ASTType::INTEGER:{
                ASTNum *num = (ASTNum*)assdecl->rhs;
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@%.*s = dso_local global %s %lld", var->name.len, var->name.mem, TypeToString[(u32)var->entity->type], num->integer);
            }break;
        };
        if(temp + cursor >= BUFF_SIZE){
            WRITE(file, buff, cursor);
            cursor = 0;
            goto GLOBAL_WRITE_LLVM_TO_BUFF;
        };
        cursor += temp;
    };
    if(cursor) WRITE(file, buff, cursor);
};