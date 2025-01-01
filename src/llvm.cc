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

struct LLVMBucket{
    char buff[BUCKET_BUFFER_SIZE+1];    //+1 for null byte
    LLVMBucket *next;
};
struct LLVMFile{
    LLVMBucket *start;
    LLVMBucket *cur;
    u32 cursor;

    void init(){
        start = (LLVMBucket*)mem::alloc(sizeof(LLVMBucket));
        start->next = nullptr;
        cur = start;
        cursor = 0;
    };
    void write(char *fmt, ...){
        va_list args;
        va_start(args, fmt);
        s32 res = vsnprintf(&cur->buff[cursor], BUCKET_BUFFER_SIZE, fmt, args);
        va_end(args);
        if(res + cursor + 1 >= BUCKET_BUFFER_SIZE){
            cur->buff[cursor] = '\0';
            LLVMBucket *buc = (LLVMBucket*)mem::alloc(sizeof(LLVMBucket));
            buc->buff[BUCKET_BUFFER_SIZE] = '\0';
            buc->next = nullptr;
            cursor = 0;
            cur->next = buc;
            cur = buc;
            va_start(args, fmt);
            res = vsnprintf(cur->buff, BUCKET_BUFFER_SIZE, fmt, args);
            va_end(args);
        };
        cursor += res;
    };
};

char *getLLVMType(ASTTypeNode *node){
    if(node->pointerDepth) return "ptr";
    return TypeToString[(u32)node->zType];
}
void lowerASTNode(ASTBase *node, LLVMFile &file){
    switch(node->type){
        case ASTType::PROC_DEF:{
            ASTProcDefDecl *proc = (ASTProcDefDecl*)node;
            char *type;
            if(proc->outputCount == 0) type = "void";
            else type = getLLVMType(proc->outputs[0]);
            file.write("define dso_local %s @%.*s(", type, proc->name.len, proc->name.mem);
            if(proc->inputCount){
                u32 x=0;
                while(true){
                    file.write("%s", getLLVMType(proc->inputs[x]->zType));
                    x++;
                    if(x == proc->inputCount) break;
                    file.write(",");
                }
            };
            file.write("){\n");
            for(u32 x=0; x<proc->bodyCount; x++) lowerASTNode(proc->body[x], file);
            file.write("}\n");
        }break;
    }
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
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@%.*s = dso_local global %s %lld\n", var->name.len, var->name.mem, TypeToString[(u32)var->entity->type], num->integer);
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
    for(u32 x=linearDepEntities.count; x > 0;){
        x -= 1;
        FileEntity &fe = linearDepEntities[x];
        if(fe.file.nodes.count == 0) continue;
        ASTFile &astFile = fe.file;
        LLVMFile llvmFile;
        llvmFile.init();
        for(u32 x=0; x<astFile.nodes.count; x++) lowerASTNode(astFile.nodes[x], llvmFile);
        LLVMBucket *buc = llvmFile.start;
        while(buc){
            WRITE(file, buc->buff, strlen(buc->buff));
            LLVMBucket *temp = buc;
            buc = buc->next;
            mem::free(temp);
        };
    };
};