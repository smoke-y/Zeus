#include "../include/basic.hh"
#include "../include/ds.hpp"
#include "../include/parser.hh"
#include "../include/checker.hh"
#include "../include/dependency.hh"
#include <stdarg.h>

#define BUCKET_BUFFER_SIZE 1023
#if(WIN)
#define WRITE(file, buff, len) WriteFile(file, buff, len, nullptr, NULL)
#elif(LIN)
#define WRITE(file, buff, len) write(file, buff, len)
#else
#define WRITE(file, buff, len)
#endif

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
    DynamicArray<u32> tempRegs;
    LLVMBucket *start;
    LLVMBucket *cur;
    u32 cursor;
    u32 label;

    void init(){
        start = (LLVMBucket*)mem::alloc(sizeof(LLVMBucket));
        start->next = nullptr;
        cur = start;
        cursor = 0;
        label = 0;
        tempRegs.init();
    };
    void uninit(){tempRegs.uninit();}
    u32 newReg(){return tempRegs[tempRegs.count-1]++;};
    void popRegion(){tempRegs.pop();};
    void pushRegion(){tempRegs.push(0);};
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
u32 lowerExpression(ASTBase *root, Type type, LLVMFile &file){
    u32 reg = file.newReg();
    switch(root->type){
        case ASTType::INTEGER:{
            ASTNum *num = (ASTNum*)root;
            char *typeStr = TypeToString[(u32)type];
            file.write("%%e%d = alloca %s\nstore %s %lld, ptr %%e%d\n", reg, typeStr, typeStr, num->integer, reg);
        }break;
    };
    return reg;
}
void lowerASTNode(ASTBase *node, LLVMFile &file);
inline void lowerBody(ASTBase **nodes, u32 count, LLVMFile &file){
    for(u32 x=0; x<count; x++) lowerASTNode(nodes[x], file);
}
void lowerASTNode(ASTBase *node, LLVMFile &file){
    switch(node->type){
        case ASTType::RETURN:{
            ASTReturn *ret = (ASTReturn*)node;
            if(ret->retCount == 0){
                file.write("ret void\n");
            }else{
                //TODO :
            }
        }break;
        case ASTType::IF:{
            ASTIf *If = (ASTIf*)node;
            char *typeStr = TypeToString[(u32)If->zType];
            u32 exprReg = lowerExpression(If->expr, If->zType, file);
            u32 ifBodyLabel = file.label++;
            u32 exitLabel = file.label++;
            u32 ifReg = file.newReg();
            u32 ifExprReg = file.newReg();
            file.write("%%t%d = load %s, ptr %%e%d\n", ifExprReg, typeStr, exprReg);
            file.write("%%t%d = icmp eq %s %%t%d, 0\n", ifReg, typeStr, ifExprReg);
            file.write("br i1 %%t%d, label %%_%d, label %%_%d\n_%d:\n", ifReg, ifBodyLabel, exitLabel, ifBodyLabel);
            lowerBody(If->ifBody, If->ifBodyCount, file);
            file.write("br label %%_%d\n_%d:\n", exitLabel, exitLabel);
            if(If->elseBodyCount) lowerBody(If->elseBody, If->elseBodyCount, file);
        }break;
        case ASTType::PROC_DEF:{
            file.pushRegion();
            ASTProcDefDecl *proc = (ASTProcDefDecl*)node;
            char *type;
            if(proc->outputCount == 0) type = "void";
            else type = getLLVMType(proc->outputs[0]);
            file.write("define dso_local %s @%.*s(", type, proc->name.len, proc->name.mem);
            if(proc->inputCount){
                u32 x=0;
                while(true){
                    char *type = getLLVMType(proc->inputs[x]->zType);
                    ASTVariable **vars = (ASTVariable**)proc->inputs[x]->lhs;
                    for(u32 i=0; i<proc->inputs[x]->lhsCount; i++){
                        ASTVariable *var = vars[i];
                        file.write("%s noundef %%%d", type, var->entity->id);
                    }
                    x++;
                    if(x == proc->inputCount) break;
                    file.write(",");
                }
            };
            file.write("){\n");
            for(u32 x=0; x<proc->bodyCount; x++) lowerASTNode(proc->body[x], file);
            file.write("}\n");
            file.popRegion();
        }break;
        case ASTType::DECLERATION:{
            ASTAssDecl *decl = (ASTAssDecl*)node;
            Type type = (((ASTVariable*)(decl->lhs[0]))->entity->type);
            u32 expReg = lowerExpression(decl->rhs, type, file);
            char *typeStr = TypeToString[(u32)type];
            ASTVariable **vars = (ASTVariable**)decl->lhs;
            for(u32 x=0; x<decl->lhsCount; x++){
                ASTVariable *var = vars[x];
                u32 id = var->entity->id;
                u32 tempReg = file.newReg();
                file.write("%%r%d = alloca %s\n", id, typeStr);
                file.write("%%t%d = load %s, ptr %%e%d\n", tempReg, typeStr, expReg);
                file.write("store %s %%t%d, ptr %%r%d\n", typeStr, tempReg, id);
            }
        }break;
        case ASTType::FOR:{
            ASTFor *For = (ASTFor*)node;
            if(For->decl){
                Type type = For->zType;
                char *typeStr = TypeToString[(u32)type];
                u32 id = ((ASTVariable*)(For->decl->lhs[0]))->entity->id;
                file.write("%%r%d = alloca %s\n", id, typeStr);
                u32 initReg = lowerExpression(For->decl->rhs, type, file);
                u32 endReg = lowerExpression(For->end, type, file);
                u32 stepReg;
                if(For->step) stepReg = lowerExpression(For->step, type, file);
                else{
                    stepReg = file.newReg();
                    file.write("%%e%d = alloca %s\nstore %s 1, ptr %%e%d\n", stepReg, typeStr, typeStr, stepReg);
                };
                u32 tmpReg =file.newReg();
                file.write("%%t%d = load %s, ptr %%e%d\n", tmpReg, typeStr, initReg);
                file.write("store %s %%t%d, ptr %%r%d\n", typeStr, tmpReg, id);
                u32 cmpLabel = file.label++;
                u32 bdyLabel = file.label++;
                u32 exitLabel = file.label++;
                u32 cmpLoadReg1 = file.newReg();
                u32 cmpLoadReg2 = file.newReg();
                u32 cmpResReg = file.newReg();
                file.write("br label %%_%d\n_%d:\n%%t%d = load %s, ptr %%r%d\n", cmpLabel, cmpLabel, cmpLoadReg1, typeStr, id);
                file.write("%%t%d = load %s, ptr %%e%d\n", cmpLoadReg2, typeStr, endReg);
                file.write("%%t%d = icmp eq %s %%t%d, %%t%d\n", cmpResReg, typeStr, cmpLoadReg1, cmpLoadReg2);
                file.write("br i1 %%t%d, label %%_%d, label %%_%d\n", cmpResReg, bdyLabel, exitLabel);
                file.write("_%d:\n", bdyLabel);
                lowerBody(For->body, For->bodyCount, file);
                u32 updReg1 = file.newReg();
                u32 updReg2 = file.newReg();
                u32 updReg3 = file.newReg();
                file.write("%%t%d = load %s, ptr %%e%d\n", updReg1, typeStr, stepReg);
                file.write("%%t%d = load %s, ptr %%r%d\n", updReg2, typeStr, id);
                file.write("%%t%d = add nsw %s %%t%d, %%t%d\n", updReg3, typeStr, updReg1, updReg2);
                file.write("store %s %%t%d, ptr %%r%d\nbr label %%_%d\n", typeStr, updReg3, id, cmpLabel);
                file.write("_%d:\n", exitLabel);
            };
        }break;
    }
};

void lowerToLLVM(char *outputPath, DynamicArray<ASTBase*> &globals){
#if(WIN)
    HANDLE file = CreateFile(outputPath, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    DEFER(CloseHandle(file));
#elif(LIN)
    int file = open(outputPath, O_RDWR|O_CREAT);
    if(file<0){
        printf("Error in opening %s: %d\n", outputPath, errno);
        return;
    };
    DEFER(close(file));
#endif
    const u32 BUFF_SIZE = 1024;
    char buff[BUFF_SIZE];
    u32 cursor = 0;
    u32 temp;
    for(u32 x=0,i=0; x<check::stringToId.count;){
        if(check::stringToId.status[i]){
DUMP_STRINGS:
            const String str = check::stringToId.keys[i];
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
                check::stringToId.getValue(str->str, &off);
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@g%d = dso_local global ptr @.str.%d\n", var->entity->id, off);
            }break;
            case ASTType::CHARACTER:
            case ASTType::INTEGER:{
                ASTNum *num = (ASTNum*)assdecl->rhs;
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@g%d = dso_local global %s %lld\n", var->entity->id, TypeToString[(u32)var->entity->type], num->integer);
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
    for(u32 x=dep::astFiles.count; x > 0;){
        x -= 1;
        ASTFile &astFile = dep::astFiles[x];
        if(astFile.nodes.count == 0) continue;
        LLVMFile llvmFile;
        llvmFile.init();
        for(u32 x=0; x<astFile.nodes.count; x++) lowerASTNode(astFile.nodes[x], llvmFile);
        llvmFile.uninit();
        LLVMBucket *buc = llvmFile.start;
        while(buc){
            WRITE(file, buc->buff, strlen(buc->buff));
            LLVMBucket *temp = buc;
            buc = buc->next;
            mem::free(temp);
        };
    };
};
