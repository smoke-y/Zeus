#include "../include/basic.hh"
#include "../include/ds.hpp"
#include "../include/parser.hh"
#include "../include/checker.hh"
#include "../include/dependency.hh"
#include <cstdio>
#include <stdarg.h>

#define BUCKET_BUFFER_SIZE 1023
#if(WIN)
#define WRITE(file, buff, len) WriteFile(file, buff, len, nullptr, NULL)
#elif(LIN)
#define WRITE(file, buff, len) write(file, buff, len)
#else
#define WRITE(file, buff, len)
#endif

char* TypeToStringTable[] = {
    "invalid",
    "invalid_defer_cast",
    "invalid_comp_string",
    "i64",
    "double",
    "void",
    "invalid_z_type_start",
    "i8",
    "i8",
    "i8",
    "i16",
    "i16",
    "i32",
    "i32",
    "float",
    "i64",
    "i64",
    "double",
    "invalid_z_type_end",
};
u32 TypeToSizeTable[] = {
    0, 0, 0,
    8, 8,
    0, 0,
    1, 1, 1,
    2, 2,
    4, 4, 4,
    8, 8, 8,
    0,
};
char* BinOpToStringTable[] = {
    "add",
    "sub",
    "mul",
    "div",
    "rem",
};

struct LLVMBucket{
    char buff[BUCKET_BUFFER_SIZE+1];    //+1 for null byte
    LLVMBucket *next;
    u32 len;
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
        start->len = 0;
        cur = start;
        cursor = 0;
        label = 0;
        tempRegs.init();
        tempRegs.push(0);
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
            cur->len = cursor;
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
        cur->len = cursor;
    };
};

char *getLLVMType(ASTTypeNode *node){
    if(node->zType == Type::COMP_STRING || node->pointerDepth) return "ptr";
    return TypeToStringTable[(u32)node->zType];
}
u32 getLLVMSize(ASTTypeNode *node){
    if(node->zType == Type::COMP_STRING || node->pointerDepth) return 8;
    return TypeToSizeTable[(u32)node->zType];
}
char *getLLVMBinOp(ASTType type){
    return BinOpToStringTable[(u32)type - (u32)ASTType::B_START - 1];
};
s32 howCast(ASTTypeNode *n1, ASTTypeNode *n2){
    /*
     * n1 --casted_to-->n2
     *  0: no llvm casting req
     * -1: trunc
     *  1: unsigned ext
     *  2: signed ext
     */
    if(n1->pointerDepth > 0 && n2->pointerDepth > 0) return 0;
    if(n1->zType == n2->zType) return 0;
    Type t1 = n1->zType;
    Type t2 = n2->zType;
    s32 ret = -1;
    if(t2 > t1){
        Type temp = t2;
        t2 = t1;
        t1 = temp;
        ret = 1;
    };
    Type acceptedT2 = Type::INVALID;
    switch(t1){
        case Type::U8:
        case Type::S8:
        case Type::CHAR:{
                            switch(t2){
                                case Type::U8:
                                case Type::S8:
                                case Type::CHAR: return 0;
                            }
                        }break;
        case Type::S16: acceptedT2 = Type::U16;break;
        case Type::S32: acceptedT2 = Type::U32;break;
        case Type::S64: acceptedT2 = Type::U64;break;
    };
    if(acceptedT2 != Type::INVALID) return 0;
    if(ret == -1) return -1;
    //t1 and t2 are swapped
    if(isSigned(t1)) return 2;
    return 1;
};
u32 lowerExpression(ASTBase *root, LLVMFile &file, Type type = Type::INVALID){
    u32 reg = file.newReg();
    switch(root->type){
        case ASTType::ARRAY_AT:{
                                   ASTArrayAt *at = (ASTArrayAt*)root;
                                   u32 atReg = lowerExpression(at->at, file);
                                   u32 parReg = lowerExpression(at->parent, file);
                                   ASTTypeNode typeNode;
                                   typeNode.zType = at->atType;
                                   typeNode.pointerDepth = 0;
                                   char *atType = getLLVMType(&typeNode);
                                   at->parentType.pointerDepth--;
                                   char *parType = getLLVMType(&at->parentType);
                                   u32 tempAtReg = file.newReg();
                                   u32 tempParReg = file.newReg();
                                   file.write("%%t%d = load %s, ptr %%e%d\n", tempAtReg, atType, atReg);
                                   file.write("%%t%d = load ptr, ptr %%e%d\n", tempParReg, parReg);
                                   file.write("%%e%d = getelementptr inbounds %s, ptr %%t%d, %s %%t%d\n", reg, parType, tempParReg, atType, tempAtReg);
                               }break;
        case ASTType::INITIALIZER_LIST:{
                                           ASTInitializerList *il = (ASTInitializerList*)root;
                                           ASTTypeNode typeNode;
                                           typeNode.zType = il->zType;
                                           typeNode.pointerDepth = 0;
                                           char *type = getLLVMType(&typeNode);
                                           u32 tmpReg = file.newReg();
                                           file.write("%%t%d = alloca [%d x %s]\n", tmpReg, il->elementCount, type);
                                           file.write("call void @llvm.memcpy.p0.p0.i64(ptr %%t%d, ptr @.init_list.%d, i64 %d, i1 false)\n",
                                                   tmpReg, il->id, il->elementCount * getLLVMSize(&typeNode));
                                           file.write("%%e%d = alloca ptr\nstore ptr %%t%d, ptr %%e%d\n", reg, tmpReg, reg);
                                       }break;
        case ASTType::CAST:{
                               ASTCast *cast = (ASTCast*)root;
                               u32 childReg = lowerExpression(cast->child, file);
                               char *srcType = getLLVMType(cast->srcType);
                               char *tarType = getLLVMType(cast->targetType);
                               s32 typeStat = howCast(cast->srcType, cast->targetType);
                               char *func = nullptr;
                               switch(typeStat){
                                   case -1:func = "trunc";break;
                                   case 1: func = "zext";break;
                                   case 2: func = "sext";break;
                               }
                               u32 tempReg = file.newReg();
                               file.write("%%t%d = load %s, ptr %%e%d\n", tempReg, srcType, childReg);
                               if(func){
                                   u32 castReg = file.newReg();
                                   file.write("%%t%d = %s %s %%t%d to %s\n", castReg, func, srcType, tempReg, tarType);
                                   tempReg = castReg;
                               };
                               file.write("%%e%d = alloca %s\n", reg, tarType);
                               file.write("store %s %%t%d, ptr %%e%d\n", tarType, tempReg, reg);
                           }break;
        case ASTType::VARIABLE:{
                                   ASTVariable *var = (ASTVariable*)root;
                                   VariableEntity *entity = var->entity;
                                   ASTTypeNode typeNode;
                                   typeNode.zType = entity->type;
                                   typeNode.pointerDepth = var->pAccessDepth;
                                   char *type = getLLVMType(&typeNode);
                                   file.write("%%e%d = bitcast %s* %%r%d to %s*\n", reg, type, entity->id, type);
                               }break;
        case ASTType::STRING:{
                                 ASTString *str = (ASTString*)root;
                                 u32 id;
                                 bool x = check::stringToId.getValue(str->str, &id);
                                 file.write("%%e%d = bitcast ptr @str.%d to ptr\n", reg, id);
                             }break;
        case ASTType::INTEGER:{
                                  ASTNum *num = (ASTNum*)root;
                                  char *typeStr = "i64";
                                  if(type != Type::INVALID) typeStr = TypeToStringTable[(u32)type];
                                  file.write("%%e%d = alloca %s\nstore %s %lld, ptr %%e%d\n", reg, typeStr, typeStr, num->integer, reg);
                              }break;
        case ASTType::PROC_CALL:{
                                    ASTProcCall *proc = (ASTProcCall*)root;
                                    ProcEntity *entity = proc->entity;
                                    for(u32 x=1; x<entity->outputCount; x++) file.newReg();
                                    u32 inputRegStart = reg + entity->outputCount;
                                    for(u32 x=0; x<proc->argCount; x++) u32 tempReg = file.newReg();
                                    char *type;
                                    for(u32 x=0; x<proc->argCount; x++){
                                        u32 reg = lowerExpression(proc->args[x], file, proc->types[x].zType);
                                        type = getLLVMType(&proc->types[x]);
                                        file.write("%%t%d = load %s, ptr %%e%d\n", inputRegStart+x, type, reg);
                                    };
                                    u32 tempReg = reg; 
                                    reg = file.newReg();
                                    if(entity->outputCount == 0) type = "void";
                                    else{
                                        for(u32 x=1; x<entity->outputCount; x++){
                                            type = getLLVMType(entity->outputs[x]);
                                            file.write("%%e%d = alloca %s\n", reg+x, type);
                                        };
                                        type = getLLVMType(entity->outputs[0]);
                                        file.write("%%t%d = ", tempReg);
                                    };
                                    file.write("call %s @%.*s(", type, proc->name.len, proc->name.mem);
                                    if(proc->argCount == 0 && entity->outputCount < 2) file.write(")\n");
                                    else{
                                        if(proc->argCount != 0){
                                            for(u32 x=0;;){
                                                type = getLLVMType(&proc->types[x]);
                                                file.write("%s %%t%d", type, inputRegStart + x);
                                                if(++x == proc->argCount) break;
                                                file.write(", ");
                                            };
                                        };
                                        if(entity->outputCount > 1){
                                            if(proc->argCount != 0) file.write(", ");
                                            for(u32 x=1;;){
                                                file.write("ptr %%e%d", reg + x);
                                                if(++x == entity->outputCount) break;
                                                file.write(", ");
                                            };
                                        };
                                        file.write(")\n");
                                    };
                                    if(entity->outputCount != 0) file.write("%%e%d = alloca %s\nstore %s %%t%d, ptr %%e%d\n", reg, type, type, tempReg, reg);
                                }break;
        default:{
                    if(root->type > ASTType::B_START && root->type < ASTType::B_END){
                        ASTBinOp *binOp = (ASTBinOp*)root;
                        u32 lreg = lowerExpression(binOp->lhs, file);
                        u32 rreg = lowerExpression(binOp->rhs, file);
                        char *opStr = getLLVMBinOp(root->type);
                        char sign = ' ';
                        if(root->type == ASTType::B_DIV || root->type == ASTType::B_MOD) sign = isSigned(binOp->zType.zType)?'s':'u';
                        u32 ltmp = file.newReg();
                        u32 rtmp = file.newReg();
                        u32 atmp = file.newReg();
                        char *typeStr = getLLVMType(&binOp->zType);
                        file.write("%%t%d = load %s, ptr %%e%d\n", ltmp, typeStr, lreg);
                        file.write("%%t%d = load %s, ptr %%e%d\n", rtmp, typeStr, rreg);
                        file.write("%%t%d = %c%s %s %%t%d, %%t%d\n", atmp, sign, opStr, typeStr, ltmp, rtmp);
                        file.write("%%e%d = alloca %s\n", reg, typeStr);
                        file.write("store %s %%t%d, ptr %%e%d\n", typeStr, atmp, reg);
                    };
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
                                     break;
                                 };
                                 if(ret->retCount > 1){
                                     for(u32 x=1; x<ret->retCount; x++){
                                         u32 reg = lowerExpression(ret->exprs[x], file);
                                         char *type = getLLVMType(&ret->types[x]);
                                         u32 retRegVal = file.newReg();
                                         file.write("%%t%d = load %s, ptr %%e%d\nstore %s %%t%d, ptr %%o%d\n", retRegVal, type, reg, type, retRegVal, x);
                                     };
                                 };
                                 u32 reg = lowerExpression(ret->exprs[0], file);
                                 char *type = getLLVMType(&ret->types[0]);
                                 u32 retRegVal = file.newReg();
                                 file.write("%%t%d = load %s, ptr %%e%d\nret %s %%t%d\n", retRegVal, type, reg, type, retRegVal);
                             }break;
        case ASTType::IF:{
                             ASTIf *If = (ASTIf*)node;
                             char *typeStr = TypeToStringTable[(u32)If->zType];
                             u32 exprReg = lowerExpression(If->expr, file, If->zType);
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
        case ASTType::PROC_DECL:{
                                    ASTProcDefDecl *proc = (ASTProcDefDecl*)node;
                                    char *type;
                                    if(proc->outputCount == 0) type = "void";
                                    else type = getLLVMType(proc->outputs[0]);
                                    file.write("declare dso_local %s @%.*s(", type, proc->name.len, proc->name.mem);
                                    for(u32 x=0;;){
                                        ASTTypeNode *inputType = proc->typeInputs[x];
                                        char *type = getLLVMType(inputType);
                                        file.write("%s", type);
                                        if(++x == proc->inputCount){
                                            if(proc->varArgs) file.write(", ...");
                                            break;
                                        };
                                        file.write(",");
                                    };
                                    file.write(")\n");
                                }break;
        case ASTType::PROC_DEF:{
                                   file.pushRegion();
                                   ASTProcDefDecl *proc = (ASTProcDefDecl*)node;
                                   char *type;
                                   if(proc->outputCount == 0) type = "void";
                                   else type = getLLVMType(proc->outputs[0]);
                                   file.write("define dso_local %s @%.*s(", type, proc->name.len, proc->name.mem);
                                   for(u32 x=0; x<proc->inputNodeCount; x++){
                                       char *type = getLLVMType(proc->inputs[x]->zType);
                                       ASTVariable **vars = (ASTVariable**)proc->inputs[x]->lhs;
                                       u32 lhsCount = proc->inputs[x]->lhsCount;
                                       for(u32 i=0; i<lhsCount; i++){
                                           ASTVariable *var = vars[i];
                                           file.write("%s noundef %%%d", type, var->entity->id);
                                           if(i+1 == lhsCount) break;
                                           file.write(", ");
                                       };
                                       if(x+1 == proc->inputNodeCount) break;
                                       file.write(", ");
                                   };
                                   if(proc->outputCount > 1 && proc->inputNodeCount) file.write(", ");
                                   for(u32 x=1; x<proc->outputCount; x++){
                                       file.write("ptr noundef %%o%d", x);
                                       if(x+1 == proc->outputCount) break;
                                       file.write(", ");
                                   };
                                   if(proc->varArgs) file.write(", ...");
                                   file.write("){\n");
                                   for(u32 x=0; x<proc->inputNodeCount; x++){
                                       char *type = getLLVMType(proc->inputs[x]->zType);
                                       ASTVariable **vars = (ASTVariable**)proc->inputs[x]->lhs;
                                       for(u32 i=0; i<proc->inputs[x]->lhsCount; i++){
                                           ASTVariable *var = vars[i];
                                           u32 reg = file.newReg();
                                           file.write("%%r%d = alloca %s\nstore %s %%%d, ptr %%r%d\n", reg, type, type, var->entity->id, reg);
                                       };
                                   };
                                   for(u32 x=0; x<proc->bodyCount; x++) lowerASTNode(proc->body[x], file);
                                   file.write("}\n");
                                   file.popRegion();
                               }break;
        case ASTType::DECLERATION:{
                                      ASTAssDecl *decl = (ASTAssDecl*)node;
                                      ASTVariable **vars = (ASTVariable**)decl->lhs;
                                      if(decl->lhsCount == 1){
                                          ASTVariable *var = vars[0];
                                          ASTTypeNode typeNode;
                                          typeNode.zType = var->entity->type;
                                          typeNode.pointerDepth = var->entity->pointerDepth;
                                          u32 expReg = lowerExpression(decl->rhs, file, typeNode.zType);
                                          u32 id = var->entity->id;
                                          u32 tempReg = file.newReg();
                                          char *typeStr = getLLVMType(&typeNode);
                                          file.write("%%r%d = alloca %s\n", id, typeStr);
                                          file.write("%%t%d = load %s, ptr %%e%d\n", tempReg, typeStr, expReg);
                                          file.write("store %s %%t%d, ptr %%r%d\n", typeStr, tempReg, id);
                                      }else{
                                          u32 expReg = lowerExpression(decl->rhs, file);
                                          for(u32 x=0; x<decl->lhsCount; x++){
                                              ASTVariable *var = vars[x];
                                              ASTTypeNode typeNode;
                                              typeNode.zType = var->entity->type;
                                              typeNode.pointerDepth = var->entity->pointerDepth;
                                              char *typeStr = getLLVMType(&typeNode);
                                              file.write("%%r%d = bitcast %s* %%e%d to %s*\n", var->entity->id, typeStr, expReg+x, typeStr); 
                                          };
                                      };
                                  }break;
        case ASTType::ASSIGNMENT:{
                                     ASTAssDecl *decl = (ASTAssDecl*)node;
                                     ASTVariable **vars = (ASTVariable**)decl->lhs;
                                     if(decl->lhsCount == 1){
                                         ASTVariable *var = vars[0];
                                         ASTTypeNode typeNode;
                                         typeNode.zType = var->entity->type;
                                         typeNode.pointerDepth = var->entity->pointerDepth;
                                         u32 expReg = lowerExpression(decl->rhs, file, typeNode.zType);
                                         u32 tempReg = file.newReg();
                                         char *typeStr = getLLVMType(&typeNode);
                                         file.write("%%t%d = load %s, ptr %%e%d\n", tempReg, typeStr, expReg);
                                         file.write("store %s %%t%d, ptr %%r%d\n", typeStr, tempReg, var->entity->id);
                                     }else{
                                         u32 expReg = lowerExpression(decl->rhs, file);
                                         for(u32 x=0; x<decl->lhsCount; x++){
                                             ASTVariable *var = vars[x];
                                             ASTTypeNode typeNode;
                                             typeNode.zType = var->entity->type;
                                             typeNode.pointerDepth = var->entity->pointerDepth;
                                             char *typeStr = getLLVMType(&typeNode);
                                             u32 tempReg = file.newReg();
                                             file.write("%%t%d = load %s, ptr %%e%d\n", tempReg, typeStr, expReg+x);
                                             file.write("store %s %%t%d, ptr %%r%d\n", typeStr, tempReg, var->entity->id);
                                         };
                                     };
                                 }break;
        case ASTType::FOR:{
                              ASTFor *For = (ASTFor*)node;
                              if(For->end != nullptr){
                                  //c-for
                                  Type type = For->decl->zType->zType;
                                  char *typeStr = TypeToStringTable[(u32)type];
                                  u32 id = ((ASTVariable*)(For->decl->lhs[0]))->entity->id;
                                  file.write("%%r%d = alloca %s\n", id, typeStr);
                                  u32 initReg = lowerExpression(For->decl->rhs, file, type);
                                  u32 endReg = lowerExpression(For->end, file, type);
                                  u32 stepReg;
                                  if(For->step) stepReg = lowerExpression(For->step, file, type);
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
                                  file.write("%%t%d = icmp slt %s %%t%d, %%t%d\n", cmpResReg, typeStr, cmpLoadReg1, cmpLoadReg2);
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
        default:{
                    lowerExpression(node, file);
                }break;
    }
};

void lowerToLLVM(char *outputPath, DynamicArray<ASTBase*> &globals){
#if(WIN)
    HANDLE file = CreateFile(outputPath, GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL);
    DEFER(CloseHandle(file));
#elif(LIN)
    int file = open(outputPath, O_TRUNC|O_WRONLY|O_CREAT, 0644);   // read/write permision
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
            const String str = check::stringToId.keys[i];
            u32 val;
            check::stringToId.getValue(str, &val);
            char tempBuff[1024];
            u32 tmpCurs = 0;
            u32 red = 0;
            for(u32 x=0; x<str.len; x++){
                if(str.mem[x] == '\\' && x+1 != str.len){
                    if(str.mem[x+1] == 'n'){
                        tempBuff[tmpCurs++] = '\\';
                        tempBuff[tmpCurs++] = '0';
                        tempBuff[tmpCurs++] = 'A';
                        x++;
                        red++;
                    };
                }else tempBuff[tmpCurs++] = str.mem[x];
            };
DUMP_STRINGS:
            temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@.str.%d = private unnamed_addr constant [%d x i8] c\"%.*s\\00\"\n@str.%d = dso_local global ptr @.str.%d\n",
                    val, str.len - red + 1, str.len + red, tempBuff, val, val);
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
                                      temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "@g%d = dso_local global %s %lld\n", var->entity->id, TypeToStringTable[(u32)var->entity->type], num->integer);
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
    cursor = 0;
    for(u32 x=0; x<check::initializerLists.count; x++){
        ASTInitializerList *il = check::initializerLists[x];
        ASTTypeNode typeNode;
        typeNode.zType = il->zType;
        typeNode.pointerDepth = 0;
        char *type = getLLVMType(&typeNode);
        temp = snprintf(buff, BUFF_SIZE, "@.init_list.%d = private unnamed_addr constant [%d x %s] [", x, il->elementCount, type);
        WRITE(file, buff, temp);
        for(u32 x=0; x<il->elementCount; x++){
            ASTNum *num = (ASTNum*)il->elements[x];
INITIALIZER_LIST_WRITE_LLVM_TO_BUFF:
            if(isInteger(typeNode.zType)){
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "%s %lld", type, num->integer);
            }else{
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "%s %f", type, num->decimal);
            }
            cursor += temp;
            if(x+1 != il->elementCount){
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, ",");
            }else{
                temp = snprintf(buff+cursor, BUFF_SIZE-cursor, "]\n");
            };
            if(temp + cursor >= BUFF_SIZE){
                WRITE(file, buff, cursor);
                cursor = 0;
                goto INITIALIZER_LIST_WRITE_LLVM_TO_BUFF;
            };
            cursor += temp;
        };
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
            WRITE(file, buc->buff, buc->len);
            LLVMBucket *temp = buc;
            buc = buc->next;
            mem::free(temp);
        };
    };
    WRITE(file, "\n\0", 2);
};
