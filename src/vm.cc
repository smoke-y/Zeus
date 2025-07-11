#include "../include/vm.hh"
#include "../include/ds.hpp"
#include <dlfcn.h>

typedef void (*ExecFuncVMType)(u32* opcodes, u32 len, char** strs);
void *vm;
ExecFuncVMType exec;

bool loadVM(){
    vm = dlopen("bin/lin/vm.so", RTLD_LAZY);
    if(!vm){
        printf("Could not load VM");
        return false;
    };
    exec = (ExecFuncVMType)dlsym(vm, "exec");
    return true;
};
void unloadVM(){dlclose(vm);};
void lowerToBytecodeAndExecOnVM(DynamicArray<parser::MacroBody> mbodies){
    u32 bytecodes[100];
    char *strs[100];
    u32 strsOff = 0;
    u32 byteOff = 0;
    for(u32 i=0; i<mbodies.count; i++){
        u32 len = mbodies[i].count;
        ASTBase **nodes = mbodies[i].body;
        for(u32 x=0; x<len; x++){
            if(nodes[x]->type == ASTType::PROC_CALL){
                ASTProcCall *procCall = (ASTProcCall*)nodes[x];
                if(cmpString(procCall->name, "log")){
                    if(procCall->args[0]->type != ASTType::STRING) return;
                    ASTString *str = (ASTString*)procCall->args[0];
                    strs[strsOff] = str->str.mem;
                    bytecodes[byteOff++] = (u32)VMBytecode::LOG;
                    bytecodes[byteOff++] = strsOff++;
                    bytecodes[byteOff++] = str->str.len;
                } else if(cmpString(procCall->name, "license")){
                    if(procCall->args[0]->type != ASTType::STRING) return;
                    ASTString *str = (ASTString*)procCall->args[0];
                    strs[strsOff] = str->str.mem;
                    bytecodes[byteOff++] = (u32)VMBytecode::LICENSE;
                    bytecodes[byteOff++] = strsOff++;
                    bytecodes[byteOff++] = str->str.len;
                };
            };
        };
    };
    exec(bytecodes, byteOff, strs);
};
