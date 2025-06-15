#pragma once

#include "../include/basic.hh"
#include "../include/ds.hpp"
#include "../include/type.hh"
#include "../include/parser.hh"

enum class ScopeType{
    GLOBAL,
    PROC,
    BLOCK,
};

struct Scope;
struct VariableEntity{
    u64 size;
    u32 id;
    Type type;
    u8 pointerDepth;
};
struct StructEntity{
    Scope *body;
    u64 size;
};
struct ProcEntity{
    union{
        ASTAssDecl  **inputs;
        ASTTypeNode **typeInputs;
    };
    ASTTypeNode **outputs;
    u32 outputCount;
    u32 inputCount;
    u32 inputNodeCount;
    bool varArgs;
    bool isDecl;
};

struct Scope{
    HashmapStr var;
    HashmapStr proc;
    //no need to free them as they live for the entire lifetime
    DynamicArray<VariableEntity*> vars;
    DynamicArray<ProcEntity*> procs;
    ScopeType type;
    u32 varId;

    void init(ScopeType stype, u32 id);
    void uninit();
};

namespace check{
    extern Scope *globalScopes;
    extern HashmapStr stringToId;
    extern DynamicArray<ASTInitializerList*> initializerLists;
    extern DynamicArray<StructEntity> structEntities;
};
