#pragma once

#include "../include/lexer.hh"
#include "../include/basic.hh"
#include "../include/type.hh"
#include "../include/ds.hpp"

#define AST_PAGE_SIZE 1024
#define BRING_TOKENS_TO_SCOPE DynamicArray<TokType> &tokTypes = lexer.tokenTypes;DynamicArray<TokenOffset> &tokOffs = lexer.tokenOffsets;

enum class ASTType{
    INVALID,
    DECLERATION,
    ASSIGNMENT,
    INTEGER,
    CHARACTER,
    DECIMAL,
    TYPE,
    IF,
    FOR,
    PROC_DEF,
    PROC_DECL,
    STRUCT,
    MODIFIER,
    VARIABLE,
    GLOBAL,
    PROC_CALL,
    INITIALIZER_LIST,
    STRING,
    ARRAY_AT,
    RETURN,
    CAST,
    BREAK,
    CONT,

    B_START,  //binary operators start
    B_ADD,
    B_SUB,
    B_MUL,
    B_DIV,
    B_MOD,
    B_EQU,
    B_NEQU,
    B_GRT,
    B_GEQU,
    B_LSR,
    B_LEQU,
    B_END,    //binary operators end
    U_START,  //unary operators start
    U_NOT,
    U_NEG,
    U_MEM,
    U_PROC_MEM,
    U_FILL,
    U_END,    //unary operators end
};

struct VariableEntity;
struct ProcEntity;

struct ASTBase{
    u32 tokenOff;
    ASTType type;
};
struct ASTTypeNode : ASTBase{
    Type zType;
    u8 pointerDepth;
};
struct ASTBinOp : ASTBase{
    ASTBase *lhs;
    ASTBase *rhs;
    union{
        bool hasBracket;
        ASTTypeNode zType;
    };
};
struct ASTUnOp : ASTBase{
    ASTTypeNode childType;
    ASTBase *child;
};
struct ASTCast : ASTBase{
    ASTBase *child;
    ASTTypeNode *targetType;
    ASTTypeNode srcType;
};
struct ASTAssDecl : ASTBase{
    ASTBase **lhs;
    ASTBase *rhs;
    ASTTypeNode *zType;
    u32 lhsCount;
};
struct ASTNum : ASTBase{
    union{
        s64  integer;
        f64  decimal;
        char character;
    };
};
struct ASTIf : ASTBase{
    ASTBase *expr;
    ASTBase **ifBody;
    ASTBase **elseBody;
    u32 ifBodyCount;
    u32 elseBodyCount;
    Type zType;
};
struct ASTFor : ASTBase{
    ASTTypeNode exprType;
    String label;
    /*
     * infinite: expr = nullptr
     * c-while: end = nullptr
     */
    union{
        //c-while
        ASTBase *expr;
        //c-for
        ASTAssDecl *decl;
    };
    //c-for
    ASTBase *step;
    ASTBase *end;
    ASTBase **body;
    u32 bodyCount;
};
struct ASTProcDefDecl : ASTBase{
    String name;
    union{
        ASTAssDecl  **inputs;
        ASTTypeNode **typeInputs;
    };
    ASTTypeNode **outputs;
    ASTBase     **body;
    u32 inputCount;
    u32 inputNodeCount;
    bool varArgs;
    u32 outputCount;
    u32 bodyCount;
};
struct ASTStruct : ASTBase{
    String name;
    ASTBase **body;
    u32 id;
    u32 bodyCount;
};
struct ASTVariable : ASTBase{
    String name;
    VariableEntity *entity;
    u8 pAccessDepth;
};
struct ASTProcCall : ASTBase{
    String name;
    ASTBase **args;
    ASTTypeNode *types;
    ProcEntity *entity;
    u32 argCount;
};
struct ASTInitializerList : ASTBase{
    ASTBase **elements;
    Type zType;
    u32 id;
    u32 elementCount;
};
struct ASTModifier : ASTBase{
    String name;
    ASTBase *child;
    VariableEntity *entity;
    u8 pAccessDepth;
};
struct ASTFlow : ASTBase{
    union{
        s32 relId;
        String label;
    };
};
struct ASTArrayAt : ASTBase{
    ASTTypeNode parentType;
    ASTBase *at;
    ASTVariable *parent;
    ASTBase *child;
    Type atType;
};
struct ASTString : ASTBase{
    String str;
};
struct ASTReturn : ASTBase{
    ASTBase **exprs;
    ASTTypeNode *types;
    u32 retCount;
};

struct ASTFile{
    DynamicArray<char*>    pages;
    DynamicArray<ASTBase*> nodes;
    DynamicArray<u32>      dependencies;
    u32 curPageWatermark;
    u32 id;

    void init(u32 astId);
    void uninit();
    ASTBase* newNode(u64 size, ASTType type, u32 tokenOff);
    //bump-allocator for AST node members
    void* balloc(u64 size);
};

String makeStringFromTokOff(u32 x, Lexer &lexer);

extern DynamicArray<ASTBase*> deferStatements;
