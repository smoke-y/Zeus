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
    BOOL,
    TYPE,
    IF,
    FOR,
    PROC_DEF,
    PROC_DECL,
    STRUCT,
    MODIFIER,
    VARIABLE,
    PROC_CALL,
    INITIALIZER_LIST,
    STRING,
    ARRAY_AT,
    RETURN,

    B_START,  //binary operators start
    B_ADD,
    B_SUB,
    B_MUL,
    B_DIV,
    B_MOD,
    B_EQU,
    B_GRT,
    B_GEQU,
    B_LSR,
    B_LEQU,
    B_END,    //binary operators end
    U_START,  //unary operators start
    U_NOT,
    U_NEG,
    U_MEM,
    U_END,    //unary operators end
};

struct VariableEntity;
struct ASTBase{
    ASTType type;
};
struct ASTBinOp : ASTBase{
    ASTBase *lhs;
    ASTBase *rhs;
    u32 tokenOff;
    bool hasBracket;
};
struct ASTUnOp : ASTBase{
    ASTBase *child;
    u32 tokenOff;
};
struct ASTTypeNode : ASTBase{
    union{
        Type zType;
        u32 tokenOff;
    };
    u8 pointerDepth;
};
struct ASTAssDecl : ASTBase{
    ASTBase **lhs;
    ASTBase *rhs;
    ASTTypeNode *zType;
    u32 lhsCount;
    union{
        u32 tokenOff;
        Type treeType;
    };
};
struct ASTNum : ASTBase{
    union{
        s64  integer;
        f64  decimal;
        bool isTrue;
        char character;
    };
};
struct ASTIf : ASTBase{
    ASTBase *expr;
    ASTBase **ifBody;
    ASTBase **elseBody;
    u32 ifBodyCount;
    u32 elseBodyCount;
    u32 exprTokenOff;
    Type zType;
};
struct ASTFor : ASTBase{
    //when expr and initializer is nullptr, then we have an infinite loop
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
    union{
        u32 tokenOff;
        Type zType;
    };
};
struct ASTProcDefDecl : ASTBase{
    String name;
    ASTAssDecl  **inputs;
    ASTTypeNode **outputs;
    ASTBase     **body;
    u32 inputCount;
    u32 outputCount;
    u32 bodyCount;
    u32 tokenOff;
};
struct ASTStruct : ASTBase{
    String name;
    ASTBase **body;
    u32 bodyCount;
    u32 tokenOff;
};
struct ASTVariable : ASTBase{
    String name;
    union{
        u32 tokenOff;
        VariableEntity *entity;
    };
    u8 pAccessDepth;
};
struct ASTModifier : ASTBase{
    String name;
    ASTBase *child;
    union{
        u32 tokenOff;
        VariableEntity *entity;
    };
    u8 pAccessDepth;
};
struct ASTProcCall : ASTBase{
    String name;
    ASTBase **args;
    u32 argCount;
    u32 tokenOff;
};
struct ASTInitializerList : ASTBase{
    ASTBase **elements;
    u32 elementCount;
};
struct ASTArrayAt : ASTBase{
    ASTBase *at;
    ASTVariable *parent;
    ASTBase *child;
};
struct ASTString : ASTBase{
    String str;
};
struct ASTReturn : ASTBase{
    ASTBase **exprs;
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
    ASTBase* newNode(u64 size, ASTType type);
    //bump-allocator for AST node members
    void* balloc(u64 size);
};

String makeStringFromTokOff(u32 x, Lexer &lexer);
