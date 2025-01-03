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
    u32 tokenOff;
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
        ASTBase *step;
    };
    VariableEntity *entity;
    String iter;
    ASTTypeNode *type;
    ASTBase *initializer;
    ASTBase *end;
    ASTBase **body;
    u32 bodyCount;
    u32 tokenOff;
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

    void init(){
        dependencies.init();
        pages.init();
        nodes.init();
        pages.push((char*)mem::alloc(AST_PAGE_SIZE));
        curPageWatermark = 0;
    };
    void uninit(){
        dependencies.uninit();
        for(u32 x=0; x<pages.count; x++) mem::free(pages[x]);
        pages.uninit();
        nodes.uninit();
    };
    ASTBase* newNode(u64 size, ASTType type){
        if(curPageWatermark+size >= AST_PAGE_SIZE){
            pages.push((char*)mem::alloc(AST_PAGE_SIZE));
            curPageWatermark = 0;
        };
        ASTBase *node = (ASTBase*)(pages[pages.count-1] + curPageWatermark);
        curPageWatermark += size;
        node->type = type;
        return node;
    };
    //bump-allocator for AST node members
    void* balloc(u64 size){
        if(curPageWatermark+size >= AST_PAGE_SIZE){
            pages.push((char*)mem::alloc(AST_PAGE_SIZE));
            curPageWatermark = 0;
        };
        char *mem = pages[pages.count-1] + curPageWatermark;
        curPageWatermark += size;
        return mem;
    };
};

//------------DEPENDENCY-SYSTEM-----------------------
struct FileEntity{
    Lexer lexer;
    ASTFile file;
};
static DynamicArray<FileEntity> linearDepEntities;
static DynamicArray<String> linearDepStrings;
//------------DEPENDENCY-SYSTEM-----------------------
//POUND-SUPPORT
static f32 pStackSize = 1;  //mb

//TODO: Hacked together af. REWRITE(return u64)
s64 string2int(const String &str){
    s64 num = 0;
    u32 len = str.len;
    for(u32 x=0; x<str.len; x+=1){
        char c = str[x];
        switch(c){
        case '.':
        case '_':
            len -= 1;
            continue;
        };
    };
    u32 y = 0;
    for(u32 x=0; x<str.len; x+=1){
        char c = str[x];
        switch(c){
        case '.':
        case '_': continue;
        };
        num += (c - '0') * pow(10, len-y-1);
        y += 1;
    };
    return num;
}
f64 string2float(const String &str){
    u32 decimal = 0;
    while(str[decimal] != '.'){decimal += 1;};
    u32 postDecimalBadChar = 0;
    for(u32 x=decimal+1; x<str.len; x+=1){
    	if(str[x] == '_'){postDecimalBadChar += 1;};
    };
    s64 num = string2int(str);
    return (f64)num/pow(10, str.len-decimal-1-postDecimalBadChar);
};
String makeStringFromTokOff(u32 x, Lexer &lexer){
    BRING_TOKENS_TO_SCOPE;
    TokenOffset off = tokOffs[x];
    String str;
    str.len = off.len;
    str.mem = lexer.fileContent + off.off;
    return str;
};
u32 getOperatorPriority(ASTType op){
    switch(op){
        case ASTType::B_ADD:
        case ASTType::B_SUB: return 1;
        case ASTType::B_MUL:
        case ASTType::B_DIV:
        case ASTType::B_MOD: return 2;
    };
    return 0;
};
ASTBase* genASTExprTree(Lexer &lexer, ASTFile &file, u32 &x);
ASTBase *genVariable(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x-1);
    if(tokTypes[x] != TokType::IDENTIFIER){
        lexer.emitErr(tokOffs[x].off, "Expected an identifier");
        return nullptr;
    };
    ASTBase *root = nullptr;
    bool childReq = false;
    ASTBase **childWriteLoc = nullptr;
    while(tokTypes[x] == TokType::IDENTIFIER){
        u32 start = x;
        x += 1;
        u8 pointerDepth = 0;
        while(tokTypes[x] == (TokType)'^'){
            pointerDepth += 1;
            x += 1;
        };
        if(tokTypes[x] == (TokType)'.'){
            ASTModifier *mod = (ASTModifier*)file.newNode(sizeof(ASTModifier), ASTType::MODIFIER);
            mod->name = makeStringFromTokOff(start, lexer);
            mod->tokenOff = start;
            mod->pAccessDepth = pointerDepth;
            childReq = true;
            if(root == nullptr){root = mod;};
            if(childWriteLoc){*childWriteLoc = mod;};
            childWriteLoc = &mod->child;
            x += 1;
        }else{
            ASTVariable *var = (ASTVariable*)file.newNode(sizeof(ASTVariable), ASTType::VARIABLE);
            var->name = makeStringFromTokOff(start, lexer);
            var->tokenOff = start;
            var->pAccessDepth = pointerDepth;
            childReq = false;
            if(tokTypes[x] == (TokType)'['){
                x++;
                ASTArrayAt *arrayAt = (ASTArrayAt*)file.newNode(sizeof(ASTArrayAt), ASTType::ARRAY_AT);
                ASTBase *at = genASTExprTree(lexer, file, x);
                if(!at) return nullptr;
                arrayAt->at = at;
                arrayAt->parent = var;
                arrayAt->child = nullptr;
                x += 2;
                if(childWriteLoc){*childWriteLoc = arrayAt;};
                childWriteLoc = &arrayAt->child;
            }else if(childWriteLoc) *childWriteLoc = var;
            if(root == nullptr){root = var;};
        };
    };
    if(childReq){
        lexer.emitErr(tokOffs[x].off, "Identifier required");
        return nullptr;
    };
    return root;
};
ASTTypeNode* genASTTypeNode(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x);
    u8 pointerDepth = 0;
    while(tokTypes[x] == (TokType)'^'){
        pointerDepth++;
        x++;
    };
    if(isType(tokTypes[x]) == false && tokTypes[x] != TokType::IDENTIFIER){
        lexer.emitErr(tokOffs[x].off, "Expected a type");
        return nullptr;
    };
    ASTTypeNode *type = (ASTTypeNode*)file.newNode(sizeof(ASTTypeNode), ASTType::TYPE);
    type->tokenOff = x++;
    type->pointerDepth = pointerDepth;
    return type;
};
ASTBase* _genASTExprTree(Lexer &lexer, ASTFile &file, u32 &xArg, u8 &bracketArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    u8 bracket = bracketArg;
    bool hasBracket = false;
    DEFER({
        xArg = x;
        bracketArg = bracket;
    });
    //opening bracket '('
    if(tokTypes[x] == (TokType)'('){
        hasBracket=true;
        while(tokTypes[x] == (TokType)'('){
            x++;
            bracket++;
        };
    };
    ASTUnOp *unOp = nullptr;
    //build unary operator
    ASTType unaryType = ASTType::INVALID;
    switch(tokTypes[x]){
        case (TokType)'-':
            unaryType = ASTType::U_NEG;
            break;
        case (TokType)'!':
            unaryType = ASTType::U_NOT;
            break;
        case (TokType)'&':
            unaryType = ASTType::U_MEM;
            break;
    };
    if(unaryType != ASTType::INVALID){
        unOp = (ASTUnOp*)file.newNode(sizeof(ASTUnOp), unaryType);
        unOp->tokenOff = x++;
    };
    //build operand
    ASTBase *lhs;
    switch(tokTypes[x]){
        case TokType::INTEGER:{
            s64 value = string2int(makeStringFromTokOff(x, lexer));
            ASTNum *num = (ASTNum*)file.newNode(sizeof(ASTNum), ASTType::INTEGER);
            num->integer = value;
            lhs = num;
        }break;
        case TokType::DECIMAL:{
            f64 value = string2float(makeStringFromTokOff(x, lexer));
            ASTNum *num = (ASTNum*)file.newNode(sizeof(ASTNum), ASTType::DECIMAL);
            num->decimal = value;
            lhs = num;
        }break;
        case TokType::K_FALSE:
        case TokType::K_TRUE:{
            ASTNum *num = (ASTNum*)file.newNode(sizeof(ASTNum), ASTType::BOOL);
            num->isTrue = (tokTypes[x] == TokType::K_TRUE)?true:false;
            lhs = num;
        }break;
        case TokType::IDENTIFIER:{
            if(tokTypes[x+1] == (TokType)'('){
                ASTProcCall *pcall = (ASTProcCall*)file.newNode(sizeof(ASTProcCall), ASTType::PROC_CALL);
                pcall->tokenOff = x;
                pcall->name = makeStringFromTokOff(x, lexer);
                x += 2;
                DynamicArray<ASTBase*> args;
                args.init();
                while(true){
                    ASTBase *arg = genASTExprTree(lexer, file, x);
                    if(!arg){
                        args.uninit();
                        return nullptr;
                    }
                    args.push(arg);
                    if(tokTypes[x] == (TokType)')') break;
                    if(tokTypes[x] != (TokType)','){
                        lexer.emitErr(tokOffs[x].off, "Expected ')' or ','");
                        return nullptr;
                    };
                    x++;
                };
                x++;
                u32 size = sizeof(ASTBase*)*args.count;
                ASTBase **argNodes = (ASTBase**)file.balloc(size);
                memcpy(argNodes, args.mem, size);
                pcall->args = argNodes;
                pcall->argCount = args.count;
                args.uninit();
                lhs = pcall;
            }else{
                lhs = genVariable(lexer, file, x);
                if(!lhs) return nullptr;
            }
        }break;
        default:{
            lexer.emitErr(tokOffs[x].off, "Invalid operand");
            return nullptr;
        }break;
    };
    if(unOp){
        unOp->child = lhs;
        lhs = unOp;
    };
    x++;
    //closing bracket ')'
    if(tokTypes[x] == (TokType)')'){
        hasBracket=false;
        while(tokTypes[x] == (TokType)')'){
            if(bracket == 0) return lhs;
            x++;
            bracket--;
        }
    };
    //build operator
    ASTType type;
    switch (tokTypes[x]) {
        case TokType::END_OF_FILE:
        case TokType::TDOT:
        case TokType::DDOT:
        case (TokType)'\n':
        case (TokType)'{':
        case (TokType)']':
        case (TokType)',': return lhs;
        case (TokType)'-': type = ASTType::B_SUB; break;
        case (TokType)'+': type = ASTType::B_ADD; break;
        case (TokType)'*': type = ASTType::B_MUL; break;
        case (TokType)'/': type = ASTType::B_DIV; break;
        case (TokType)'=':{
            if(tokTypes[x+1] == (TokType)'='){
                x++;
                type = ASTType::B_EQU;
                break;
            };
        };
        case (TokType)'>':{
            if(tokTypes[x+1] == (TokType)'='){
                x++;
                type = ASTType::B_GEQU;
            }else type = ASTType::B_GRT;
        }break;
        case (TokType)'<':{
            if(tokTypes[x+1] == (TokType)'='){
                x++;
                type = ASTType::B_LEQU;
            }else type = ASTType::B_LSR;
        }break;
        default:{
            lexer.emitErr(tokOffs[x].off, "Invalid operator");
            return nullptr;
        }break;
    };
    ASTBinOp *binOp = (ASTBinOp*)file.newNode(sizeof(ASTBinOp), type);
    binOp->tokenOff = x;
    binOp->hasBracket = hasBracket;
    x++;
    //build rest of expression
    ASTBase *rhs = _genASTExprTree(lexer, file, x, bracket);
    if(!rhs){return nullptr;};
    binOp->lhs = lhs;
    binOp->rhs = rhs;
    if(rhs->type > ASTType::B_START && rhs->type < ASTType::B_END){
        u32 rhsPriority = getOperatorPriority(rhs->type);
        u32 curPriority = getOperatorPriority(binOp->type);
        ASTBinOp *rhsBin = (ASTBinOp*)rhs;
        if(rhsPriority < curPriority && !rhsBin->hasBracket){
            //fix tree branches(https://youtu.be/MnctEW1oL-E?si=6NnDgPSeX0F-aFD_&t=3696)
            binOp->rhs = rhsBin->lhs;
            rhsBin->lhs = binOp;
            return rhsBin;
        };
    };
    return binOp;
};
ASTBase* genASTExprTree(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u8 bracket = 0;
    u32 start = xArg;
    switch(tokTypes[xArg]){
        case (TokType)'{':{
            //initializer list
            u32 x = xArg;
            DEFER(xArg = x);
            x++;
            DynamicArray<ASTBase*> elements;
            elements.init();
            while(true){
                ASTBase *node = genASTExprTree(lexer, file, x);
                if(!node){
                    elements.uninit();
                    return nullptr;
                }
                elements.push(node);
                if(tokTypes[x] == (TokType)'}') break;
                if(tokTypes[x] != (TokType)','){
                    lexer.emitErr(tokOffs[x].off, "Expected ','");
                    elements.uninit();
                    return nullptr;
                };
                x++;
            };
            x++;
            u32 size = sizeof(ASTBase*)*elements.count;
            ASTBase **elementNodes = (ASTBase**)file.balloc(size);
            memcpy(elementNodes, elements.mem, size);
            ASTInitializerList *list = (ASTInitializerList*)file.newNode(sizeof(ASTInitializerList), ASTType::INITIALIZER_LIST);
            list->elements = elementNodes;
            list->elementCount = elements.count;
            elements.uninit();
            return list;
        }break;
        case TokType::DOUBLE_QUOTES:{
            ASTString *str = (ASTString*)file.newNode(sizeof(ASTString), ASTType::STRING);
            str->str = makeStringFromTokOff(xArg, lexer);
            xArg++;
            return str;
        }break;
        case TokType::SINGLE_QUOTES:{
            ASTNum *character = (ASTNum*)file.newNode(sizeof(ASTNum), ASTType::CHARACTER);
            character->character = (char)lexer.fileContent[tokOffs[xArg].off];
            xArg++;
            return character;
        }break;
    };
    ASTBase *tree = _genASTExprTree(lexer, file, xArg, bracket);
    if(bracket != 0){
        lexer.emitErr(tokOffs[start].off, "Expected %d closing bracket%sin this expression", bracket, (bracket==1)?" ":"s ");
        return nullptr;
    };
    return tree;
};

inline u32 eatNewLine(DynamicArray<TokType> &types, u32 x){
    while(types[x] == (TokType)'\n') x++;
    return x;
};
bool parseBlock(Lexer &lexer, ASTFile &file, DynamicArray<ASTBase*> &table, u32 &xArg);
ASTBase** parseBody(Lexer &lexer, ASTFile &file, u32 &xArg, u32 &count, bool insertReturn = false){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x);
    if(tokTypes[x] == (TokType)'{'){
        u32 start = x;
        x++;
        x = eatNewLine(lexer.tokenTypes, x);
        DynamicArray<ASTBase*> bodyTable;
        bodyTable.init();
        while(tokTypes[x] != (TokType)'}'){
            if(!parseBlock(lexer, file, bodyTable, x)){
                bodyTable.uninit();
                return nullptr;
            };
            if(tokTypes[x] == TokType::END_OF_FILE){
                lexer.emitErr(tokOffs[start].off, "Expected closing '}'");
                return nullptr;
            };
        };
        if(insertReturn){
            ASTReturn *ret = (ASTReturn*)file.newNode(sizeof(ASTReturn), ASTType::RETURN);
            ret->retCount = 0;
            bodyTable.push(ret);
        }
        u32 size = sizeof(ASTBase*) * bodyTable.count;
        ASTBase **bodyNodes = (ASTBase**)file.balloc(size);
        memcpy(bodyNodes, bodyTable.mem, size);
        bodyTable.uninit();
        x++;
        count = bodyTable.count;
        return bodyNodes;
    }else if(tokTypes[x] == (TokType)':'){
        x++;
        DynamicArray<ASTBase*> bodyTable;
        bodyTable.init(1);
        if(!parseBlock(lexer, file, bodyTable, x)) return nullptr;
        ASTBase **bodyNode = (ASTBase**)file.balloc(sizeof(ASTBase*));
        *bodyNode = bodyTable[0];
        bodyTable.uninit();
        count = 1;
        return bodyNode;
    }else{
        lexer.emitErr(tokOffs[x].off, "Expected '{' or ':'");
        return nullptr;
    };
};
ASTAssDecl* parseAssDecl(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    u32 start = x;
    DEFER(xArg = x);
    DynamicArray<ASTBase*> lhs;
    lhs.init();
    ASTBase *var = genVariable(lexer, file, x);
    if(!var){
        lhs.uninit();
        return nullptr;
    }
    lhs.push(var);
    u32 lhsCount = 1;
    x++;
    while(tokTypes[x] != (TokType)':' && tokTypes[x] != (TokType)'='){
        if(tokTypes[x] != (TokType)','){
            lexer.emitErr(tokOffs[x].off, "Expected ',' or ':'");
            return nullptr;
        };
        x++;
        var = genVariable(lexer, file, x);
        if(!var){
            lhs.uninit();
            return nullptr;
        }
        x++;
        lhsCount++;
        lhs.push(var);
    };
    ASTAssDecl *assdecl = (ASTAssDecl*)file.newNode(sizeof(ASTAssDecl), ASTType::DECLERATION);
    u32 size = sizeof(ASTBase*)*lhsCount;
    ASTBase **lhsNodes = (ASTBase**)file.balloc(size);
    memcpy(lhsNodes, lhs.mem, size);
    lhs.uninit();
    assdecl->lhsCount = lhsCount;
    assdecl->lhs = lhsNodes;
    if(tokTypes[x] == (TokType)'='){assdecl->type = ASTType::ASSIGNMENT;}
    else{
        x++;
        if(tokTypes[x] != (TokType)'='){
            ASTTypeNode *type = genASTTypeNode(lexer, file, x);
            assdecl->zType = type;
            if(tokTypes[x] != (TokType)'='){
                assdecl->rhs = nullptr;
                return assdecl;
            };
        }else{assdecl->zType = nullptr;};
    };
    assdecl->tokenOff = x;
    x++;
    ASTBase *expr = genASTExprTree(lexer, file, x);
    if(!expr) return nullptr;
    assdecl->rhs = expr;
    return assdecl;
};
bool parseBlock(Lexer &lexer, ASTFile &file, DynamicArray<ASTBase*> &table, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    x = eatNewLine(tokTypes, x);
    DEFER({
        x = eatNewLine(tokTypes, x);
        xArg = x;
    });
    u32 start = x;
    switch(tokTypes[x]){
        case TokType::P_STACK_SIZE:{
            if(tokTypes[++x] != (TokType)'='){
                lexer.emitErr(tokOffs[x].off, "Expected a '='");
                return false;
            };
            if(tokTypes[++x] == TokType::INTEGER) pStackSize = (f32)string2int(makeStringFromTokOff(x, lexer));
            else if(tokTypes[x] == TokType::DECIMAL) pStackSize = (f32)string2float(makeStringFromTokOff(x, lexer));
            else{
                lexer.emitErr(tokOffs[x].off, "Expected an integer or a decimal");
                return false;
            };
            x++;
        }break;
        case TokType::P_IMPORT:{
            if(tokTypes[++x] != TokType::DOUBLE_QUOTES){
                lexer.emitErr(tokOffs[x].off, "Expected a string");
                return false;
            };
            String name = makeStringFromTokOff(x, lexer);
            file.dependencies.push(linearDepStrings.count);
            linearDepStrings.push(name);
            x++;
        }break;
        case TokType::K_FOR:{
            ASTFor *For = (ASTFor*)file.newNode(sizeof(ASTFor), ASTType::FOR);
            For->tokenOff = ++x;
            if(tokTypes[x+1] == (TokType)':'){
                //c-for
                For->iter = makeStringFromTokOff(x, lexer);
                x += 2;
                ASTBase *node;
                if(tokTypes[x] != (TokType)'='){
                    ASTTypeNode *typeNode = genASTTypeNode(lexer, file, x);
                    if(!typeNode) return false;
                    For->type = typeNode;
                }else For->type = nullptr;
                if(tokTypes[x] != (TokType)'='){
                    lexer.emitErr(tokOffs[x].off, "Expected '='");
                    return false;
                };
                x++;
                node = genASTExprTree(lexer, file, x);
                if(!node) return false;
                For->initializer = node;
                if(tokTypes[x] != TokType::TDOT){
                    lexer.emitErr(tokOffs[x].off, "Expected '...'");
                    return false;
                }
                node = genASTExprTree(lexer, file, ++x);
                if(!node) return false;
                For->end = node;
                if(tokTypes[x] == TokType::DDOT){
                    node = genASTExprTree(lexer, file, ++x);
                    if(!node) return false;
                    For->step = node;
                }else For->step = nullptr;
            }else{
                For->initializer = nullptr;
                if(tokTypes[x] == (TokType)'{' || tokTypes[x] == (TokType)':'){
                    //for ever
                    For->expr = nullptr;
                }else{
                    //c-while
                    ASTBase *node = genASTExprTree(lexer, file, x);
                    if(!node) return false;
                    For->expr = node;
                };
            };
            u32 count;
            ASTBase **body = parseBody(lexer, file, x, count);
            if(!body) return false;
            For->body = body;
            For->bodyCount = count;
            table.push(For);
        }break;
        case TokType::K_IF:{
            ASTIf *If = (ASTIf*)file.newNode(sizeof(ASTIf), ASTType::IF);
            If->exprTokenOff = ++x;
            ASTBase *expr = genASTExprTree(lexer, file, x);
            if(!expr) return false;
            If->expr = expr;
            x = eatNewLine(lexer.tokenTypes, x);
            u32 count;
            ASTBase **bodyNodes = parseBody(lexer, file, x, count);
            if(!bodyNodes) return false;
            If->ifBody = bodyNodes;
            If->ifBodyCount = count;
            if(tokTypes[x] == TokType::K_ELSE){
                if(tokTypes[++x] == TokType::K_IF){
                    //else if
                    DynamicArray<ASTBase*> elseIfBody;
                    elseIfBody.init();
                    if(!parseBlock(lexer, file, elseIfBody, x)){
                        elseIfBody.uninit();
                        return false;  
                    };
                    ASTBase **elseIfNode = (ASTBase**)file.balloc(sizeof(ASTBase*));
                    *elseIfNode = elseIfBody[0];
                    elseIfBody.uninit();
                    If->elseBody = elseIfNode;
                    If->elseBodyCount = 1;
                }else{
                    bodyNodes = parseBody(lexer, file, x, count);
                    if(!bodyNodes) return false;
                    If->elseBody = bodyNodes;
                    If->elseBodyCount = count;
                };
            }else If->elseBodyCount = 0;
            table.push(If);
        }break;
        case TokType::IDENTIFIER:{
            x++;
            if(tokTypes[x] == (TokType)':' && tokTypes[x+1] == (TokType)':'){
                //struct (or) proc (or) enum
                x += 2;
                switch(tokTypes[x]){
                    case TokType::K_STRUCT:{
                        ASTStruct *Struct = (ASTStruct*)file.newNode(sizeof(ASTStruct), ASTType::STRUCT);
                        Struct->name = makeStringFromTokOff(start, lexer);
                        Struct->tokenOff = start;
                        u32 count;
                        ASTBase **body = parseBody(lexer, file, ++x, count);
                        if(!body) return false;
                        Struct->body = body;
                        Struct->bodyCount = count;
                        table.push(Struct);
                    }break;
                    case TokType::K_PROC:{
                        if(tokTypes[++x] != (TokType)'('){
                            lexer.emitErr(tokOffs[x].off, "Expected '('");
                            return false;
                        };
                        ASTProcDefDecl *proc = (ASTProcDefDecl*)file.newNode(sizeof(ASTProcDefDecl), ASTType::PROC_DEF);
                        proc->name = makeStringFromTokOff(start, lexer);
                        proc->tokenOff = start;
                        if(tokTypes[++x] == (TokType)')'){proc->inputCount = 0;}
                        else{
                            DynamicArray<ASTAssDecl*> inputs;
                            inputs.init();
                            while(true){
                                ASTAssDecl *input = parseAssDecl(lexer, file, x);
                                if(!input){
                                    inputs.uninit();
                                    return false;
                                };
                                inputs.push(input);
                                if(tokTypes[x] != (TokType)')' && tokTypes[x] != (TokType)','){
                                    lexer.emitErr(tokOffs[x].off, "Expected ')' or ','");
                                    inputs.uninit();
                                    return false;
                                }else if(tokTypes[x] == (TokType)')') break;
                                x++;
                            };
                            u32 size = sizeof(ASTAssDecl*)*inputs.count;
                            ASTAssDecl **inputNodes = (ASTAssDecl**)file.balloc(size);
                            memcpy(inputNodes, inputs.mem, size);
                            proc->inputs = inputNodes;
                            proc->inputCount = inputs.count;
                            inputs.uninit();
                        };
                        bool defaultReturn = true;
                        if(tokTypes[++x] == (TokType)'-'){
                            if(tokTypes[++x] != (TokType)'>'){
                                lexer.emitErr(tokOffs[x].off, "Expected '>'");
                                return false;
                            }
                            bool bracket = false;
                            if(tokTypes[++x] == (TokType)'('){
                                bracket = true;
                                x++;
                            };
                            DynamicArray<ASTTypeNode*> outputs;
                            outputs.init();
                            while(true){
                                ASTTypeNode *output = genASTTypeNode(lexer, file, x);
                                if(!output){
                                    outputs.uninit();
                                    return false;
                                };
                                outputs.push(output);
                                if(tokTypes[x] != (TokType)')' && tokTypes[x] != (TokType)',' && tokTypes[x] != (TokType)'{'){
                                    lexer.emitErr(tokOffs[x].off, "Expected ')' or ',' or '{'");
                                    outputs.uninit();
                                    return false;
                                }else if(tokTypes[x] == (TokType)'{') break;
                                else if(tokTypes[x] == (TokType)')'){
                                    if(!bracket){
                                        lexer.emitErr(tokOffs[x].off, "No opening bracket to match this closing bracket");
                                        outputs.uninit();
                                        return false;
                                    }else{
                                        x++;
                                        break;
                                    };
                                }
                                x++;
                            };
                            u32 size = sizeof(ASTBase*)*outputs.count;
                            ASTTypeNode **outputNodes = (ASTTypeNode**)file.balloc(size);
                            memcpy(outputNodes, outputs.mem, size);
                            proc->outputs = outputNodes;
                            proc->outputCount = outputs.count;
                            defaultReturn = outputs.count == 0;
                            outputs.uninit();
                        }else proc->outputCount = 0;
                        u32 count;
                        ASTBase **body = parseBody(lexer, file, x, count, defaultReturn);
                        proc->body = body;
                        proc->bodyCount = count;
                        table.push(proc);
                    }break;
                };
                return true;
            };
            bool shouldParseAssOrDecl = false;
            while(tokTypes[x] != (TokType)'\n' && tokTypes[x] != TokType::END_OF_FILE){
                if((tokTypes[x] == (TokType)'=' && tokTypes[x+1] != (TokType)'=') || tokTypes[x] == (TokType)':'){
                    shouldParseAssOrDecl = true;
                    break;
                };
                x++;
            };
            if(!shouldParseAssOrDecl){
                x = start;
                goto PARSE_EXPRESSION;
            };
            ASTBase *assdecl = parseAssDecl(lexer, file, start);
            if(!assdecl) return false;
            x = start;
            table.push(assdecl);
        }break;
        case TokType::K_ELSE:{
            lexer.emitErr(tokOffs[x].off, "Expected 'if' before 'else'");
            return false;
        }break;
        default:{
            PARSE_EXPRESSION:
            ASTBase *expr = genASTExprTree(lexer, file, x);
            if(!expr) return false;
            table.push(expr);
        }break;
    };
    return true;
};
bool parseFile(Lexer &lexer, ASTFile &file){
    BRING_TOKENS_TO_SCOPE;
    u32 cursor = eatNewLine(tokTypes, 0);
    while(tokTypes[cursor] != TokType::END_OF_FILE){
        if(!parseBlock(lexer, file, file.nodes, cursor)) return false;
    };
    return true;
};

#if(DBG)

#define PLOG(...) pad(padding);printf(__VA_ARGS__)

namespace dbg{
    inline void pad(u8 padding){
        printf("\n");
        for(u8 x=0; x<padding; x++) printf("    ");
    };
    inline void dumpStrings(String *strs, u32 count){
        for(u32 x=0; x<count; x++){
            const String &str = strs[x];
            printf("%.*s ", str.len, str.mem);
        };
    };
    void dumpASTNode(ASTBase *node, Lexer &lexer, u8 padding);
    void dumpASTBody(ASTBase **bodyNodes, u32 count, Lexer &lexer, u8 padding){
        for(u32 x=0; x<count; x++){
            ASTBase *node = bodyNodes[x];
            dumpASTNode(node, lexer, padding);
        };
    };
    void dumpASTNode(ASTBase *node, Lexer &lexer, u8 padding){
        PLOG("[NODE]");
        PLOG("type: ");
        bool hasNotDumped = true;
        switch(node->type){
            case ASTType::U_MEM: printf("u_not"); hasNotDumped = false;
            case ASTType::U_NEG: if(hasNotDumped){printf("u_neg"); hasNotDumped = false;};
            case ASTType::U_NOT:{
                if(hasNotDumped) printf("u_not");
                PLOG("child:");
                ASTUnOp *unOp = (ASTUnOp*)node;
                dumpASTNode(unOp->child, lexer, padding+1);
            }break;
            case ASTType::BOOL:{
                ASTNum *num = (ASTNum*)node;
                printf("bool");
                PLOG("value: %s", (num->isTrue)?"true":"false");
            }break;
            case ASTType::CHARACTER:{
                ASTNum *num = (ASTNum*)node;
                printf("character");
                PLOG("value: \'%c\'(%d)", num->character, (u8)num->character);
            }break;
            case ASTType::STRING:{
                ASTString *str = (ASTString*)node;
                printf("string");
                PLOG("value: %.*s", str->str.len, str->str.mem);
            }break;
            case ASTType::ARRAY_AT:{
                ASTArrayAt *arrayAt = (ASTArrayAt*)node;
                printf("array_at");
                PLOG("at:");
                dumpASTNode(arrayAt->at, lexer, padding+1);
                PLOG("parent:");
                dumpASTNode(arrayAt->parent, lexer, padding+1);
                if(arrayAt->child){
                    PLOG("child:");
                    dumpASTNode(arrayAt->child, lexer, padding+1);
                };
            }break;
            case ASTType::INITIALIZER_LIST:{
                ASTInitializerList *list = (ASTInitializerList*)node;
                printf("initializer_list");
                PLOG("elements:");
                dumpASTBody(list->elements, list->elementCount, lexer, padding+1);
            }break;
            case ASTType::PROC_CALL:{
                ASTProcCall *pcall = (ASTProcCall*)node;
                printf("proc_call");
                PLOG("name: %.*s", pcall->name.len, pcall->name.mem);
                PLOG("args:");
                dumpASTBody(pcall->args, pcall->argCount, lexer, padding+1);
            }break;
            case ASTType::VARIABLE:{
                ASTVariable *var = (ASTVariable*)node;
                printf("variable");
                PLOG("name: %.*s", var->name.len, var->name.mem);
                PLOG("pointer_access_depth: %d", var->pAccessDepth);
            }break;
            case ASTType::MODIFIER:{
                ASTModifier *mod = (ASTModifier*)node;
                printf("modifier");
                PLOG("name: %.*s", mod->name.len, mod->name.mem);
                PLOG("pointer_access_depth: %d", mod->pAccessDepth);
                PLOG("child:");
                dumpASTNode(mod->child, lexer, padding+1);
            }break;
            case ASTType::STRUCT:{
                ASTStruct *Struct = (ASTStruct*)node;
                printf("struct");
                PLOG("name: %.*s", Struct->name.len, Struct->name.mem);
                PLOG("body:");
                dumpASTBody(Struct->body, Struct->bodyCount, lexer, padding+1);
            }break;
            case ASTType::PROC_DEF:{
                ASTProcDefDecl *proc = (ASTProcDefDecl*)node;
                printf("proc_def");
                PLOG("name: %.*s", proc->name.len, proc->name.mem);
                if(proc->inputCount){
                    PLOG("input:");
                    dumpASTBody((ASTBase**)proc->inputs, proc->inputCount, lexer, padding+1);
                };
                if(proc->outputCount){
                    PLOG("output:");
                    dumpASTBody((ASTBase**)proc->outputs, proc->outputCount, lexer, padding+1);
                }
                PLOG("body:");
                dumpASTBody(proc->body, proc->bodyCount, lexer, padding+1);
            }break;
            case ASTType::FOR:{
                ASTFor *For = (ASTFor*)node;
                if(For->expr == nullptr && For->initializer == nullptr){
                    printf("for ever");
                }else if(For->initializer != nullptr){
                    printf("for(c-for)");
                    PLOG("iter: %.*s", For->iter.len, For->iter.mem);
                    if(For->type){
                        PLOG("type:");
                        dumpASTNode(For->type, lexer, padding+1);
                    }
                    PLOG("start:");
                    dumpASTNode(For->initializer, lexer, padding+1);
                    PLOG("end:");
                    dumpASTNode(For->end, lexer, padding+1);
                    if(For->step){
                        PLOG("step:");
                        dumpASTNode(For->step, lexer, padding+1);
                    };
                }else{
                    printf("for(c-while)");
                    PLOG("expr:");
                    dumpASTNode(For->expr, lexer, padding+1);
                };
                PLOG("body:");
                dumpASTBody(For->body, For->bodyCount, lexer, padding+1);
            }break;
            case ASTType::IF:{
                ASTIf *If = (ASTIf*)node;
                printf("if");
                PLOG("expr:");
                dumpASTNode(If->expr, lexer, padding+1);
                PLOG("if_body(%d):", If->ifBodyCount);
                dumpASTBody(If->ifBody, If->ifBodyCount, lexer, padding+1);
                if(If->elseBodyCount != 0){
                    PLOG("else_body(%d):", If->elseBodyCount);
                    dumpASTBody(If->elseBody, If->elseBodyCount, lexer, padding+1);
                };
            }break;
            case ASTType::ASSIGNMENT:{
                ASTAssDecl *assdecl = (ASTAssDecl*)node;
                printf("assignment");
                PLOG("lhs: ");
                dumpASTBody(assdecl->lhs, assdecl->lhsCount, lexer, padding+1);
                PLOG("rhs:");
                dumpASTNode(assdecl->rhs, lexer, padding+1);
            }break;
            case ASTType::DECLERATION:{
                ASTAssDecl *assdecl = (ASTAssDecl*)node;
                printf("decleration");
                if(assdecl->zType){
                    PLOG("type:");
                    dumpASTNode(assdecl->zType, lexer, padding+1);
                };
                PLOG("lhs: ");
                dumpASTBody(assdecl->lhs, assdecl->lhsCount, lexer, padding+1);
                if(assdecl->rhs){
                    PLOG("rhs:");
                    dumpASTNode(assdecl->rhs, lexer, padding+1);
                };
            }break;
            case ASTType::DECIMAL:{
                ASTNum *num = (ASTNum*)node;
                printf("decimal");
                PLOG("value: %f", num->decimal);
            }break;
            case ASTType::INTEGER:{
                ASTNum *num = (ASTNum*)node;
                printf("integer");
                PLOG("value: %lld", num->integer);
            }break;
            case ASTType::TYPE:{
                ASTTypeNode *type = (ASTTypeNode*)node;
                printf("type");
                String ztype = makeStringFromTokOff(type->tokenOff, lexer);
                PLOG("z_type: %.*s", ztype.len, ztype.mem);
                PLOG("pointer_depth: %d", type->pointerDepth);
            }break;
            case ASTType::RETURN:{
                ASTReturn *ret = (ASTReturn*)node;
                printf("return");
                for(u32 x=0; x<ret->retCount; x++) dumpASTNode(ret->exprs[0], lexer, padding+1);
            }break;
            case ASTType::B_LEQU: if(hasNotDumped){printf("lequ");hasNotDumped=false;};
            case ASTType::B_GEQU: if(hasNotDumped){printf("gequ");hasNotDumped=false;};
            case ASTType::B_GRT: if(hasNotDumped){printf("grt");hasNotDumped=false;};
            case ASTType::B_LSR: if(hasNotDumped){printf("lsr");hasNotDumped=false;};
            case ASTType::B_EQU: if(hasNotDumped){printf("equ");hasNotDumped=false;};
            case ASTType::B_ADD: if(hasNotDumped){printf("add");hasNotDumped=false;};
            case ASTType::B_SUB: if(hasNotDumped){printf("sub");hasNotDumped=false;};
            case ASTType::B_MUL: if(hasNotDumped){printf("mul");hasNotDumped=false;};
            case ASTType::B_DIV:{
                if(hasNotDumped){printf("div");hasNotDumped=false;};
                ASTBinOp *op = (ASTBinOp*)node;
                PLOG("lhs:");
                dumpASTNode(op->lhs, lexer, padding+1);
                PLOG("rhs:");
                dumpASTNode(op->rhs, lexer, padding+1);
            }break;
            default: UNREACHABLE;
        };
    };
    void dumpASTFile(ASTFile &file, Lexer &lexer){
        for(u32 x=0; x<file.nodes.count; x++){
            dumpASTNode(file.nodes[x], lexer, 0);
        };
        printf("\n");
    };
};
#endif