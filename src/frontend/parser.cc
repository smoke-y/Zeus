#define AST_PAGE_SIZE 1024
#define BRING_TOKENS_TO_SCOPE DynamicArray<TokType> &tokTypes = lexer.tokenTypes;DynamicArray<TokenOffset> &tokOffs = lexer.tokenOffsets;

enum class ASTType{
    IDENTIFIER,
    DECLERATION,
    ASSIGNMENT,
    INTEGER,
    DECIMAL,
    TYPE,
    IF,
    FOR,
    PROC_DEF,
    PROC_DECL,

    B_START,  //binary operators start
    B_ADD,
    B_SUB,
    B_MUL,
    B_DIV,
    B_MOD,
    B_END,    //binary operators end
};

struct ASTBase{
    ASTType type;
};
struct ASTIdentifier : ASTBase{
    String name;
};
struct ASTBinOp : ASTBase{
    ASTBase *lhs;
    ASTBase *rhs;
    u32 tokenOff;
    bool hasBracket;
};
struct ASTTypeNode : ASTBase{
    union{
        Type zType;
        u32 tokenOff;
    };
    u8 pointerDepth;
};
struct ASTAssDecl : ASTBase{
    String  *lhs;
    ASTBase *rhs;
    ASTTypeNode *zType;
    u32 lhsCount;
};
struct ASTNum : ASTBase{
    union{
        s64 integer;
        f64 decimal;
    };
};
struct ASTIf : ASTBase{
    ASTBase *expr;
    ASTBase **ifBody;
    ASTBase **elseBody;
    u32 ifBodyCount;
    u32 elseBodyCount;
};
struct ASTFor : ASTBase{
    //when expr and intializer is nullptr, then we have an infinite loop
    union{
    //c-while
        ASTBase *expr;
    //c-for
        ASTBase *step;
    };
    String iter;
    ASTTypeNode *type;
    ASTBase *intializer;
    ASTBase *end;
    ASTBase **body;
    u32 bodyCount;
};
struct ASTProcDefDecl : ASTBase{
    String name;
    ASTBase     **inputs;
    ASTTypeNode **outputs;
    ASTBase     **body;
    u32 inputCount;
    u32 outputCount;
    u32 bodyCount;
};

struct ASTFile{
    DynamicArray<char*>    pages;
    DynamicArray<ASTBase*> nodes;
    u32 curPageWatermark;

    void init(){
        pages.init();
        nodes.init();
        pages.push((char*)mem::alloc(AST_PAGE_SIZE));
        curPageWatermark = 0;
    };
    void uninit(){
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
        case '_':
            continue;
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
        default:{
            lexer.emitErr(tokOffs[x].off, "Invalid operand");
            return nullptr;
        }break;
    };
    x++;
    //closing bracket ')'
    if(tokTypes[x] == (TokType)')'){
        hasBracket=false;
        while(tokTypes[x] == (TokType)')'){
            x++;
            if(bracket == 0) return lhs;
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
        case (TokType)',': return lhs;
        case (TokType)'-': type = ASTType::B_SUB; break;
        case (TokType)'+': type = ASTType::B_ADD; break;
        case (TokType)'*': type = ASTType::B_MUL; break;
        case (TokType)'/': type = ASTType::B_DIV; break;
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
ASTBase* genASTExprTree(Lexer &lexer, ASTFile &file, u32 &x){
    BRING_TOKENS_TO_SCOPE;
    u8 bracket = 0;
    u32 start = x;
    ASTBase *tree = _genASTExprTree(lexer, file, x, bracket);
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
ASTBase** parseBody(Lexer &lexer, ASTFile &file, u32 &xArg, u32 &count){
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
ASTBase* parseAssDecl(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    u32 start = x;
    DEFER(xArg = x);
    if(tokTypes[x] != TokType::IDENTIFIER){
        lexer.emitErr(tokOffs[x].off, "Expected an identifier");
        return nullptr;
    };
    x++;
    while(tokTypes[x] != (TokType)':' && tokTypes[x] != (TokType)'='){
        if(tokTypes[x] != (TokType)','){
            lexer.emitErr(tokOffs[x].off, "Expected ',' or ':'");
            return nullptr;
        };
        x++;
        if(tokTypes[x] != TokType::IDENTIFIER){
            lexer.emitErr(tokOffs[x].off, "Expected an identifier");
            return nullptr;
        };
        x++;
    };
    ASTAssDecl *assdecl = (ASTAssDecl*)file.newNode(sizeof(ASTAssDecl), ASTType::DECLERATION);
    u32 lhsCount = ((x-start)/2) + 1;
    String *strs = (String*)file.balloc(lhsCount * sizeof(String));
    assdecl->lhsCount = lhsCount;
    assdecl->lhs = strs;
    for(u32 i=start, j=0; i<x; i+=2, j++){
        String &str = strs[j];
        const TokenOffset &off = tokOffs[i];
        str.len = off.len;
        str.mem = lexer.fileContent + off.off;
    };
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
    x++;
    ASTBase *expr = genASTExprTree(lexer, file, x);
    if(!expr) return nullptr;
    assdecl->rhs = expr;
    return assdecl;
};
bool parseBlock(Lexer &lexer, ASTFile &file, DynamicArray<ASTBase*> &table, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER({
        x = eatNewLine(tokTypes, x);
        xArg = x;
    });
    u32 start = x;
    switch(tokTypes[x]){
        case TokType::K_FOR:{
            x++;
            ASTFor *For = (ASTFor*)file.newNode(sizeof(ASTFor), ASTType::FOR);
            if(tokTypes[x+1] == (TokType)':'){
                //c-for
                For->iter = makeStringFromTokOff(x, lexer);
                x += 2;
                ASTBase *node;
                if(tokTypes[x] != (TokType)'='){
                    ASTTypeNode *typeNode = genASTTypeNode(lexer, file, x);
                    if(!node) return false;
                    For->type = typeNode;
                }else For->type = nullptr;
                if(tokTypes[x] != (TokType)'='){
                    lexer.emitErr(tokOffs[x].off, "Expected '='");
                    return false;
                };
                x++;
                node = genASTExprTree(lexer, file, x);
                if(!node) return false;
                For->intializer = node;
                if(tokTypes[x] != TokType::TDOT){
                    lexer.emitErr(tokOffs[x].off, "Expected '...'");
                    return false;
                }
                x++;
                node = genASTExprTree(lexer, file, x);
                if(!node) return false;
                For->end = node;
                if(tokTypes[x] == TokType::DDOT){
                    x++;
                    node = genASTExprTree(lexer, file, x);
                    if(!node) return false;
                    For->step = node;
                }else For->step = nullptr;
            }else{
                For->intializer = nullptr;
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
            x++;
            ASTBase *expr = genASTExprTree(lexer, file, x);
            if(!expr) return false;
            ASTIf *If = (ASTIf*)file.newNode(sizeof(ASTIf), ASTType::IF);
            If->expr = expr;
            x = eatNewLine(lexer.tokenTypes, x);
            u32 count;
            ASTBase **bodyNodes = parseBody(lexer, file, x, count);
            if(!bodyNodes) return false;
            If->ifBody = bodyNodes;
            If->ifBodyCount = count;
            if(tokTypes[x] == TokType::K_ELSE){
                x++;
                if(tokTypes[x] == TokType::K_IF){
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
        case TokType::IDENTIFIER:
            x++;
            switch(tokTypes[x]){
                case (TokType)',':
                case (TokType)'=':
                case (TokType)':':{
                    if(tokTypes[x+1] == (TokType)':'){
                        //procedure (or) struct (or) enum
                        x += 2;
                        switch(tokTypes[x]){
                            case TokType::K_PROC:{
                                x++;
                                if(tokTypes[x] != (TokType)'('){
                                    lexer.emitErr(tokOffs[x].off, "Expected '('");
                                    return false;
                                };
                                ASTProcDefDecl *proc = (ASTProcDefDecl*)file.newNode(sizeof(ASTProcDefDecl), ASTType::PROC_DEF);
                                proc->name = makeStringFromTokOff(start, lexer);
                                x++;
                                if(tokTypes[x] == (TokType)')'){proc->inputCount = 0;}
                                else{
                                    DynamicArray<ASTBase*> inputs;
                                    inputs.init();
                                    while(true){
                                        ASTBase *input = parseAssDecl(lexer, file, x);
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
                                    };
                                    u32 size = sizeof(ASTBase*)*inputs.count;
                                    ASTBase **inputNodes = (ASTBase**)file.balloc(size);
                                    memcpy(inputNodes, inputs.mem, size);
                                    proc->inputs = inputNodes;
                                    proc->inputCount = inputs.count;
                                    inputs.uninit();
                                };
                                x++;
                                u32 count;
                                ASTBase **body = parseBody(lexer, file, x, count);
                                proc->body = body;
                                proc->bodyCount = count;
                                table.push(proc);
                            }break;
                        };
                    }else{
                        //decleration (or) assignment
                        ASTBase *assdecl = parseAssDecl(lexer, file, start);
                        if(!assdecl) return false;
                        x = start;
                        table.push(assdecl);
                    };
                }break;
            }break;
        case TokType::K_ELSE:{
            lexer.emitErr(tokOffs[x].off, "Expected 'if' before 'else'");
            return false;
        }break;
        default:{
            ASTBase *expr = genASTExprTree(lexer, file, x);
            if(!expr) return false;
            table.push(expr);
        };
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

#define PLOG(fmt, ...) pad(padding);printf(fmt, __VA_ARGS__)

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
            case ASTType::PROC_DEF:{
                ASTProcDefDecl *proc = (ASTProcDefDecl*)node;
                printf("proc_def");
                PLOG("name: %.*s", proc->name.len, proc->name.mem);
                if(proc->inputCount){
                    PLOG("input:");
                    dumpASTBody(proc->inputs, proc->inputCount, lexer, padding+1);
                };
                PLOG("body:");
                dumpASTBody(proc->body, proc->bodyCount, lexer, padding+1);
            }break;
            case ASTType::FOR:{
                ASTFor *For = (ASTFor*)node;
                if(For->expr == nullptr && For->intializer == nullptr){
                    printf("for ever");
                }else if(For->intializer != nullptr){
                    printf("for(c-for)");
                    PLOG("iter: %.*s", For->iter.len, For->iter.mem);
                    if(For->type){
                        PLOG("type:");
                        dumpASTNode(For->type, lexer, padding+1);
                    }
                    PLOG("start:");
                    dumpASTNode(For->intializer, lexer, padding+1);
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
                dumpStrings(assdecl->lhs, assdecl->lhsCount);
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
                dumpStrings(assdecl->lhs, assdecl->lhsCount);
                if(assdecl->rhs){
                    PLOG("rhs:");
                    dumpASTNode(assdecl->rhs, lexer, padding+1);
                };
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
            }break;
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