#include "../include/parser.hh"

void ASTFile::init(){
    dependencies.init();
    pages.init();
    nodes.init();
    pages.push((char*)mem::alloc(AST_PAGE_SIZE));
    curPageWatermark = 0;
};
void ASTFile::uninit(){
    dependencies.uninit();
    for(u32 x=0; x<pages.count; x++) mem::free(pages[x]);
    pages.uninit();
    nodes.uninit();
};
ASTBase* ASTFile::newNode(u64 size, ASTType type){
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
void* ASTFile::balloc(u64 size){
    if(curPageWatermark+size >= AST_PAGE_SIZE){
        pages.push((char*)mem::alloc(AST_PAGE_SIZE));
        curPageWatermark = 0;
    };
    char *mem = pages[pages.count-1] + curPageWatermark;
    curPageWatermark += size;
    return mem;
};

//------------DEPENDENCY-SYSTEM-----------------------
static DynamicArray<FileEntity> linearDepEntities;
static DynamicArray<String> linearDepStrings;
//------------DEPENDENCY-SYSTEM-----------------------
//POUND-SUPPORT
static f32 pStackSize = 1;  //mb

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
s32 getBracketEnding(DynamicArray<TokType> &src, u32 off, char braOpen, char braClose){
    u32 level = 1;
    while(src[off] != (TokType)('\n')){
        if(src[off] == (TokType)braOpen) level++;
        else if(src[off] == (TokType)braClose) level--;
        if(level == 0) return off;
        off++;
    };
    return -1;
};
s32 getTokenOff(TokType type, Lexer &lexer, u32 x){
    BRING_TOKENS_TO_SCOPE;
    while(tokTypes[x] != TokType::END_OF_FILE || tokTypes[x] != (TokType)'\n'){
        x++;
        if(tokTypes[x] == type) return x;
    };
    return -1;
};
s32 getCommanEnding(DynamicArray<TokType> &src, u32 off){
    u32 level = 0;
    while(src[off] != (TokType)'\n'){
        if(src[off] == (TokType)',' && level == 0) return off;
        else if(src[off] == (TokType)'(') level++;
        else if(src[off] == (TokType)')') level--;
        off++;
    };
    return -1;
};
u32 getEnding(DynamicArray<TokType> &src, u32 off){
    while(src[off] != TokType::END_OF_FILE && src[off] != (TokType)'\n') off++;
    return off;
};
s32 getBodyStartOrReportErr(u32 off, Lexer &lexer){
    BRING_TOKENS_TO_SCOPE;
    u32 start = off;
    while(tokTypes[off] != (TokType)'\n' && tokTypes[off] != TokType::END_OF_FILE){
        off++;
        if(tokTypes[off] == (TokType)'{') return off;
    };
    lexer.emitErr(start, "Expected '{' for the body");
    return -1;
};
ASTBase* genASTExprTree(Lexer &lexer, ASTFile &file, u32 &x, u32 end);
ASTBase *genVariable(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    if(tokTypes[x] != TokType::IDENTIFIER){
        lexer.emitErr(x, "Expected an identifier");
        return nullptr;
    };
    DEFER(xArg = x-1);
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
                s32 end = getBracketEnding(tokTypes, x, '[', ']');
                if(end == -1){
                    lexer.emitErr(x-1, "Expected ending ']'");
                    return nullptr;
                };
                ASTArrayAt *arrayAt = (ASTArrayAt*)file.newNode(sizeof(ASTArrayAt), ASTType::ARRAY_AT);
                ASTBase *at = genASTExprTree(lexer, file, x, end);
                if(!at) return nullptr;
                arrayAt->at = at;
                arrayAt->parent = var;
                arrayAt->child = nullptr;
                x += 2;
                if(childWriteLoc){*childWriteLoc = arrayAt;};
                childWriteLoc = &arrayAt->child;
                var = (ASTVariable*) arrayAt;
            }else if(childWriteLoc) *childWriteLoc = var;
            if(root == nullptr){root = var;};
        };
    };
    if(childReq){
        lexer.emitErr(x, "Identifier required");
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
        lexer.emitErr(x, "Expected a type");
        return nullptr;
    };
    ASTTypeNode *type = (ASTTypeNode*)file.newNode(sizeof(ASTTypeNode), ASTType::TYPE);
    type->tokenOff = x++;
    type->pointerDepth = pointerDepth;
    return type;
};
ASTBase* _genASTExprTree(Lexer &lexer, ASTFile &file, u32 &xArg, u8 &bracketArg, u32 end){
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
                                         s32 bracketEnding = getBracketEnding(tokTypes, x+2, '(', ')');
                                         if(bracketEnding == -1){
                                             lexer.emitErr(x-1, "Expected closing ')'");
                                             return nullptr;
                                         };
                                         ASTProcCall *pcall = (ASTProcCall*)file.newNode(sizeof(ASTProcCall), ASTType::PROC_CALL);
                                         pcall->tokenOff = x;
                                         pcall->name = makeStringFromTokOff(x, lexer);
                                         x += 1;
                                         DynamicArray<ASTBase*> args;
                                         args.init();
                                         while(tokTypes[x] != (TokType)')'){
                                             x++;
                                             s32 end = getCommanEnding(tokTypes, x);
                                             if(end == -1) end = bracketEnding;
                                             ASTBase *arg = genASTExprTree(lexer, file, x, end);
                                             if(arg == nullptr){
                                                 args.uninit();
                                                 return nullptr;
                                             }
                                             args.push(arg);
                                             x = end;
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
                    lexer.emitErr(x, "Invalid operand");
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
    if(x >= end) return lhs;
    //build operator
    ASTType type;
    switch (tokTypes[x]) {
        case (TokType)'-': type = ASTType::B_SUB; break;
        case (TokType)'+': type = ASTType::B_ADD; break;
        case (TokType)'*': type = ASTType::B_MUL; break;
        case (TokType)'/': type = ASTType::B_DIV; break;
        case (TokType)'=':{
                              if(tokTypes[x+1] == (TokType)'=') type = ASTType::B_EQU;
                              else{
                                  lexer.emitErr(x+1, "Expected '='");
                                  return nullptr;
                              };
                          }break; 
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
                    lexer.emitErr(x, "Invalid operator");
                    return nullptr;
                }break;
    };
    ASTBinOp *binOp = (ASTBinOp*)file.newNode(sizeof(ASTBinOp), type);
    binOp->tokenOff = x;
    binOp->hasBracket = hasBracket;
    x++;
    //build rest of expression
    ASTBase *rhs = _genASTExprTree(lexer, file, x, bracket, end);
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
ASTBase* genASTExprTree(Lexer &lexer, ASTFile &file, u32 &xArg, u32 end){
    BRING_TOKENS_TO_SCOPE;
    u8 bracket = 0;
    u32 start = xArg;
    switch(tokTypes[xArg]){
        case (TokType)'{':{
                              //initializer list
                              u32 x = xArg;
                              s32 bracketEnding = getBracketEnding(tokTypes, x, '{', '}');
                              if(bracketEnding == -1){
                                  lexer.emitErr(x, "Expected ending '}'");
                                  return nullptr;
                              }; 
                              x++;
                              DynamicArray<ASTBase*> elements;
                              DEFER({
                                      xArg = x;
                                      elements.uninit();
                                      });
                              elements.init();
                              while(true){
                                  s32 end = getCommanEnding(tokTypes, x);
                                  if(end == -1) end = bracketEnding;
                                  ASTBase *node = genASTExprTree(lexer, file, x, end);
                                  if(!node) return nullptr;
                                  elements.push(node);
                                  if(tokTypes[x] == (TokType)'}') break;
                                  if(tokTypes[x] != (TokType)','){
                                      lexer.emitErr(x, "Expected ','");
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
    ASTBase *tree = _genASTExprTree(lexer, file, xArg, bracket, end);
    if(bracket != 0){
        lexer.emitErr(start, "Expected %d closing bracket%sin this expression", bracket, (bracket==1)?" ":"s ");
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
    if(tokTypes[x] != (TokType)'{'){
        lexer.emitErr(x, "Expected '{'");
        return nullptr;
    };
    u32 start = x;
    x = eatNewLine(lexer.tokenTypes, x+1);
    DynamicArray<ASTBase*> bodyTable;
    DEFER({
            xArg = x;
            bodyTable.uninit();
            });
    bodyTable.init();
    while(tokTypes[x] != (TokType)'}'){
        if(!parseBlock(lexer, file, bodyTable, x)) return nullptr;
        if(tokTypes[x] == TokType::END_OF_FILE){
            lexer.emitErr(start, "Expected closing '}'");
            return nullptr;
        };
    };
    u32 size = sizeof(ASTBase*) * bodyTable.count;
    ASTBase **bodyNodes = (ASTBase**)file.balloc(size);
    memcpy(bodyNodes, bodyTable.mem, size);
    x++;
    count = bodyTable.count;
    return bodyNodes;
};
ASTAssDecl* parseAssDecl(Lexer &lexer, ASTFile &file, u32 &xArg, u32 ending = 0){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    u32 start = x;
    DynamicArray<ASTBase*> lhs;
    lhs.init();
    DEFER({
            xArg = x;
            lhs.uninit();
            });
    ASTBase *var = genVariable(lexer, file, x);
    if(!var) return nullptr;
    lhs.push(var);
    u32 lhsCount = 1;
    x++;
    while(tokTypes[x] != (TokType)':' && tokTypes[x] != (TokType)'='){
        if(tokTypes[x] != (TokType)','){
            lexer.emitErr(x, "Expected ',' or ':'");
            return nullptr;
        };
        x++;
        var = genVariable(lexer, file, x);
        if(!var) return nullptr;
        x++;
        lhsCount++;
        lhs.push(var);
    };
    ASTAssDecl *assdecl = (ASTAssDecl*)file.newNode(sizeof(ASTAssDecl), ASTType::DECLERATION);
    u32 size = sizeof(ASTBase*)*lhsCount;
    ASTBase **lhsNodes = (ASTBase**)file.balloc(size);
    memcpy(lhsNodes, lhs.mem, size);
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
    if(ending == 0) ending = getEnding(tokTypes, x);
    ASTBase *expr = genASTExprTree(lexer, file, x, ending);
    if(!expr) return nullptr;
    assdecl->rhs = expr;
    return assdecl;
};
ASTFor* parseForLoop(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x);
    ASTFor *For = (ASTFor*)file.newNode(sizeof(ASTFor), ASTType::FOR);
    For->tokenOff = x++;
    s32 bodyStart = getBodyStartOrReportErr(x, lexer);
    if(bodyStart == -1) return nullptr;
    s32 tdot = getTokenOff(TokType::TDOT, lexer, x);
    if(tdot != -1){
        //c-for
        For->decl = parseAssDecl(lexer, file, x, tdot);
        u32 end = x;
        while(tokTypes[end] != TokType::DDOT && tokTypes[end] != (TokType)'{') end++;
        x++;
        ASTBase *node = genASTExprTree(lexer, file, x, end);
        if(!node) return nullptr;
        For->end = node;
        if(tokTypes[x] == TokType::DDOT){
            x++;
            end = getBodyStartOrReportErr(x, lexer);
            if(end == -1) return nullptr;
            node = genASTExprTree(lexer, file, x, end);
            if(!node) return nullptr;
            For->step = node;
        }else For->step = nullptr;
    }else{
        if(tokTypes[x] != (TokType)'{' || tokTypes[x] != (TokType)':'){
            //c-while
            ASTBase *node = genASTExprTree(lexer, file, x, bodyStart);
            if(!node) return nullptr;
            For->expr = node;
        }else{
            //for ever
            For->decl = nullptr;
        };
    };
    u32 count;
    ASTBase **body = parseBody(lexer, file, x, count);
    if(!body) return nullptr;
    For->body = body;
    For->bodyCount = count;
    return For;
};
ASTIf *parseIfElse(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x);
    x++;
    s32 end = getBodyStartOrReportErr(x, lexer);
    if(end == -1) return nullptr;
    ASTBase *expr = genASTExprTree(lexer, file, x, end);
    if(!expr) return nullptr;
    ASTIf *If = (ASTIf*)file.newNode(sizeof(ASTIf), ASTType::IF);
    If->exprTokenOff = x;
    If->expr = expr;
    x = eatNewLine(lexer.tokenTypes, x);
    u32 count;
    ASTBase **bodyNodes = parseBody(lexer, file, x, count);
    if(!bodyNodes) return nullptr;
    If->ifBody = bodyNodes;
    If->ifBodyCount = count;
    if(tokTypes[x] == TokType::K_ELSE){
        if(tokTypes[++x] == TokType::K_IF){
            //else if
            DynamicArray<ASTBase*> elseIfBody;
            elseIfBody.init();
            if(!parseBlock(lexer, file, elseIfBody, x)){
                elseIfBody.uninit();
                return nullptr;
            };
            ASTBase **elseIfNode = (ASTBase**)file.balloc(sizeof(ASTBase*));
            *elseIfNode = elseIfBody[0];
            elseIfBody.uninit();
            If->elseBody = elseIfNode;
            If->elseBodyCount = 1;
        }else{
            bodyNodes = parseBody(lexer, file, x, count);
            if(!bodyNodes) return nullptr;
            If->elseBody = bodyNodes;
            If->elseBodyCount = count;
        };
    }else If->elseBodyCount = 0;
    return If;
}
ASTStruct *parseStruct(Lexer &lexer, ASTFile &file, u32 &xArg){
    u32 x = xArg;
    DEFER(xArg = x);
    ASTStruct *Struct = (ASTStruct*)file.newNode(sizeof(ASTStruct), ASTType::STRUCT);
    Struct->name = makeStringFromTokOff(x - 3, lexer);
    Struct->tokenOff = x-3;
    u32 count;
    ASTBase **body = parseBody(lexer, file, ++x, count);
    if(!body) return nullptr;
    Struct->body = body;
    Struct->bodyCount = count;
    return Struct;
};
ASTProcDefDecl *parseProcDefDecl(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x);
    if(tokTypes[++x] != (TokType)'('){
        lexer.emitErr(x, "Expected '('");
        return nullptr;
    };
    s32 braStart = getBodyStartOrReportErr(x, lexer);
    if(braStart == -1) return nullptr;
    ASTProcDefDecl *proc = (ASTProcDefDecl*)file.newNode(sizeof(ASTProcDefDecl), ASTType::PROC_DEF);
    proc->name = makeStringFromTokOff(x-4, lexer);
    proc->tokenOff = x-4;
    if(tokTypes[x+1] == (TokType)')'){
        x++;
        proc->inputCount = 0;
    }else{
        DynamicArray<ASTAssDecl*> inputs;
        inputs.init();
        while(tokTypes[x] == (TokType)')'){
            x++;
            s32 end = getCommanEnding(tokTypes, x);
            if(end == -1) end = getBracketEnding(tokTypes, x, '(', ')');
            ASTAssDecl *input = parseAssDecl(lexer, file, x, end);
            if(!input){
                inputs.uninit();
                return nullptr;
            };
            inputs.push(input);
        };
        u32 size = sizeof(ASTAssDecl*)*inputs.count;
        ASTAssDecl **inputNodes = (ASTAssDecl**)file.balloc(size);
        memcpy(inputNodes, inputs.mem, size);
        proc->inputs = inputNodes;
        proc->inputCount = inputs.count;
        inputs.uninit();
    };
    if(tokTypes[++x] == (TokType)'-'){
        if(tokTypes[++x] != (TokType)'>'){
            lexer.emitErr(x, "Expected '>'");
            return nullptr;
        }
        bool bracket = false;
        if(tokTypes[++x] == (TokType)'('){
            bracket = true;
            x++;
        };
        DynamicArray<ASTTypeNode*> outputs;
        outputs.init();
        DEFER(outputs.uninit());
        while(true){
            ASTTypeNode *output = genASTTypeNode(lexer, file, x);
            if(!output) return nullptr;
            outputs.push(output);
            if(tokTypes[x] != (TokType)')' && tokTypes[x] != (TokType)',' && tokTypes[x] != (TokType)'{'){
                lexer.emitErr(x, "Expected ')' or ',' or '{'");
                return nullptr;
            }else if(tokTypes[x] == (TokType)'{'){
                if(bracket){
                    lexer.emitErr(x, "Expected ending ')' bracket");
                    return nullptr;
                };
                break;
            }else if(tokTypes[x] == (TokType)')'){
                if(!bracket){
                    lexer.emitErr(x, "No opening bracket to match this closing bracket");
                    return nullptr;
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
    }else proc->outputCount = 0;
    u32 count;
    ASTBase **body = parseBody(lexer, file, x, count);
    proc->body = body;
    proc->bodyCount = count;
    return proc;
};
bool parseBlock(Lexer &lexer, ASTFile &file, DynamicArray<ASTBase*> &table, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    x = eatNewLine(tokTypes, x);
    DEFER({
            x = eatNewLine(tokTypes, x);
            xArg = x;
            });
    switch(tokTypes[x]){
        case TokType::P_IMPORT:{
                                   if(tokTypes[++x] != TokType::DOUBLE_QUOTES){
                                       lexer.emitErr(x, "Expected a string");
                                       return false;
                                   };
                                   String name = makeStringFromTokOff(x, lexer);
                                   file.dependencies.push(linearDepStrings.count);
                                   linearDepStrings.push(name);
                                   x++;
                               }break;
        case TokType::K_FOR:{
                                ASTFor *For = parseForLoop(lexer, file, x);
                                if(For == nullptr) return false;
                                table.push(For);
                            }break;
        case TokType::K_IF:{ 
                               ASTIf *If = parseIfElse(lexer, file, x);
                               if(If == nullptr) return false;
                               table.push(If);
                           }break;
        case TokType::IDENTIFIER:{
                                     if(tokTypes[x+1] == (TokType)':' && tokTypes[x+2] == (TokType)':'){
                                         //struct (or) proc (or) enum
                                         x += 3;
                                         switch(tokTypes[x]){
                                             case TokType::K_STRUCT:{
                                                                        ASTStruct *Struct = parseStruct(lexer, file, x);
                                                                        if(Struct == nullptr) return false;
                                                                        table.push(Struct);
                                                                    }break;

                                             case TokType::K_PROC:{
                                                                      ASTProcDefDecl *proc = parseProcDefDecl(lexer, file, x);
                                                                      if(proc == nullptr) return false;
                                                                      table.push(proc);
                                                                  }break;
                                         };
                                         return true;
                                     };
                                     bool shouldParseAssOrDecl = false;
                                     u32 v = x;
                                     while(tokTypes[v] != (TokType)'\n' && tokTypes[v] != TokType::END_OF_FILE){
                                         if((tokTypes[v] == (TokType)'=' && tokTypes[v+1] != (TokType)'=') || tokTypes[v] == (TokType)':'){
                                             shouldParseAssOrDecl = true;
                                             break;
                                         };
                                         v++;
                                     };
                                     if(!shouldParseAssOrDecl) goto PARSE_EXPRESSION;
                                     ASTBase *assdecl = parseAssDecl(lexer, file, x);
                                     if(!assdecl) return false;
                                     table.push(assdecl);
                                 }break;
        case TokType::K_ELSE:{
                                 lexer.emitErr(x, "Expected 'if' before 'else'");
                                 return false;
                             }break;
        default:{
PARSE_EXPRESSION:
                    ASTBase *expr = genASTExprTree(lexer, file, x, getEnding(tokTypes, x));
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
                                  if(For->expr == nullptr && For->decl == nullptr){
                                      printf("for ever");
                                  }else if(For->decl != nullptr){
                                      printf("for(c-for)");
                                      dumpASTNode(For->decl, lexer, padding+1);
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
