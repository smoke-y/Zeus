#define AST_PAGE_SIZE 1024
#define BRING_TOKENS_TO_SCOPE DynamicArray<TokType> &tokTypes = lexer.tokenTypes;DynamicArray<TokenOffset> &tokOffs = lexer.tokenOffsets;

enum class ASTType{
    IDENTIFIER,
    DECLERATION,
    ASSIGNMENT,
    INTEGER,
    DECIMAL,

    B_ADD,   //binary operators start
    B_SUB,
    B_MUL,
    B_DIV,
    B_MOD,   //binary operators end
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
struct ASTAssDecl : ASTBase{
    String  *lhs;
    ASTBase *rhs;
    u32 lhsCount;
};
struct ASTNum : ASTBase{
    union{
        s64 integer;
        f64 decimal;
    };
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
ASTBase* _genASTExprTree(Lexer &lexer, ASTFile &file, u32 &xArg, u32 end){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x);
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
    if(x == end){return lhs;};
    //build operator
    ASTType type;
    switch (tokTypes[x]) {
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
    binOp->hasBracket = false;
    x++;
    //build rest of expression
    ASTBase *rhs = _genASTExprTree(lexer, file, x, end);
    if(rhs == nullptr){return nullptr;};
    binOp->lhs = lhs;
    binOp->rhs = rhs;
    return binOp;
};
ASTBase* genASTExprTree(Lexer &lexer, ASTFile &file, u32 &x, u32 end){
    //TODO: check brackets
    return _genASTExprTree(lexer, file, x, end);
};

inline bool isEndOfLineOrFile(DynamicArray<TokType> &types, u32 x){
    return types[x] == (TokType)'\n' || types[x] == TokType::END_OF_FILE;
};
inline u32 getEndOfLineOrFile(DynamicArray<TokType> &types, u32 x){
    while(!isEndOfLineOrFile(types, x)) x++;
    return x;
}
bool parseBlock(Lexer &lexer, ASTFile &file, u32 &xArg){
    BRING_TOKENS_TO_SCOPE;
    u32 x = xArg;
    DEFER(xArg = x);
    u32 start = x;
    switch(tokTypes[x]){
        case TokType::IDENTIFIER:
            x++;
            switch(tokTypes[x]){
                case (TokType)',':
                case (TokType)'=':
                case (TokType)':':{
                    //decleration (or) assignment
                    while(tokTypes[x] != (TokType)':' && tokTypes[x] != (TokType)'='){
                        if(tokTypes[x] != (TokType)','){
                            lexer.emitErr(tokOffs[x].off, "Expected ',' or ':'");
                            return false;
                        };
                        x++;
                        if(tokTypes[x] != TokType::IDENTIFIER){
                            lexer.emitErr(tokOffs[x].off, "Expected an identifier");
                            return false;
                        };
                        x++;
                    };
                    ASTType type = ASTType::DECLERATION;
                    if(tokTypes[x] == (TokType)'=') type = ASTType::ASSIGNMENT;
                    ASTAssDecl *assdecl = (ASTAssDecl*)file.newNode(sizeof(ASTAssDecl), type);
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
                    file.nodes.push(assdecl);
                }break;
            };
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
    void dumpASTNode(ASTBase *node, u8 padding=0){
        PLOG("[NODE]");
        PLOG("type: ");
        bool hasNotDumped = true;
        switch(node->type){
            case ASTType::ASSIGNMENT:{
                ASTAssDecl *assdecl = (ASTAssDecl*)node;
                printf("assignment");
                PLOG("lhs: ");
                dumpStrings(assdecl->lhs, assdecl->lhsCount);
            }break;
            case ASTType::DECLERATION:{
                ASTAssDecl *assdecl = (ASTAssDecl*)node;
                printf("decleration");
                PLOG("lhs: ");
                dumpStrings(assdecl->lhs, assdecl->lhsCount);
            }break;
            case ASTType::INTEGER:{
                ASTNum *num = (ASTNum*)node;
                printf("integer");
                PLOG("value: ");
                printf("%lld", num->integer);
            }break;
            case ASTType::B_ADD: if(hasNotDumped){printf("add");hasNotDumped=false;};
            case ASTType::B_SUB: if(hasNotDumped){printf("sub");hasNotDumped=false;};
            case ASTType::B_MUL: if(hasNotDumped){printf("mul");hasNotDumped=false;};
            case ASTType::B_DIV:{
                if(hasNotDumped){printf("div");hasNotDumped=false;};
                ASTBinOp *op = (ASTBinOp*)node;
                PLOG("lhs:");
                dumpASTNode(op->lhs, padding+1);
                PLOG("rhs:");
                dumpASTNode(op->rhs, padding+1);
            }break;
            default: UNREACHABLE;
        };
    };
    void dumpASTFile(ASTFile &file){
        for(u32 x=0; x<file.nodes.count; x++){
            dumpASTNode(file.nodes[x]);
        };
        printf("\n");
    };
};
#endif