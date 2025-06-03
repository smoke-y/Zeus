#include "../include/checker.hh"
#include "../include/lexer.hh"
#include "../include/dependency.hh"

void Scope::init(ScopeType stype, u32 id){
    type = stype;
    varId = id;
    var.init();
    vars.init();
    proc.init();
    procs.init();
};
void Scope::uninit(){
    vars.uninit();
    var.uninit();
    proc.uninit();
    procs.uninit();
};

namespace check{
    Scope *globalScopes;
    DynamicArray<Scope*> structScopes;
    DynamicArray<Scope*> blockScopes;
    HashmapStr stringToId;
    static HashmapStr structToOff;
    static DynamicArray<StructEntity> structEntities;
    static ASTReturn defaultReturn;

    void init(){
        defaultReturn.retCount = 0;
        u32 size = sizeof(Scope) * dep::lexers.count;
        globalScopes = (Scope*)mem::alloc(size);
        memset(globalScopes, 0, size);
        structToOff.init();
        structEntities.init();
        structScopes.init();
        blockScopes.init();
        stringToId.init();
    };
    void uninit(){
        structToOff.uninit();
        structEntities.uninit();
        for(u32 x=0; x<dep::lexers.count; x++) globalScopes[x].uninit();
        mem::free(globalScopes);
        for(u32 x=0; x<structScopes.count; x++) structScopes[x]->uninit();
        structScopes.uninit();
        for(u32 x=0; x<blockScopes.count; x++) blockScopes[x]->uninit();
        blockScopes.uninit();
        stringToId.uninit();
    };
    Scope *newStructScope(){
        Scope *scope = (Scope*)mem::alloc(sizeof(Scope));
        scope->init(ScopeType::BLOCK, 0);
        structScopes.push(scope);
        return scope;
    };
    Scope *newBlockScope(u32 id){
        Scope *scope = (Scope*)mem::alloc(sizeof(Scope));
        scope->init(ScopeType::BLOCK, id);
        blockScopes.push(scope);
        return scope;
    };
};

VariableEntity *getVariableEntity(ASTBase *node, DynamicArray<Scope*> &scopes){
    String name;
    switch(node->type){
        case ASTType::VARIABLE:{
                                   ASTVariable *var = (ASTVariable*)node;
                                   name = var->name;
                               }break;
        case ASTType::MODIFIER:{
                                   ASTModifier *mod = (ASTModifier*)node;
                                   name = mod->name;
                               }break;
        default: return nullptr;
    }
    for(u32 x=scopes.count; x!=0;){
        x -= 1;
        Scope *scope = scopes[x];
        u32 off;
        if(!scope->var.getValue(name, &off)) continue;
        return scope->vars[off];
    };
    return nullptr;
};
StructEntity *getStructEntity(String name){
    u32 off;
    if(!check::structToOff.getValue(name, &off)) return nullptr;
    return &check::structEntities[off];
};
StructEntity *getStructEntity(Type type){
    u32 off = (u32)type - (u32)Type::COUNT - 1;
    if(off > check::structEntities.count) return nullptr;
    return &check::structEntities[off];
};
ProcEntity *getProcEntity(String name, DynamicArray<Scope*> &scopes){
    for(u32 x=scopes.count; x!=0;){
        x -= 1;
        Scope *scope = scopes[x];
        u32 off;
        if(!scope->proc.getValue(name, &off)) continue;
        return scope->procs[off];
    };
    return nullptr;
};

bool fillTypeInfo(Lexer &lexer, ASTTypeNode *node){
    BRING_TOKENS_TO_SCOPE;
    if(isType(tokTypes[node->tokenOff])){
        node->zType = (Type)((u32)tokTypes[node->tokenOff] - (u32)TokType::K_TYPE_START + 1);  //+1 since Type::BOOL
        return true;
    };
    if(tokTypes[node->tokenOff] != TokType::IDENTIFIER){
        lexer.emitErr(node->tokenOff, "Expected a type or a struct name");
        return false;
    };
    String name = makeStringFromTokOff(node->tokenOff, lexer);
    u32 off;
    if(!check::structToOff.getValue(name, &off)){
        lexer.emitErr(node->tokenOff, "Structure not defined");
        return false;
    };
    node->zType = (Type)(off + (u32)Type::COUNT + 1);
    return true;
};
Type checkModifierChain(Lexer &lexer, ASTBase *root, VariableEntity *entity){
    BRING_TOKENS_TO_SCOPE;
    Type structType = entity->type;
    StructEntity *structEntity = getStructEntity(structType);
    Scope *structBodyScope = structEntity->body;
    while(root){
        switch(root->type){
            case ASTType::MODIFIER:{
                                       ASTModifier *mod = (ASTModifier*)root;
                                       u32 off;
                                       if(!structBodyScope->var.getValue(mod->name, &off)){
                                           lexer.emitErr(mod->tokenOff, "%.*s does not belong to the defined structure", mod->name.len, mod->name.mem);
                                           return Type::INVALID;
                                       }
                                       return checkModifierChain(lexer, mod->child, structBodyScope->vars[off]);
                                   }break;
                                   //TODO: array_at
            case ASTType::VARIABLE:{
                                       ASTVariable *var = (ASTVariable*)root;
                                       u32 off;
                                       if(!structBodyScope->var.getValue(var->name, &off)){
                                           lexer.emitErr(var->tokenOff, "%.*s does not belong to the defined structure", var->name.len, var->name.mem);
                                           return Type::INVALID;
                                       };
                                       return structBodyScope->vars[off]->type;
                                   }break;
        };
    };
    return Type::INVALID;
};
u64 getSize(Lexer &lexer, Type type, u32 tokenOff){
    switch(type){
        case Type::COMP_STRING:
        case Type::COMP_DECIMAL:
        case Type::COMP_INTEGER:
        case Type::S64:
        case Type::U64:  return 64;
        case Type::BOOL:
        case Type::S32:
        case Type::U32:  return 32;
        case Type::S16:
        case Type::U16:  return 16;
        case Type::CHAR:
        case Type::S8:
        case Type::U8:   return 8;
        default:{
                    StructEntity *structEntity = getStructEntity(type);
                    if(structEntity == nullptr){
                        lexer.emitErr(tokenOff, "Structure not defined");
                        return 0;
                    };
                    return structEntity->size;
                }break;
    };
};

Type checkTree(Lexer &lexer, ASTBase *node, DynamicArray<Scope*> &scopes, u32 &pointerDepth){
    BRING_TOKENS_TO_SCOPE;
    pointerDepth = 0;
    ASTType unOpType = ASTType::INVALID;
    u32 unOpTokenOff;
    while(node->type > ASTType::U_START && node->type < ASTType::U_END){
        //unary ops return the type of the child
        ASTUnOp *unOp = (ASTUnOp*)node;
        unOpType = unOp->type;
        unOpTokenOff = unOp->tokenOff;
        node = unOp->child;
    };
    Type type = Type::INVALID;
    switch(node->type){
        case ASTType::CHARACTER: type = Type::CHAR;break;
        case ASTType::BOOL:      type = Type::BOOL;break;
        case ASTType::INTEGER:   type = Type::COMP_INTEGER;break;
        case ASTType::DECIMAL:   type = Type::COMP_DECIMAL;break;
        case ASTType::PROC_CALL:{
                                    ASTProcCall *proc = (ASTProcCall*)node;
                                    ProcEntity *entity = getProcEntity(proc->name,scopes);
                                    proc->entity = entity;
                                    if(entity == nullptr){
                                        lexer.emitErr(proc->tokenOff, "Procedure not defined/declared");
                                        return Type::INVALID;
                                    };
                                    if(entity->varArgs == false){
                                        if(entity->inputCount != proc->argCount){
                                            lexer.emitErr(proc->tokenOff, "Procedure defined with %d inputs, but received %d inputs", entity->inputCount, proc->argCount);
                                            return Type::INVALID;
                                        };
                                    }else{
                                        if(proc->argCount < entity->inputCount){
                                            lexer.emitErr(proc->tokenOff, "Procedure defined with %d inputs, but received %d inputs", entity->inputCount, proc->argCount);
                                            return Type::INVALID;
                                        };
                                    };
                                    for(u32 x=0; x<proc->argCount; x++){
                                        u32 pd;
                                        Type type = checkTree(lexer, proc->args[x], scopes, pd); 
                                        if(type == Type::INVALID) return Type::INVALID;
                                        ASTTypeNode *typeNode = &proc->types[x];
                                        typeNode->zType = type;
                                        typeNode->pointerDepth = pd;
                                        if(x < entity->inputCount){
                                            ASTTypeNode *typeNode;
                                            if(entity->isDecl) typeNode = entity->typeInputs[x];
                                            else typeNode = entity->inputs[x]->zType;
                                            //only exception
                                            if(typeNode->pointerDepth == 1 && typeNode->zType == Type::CHAR && type == Type::COMP_STRING) continue;
                                            if(typeNode->zType != type){
                                                lexer.emitErr(proc->tokenOff, "Argument %d type(%s) does not match procedure declaration's type(%s)", x, typeToStr[(u32)type], typeToStr[(u32)typeNode->zType]);
                                                return Type::INVALID;
                                            };
                                            if(typeNode->pointerDepth > 0 && pd == 0){
                                                lexer.emitErr(proc->tokenOff, "Argument %d requires pointer depth %d, but argument is not a pointer", x, typeNode->pointerDepth);
                                                return Type::INVALID;
                                            };
                                            if(typeNode->pointerDepth != pd){
                                                lexer.emitWarn(proc->tokenOff, "Argument %d pointer depth is %d, but procedure was declared with %d", x, pd, typeNode->pointerDepth);
                                            };
                                        };
                                    };
                                    if(entity->outputCount == 0) type = Type::VOID;
                                    else type = entity->outputs[0]->zType;
                                }break;
        case ASTType::STRING:{
                                 u32 off;
                                 ASTString *str = (ASTString*)node;
                                 if(!check::stringToId.getValue(str->str, &off)){
                                     check::stringToId.insertValue(str->str, check::stringToId.count);
                                 };
                                 type = Type::COMP_STRING;
                             }break;
        case ASTType::VARIABLE:{
                                   VariableEntity *entity = getVariableEntity(node, scopes);
                                   if(entity == nullptr){
                                       ASTVariable *var = (ASTVariable*)node;
                                       lexer.emitErr(var->tokenOff, "Variable not defined");
                                       return Type::INVALID;
                                   };
                                   ASTVariable *var = (ASTVariable*)node;
                                   var->entity = entity;
                                   pointerDepth = (pointerDepth>entity->pointerDepth)?pointerDepth:entity->pointerDepth;
                                   type = entity->type;
                               }break;
        case ASTType::MODIFIER:{
                                   ASTModifier *mod = (ASTModifier*)node;
                                   VariableEntity *entity = getVariableEntity(node, scopes);
                                   if(entity == nullptr){
                                       lexer.emitErr(mod->tokenOff, "Variable not defined");
                                       return Type::INVALID;
                                   };
                                   mod->entity = entity;
                                   type = checkModifierChain(lexer, mod->child, entity);
                               }break;
        default:{
                    if(node->type > ASTType::B_START && node->type < ASTType::B_END){
                        ASTBinOp *binOp = (ASTBinOp*)node;
                        u32 lhsUsingPointer, rhsUsingPointer;
                        Type lhsType = checkTree(lexer, binOp->lhs, scopes, lhsUsingPointer);
                        Type rhsType = checkTree(lexer, binOp->rhs, scopes, rhsUsingPointer);
                        if(lhsUsingPointer && rhsUsingPointer
                                && (binOp->type != ASTType::B_ADD || binOp->type != ASTType::B_SUB)){
                            lexer.emitErr(binOp->tokenOff, "Can only add/sub 2 pointers");
                            return Type::INVALID;
                        };
                        if(lhsType > Type::COUNT || rhsType > Type::COUNT){
                            lexer.emitErr(binOp->tokenOff, "Cannot perform binary operation with structures");
                            return Type::INVALID;
                        };
                        type = (lhsType <= rhsType)?lhsType:rhsType;
                    };
                }break;
    };
    switch(unOpType){
        case ASTType::U_NOT:{
                                if(type != Type::BOOL){
                                    lexer.emitErr(tokOffs[unOpTokenOff+1].off, "Cannot '!' on this");
                                    return Type::INVALID;

                                };
                            }break;
        case ASTType::U_NEG:{
                                if(isNumber(type) == false){
                                    lexer.emitErr(tokOffs[unOpTokenOff+1].off, "Cannot '-' on this");
                                    return Type::INVALID;
                                };
                            }break;
    };
    return type;
};
u64 checkDecl(ASTAssDecl *assdecl, DynamicArray<Scope*> &scopes, Lexer &lexer){
    BRING_TOKENS_TO_SCOPE;
    u32 typePointerDepth;
    Type typeType = Type::INVALID;
    u32 tokenOff = assdecl->tokenOff;
    if(assdecl->zType->zType != Type::INVALID){
        if(!fillTypeInfo(lexer, assdecl->zType)) return 0;
        ASTTypeNode *type = assdecl->zType;
        typeType = type->zType;
        typePointerDepth = type->pointerDepth;
    };
    if(assdecl->rhs){
        u32 treePointerDepth;
        Type treeType = checkTree(lexer, assdecl->rhs, scopes, treePointerDepth);
        if(treeType == Type::INVALID) return 0;
        assdecl->treeType = treeType;
        if(typeType != Type::INVALID){
            if(treePointerDepth != typePointerDepth){
                lexer.emitErr(tokenOff, "Expression tree pointer depth is not equal to type pointer depth");
                return 0;
            };
            if(treeType < typeType){
                lexer.emitErr(tokenOff, "Explicit cast required");
                return 0;
            };
        }else{
            typeType = treeType;
            typePointerDepth = treePointerDepth;
        };
    };
    Scope *scope = scopes[scopes.count-1];
    u64 size;
    if(typePointerDepth > 0) size = 64;
    else size = getSize(lexer, typeType, assdecl->tokenOff);
    for(u32 x=0; x<assdecl->lhsCount; x++){
        ASTBase *lhsNode = assdecl->lhs[x];
        if(getVariableEntity(lhsNode, scopes)){
            lexer.emitErr(tokenOff, "Redefinition");
            return 0;
        };
        String name;
        VariableEntity *entity = (VariableEntity*)mem::alloc(sizeof(VariableEntity));
        scope->vars.push(entity);
        switch(lhsNode->type){
            case ASTType::VARIABLE:{
                                       ASTVariable *var = (ASTVariable*)lhsNode;
                                       name = var->name;
                                       var->entity = entity;
                                   }break;
            case ASTType::MODIFIER:{
                                       ASTModifier *mod = (ASTModifier*)lhsNode;
                                       name = mod->name;
                                       mod->entity = entity;
                                   }break;
            default: return 0;
        };
        u32 id = scope->varId++;
        scope->var.insertValue(name, id);
        entity->pointerDepth = typePointerDepth;
        entity->type = typeType;
        entity->id = id;
        entity->size = size;
        //TODO: Fill type information(array please) of assdecl
    };
    return size;
};

bool checkASTNode(Lexer &lexer, ASTBase *node, DynamicArray<Scope*> &scopes);
u32 checkFor(ASTFor *For, DynamicArray<Scope*> &scopes, Lexer &lexer){
    Scope *scope = scopes[scopes.count-1];
    Scope *body = check::newBlockScope(scope->varId);
    scopes.push(body);
    if(For->decl != nullptr){
        //c-for
        if(checkDecl(For->decl, scopes, lexer) == 0) return 0;
        u32 endPointerDepth;
        Type endType = checkTree(lexer, For->end, scopes, endPointerDepth);
        if(endType == Type::INVALID) return false;
        Type startType = (For->decl->zType != nullptr)?For->decl->zType->zType:For->decl->treeType;
        if(startType != endType){
            lexer.emitErr(For->tokenOff, "Initializer type not equal to end type");
            return false;
        };
        if(For->step){
            u32 stepPointerDepth;
            Type stepType = checkTree(lexer, For->step, scopes, stepPointerDepth);
            if(stepType == Type::INVALID) return false;
            if(isInteger(stepType) == false){
                lexer.emitErr(For->tokenOff, "Step type should be an integer");
                return false;
            };
            if(stepPointerDepth > 0){
                lexer.emitErr(For->tokenOff, "Step expression tree cannot contain pointers");
                return false;
            };
        };
        For->zType = startType;
    }else{
        //c-while
        if(!checkASTNode(lexer, For->expr, scopes)) return false;
    };
    for(u32 x=0; x<For->bodyCount; x++){
        if(!checkASTNode(lexer, For->body[x], scopes)) return false;
    };
    scopes.pop();
    return body->varId;
};
bool checkProcDecl(ASTProcDefDecl *proc, DynamicArray<Scope*> &scopes, Lexer &lexer){
    Scope *scope = scopes[scopes.count-1];
    if(scope->type != ScopeType::GLOBAL){
        lexer.emitErr(proc->tokenOff, "Procedure can only be defined in the global scope");
        return false;
    };
    if(getProcEntity(proc->name, scopes)){
        lexer.emitErr(proc->tokenOff, "Procedure with this name already exists");
        return false;
    };
    scope->proc.insertValue(proc->name, scope->procs.count);
    ProcEntity *entity = (ProcEntity*)mem::alloc(sizeof(ProcEntity));
    scope->procs.push(entity);
    Scope *body = check::newBlockScope(0);
    entity->isDecl = true;
    entity->varArgs = proc->varArgs;
    entity->inputs = proc->inputs;
    entity->inputCount = proc->inputCount;
    entity->outputs = proc->outputs;
    entity->outputCount = proc->outputCount;
    scopes.push(body);
    DynamicArray<Scope*> procInputScope;
    procInputScope.init(1);
    procInputScope.push(body);
    DEFER({
            procInputScope.uninit();
            scopes.pop();
            });
    for(u32 x=0; x<proc->inputCount; x++){
        if(!fillTypeInfo(lexer, proc->typeInputs[x])) return false;
    };
    for(u32 x=0; x<proc->outputCount; x++){
        if(!fillTypeInfo(lexer, proc->outputs[x])) return false;
    };
    return true;
}
bool checkProcDef(ASTProcDefDecl *proc, DynamicArray<Scope*> &scopes, Lexer &lexer){
    Scope *scope = scopes[scopes.count-1];
    if(scope->type != ScopeType::GLOBAL){
        lexer.emitErr(proc->tokenOff, "Procedure can only be defined in the global scope");
        return false;
    };
    if(getProcEntity(proc->name, scopes)){
        lexer.emitErr(proc->tokenOff, "Procedure with this name already exists");
        return false;
    };
    scope->proc.insertValue(proc->name, scope->procs.count);
    ProcEntity *entity = (ProcEntity*)mem::alloc(sizeof(ProcEntity));
    scope->procs.push(entity);
    Scope *body = check::newBlockScope(0);
    entity->isDecl = false;
    entity->varArgs = proc->varArgs;
    entity->inputs = proc->inputs;
    entity->inputCount = proc->inputCount;
    entity->outputs = proc->outputs;
    entity->outputCount = proc->outputCount;
    scopes.push(body);
    DynamicArray<Scope*> procInputScope;
    procInputScope.init(1);
    procInputScope.push(body);
    DEFER({
            procInputScope.uninit();
            scopes.pop();
            });
    for(u32 x=0; x<proc->inputCount; x++){
        if(proc->inputs[x]->type != ASTType::DECLERATION){
            lexer.emitErr(proc->tokenOff, "One of the input is not a decleration");
            return false;
        };
        ASTAssDecl *input = proc->inputs[x];
        if(input->rhs){
            lexer.emitErr(proc->tokenOff, "Zeus does not support default argument");
            return false;
        };
        if(checkDecl(input, procInputScope, lexer) == 0) return false;
    };

    for(u32 x=0; x<proc->outputCount; x++){
        if(!fillTypeInfo(lexer, proc->outputs[x])) return false;
    };
    for(u32 x=0; x<proc->bodyCount; x++){
        if(!checkASTNode(lexer, proc->body[x], scopes)) return false;
    };
    //NOTE: we check return statements here 
    bool hasRet = false;
    for(u32 x=0; x<proc->bodyCount; x++){
        if(proc->body[x]->type == ASTType::RETURN){
            ASTReturn *ret = (ASTReturn*)proc->body[x];
            if(hasRet){
                lexer.emitWarn(ret->tokenOff, "Proc has already returned. Dead code detected");
                hasRet = false;
            };
            hasRet = true;
            if(proc->outputCount != ret->retCount){
                lexer.emitErr(ret->tokenOff, "Proc defined with %d output but return statement has %d expressions", proc->outputCount, ret->retCount);
                return false;
            };
            for(u32 i=0; i<proc->outputCount; i++){
                u32 pointerDepth;
                Type type= checkTree(lexer, ret->exprs[i], scopes, pointerDepth);
                ASTTypeNode *typeNode = proc->outputs[i];
                if(type != typeNode->zType){
                    lexer.emitErr(ret->tokenOff, "Proc declared return type is not matching return statement type");
                    return false;
                };
                if(pointerDepth != typeNode->pointerDepth){
                    lexer.emitWarn(ret->tokenOff, "Proc defined with pointer depth %d at position %d, but return statement has pointer depth %d\n", typeNode->pointerDepth, i, pointerDepth);
                    return false;
                };
            };
        };
    };
    if(hasRet == false && proc->outputCount > 0){
        lexer.emitErr(proc->tokenOff, "Proc declared %d variables to be returned but found no return statements in body", proc->outputCount);
        return false;
    };
    return true;
};
bool checkStructDef(ASTStruct *Struct, DynamicArray<Scope*> &scopes, Lexer &lexer){
    if(getStructEntity(Struct->name)){
        lexer.emitErr(Struct->tokenOff, "Structure already defined");
        return false;
    };
    check::structToOff.insertValue(Struct->name, check::structEntities.count);
    StructEntity *entity = &check::structEntities.newElem();
    Scope *body = check::newStructScope();
    entity->body = body;
    u64 size = 0;
    scopes.push(body);
    for(u32 x=0; x<Struct->bodyCount; x++){
        ASTAssDecl *node = (ASTAssDecl*)Struct->body[x];
        if(node->type != ASTType::DECLERATION){
            lexer.emitErr(Struct->tokenOff, "Body should contain only declerations");
            return false;
        };
        if(node->rhs){
            lexer.emitErr(Struct->tokenOff, "Body should not contain decleration with RHS(expression tree)");
            return false;
        }
        u64 temp = checkDecl(node, scopes, lexer);
        if(temp == 0) return false;
        size += temp;
    };
    entity->size = size;
    scopes.pop();
    return true;
};
bool checkAss(ASTAssDecl *assdecl, DynamicArray<Scope*> &scopes, Lexer &lexer){
    if(assdecl->lhsCount > 1 && assdecl->rhs->type != ASTType::PROC_CALL){
        lexer.emitErr(assdecl->tokenOff, "If LHS has many elements, then RHS should be a procedure call returning same number of elements");
        return false;
    };
    for(u32 x=0; x<assdecl->lhsCount; x++){
        ASTBase *node = assdecl->lhs[x];
        VariableEntity *entity = getVariableEntity(node, scopes);
        if(entity == nullptr){
            if(node->type == ASTType::VARIABLE || node->type == ASTType::MODIFIER){
                lexer.emitErr(assdecl->tokenOff, "Variable not defined in LHS(%d)", x);
                return false;
            };
            lexer.emitErr(assdecl->tokenOff, "Only variable or modifiers allowed in LHS");
            return false;
        };
        if(node->type == ASTType::MODIFIER){
            ASTModifier *mod = (ASTModifier*)node;
            if(checkModifierChain(lexer, mod->child, entity) == Type::INVALID) return false;
        };
    };
    if(assdecl->lhsCount > 1){
        ASTProcCall *procCall = (ASTProcCall*)assdecl->rhs;
        ProcEntity *entity = getProcEntity(procCall->name, scopes);
        if(entity == nullptr){
            lexer.emitErr(procCall->tokenOff, "Procedure not defined");
            return false;
        };
        if(entity->outputCount > assdecl->lhsCount){
            lexer.emitErr(assdecl->tokenOff, "RHS returns more than what LHS can catch");
            return false;
        };
        if(entity->outputCount < assdecl->lhsCount){
            lexer.emitErr(assdecl->tokenOff, "RHS returns less than what LHS can catch");
            return false;
        };
        if(entity->inputCount != procCall->argCount){
            lexer.emitErr(procCall->tokenOff, "Procedure defined with %d input%sbut you provided %d input%s",
                    entity->inputCount, entity->inputCount>1?"s ":" ", procCall->argCount, procCall->argCount>1?"s ":" ");
        };
        for(u32 x=0; x<entity->inputCount; x++){
            if(!checkASTNode(lexer, procCall->args[x], scopes)) return false;
        };
    }else{
        u32 treePointerDepth;
        Type treeType = checkTree(lexer, assdecl->rhs, scopes, treePointerDepth);
        if(treeType == Type::INVALID) return false;
    }
    //TODO: Fill type information(array please) of assdecl
    return true;
};
u32 checkIf(ASTIf *If, DynamicArray<Scope*> &scopes, Lexer &lexer){
    u32 newId;
    u32 treePointerDepth;
    Scope *scope = scopes[scopes.count - 1];
    Type treeType = checkTree(lexer, If->expr, scopes, treePointerDepth);
    if(treeType == Type::INVALID) return false;
    if(treeType > Type::COUNT && treePointerDepth == 0){
        lexer.emitErr(If->exprTokenOff, "Invalid expression");
        return false;
    };
    If->zType = treeType;
    Scope *bodyScope = check::newBlockScope(scope->varId);
    scopes.push(bodyScope);
    for(u32 x=0; x<If->ifBodyCount; x++){
        if(!checkASTNode(lexer, If->ifBody[x], scopes)) return false;
    };
    scopes.pop();
    newId = scope->varId;
    if(If->elseBodyCount > 0){
        Scope *elseBodyScope = check::newBlockScope(newId);
        elseBodyScope->init(ScopeType::BLOCK, newId);
        scopes.push(elseBodyScope);
        for(u32 x=0; x<If->elseBodyCount; x++){
            if(!checkASTNode(lexer, If->elseBody[x], scopes)) return false;
        };
        scopes.pop();
        newId = elseBodyScope->varId;
    };
    return newId;
};
bool checkASTNode(Lexer &lexer, ASTBase *node, DynamicArray<Scope*> &scopes){
    BRING_TOKENS_TO_SCOPE;
    Scope *scope = scopes[scopes.count-1];
    u32 newId = scope->varId;
    DEFER(scope->varId = newId);
    switch(node->type){
        case ASTType::RETURN:{
                                 ASTReturn *ret = (ASTReturn*)node;
                                 for(u32 x=0; x<ret->retCount; x++){
                                     u32 pd;
                                     Type type = checkTree(lexer, ret->exprs[x], scopes, pd);
                                     if(type == Type::INVALID) return false;
                                     ASTTypeNode *typeNode = &ret->types[x];
                                     typeNode->zType = type;
                                     typeNode->pointerDepth = pd;
                                 };
                             }break;
        case ASTType::FOR:{
                              newId = checkFor((ASTFor*)node, scopes, lexer);
                              if(newId == 0) return false;
                          }break;
        case ASTType::PROC_DECL:{
                                    if(checkProcDecl((ASTProcDefDecl*)node, scopes, lexer) == false) return false;
                                }break;
        case ASTType::PROC_DEF:{
                                   if(checkProcDef((ASTProcDefDecl*)node, scopes, lexer) == false) return false;
                               }break;
        case ASTType::STRUCT:{
                                 if(checkStructDef((ASTStruct*)node, scopes, lexer) == false) return false;
                             }break;
        case ASTType::DECLERATION:{
                                      if(checkDecl((ASTAssDecl*)node, scopes, lexer) == 0) return false;
                                      newId = scope->varId;
                                  }break;
        case ASTType::ASSIGNMENT:{
                                     if(checkAss((ASTAssDecl*)node, scopes, lexer) == 0) return false;
                                 }break;
        case ASTType::IF:{
                             newId = checkIf((ASTIf*)node, scopes, lexer);
                             if(newId == 0) return false;
                         }break;
        default:{
                    u32 pointerDepth;
                    return checkTree(lexer, node, scopes, pointerDepth) != Type::INVALID;
                }break;
    };
    return true;
};
bool checkASTFile(Lexer &lexer, ASTFile &file, DynamicArray<ASTBase*> &globals){
    Scope &scope = check::globalScopes[file.id];
    scope.init(ScopeType::GLOBAL, 0);
    DynamicArray<Scope*> scopes;
    scopes.init();
    DEFER(scopes.uninit());
    for(u32 x=0; x<file.dependencies.count; x++) scopes.push(&check::globalScopes[file.dependencies[x]]);
    scopes.push(&scope);
    for(u32 x=0; x<file.nodes.count; x++){
        if(!checkASTNode(lexer, file.nodes[x], scopes)) return false;
    };
    const u32 curFileOff = &scope - check::globalScopes;
    for(u32 x=0; x<file.nodes.count; x++){
        ASTBase *node = file.nodes[x];
        switch(node->type){
            case ASTType::PROC_DECL:
            case ASTType::PROC_DEF:
            case ASTType::STRUCT: break;
            case ASTType::DECLERATION:{
                                          ASTAssDecl *assdecl = (ASTAssDecl*)node;
                                          if(assdecl->lhsCount > 1){
                                              lexer.emitErr(assdecl->tokenOff, "In the global scope, lhs count has to be 1");
                                              return false;
                                          };
                                          if(assdecl->lhs[0]->type != ASTType::VARIABLE){
                                              lexer.emitErr(assdecl->tokenOff, "In the global scope, lhs has to be a variable");
                                              return false;
                                          };
                                          switch(assdecl->rhs->type){
                                              case ASTType::INTEGER:
                                              case ASTType::DECIMAL:
                                              case ASTType::CHARACTER:
                                              case ASTType::STRING: break;
                                              default:{
                                                          lexer.emitErr(assdecl->tokenOff, "In the global scope, rhs has to be an integer, decimal, character or a string. No expressions allowed");
                                                          return false;
                                                      }break;
                                          };
                                          for(u32 y=curFileOff; y<dep::fileToId.count-1; y++){
                                              const u32 fileOff = y+1;
                                              ASTVariable *var = (ASTVariable*)assdecl->lhs[0];
                                              if(check::globalScopes[fileOff].var.count > 0){
                                                  u32 off;
                                                  if(check::globalScopes[fileOff].var.getValue(var->name, &off)){
                                                      lexer.emitErr(assdecl->tokenOff, "Variable already declared at global scope in %s",dep::lexers[fileOff].fileName);
                                                      return false;
                                                  };
                                              };
                                          };
                                          ASTVariable *var = (ASTVariable*)assdecl->lhs[0];
                                          var->entity->id = globals.count;
                                          globals.push(node);
                                          ASTBase *lastNode = file.nodes.pop();
                                          if(lastNode != node){
                                              file.nodes[x] = lastNode;
                                              continue;
                                          };
                                      }break;
        };
    };
    return true;
};
