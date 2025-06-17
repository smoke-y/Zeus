#include "../include/checker.hh"
#include "../include/lexer.hh"
#include "../include/dependency.hh"

static ASTFile *curASTFileForCastNodeAlloc = nullptr;

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
    DynamicArray<ASTInitializerList*> initializerLists;
    DynamicArray<StructEntity> structEntities;
    static HashmapStr structToOff;
    static ASTReturn defaultReturn;
    static HashmapStr loopLabels;

    void init(){
        defaultReturn.retCount = 0;
        u32 size = sizeof(Scope) * dep::lexers.count;
        globalScopes = (Scope*)mem::alloc(size);
        memset(globalScopes, 0, size);
        loopLabels.init();
        structToOff.init();
        initializerLists.init();
        structEntities.init();
        structScopes.init();
        blockScopes.init();
        stringToId.init();
    };
    void uninit(){
        loopLabels.uninit();
        structToOff.uninit();
        initializerLists.uninit();
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

VariableEntity *getVariableEntity(ASTBase *node, DynamicArray<Scope*> &scopes, bool &isGlobal){
    isGlobal = false;
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
        if(scope->type == ScopeType::GLOBAL) isGlobal = true;
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
    u32 off = (u32)type - (u32)Type::Z_TYPE_END - 1;
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
        node->zType = (Type)((u32)tokTypes[node->tokenOff] - (u32)TokType::K_TYPE_START + (u32)(Type::Z_TYPE_START));
        if(node->zType == Type::PTR){
            if(node->pointerDepth > 0){
                lexer.emitErr(node->tokenOff, "rawptr has to have pointer depth 0");
                return false;
            };
            node->pointerDepth = 1;
        };
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
    node->zType = (Type)(off + (u32)Type::Z_TYPE_END + 1);
    return true;
};
Type checkModifierChain(Lexer &lexer, ASTBase *root, VariableEntity *entity, u32 &memPd){
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
                                       return checkModifierChain(lexer, mod->child, structBodyScope->vars[off], memPd);
                                   }break;
                                   //TODO: array_at
            case ASTType::VARIABLE:{
                                       ASTVariable *var = (ASTVariable*)root;
                                       u32 off;
                                       if(!structBodyScope->var.getValue(var->name, &off)){
                                           lexer.emitErr(var->tokenOff, "%.*s does not belong to the defined structure", var->name.len, var->name.mem);
                                           return Type::INVALID;
                                       };
                                       memPd = structBodyScope->vars[off]->pointerDepth;
                                       return structBodyScope->vars[off]->type;
                                   }break;
        };
    };
    return Type::INVALID;
};
u64 getSize(Lexer &lexer, Type type, u32 tokenOff){
    switch(type){
        case Type::PTR:
        case Type::COMP_STRING:
        case Type::COMP_DECIMAL:
        case Type::COMP_INTEGER:
        case Type::S64:
        case Type::U64:  return 64;
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

Type _checkTree(Lexer &lexer, ASTBase **nnode, DynamicArray<Scope*> &scopes, u32 &pointerDepth);
Type checkTree(Lexer &lexer, ASTBase **nnode, DynamicArray<Scope*> &scopes, u32 &pointerDepth, Type typeCheck, u32 typeCheckPd=0){
    BRING_TOKENS_TO_SCOPE;
    pointerDepth = 0;
    ASTBase *node = *nnode;
    Type treeType;
    if(node->type == ASTType::INITIALIZER_LIST){
        typeCheckPd--;
        ASTInitializerList *il = (ASTInitializerList*)node;
        treeType = checkTree(lexer, &il->elements[0], scopes, pointerDepth, typeCheck, typeCheckPd);
        for(u32 x=1; x<il->elementCount; x++){
            u32 pd;
            Type elemType = checkTree(lexer, &il->elements[x], scopes, pd, typeCheck, typeCheckPd);
            if(il->elements[x]->type != ASTType::INTEGER && il->elements[x]->type != ASTType::DECIMAL && il->elements[x]->type != ASTType::STRING && il->elements[x]->type != ASTType::CHARACTER){
                lexer.emitErr(il->tokenOff, "Elements %d has to be a compile time type", x);
                return Type::INVALID;
            };
            if(elemType != treeType){
                lexer.emitErr(il->tokenOff, "Elements %d's type(%s) does not match the type of the first element(%s)", x, typeToStr(elemType), typeToStr(treeType));
                return Type::INVALID;
            };
        };
        il->id = check::initializerLists.count;
        il->zType = typeCheck;
        check::initializerLists.push(il);
    }else{
        treeType = _checkTree(lexer, nnode, scopes, pointerDepth);
        if(treeType == Type::INVALID) return Type::INVALID;
    };
    if(typeCheck == Type::INVALID) return treeType;
    ASTCast *cast = nullptr;
    if(treeType == Type::DEFER_CAST){
        cast = (ASTCast*)node;
        if(canWeCast(cast->srcType.zType, cast->srcType.pointerDepth, typeCheck, typeCheckPd, node->tokenOff, lexer) == false) return Type::INVALID;
    }
    if(cast == nullptr){
        if(canWeCast(treeType, pointerDepth, typeCheck, typeCheckPd, node->tokenOff, lexer) == false) return Type::INVALID;
        if(implicitOk(treeType, typeCheck)){
            if(treeType == typeCheck || isCompType(treeType)) return typeCheck;
            //alloc a cast node and poke it for an implicit casting
            cast = (ASTCast*)curASTFileForCastNodeAlloc->newNode(sizeof(ASTCast), ASTType::CAST, 0);
            cast->targetType = (ASTTypeNode*)curASTFileForCastNodeAlloc->newNode(sizeof(ASTTypeNode), ASTType::TYPE, 0);
            *nnode = (ASTBase*)cast;
            cast->srcType.zType = treeType;
            cast->srcType.pointerDepth = pointerDepth;
            cast->child = node;
        }else{
            lexer.emitErr(node->tokenOff, "Explicit cast required");
            return Type::INVALID;
        };
    };
    cast->targetType->zType = typeCheck;
    cast->targetType->pointerDepth = typeCheckPd;
    return typeCheck;
};
Type _checkTree(Lexer &lexer, ASTBase **nnode, DynamicArray<Scope*> &scopes, u32 &pointerDepth){
    BRING_TOKENS_TO_SCOPE;
    pointerDepth = 0;
    ASTType unOpType = ASTType::INVALID;
    u32 unOpTokenOff;
    ASTBase *node = *nnode;
    ASTUnOp *rootUnop = nullptr;
    if(node->type > ASTType::U_START && node->type < ASTType::U_END) rootUnop = (ASTUnOp*)node;
    if(node->type == ASTType::U_PROC_MEM){
        if(rootUnop->child->type != ASTType::VARIABLE){
            lexer.emitErr(rootUnop->tokenOff, "Child has to be a procedure name");
            return Type::INVALID;
        };
        ASTVariable *proc = (ASTVariable*)rootUnop->child;
        ProcEntity *entity = getProcEntity(proc->name,scopes);
        proc->entity = (VariableEntity*)entity;
        if(entity == nullptr){
            lexer.emitErr(proc->tokenOff, "Procedure not defined/declared");
            return Type::INVALID;
        };
        return Type::PTR;
    };
    while(node->type > ASTType::U_START && node->type < ASTType::U_END){
        //unary ops return the type of the child
        ASTUnOp *unOp = (ASTUnOp*)node;
        if(unOp->type == ASTType::U_MEM) pointerDepth++;
        unOpType = unOp->type;
        unOpTokenOff = unOp->tokenOff;
        node = unOp->child;
    };
    Type type = Type::INVALID;
    Type treeType;  //used for ASTType::CAST
    switch(node->type){
        case ASTType::CHARACTER: type = Type::CHAR;break;
        case ASTType::INTEGER:   type = Type::COMP_INTEGER;break;
        case ASTType::DECIMAL:   type = Type::COMP_DECIMAL;break;
        case ASTType::ARRAY_AT:{
                                   ASTArrayAt *at = (ASTArrayAt*)node;
                                   u32 atPd;
                                   Type atType = checkTree(lexer, &at->at, scopes, atPd, Type::INVALID);
                                   if(atType == Type::INVALID) return Type::INVALID;
                                   if(isInteger(atType) == false){
                                       lexer.emitErr(at->tokenOff, "At expression has to be of type integer");
                                       return Type::INVALID;
                                   };
                                   if(atPd != 0){
                                       lexer.emitErr(at->tokenOff, "At expression's pointer depth has to be 0");
                                       return Type::INVALID;
                                   };
                                   at->atType = atType;
                                   u32 parentPd;
                                   Type parentType = checkTree(lexer, (ASTBase**)&at->parent, scopes, parentPd, Type::INVALID);
                                   if(parentType == Type::INVALID) return Type::INVALID;
                                   if(parentPd == 0){
                                       lexer.emitErr(at->parent->tokenOff, "Parent has to be a pointer");
                                       return Type::INVALID;
                                   };
                                   at->parentType.zType = parentType;
                                   at->parentType.pointerDepth = parentPd;
                                   if(at->child){
                                       u32 childPd;
                                       Type childType = checkTree(lexer, &at->child, scopes, childPd, Type::INVALID);
                                       if(childType == Type::INVALID) return Type::INVALID;
                                   };
                                   type = parentType;
                                   pointerDepth = parentPd - 1;
                               }break;
        case ASTType::CAST:{
                               ASTCast *cast = (ASTCast*)node;
                               treeType = checkTree(lexer, &cast->child, scopes, pointerDepth, Type::INVALID);
                               if(treeType == Type::INVALID) return Type::INVALID;
                               cast->srcType.zType = treeType;
                               cast->srcType.pointerDepth = pointerDepth;
                               type = Type::DEFER_CAST;
                           }break;
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
                                    u32 curInputNode = 0;
                                    u32 curInputLhsCount = entity->inputs[0]->lhsCount;
                                    for(u32 x=0; x<proc->argCount; x++){
                                        Type inputType = Type::INVALID;
                                        u32 inputPd;
                                        if(x < entity->inputCount){
                                            if(entity->isDecl == false){
                                                inputType = entity->inputs[curInputNode]->zType->zType;
                                                inputPd = entity->inputs[curInputNode]->zType->pointerDepth;
                                                if(x == curInputLhsCount-1 && curInputNode+1 != entity->inputNodeCount){
                                                    curInputNode++;
                                                    curInputLhsCount += entity->inputs[curInputNode]->lhsCount;
                                                };
                                            }else{
                                                Type inputType = entity->typeInputs[x]->zType;
                                                u32 inputPd = entity->typeInputs[x]->pointerDepth;
                                            };
                                        };
                                        u32 pd;
                                        Type type = checkTree(lexer, &proc->args[x], scopes, pd, inputType, inputPd); 
                                        if(type == Type::INVALID) return Type::INVALID;
                                        ASTTypeNode *typeNode = &proc->types[x];
                                        typeNode->zType = type;
                                        typeNode->pointerDepth = pd;
                                    };
                                    if(entity->outputCount == 0) type = Type::VOID;
                                    else{
                                        type = entity->outputs[0]->zType;
                                        pointerDepth = entity->outputs[0]->pointerDepth;
                                    };
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
                                   bool isGloabl;
                                   VariableEntity *entity = getVariableEntity(node, scopes, isGloabl);
                                   ASTVariable *var = (ASTVariable*)node;
                                   if(entity == nullptr){
                                       lexer.emitErr(var->tokenOff, "Variable not defined");
                                       return Type::INVALID;
                                   };
                                   var->entity = entity;
                                   if(var->pAccessDepth > 0 && var->pAccessDepth - entity->pointerDepth > 0){
                                       lexer.emitErr(var->tokenOff, "Pointer resolved %d times more than required", var->pAccessDepth - entity->pointerDepth);
                                       return Type::INVALID;
                                   };
                                   pointerDepth += var->pAccessDepth?entity->pointerDepth - var->pAccessDepth:entity->pointerDepth;
                                   type = entity->type;
                                   if(isGloabl) node->type = ASTType::GLOBAL;
                               }break;
        case ASTType::MODIFIER:{
                                   ASTModifier *mod = (ASTModifier*)node;
                                   bool isGlobal;
                                   VariableEntity *entity = getVariableEntity(node, scopes, isGlobal);
                                   if(entity == nullptr){
                                       lexer.emitErr(mod->tokenOff, "Variable not defined");
                                       return Type::INVALID;
                                   };
                                   mod->entity = entity;
                                   type = checkModifierChain(lexer, mod->child, entity, pointerDepth);
                               }break;
        default:{
                    // @type: type clamp down 1
                    if(node->type > ASTType::B_START && node->type < ASTType::B_END){
                        ASTBinOp *binOp = (ASTBinOp*)node;
                        u32 lhsUsingPointer, rhsUsingPointer;
                        Type lhsType = checkTree(lexer, &binOp->lhs, scopes, lhsUsingPointer, Type::INVALID); 
                        Type rhsType = checkTree(lexer, &binOp->rhs, scopes, rhsUsingPointer, Type::INVALID);
                        if(lhsUsingPointer && rhsUsingPointer
                                && (binOp->type != ASTType::B_ADD || binOp->type != ASTType::B_SUB)){
                            lexer.emitErr(binOp->tokenOff, "Can only add/sub 2 pointers");
                            return Type::INVALID;
                        };
                        if((lhsType > Type::Z_TYPE_END && lhsUsingPointer == 0) || (rhsType > Type::Z_TYPE_END && rhsUsingPointer == 0)){
                            lexer.emitErr(binOp->tokenOff, "Cannot perform binary operation with structures");
                            return Type::INVALID;
                        };
                        if(lhsUsingPointer == 0 && rhsUsingPointer == 0){
                            bool matching = true;
                            if(isInteger(lhsType) ^ isInteger(rhsType)) matching = false;
                            else if(isDecimal(lhsType) ^ isDecimal(rhsType)) matching = false;
                            else if(lhsType == Type::CHAR ^ rhsType == Type::CHAR) matching = false;
                            if(matching == false){
                                lexer.emitErr(binOp->tokenOff, "lhs group does not match with rhs group");
                                return Type::INVALID;
                            };
                        };
                        if(lhsUsingPointer == 0 && rhsUsingPointer > 0){
                            if(isInteger(lhsType) == false){
                                lexer.emitErr(binOp->tokenOff, "lhs type has to be an integer if rhs is a pointer");
                                return Type::INVALID;
                            };
                        }else if(rhsUsingPointer == 0 && lhsUsingPointer > 0){
                            if(isInteger(rhsType) == false){
                                lexer.emitErr(binOp->tokenOff, "rhs type has to be an integer if lhs is a pointer");
                                return Type::INVALID;
                            };
                        };
                        if(lhsType > Type::Z_TYPE_END) type = lhsType;
                        else if(rhsType > Type::Z_TYPE_END) type = rhsType;
                        else if(isCompType(lhsType)) type = rhsType;
                        else if(isCompType(rhsType)) type = lhsType;
                        else type = (lhsType <= rhsType)?lhsType:rhsType;
                        if(isDecimal(type) && binOp->type == ASTType::B_MOD){
                            lexer.emitErr(binOp->tokenOff, "Cannot mod(%) with floats");
                            return Type::INVALID;
                        }
                        pointerDepth = (lhsUsingPointer < rhsUsingPointer)?rhsUsingPointer:lhsUsingPointer;
                        binOp->zType.zType = type;
                        binOp->zType.pointerDepth = pointerDepth;
                    };
                }break;
    };
    switch(unOpType){
        case ASTType::U_NOT:{
                                if(isInteger(type)){
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
    if(type != Type::DEFER_CAST) treeType = type;
    while(rootUnop){
        rootUnop->childType.zType = treeType;
        rootUnop->childType.pointerDepth = pointerDepth;
        if(rootUnop->child->type > ASTType::U_START && rootUnop->child->type < ASTType::U_END) rootUnop = (ASTUnOp*)rootUnop->child;
        else rootUnop = nullptr;
    };
    return type;
};
u64 checkDecl(ASTAssDecl *assdecl, DynamicArray<Scope*> &scopes, Lexer &lexer, bool isInsideProcInput=false){
    BRING_TOKENS_TO_SCOPE;
    u32 typePointerDepth;
    Type typeType;
    u32 tokenOff = assdecl->tokenOff;
    if(isInsideProcInput && assdecl->rhs != nullptr){
        lexer.emitErr(assdecl->tokenOff, "Zeus does not support default argument");
        return false;
    };
    if(assdecl->lhsCount > 1 && isInsideProcInput == false){
        if(assdecl->rhs->type != ASTType::PROC_CALL){
            lexer.emitErr(assdecl->tokenOff, "If LHS has many elements, then RHS should be a procedure call returning same number of elements");
            return false;
        };
    };
    if(assdecl->zType->zType == Type::INVALID && assdecl->rhs->type == ASTType::INITIALIZER_LIST){
        lexer.emitErr(assdecl->tokenOff, "Type has to provided if using an intializer list");
        return false;
    };
    if(assdecl->zType->zType != Type::INVALID){
        if(!fillTypeInfo(lexer, assdecl->zType)) return 0;
        ASTTypeNode *type = assdecl->zType;
        typeType = type->zType;
        typePointerDepth = type->pointerDepth;
    };
    u64 size;
    ProcEntity *pentity = nullptr;
    if(assdecl->rhs){
        u32 treePointerDepth;
        Type treeType = checkTree(lexer, &assdecl->rhs, scopes, treePointerDepth, assdecl->zType->zType, assdecl->zType->pointerDepth);
        ASTVariable *var = (ASTVariable*) assdecl->lhs[0];
        if(treeType == Type::INVALID) return 0;
        if(assdecl->zType->zType == Type::INVALID){
            assdecl->zType->zType = treeType;
            assdecl->zType->pointerDepth = treePointerDepth;
            typeType = treeType;
            typePointerDepth = treePointerDepth;
        };
        if(assdecl->rhs->type == ASTType::INITIALIZER_LIST) typePointerDepth = 1;
        if(assdecl->rhs->type == ASTType::PROC_CALL){
            ASTProcCall *proc = (ASTProcCall*)assdecl->rhs;
            pentity = getProcEntity(proc->name, scopes);
            if(assdecl->lhsCount != pentity->outputCount){
                lexer.emitErr(assdecl->tokenOff, "Lhs(%d) is not equal to rhs return count(%d)", assdecl->lhsCount, pentity->outputCount);
                return 0;
            };
        };
    };
    if(typePointerDepth > 0) size = 64;
    else size = getSize(lexer, typeType, assdecl->tokenOff);
    for(u32 x=0; x<assdecl->lhsCount; x++){
        ASTBase *lhsNode = assdecl->lhs[x];
        if(lhsNode->type != ASTType::VARIABLE){
            lexer.emitErr(assdecl->tokenOff, "%d lhs has to be a variable", x);
            return 0;
        };
        bool isGlobal;
        if(getVariableEntity(lhsNode, scopes, isGlobal)){
            lexer.emitErr(tokenOff, "Redefinition");
            return 0;
        };
        Scope *scope = scopes[scopes.count-1];
        VariableEntity *entity = (VariableEntity*)mem::alloc(sizeof(VariableEntity));
        if(lhsNode->type == ASTType::VARIABLE){
            ASTVariable *var = (ASTVariable*)lhsNode;
            scope->var.insertValue(var->name, scope->vars.count);
            var->entity = entity;
        }else{
            ASTModifier *mod = (ASTModifier*)lhsNode;
            scope->var.insertValue(mod->name, scope->vars.count);
            mod->entity = entity;
        }
        scope->vars.push(entity);
        if(pentity == nullptr){
            entity->pointerDepth = typePointerDepth;
            entity->type = convertFromComptype(typeType);
        }else{
            entity->pointerDepth = pentity->outputs[x]->pointerDepth;
            entity->type = pentity->outputs[x]->zType;
        };
        entity->id = scope->varId++;
        entity->size = size;
    };
    return size;
};
bool checkAss(ASTAssDecl *assdecl, DynamicArray<Scope*> &scopes, Lexer &lexer){
    if(assdecl->lhsCount > 1 && assdecl->rhs->type != ASTType::PROC_CALL){
        lexer.emitErr(assdecl->tokenOff, "If LHS has many elements, then RHS should be a procedure call returning same number of elements");
        return false;
    };
    if(assdecl->rhs->type == ASTType::INITIALIZER_LIST){
        lexer.emitErr(assdecl->tokenOff, "While assigning, rhs cannot be an intializer list");
        return false;
    };
    DynamicArray<VariableEntity*> entities;
    entities.init();
    DEFER(entities.uninit());
    bool isGlobal;
    Type memType = Type::INVALID;
    u32 memPd;
    for(u32 x=0; x<assdecl->lhsCount; x++){
        ASTBase *node = assdecl->lhs[x];
        VariableEntity *entity = getVariableEntity(node, scopes, isGlobal);
        if(node->type == ASTType::VARIABLE){
            if(entity == nullptr){
                lexer.emitErr(assdecl->tokenOff, "Variable not defined in LHS(%d)", x);
                return false;
            };
            ASTVariable *var = (ASTVariable*)node;
            var->entity = entity;
        };
        if(node->type == ASTType::MODIFIER){
            ASTModifier *mod = (ASTModifier*)node;
            mod->entity = entity;
            Type mem = checkModifierChain(lexer, mod->child, entity, memPd);
            if(mem == Type::INVALID) return false;
            if(memType == Type::INVALID) memType = mem;
        };
        entities.push(entity);
    };
    u32 treePointerDepth;
    Type treeType = checkTree(lexer, &assdecl->rhs, scopes, treePointerDepth,
            memType==Type::INVALID?entities[0]->type:memType, memType==Type::INVALID?entities[0]->pointerDepth:memPd);
    if(treeType == Type::INVALID) return false;
    if(assdecl->lhsCount > 1){
        ASTProcCall *procCall = (ASTProcCall*)assdecl->rhs;
        ProcEntity *entity = getProcEntity(procCall->name, scopes);
        if(assdecl->lhsCount != entity->outputCount){
            lexer.emitErr(assdecl->tokenOff, "Lhs(%d) is not equal to rhs return count(%d)", assdecl->lhsCount, entity->outputCount);
            return false;
        };
        for(u32 x=0; x<assdecl->lhsCount; x++){
            if(entities[x]->type != entity->outputs[x]->zType){
                lexer.emitErr(assdecl->tokenOff, "Argument %d's type(%s) is not equal to rhs return type(%d). Casting not allowed", x, typeToStr(entities[x]->type), typeToStr(entity->outputs[x]->zType));
                return false;
            };
            if(entities[x]->pointerDepth != entity->outputs[x]->pointerDepth){
                lexer.emitErr(assdecl->tokenOff, "Argument %d's pointer depth(%d) is not equal to rhs's pointer depth (%d)", x, entities[x]->pointerDepth, entity->outputs[x]->pointerDepth);
                return false;
            };
        };
    }else{
        assdecl->zType->zType = treeType;
        assdecl->zType->pointerDepth = treePointerDepth;
    }
    return true;
};
bool checkASTNode(Lexer &lexer, ASTBase *node, DynamicArray<Scope*> &scopes);
u32 checkFor(ASTFor *For, DynamicArray<Scope*> &scopes, Lexer &lexer){
    Scope *scope = scopes[scopes.count-1];
    Scope *body = check::newBlockScope(scope->varId);
    scopes.push(body);
    if(For->label.len != 0){
        u32 val;
        if(check::loopLabels.getValue(For->label, &val)){
            lexer.emitErr(For->tokenOff, "Loop name(%.*s) has already been taken", For->label.len, For->label.mem);
            return 0;
        }; 
        check::loopLabels.insertValue(For->label, check::loopLabels.count);
    };
    if(For->end != nullptr){
        //c-for
        if(For->decl->lhsCount != 1){
            lexer.emitErr(For->tokenOff, "C-style for loop has to have only 1 declared variable");
            return 0;
        };
        if(checkDecl(For->decl, scopes, lexer) == 0) return 0;
        u32 endPointerDepth;
        Type forVarType = For->decl->zType->zType;
        u32 forVarPd = For->decl->zType->pointerDepth;
        Type endType = checkTree(lexer, &For->end, scopes, endPointerDepth, forVarType, forVarPd);
        if(endType == Type::INVALID) return 0;
        if(For->step){
            u32 stepPointerDepth;
            Type stepType = checkTree(lexer, &For->step, scopes, stepPointerDepth, forVarType, forVarPd);
            if(stepType == Type::INVALID) return 0;
            if(isInteger(stepType) == false){
                lexer.emitErr(For->tokenOff, "Step type should be an integer");
                return 0;
            };
            if(stepPointerDepth > 0){
                lexer.emitErr(For->tokenOff, "Step expression tree cannot contain pointers");
                return 0;
            };
        };
    }else if(For->expr){
        //c-while
        u32 pd;
        Type type = checkTree(lexer, &For->expr, scopes, pd, Type::INVALID);
        if(type == Type::INVALID) return false;
        For->exprType.zType = type;
        For->exprType.pointerDepth = pd;
    };
    for(u32 x=0; x<For->bodyCount; x++){
        if(!checkASTNode(lexer, For->body[x], scopes)) return false;
    };
    scopes.pop();
    if(For->label.len != 0) check::loopLabels.removeValue(For->label);
    return body->varId;
};
bool checkProc(ASTProcDefDecl *proc, DynamicArray<Scope*> &scopes, Lexer &lexer, bool isDecl){
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
    entity->isDecl = isDecl;
    entity->varArgs = proc->varArgs;
    entity->inputs = proc->inputs;
    entity->inputCount = proc->inputCount;
    entity->inputNodeCount = proc->inputNodeCount;
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
    if(isDecl == false){
        for(u32 x=0; x<proc->inputNodeCount; x++){
            if(proc->inputs[x]->type != ASTType::DECLERATION){
                lexer.emitErr(proc->tokenOff, "Input %d is not a decleration", x);
                return false;
            };
            ASTAssDecl *input = proc->inputs[x];
            if(checkDecl(input, procInputScope, lexer, true) == 0) return false;
        };
    }else{
        for(u32 x=0; x<proc->inputCount; x++){
            if(!fillTypeInfo(lexer, proc->typeInputs[x])) return false;
        };
    };
    for(u32 x=0; x<proc->outputCount; x++){
        if(!fillTypeInfo(lexer, proc->outputs[x])) return false;
    };
    if(isDecl) return true;
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
                Type defOutType = entity->outputs[i]->zType;
                u32 defOutPd = entity->outputs[i]->pointerDepth;
                Type type= checkTree(lexer, &ret->exprs[i], scopes, pointerDepth, defOutType, defOutPd);
                ret->types[i].zType = type;
                ret->types[i].pointerDepth = pointerDepth;
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
    u32 id = check::structEntities.count;
    check::structToOff.insertValue(Struct->name, id);
    Struct->id = id;
    StructEntity *entity = &check::structEntities.newElem();
    Scope *body = check::newStructScope();
    entity->body = body;
    entity->id = id;
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
u32 checkIf(ASTIf *If, DynamicArray<Scope*> &scopes, Lexer &lexer){
    u32 newId;
    u32 treePointerDepth;
    Scope *scope = scopes[scopes.count - 1];
    Type treeType = checkTree(lexer, &If->expr, scopes, treePointerDepth, Type::INVALID);
    if(treeType == Type::INVALID) return false;
    if(treeType > Type::Z_TYPE_END && treePointerDepth == 0){
        lexer.emitErr(If->tokenOff, "Invalid expression");
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
        case ASTType::RETURN: return true;
        case ASTType::CONT:
        case ASTType::BREAK:{
                                ASTFlow *flow = (ASTFlow*)node;
                                if(flow->label.len != 0){
                                    u32 val;
                                    if(check::loopLabels.getValue(flow->label, &val) == false){
                                        lexer.emitErr(flow->tokenOff, "Label(%.*s) not defined", flow->label.len, flow->label.mem);
                                        return false;
                                    };
                                    flow->relId = val;
                                }else flow->relId = -1;
                            }break;
        case ASTType::FOR:{
                              newId = checkFor((ASTFor*)node, scopes, lexer);
                              if(newId == 0) return false;
                          }break;
        case ASTType::PROC_DECL:{
                                    if(checkProc((ASTProcDefDecl*)node, scopes, lexer, true) == false) return false;
                                }break;
        case ASTType::PROC_DEF:{
                                   if(checkProc((ASTProcDefDecl*)node, scopes, lexer, false) == false) return false;
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
                    return checkTree(lexer, &node, scopes, pointerDepth, Type::INVALID) != Type::INVALID;
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
    curASTFileForCastNodeAlloc = &file;
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
