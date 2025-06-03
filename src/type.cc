#include "../include/type.hh"
#include "../include/basic.hh"
#include "../include/lexer.hh"

char *typeToStr[] = {
    "invalid",
    "f64",
    "s64",
    "u64",
    "f32",
    "s32",
    "u32",
    "s16",
    "u16",
    "char",
    "s8",
    "u8",
    "comp_decimal",
    "comp_integer",
    "comp_string",
    "void",
};

bool isNumber(Type type) {return type >= Type::S64 && type <= Type::COMP_INTEGER && type != Type::CHAR;}
bool isInteger(Type type){
    switch(type){
        case Type::COMP_INTEGER:
        case Type::S64:
        case Type::S32:
        case Type::S16:
        case Type::U64:
        case Type::U32:
        case Type::U16: return true;
    };
    return false;
};
bool isDecimal(Type type){
    switch(type){
        case Type::F64:
        case Type::F32:
        case Type::COMP_DECIMAL: return true;
    };
    return false;
};
bool typeOk(Type t1, u32 pd1, Type t2, u32 pd2, u32 off, Lexer &lexer){
    if(t1 == Type::INVALID || t1 == Type::VOID) return false;
    if(t2 == Type::INVALID || t2 == Type::VOID) return false;
    if(t2 == Type::COMP_DECIMAL || t2 == Type::COMP_INTEGER || t2 == Type::COMP_STRING){
        Type temp = t2;
        u32 pdTemp = pd2;
        t2 = t1;
        pd2 = pd1;
        t1 = temp;
        pd1 = pdTemp;
    };
    switch(t1){
        case Type::COMP_DECIMAL:{
                                    switch(t2){
                                        case Type::F64:
                                        case Type::F32: goto __CHECK_POINTER_DEPTH;
                                    };
                                    return false;
                                }break;
        case Type::COMP_INTEGER:{
                                    switch(t2){
                                        case Type::S64:
                                        case Type::U64:
                                        case Type::S32:
                                        case Type::U32:
                                        case Type::S16:
                                        case Type::U16:
                                        case Type::S8:
                                        case Type::U8: goto __CHECK_POINTER_DEPTH;
                                    };
                                    return false;
                                }break;
        case Type::COMP_STRING:{
                                   if(t2 == Type::CHAR){
                                       if(pd2 > 1){
                                           lexer.emitWarn(off, "String(char of pointer depth 1) to char of pointer depth %d", pd2);
                                       };
                                       return true;
                                   };
                                   return false;
                               }break;
    };
    if(isInteger(t1) ^ isInteger(t2)) return false;
    else if(isDecimal(t1) ^ isDecimal(t2)) return false;
    else if(!(t1 == Type::CHAR && t2 == Type::CHAR)) return false;
    else return false;
    if(t2 > t1){
        Type temp = t2;
        u32 pdTemp = pd2;
        t2 = t1;
        pd2 = pd1;
        t1 = temp;
        pd1 = pdTemp;
    };
    Type acceptedT2;
    if(isInteger(t1)){
        switch(t1){
            case Type::S64: acceptedT2 = Type::U64;break;
            case Type::S32: acceptedT2 = Type::U32;break;
            case Type::S16: acceptedT2 = Type::U16;break;
            case Type::S8: acceptedT2 = Type::U8;break;
        };
    }else acceptedT2 = t1;
    if(t2 != acceptedT2){
        lexer.emitErr(off, "Explicit cast required");
        return false;
    };
__CHECK_POINTER_DEPTH:
    if(pd1 == pd2) return true;
    if((pd1 == 0 && pd2 > 0) || (pd1 > 0 && pd2 == 0)){
        lexer.emitErr(off, "One is a pointer and is not");
        return false;
    };
    lexer.emitWarn(off, "Pointer depths do not match %d != %d", pd1, pd2);
    return true;
};
