#include "../include/type.hh"

char* typeToStrTable[] = {
    "invalid",
    "defer_cast",
    "defer_fill",
    "comp_string",
    "comp_integer",
    "comp_decimal",
    "void",
    "z_type_start",
    "u8",
    "s8",
    "char",
    "u16",
    "s16",
    "u32",
    "s32",
    "f32",
    "u64",
    "s64",
    "f64",
    "ptr",
    "z_type_end",
};

Type convertFromComptype(Type type){
    if(type == Type::COMP_INTEGER) return Type::S64;
    if(type == Type::COMP_DECIMAL) return Type::F64;
    return type;
};
Type tokTypeToZeusType(TokType type){
    return (Type)((u32)type - (u32)TokType::K_TYPE_START + (u32)(Type::Z_TYPE_START));
}
inline char *typeToStr(Type type){return typeToStrTable[(u32)type];};
bool isNumber(Type type) {return (type >= Type::U8 && type <= Type::F64) || type == Type::COMP_INTEGER || type == Type::COMP_DECIMAL;}
bool isSigned(Type type){
    switch(type){
        case Type::COMP_INTEGER:
        case Type::S8:
        case Type::S16:
        case Type::S32:
        case Type::S64:
        case Type::F32:
        case Type::F64: return true;
    };
    return false;
};
bool isCompType(Type type){return type >= Type::COMP_STRING && type <= Type::COMP_DECIMAL;};
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
bool canWeCast(Type t1, u32 pd1, Type t2, u32 pd2, u32 off, Lexer &lexer){
    if(t2 == Type::VOID || t1 == Type::VOID) return false;
    if(pd1 == 0 && pd2 == 0){
        if(isInteger(t1) ^ isInteger(t2)){
            lexer.emitErr(off, "Both have to be integers");
            return false;
        }else if(isDecimal(t1) ^ isDecimal(t2)){
            lexer.emitErr(off, "Both have to be decimals");
            return false;
        };
    };
    if(t2 == Type::COMP_STRING){
        t2 = t1;
        t1 = Type::COMP_STRING;
        u32 temp = pd2;
        pd2 = pd1;
        pd1 = pd2;
    };
    if(t1 == Type::COMP_STRING){
        if(t2 == Type::CHAR){
            if(pd2 > 1) lexer.emitWarn(off, "String(char of pointer depth 1) to char of pointer depth %d", pd2);
            return true;
        };
        return false;
    };
    if((pd1 == 0 && pd2 > 0) || (pd1 > 0 && pd2 == 0)){
        lexer.emitErr(off, "One is a pointer and is not");
        return false;
    };
    if(pd1 != pd2) lexer.emitWarn(off, "Pointer depths do not match %d != %d", pd1, pd2);
    return true;
};
bool implicitOk(Type t1, Type t2){
    /*
     * t1 --casted_to--> t2
     * @type: type clamp down 2
     * handle comp_types
     */
    if(t2 == Type::COMP_DECIMAL || t2 == Type::COMP_INTEGER || t2 == Type::COMP_STRING){
        Type temp = t2;
        t2 = t1;
        t1 = temp;
    };
    switch(t1){
        case Type::COMP_DECIMAL:{
                                    switch(t2){
                                        case Type::COMP_DECIMAL:
                                        case Type::F64:
                                        case Type::F32: return true;
                                    };
                                    return false;
                                }break;
        case Type::COMP_INTEGER:{
                                    switch(t2){
                                        case Type::COMP_INTEGER:
                                        case Type::S64:
                                        case Type::U64:
                                        case Type::S32:
                                        case Type::U32:
                                        case Type::S16:
                                        case Type::U16:
                                        case Type::S8:
                                        case Type::U8: return true;
                                    };
                                    return false;
                                }break;
        case Type::COMP_STRING: return true;
    };
    if(t1 == Type::CHAR && t2 == Type::CHAR) return true; 
    if(t1 > t2 && t1 != Type::PTR) return false;
    return true;
};
