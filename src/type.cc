#include "../include/type.hh"

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

char *typeToStr[] = {
    "invalid",
    "bool",
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
};
