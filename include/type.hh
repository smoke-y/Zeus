#pragma once

#include "../include/basic.hh"
#include "../include/lexer.hh"

enum class Type{
    INVALID,
    DEFER_CAST,
    COMP_STRING,
    COMP_INTEGER,
    COMP_DECIMAL,
    VOID,
    Z_TYPE_START,
    U8,
    S8,
    CHAR,
    U16,
    S16,
    U32,
    S32,
    F32,
    U64,
    S64,
    F64,
    Z_TYPE_END,
};

Type convertFromComptype(Type type);
char *typeToStr(Type type);
bool isSigned(Type type);
bool isCompType(Type type);
bool isNumber(Type type);
bool isInteger(Type type);
bool isDecimal(Type type);
bool canWeCast(Type t1, u32 pd1, Type t2, u32 pd2, u32 off, Lexer &lexer);
bool implicitOk(Type t1, Type t2);
