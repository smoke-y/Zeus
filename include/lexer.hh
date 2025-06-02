#pragma once

#include "../include/basic.hh"
#include "../include/ds.hpp"
#include <stdarg.h>

enum class TokType {
    END_OF_FILE,

    //room for ASCII chars

    IDENTIFIER = 256,
    INTEGER,
    DECIMAL,
    SINGLE_QUOTES,
    DOUBLE_QUOTES,
    DDOT,
    TDOT,

    K_START,       //keywords start
    K_TYPE_START,  //type start
    K_F64,
    K_S64,
    K_U64,
    K_F32,
    K_S32,
    K_U32,
    K_S16,
    K_U16,
    K_CHAR,
    K_S8,
    K_U8,
    K_TYPE_END,    //type end
    K_PROC_DEF,
    K_PROC_DECL,
    K_IF,
    K_STRUCT,
    K_FOR,
    K_CONSTANT,
    K_RETURN,
    K_ELSE,
    K_TRUE,
    K_FALSE,
    K_END,       //keywords end

    P_START,     //poundwords start
    P_IMPORT,
    P_LINK,
    P_END,       //poundwords end
};
struct TokenOffset {
    u32 off;
    u16 len;
};

struct Lexer{
    DynamicArray<TokenOffset> tokenOffsets;
    DynamicArray<TokType> tokenTypes;
    char *fileName;
    char *fileContent;

    bool init(char *fn);
    void uninit();
    void emitWarn(u32 off, char *fmt, ...);
    void emitErr(u32 off, char *fmt, ...);
    void emitErrAbs(u32 off, char *fmt, ...);
    b32 genTokens();
};

bool isType(TokType type);
