#pragma once

#include "../include/parser.hh"
#include "ds.hpp"

enum class VMBytecode{
    LOG,
    LICENSE,
};

bool loadVM();
void unloadVM();
void lowerToBytecodeAndExecOnVM(DynamicArray<parser::MacroBody> mbodies);
