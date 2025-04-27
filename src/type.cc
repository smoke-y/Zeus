#include "../include/type.hh"

bool isNumber(Type type) {return type >= Type::S64 && type <= Type::COMP_INTEGER && type != Type::CHAR;}
