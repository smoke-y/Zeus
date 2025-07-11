//@ignore
#if(__clang__)
#pragma clang diagnostic ignored "-Wwritable-strings"
#pragma clang diagnostic ignored "-Wswitch"
#pragma clang diagnostic ignored "-Wdeprecated-declarations"
#pragma clang diagnostic ignored "-Wmicrosoft-include"
#pragma clang diagnostic ignored "-Wmicrosoft-goto"
#pragma clang diagnostic ignored "-Wswitch"
#pragma clang diagnostic ignored "-Wint-to-pointer-cast"
#endif

#include <stdlib.h>
#include <math.h>

#if(SIMD)
#include <immintrin.h>
#endif
#if(WIN)
#include "windows.h"
#elif(LIN)
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#endif

#include "mem.cc"

#include "report.cc"
#include "lexer.cc"
#include "type.cc"
#include "parser.cc"
#include "dependency.cc"
#include "checker.cc"
#include "vm.cc"
#include "llvm.cc"
