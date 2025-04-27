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
#endif

#include "mem.cc"

#include "report.cc"
#include "lexer.cc"
#include "type.cc"
#include "parser.cc"
#include "checker.cc"

#include "riscvAsm.cc"
#include "llvm.cc"
