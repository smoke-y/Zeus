#include "basic.hh"
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>



namespace mem{
    void init();
    void uninit();
    void *alloc(u64 size);
    void *calloc(u64 size);
    void free(void *ptr);
};
