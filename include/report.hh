#pragma once

#include "basic.hh"

#define MAX_ERRORS 10
#define MAX_WARNINGS 10

namespace report{
    struct Report {
        u64   off;
        char *fileName;
        char *msg;
        char *fileContent;
    };

    extern Report errors[MAX_ERRORS];
    extern u8 errorOff;
    extern char reportBuff[1024];
    extern u32 reportBuffTop; 

    void flushReports();
}
