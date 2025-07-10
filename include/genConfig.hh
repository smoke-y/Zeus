#pragma once

#include "../include/basic.hh"

enum class LinkConfigCLibs{
    LIBC,
    MATH,
    COUNT
};

static char *LinkConfigCLibsToStr[] = {
    "c",
    "m",
};

struct GenConfig{
    u32 linkCLibs;
    u8 optimzation;
    bool isSharedLib;
};

extern GenConfig genConfig;
