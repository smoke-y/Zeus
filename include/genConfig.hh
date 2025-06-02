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
};

void initGenConfig();

extern GenConfig genConfig;
