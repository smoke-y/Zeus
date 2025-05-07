#include "../include/basic.hh"
#include "../include/ds.hpp"
#include "../include/lexer.hh"
#include "../include/parser.hh"

namespace dep{
    extern DynamicArray<u32> deps;
    extern HashmapStr fileToId;
    extern DynamicArray<Lexer> lexers;
    extern DynamicArray<ASTFile> astFiles;
    extern DynamicArray<u32> depCallStack;


    void init();
    void uninit();
    void insertCallFrameIfNotInserted(u32 src);
    s32 insertFileToDepsAndInitLexer(String fileName);
};
