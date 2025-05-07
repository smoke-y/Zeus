#include "../include/dependency.hh"

namespace dep{
    DynamicArray<u32> deps;
    HashmapStr fileToId;
    DynamicArray<Lexer> lexers;
    DynamicArray<ASTFile> astFiles;
    DynamicArray<u32> depCallStack;

    void init(){
        deps.init();
        fileToId.init();
        lexers.init();
        astFiles.init();
        depCallStack.init();
    };
    void uninit(){
        deps.uninit();
        fileToId.uninit();
        depCallStack.uninit();
        for(u32 x=0; x<astFiles.count; x++) astFiles[x].uninit();
        for(u32 x=0; x<lexers.count; x++) lexers[x].uninit();
        astFiles.init();
        lexers.uninit();
    };

    void insertCallFrameIfNotInserted(u32 id){
        for(u32 x=depCallStack.count; x>0; x--){
            if(depCallStack[x] == id) return;
        };
        depCallStack.push(id);
    };
    s32 insertFileToDepsAndInitLexer(String fileName){
        u32 value;
        if(fileToId.getValue(fileName, &value) == false){
            value = fileToId.count;
            fileToId.insertValue(fileName, value);
            Lexer &lexer = lexers.newElem();
            char c = fileName.mem[fileName.len];
            fileName.mem[fileName.len] = '\0';
            bool stat = lexer.init(fileName.mem);
            fileName.mem[fileName.len] = c;
            if(!stat) return -1;
        };
        //check for circular deps
        for(u32 x=depCallStack.count; x>0; x--){
            if(depCallStack[x] == value) return -2;
        };
        deps.push(value);
        return value;
    };
};
