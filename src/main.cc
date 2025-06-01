#include "build.hh"

s32 main(s32 argc, char **argv){
    mem::init();
    if(argc < 2){
        printf("no entryfile provided\n");
        return EXIT_FAILURE;
    };
    char *inputPath = argv[1];
    char *outputPath = "out.asm";
    if(argc == 3) outputPath = argv[2];

    Word::init(Word::keywords, Word::keywordsData, ARRAY_LENGTH(Word::keywordsData));
    Word::init(Word::poundwords, Word::poundwordsData, ARRAY_LENGTH(Word::poundwordsData));
    dep::init();
    DEFER({
            Word::uninit(Word::keywords);
            Word::uninit(Word::poundwords);
            dep::uninit();
            mem::uninit();
            });
    s32 mainFileId = dep::insertFileToDepsAndInitLexer({inputPath, (u32)strlen(inputPath)});
    if(mainFileId == -1){
        printf("Invalid entryfile location\n");
        return EXIT_FAILURE;
    };
    dep::insertCallFrameIfNotInserted(mainFileId);

    for(u32 x=0; x<dep::lexers.count; x++){
        Lexer &lexer = dep::lexers[x];
        if(!lexer.genTokens()){
            report::flushReports();
            return EXIT_FAILURE;
        };
        ASTFile &file = dep::astFiles.newElem();
        file.init(x);
        if(!parseFile(lexer, file)){
            report::flushReports();
            return EXIT_FAILURE;
        };
    };

    check::init();
    DynamicArray<ASTBase*> globals;
    globals.init();
    u32 size = sizeof(bool) * dep::lexers.count;
    bool *status = (bool*)mem::alloc(size);
    DEFER({
            mem::free(status);
            globals.uninit();
            check::uninit();
            printf("\nDone :)\n");
            });
#if(DBG)
    for(u32 x=0; x<dep::lexers.count; x++){
        Lexer &lexer = dep::lexers[x];
        //printf("--------------FILE: %s--------------", lexer.fileName);
        //dbg::dumpLexerTokens(lexer);
        dbg::dumpASTFile(dep::astFiles[x], lexer);
    }
#endif
    memset(status, false, size);
    for(u32 x=dep::deps.count; x > 0;){
        x -= 1;
        u32 id = dep::deps[x];
        if(status[id] == true) continue;
        status[id] = true;
        if(!checkASTFile(dep::lexers[id], dep::astFiles[id], globals)){
            report::flushReports();
            return EXIT_FAILURE;
        };
    };
    lowerToLLVM("bin/out.ll", globals);
    system("clang bin/out.ll -o bin/out");
    return EXIT_SUCCESS;
};
