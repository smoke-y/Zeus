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

#include "include.hh"

s32 main(s32 argc, char **argv){
#if(DBG)
    DEFER(printf("\nDone :)\n"));
#endif
    if(argc < 2){
        printf("no entryfile provided\n");
        return EXIT_SUCCESS;
    };
    char *inputPath = argv[1];
    char *outputPath = "out.asm";
    if(argc == 3) outputPath = argv[2];

    Word::init(Word::keywords, Word::keywordsData, ARRAY_LENGTH(Word::keywordsData));
    Word::init(Word::poundwords, Word::poundwordsData, ARRAY_LENGTH(Word::poundwordsData));
    linearDepEntities.init();
    linearDepStrings.init();

    FileEntity &mainFileEntity = linearDepEntities.newElem();
    mainFileEntity.lexer.init(inputPath);
    mainFileEntity.file.init();

    DEFER({
        for(u32 x=0; x<linearDepEntities.count; x++){
            FileEntity &fe = linearDepEntities[x];
            fe.lexer.uninit();
            fe.file.uninit();
        }
        linearDepEntities.uninit();
        linearDepStrings.uninit();
        Word::uninit(Word::keywords);
        Word::uninit(Word::poundwords);
    });

    if(!mainFileEntity.lexer.genTokens()){
        report::flushReports();
        return EXIT_SUCCESS;
    };
    if(!parseFile(mainFileEntity.lexer, mainFileEntity.file)){
        report::flushReports();
        return EXIT_SUCCESS;
    };
    while(linearDepStrings.count != 0){
        String path = linearDepStrings.pop();
        char c = path.mem[path.len];
        path.mem[path.len] = '\0';
        FileEntity &fe = linearDepEntities.newElem();
        fe.lexer.init(path.mem);
        fe.file.init();
        path.mem[path.len] = c;
        if(!fe.lexer.genTokens()){
            report::flushReports();
            return EXIT_SUCCESS;
        };
        if(!parseFile(fe.lexer, fe.file)){
            report::flushReports();
            return EXIT_SUCCESS;
        };
    };
#if(DBG)
    for(u32 x=0; x<linearDepEntities.count; x++){
        FileEntity &fe = linearDepEntities[x];
        printf("--------------FILE: %s--------------", fe.lexer.fileName);
        dbg::dumpASTFile(fe.file, fe.lexer);
    }
#endif
    return EXIT_SUCCESS;
};