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

s32 main(){
#if(DBG)
    DEFER(printf("Done :)\n"));
#endif
    Word::init(Word::keywords, Word::keywordsData, ARRAY_LENGTH(Word::keywordsData));
    Word::init(Word::poundwords, Word::poundwordsData, ARRAY_LENGTH(Word::poundwordsData));
    DEFER({
        Word::uninit(Word::keywords);
        Word::uninit(Word::poundwords);
    });
    Lexer lexer;
    lexer.init("test/t1.zs");
    DEFER(lexer.uninit());
    if(lexer.genTokens()){
        dbg::dumpLexerTokens(lexer);
        ASTFile file;
        file.init();
        DEFER(file.uninit());
        u32 cursor = 0;
        if(parseBlock(lexer, file, cursor) == false){
            report::flushReports();
        }else{
            dbg::dumpASTFile(file);
        };
    }else{
        report::flushReports();
    };
};