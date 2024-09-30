#include <gtest/gtest.h>
#include <km_u16.h>
#include "../include/kmcompx.h"
#include "../include/kmcmplibapi.h"
#include "../src/compfile.h"
#include "../src/CompilerErrors.h"
#include "../../common/include/kmn_compiler_errors.h"
#include "../../../../common/include/km_types.h"
#include "../../../../common/include/kmx_file.h"

PKMX_WCHAR strtowstr(PKMX_STR in);
PKMX_STR wstrtostr(PKMX_WCHAR in);
KMX_BOOL ProcessBeginLine(PFILE_KEYBOARD fk, PKMX_WCHAR p);
KMX_DWORD ValidateMatchNomatchOutput(PKMX_WCHAR p);
KMX_BOOL IsValidKeyboardVersion(KMX_WCHAR *dpString);
PKMX_WCHAR GetDelimitedString(PKMX_WCHAR *p, KMX_WCHAR const * Delimiters, KMX_WORD Flags);
int LineTokenType(PKMX_WCHAR *str);
KMX_DWORD GetXStringImpl(PKMX_WCHAR tstr, PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_WCHAR const * token,
  PKMX_WCHAR output, int max, int offset, PKMX_WCHAR *newp, int isUnicode
);
KMX_DWORD ProcessEthnologueStore(PKMX_WCHAR p);
KMX_DWORD GetRHS(PFILE_KEYBOARD fk, PKMX_WCHAR p, PKMX_WCHAR buf, int bufsize, int offset, int IsUnicode);
bool isIntegerWstring(PKMX_WCHAR p);
bool hasPreamble(std::u16string result);
KMX_DWORD ProcessKeyLineImpl(PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_BOOL IsUnicode, PKMX_WCHAR pklIn, PKMX_WCHAR pklKey, PKMX_WCHAR pklOut);

namespace kmcmp {
    extern int nErrors;
    extern int currentLine;
    extern int ErrChr;
    extern std::string messageFilename;
    extern int BeginLine[4];
    extern int CompileTarget;
}

class CompilerTest : public testing::Test {
    protected:
        FILE_KEYBOARD fileKeyboard;

    	CompilerTest() {}
	    ~CompilerTest() override {}
	    void SetUp() override {
            initGlobals();
            initFileKeyboard(fileKeyboard);
        }
	    void TearDown() override {
            deleteFileKeyboard(fileKeyboard);
        }

        void initGlobals() {
            kmcmp::msgproc = msgproc_collect;
            msgproc_errors.clear();
            kmcmp::nErrors = 0;
            kmcmp::currentLine = 0;
            kmcmp::ErrChr = 0;
            kmcmp::messageFilename = "";
            kmcmp::BeginLine[BEGIN_ANSI] = -1;
            kmcmp::BeginLine[BEGIN_UNICODE] = -1;
            kmcmp::BeginLine[BEGIN_NEWCONTEXT] = -1;
            kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = -1;
            kmcmp::CompileTarget = CKF_KEYMAN;
        }

        void initFileKeyboard(FILE_KEYBOARD &fk) {
            fk.KeyboardID        = 0;
            fk.version           = VERSION_90;
            fk.dpStoreArray      = nullptr;
            fk.dpGroupArray      = nullptr;
            fk.cxStoreArray      = 0;
            fk.cxGroupArray      = 0;
            fk.StartGroup[0]     = 0;
            fk.StartGroup[1]     = 0;
            fk.dwHotKey          = 0;
            fk.szName[0]         = u'\0';
            fk.szLanguageName[0] = u'\0';
            fk.szCopyright[0]    = u'\0';
            fk.szMessage[0]      = u'\0';
            fk.lpBitmap          = nullptr;
            fk.dwBitmapSize      = 0;
            fk.dwFlags           = 0;
            fk.currentGroup      = 0;
            fk.currentStore      = 0;
            fk.cxDeadKeyArray    = 0;
            fk.dpDeadKeyArray    = nullptr;
            fk.cxVKDictionary    = 0;
            fk.dpVKDictionary    = nullptr;
            fk.extra             = nullptr;
        }

        void initFileStoreArray(FILE_KEYBOARD &fk, std::vector<const KMX_WCHAR*> names) {
            fk.dpStoreArray = new FILE_STORE[100];
            fk.cxStoreArray = names.size();

            for (int i=0; i<fk.cxStoreArray; i++) {
                fk.dpStoreArray[i].dwSystemID  = 0;
                u16cpy(fk.dpStoreArray[i].szName, names[i]);
                fk.dpStoreArray[i].dpString    = nullptr;
                fk.dpStoreArray[i].fIsStore    = FALSE;
                fk.dpStoreArray[i].fIsReserved = FALSE;
                fk.dpStoreArray[i].fIsOption   = FALSE;
                fk.dpStoreArray[i].fIsDebug    = FALSE;
                fk.dpStoreArray[i].fIsCall     = FALSE;
            }
        }

        void initFileGroupArray(FILE_KEYBOARD &fk, KMX_BOOL fUsingKeys) {
            fk.dpGroupArray = new FILE_GROUP[1];
            fk.cxGroupArray = 1;

            fk.dpGroupArray->szName[0] = 0;
            fk.dpGroupArray->cxKeyArray = 0;
            fk.dpGroupArray->dpKeyArray = nullptr;
            fk.dpGroupArray->dpMatch = nullptr;
            fk.dpGroupArray->dpNoMatch = nullptr;
            fk.dpGroupArray->fUsingKeys = fUsingKeys;
            fk.dpGroupArray->fReadOnly = FALSE;
            fk.dpGroupArray->Line = 0;
        }

        void deleteFileKeyboard(FILE_KEYBOARD &fk) {
            if (fk.dpStoreArray)   { delete[] fk.dpStoreArray;   }
            if (fk.dpGroupArray)   { delete[] fk.dpGroupArray;   }
            if (fk.lpBitmap)       { delete   fk.lpBitmap;       }
            if (fk.dpDeadKeyArray) { delete[] fk.dpDeadKeyArray; }
            if (fk.dpVKDictionary) { delete   fk.dpVKDictionary; }
            if (fk.extra)          { delete   fk.extra;          }
        }

    public:
        static std::vector<KMCMP_COMPILER_RESULT_MESSAGE> msgproc_errors;

        static void msgproc_collect(const KMCMP_COMPILER_RESULT_MESSAGE &message, void* context) {
            msgproc_errors.push_back(message);
        }
};

std::vector<KMCMP_COMPILER_RESULT_MESSAGE> CompilerTest::msgproc_errors;

TEST_F(CompilerTest, strtowstr_test) {
    EXPECT_EQ(0, u16cmp(u"hello", strtowstr((PKMX_STR)"hello")));
    EXPECT_EQ(0, u16cmp(u"", strtowstr((PKMX_STR)"")));
};

TEST_F(CompilerTest, wstrtostr_test) {
    EXPECT_EQ(0, strcmp("hello", wstrtostr((PKMX_WCHAR)u"hello")));
    EXPECT_EQ(0, strcmp("", wstrtostr((PKMX_WCHAR)u"")));
};

TEST_F(CompilerTest, ReportCompilerMessage_test) {
    kmcmp::msgproc = msgproc_collect;
    kmcmp::currentLine = 42;
    std::vector<std::string> params{"parameter"};
    kmcmp::messageFilename = "filename";
    kmcmp::ErrChr = 0;

    // SevFatal
    EXPECT_EQ(0, kmcmp::nErrors);
    EXPECT_EQ(SevFatal, KmnCompilerMessages::FATAL_CannotCreateTempfile & SevFatal);
    ReportCompilerMessage(KmnCompilerMessages::FATAL_CannotCreateTempfile, params);
    EXPECT_EQ(1, kmcmp::nErrors);
    EXPECT_EQ(KmnCompilerMessages::FATAL_CannotCreateTempfile, msgproc_errors[0].errorCode);
    EXPECT_EQ(kmcmp::currentLine+1, msgproc_errors[0].lineNumber);
    EXPECT_EQ(kmcmp::ErrChr, msgproc_errors[0].columnNumber);
    EXPECT_TRUE(msgproc_errors[0].filename == kmcmp::messageFilename);
    EXPECT_TRUE(msgproc_errors[0].parameters == params);

    // SevError
    EXPECT_EQ(SevError, KmnCompilerMessages::ERROR_InvalidLayoutLine & SevError);
    ReportCompilerMessage(KmnCompilerMessages::ERROR_InvalidLayoutLine);
    EXPECT_EQ(2, kmcmp::nErrors);
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidLayoutLine, msgproc_errors[1].errorCode);

    // SevWarn
    EXPECT_EQ(SevWarn, KmnCompilerMessages::WARN_ReservedCharacter & SevWarn);
    ReportCompilerMessage(KmnCompilerMessages::WARN_ReservedCharacter);
    EXPECT_EQ(2, kmcmp::nErrors);
    EXPECT_EQ(KmnCompilerMessages::WARN_ReservedCharacter, msgproc_errors[2].errorCode);

    // SevHint
    EXPECT_EQ(SevHint, KmnCompilerMessages::HINT_NonUnicodeFile & SevHint);
    ReportCompilerMessage(KmnCompilerMessages::HINT_NonUnicodeFile);
    EXPECT_EQ(2, kmcmp::nErrors);
    EXPECT_EQ(KmnCompilerMessages::HINT_NonUnicodeFile, msgproc_errors[3].errorCode);

    // SevInfo
    EXPECT_EQ(SevInfo, KmnCompilerMessages::INFO_MinimumCoreEngineVersion & SevInfo);
    ReportCompilerMessage(KmnCompilerMessages::INFO_MinimumCoreEngineVersion);
    EXPECT_EQ(2, kmcmp::nErrors);
    EXPECT_EQ(KmnCompilerMessages::INFO_MinimumCoreEngineVersion, msgproc_errors[4].errorCode);
};

TEST_F(CompilerTest, ProcessBeginLine_test) {
    KMX_WCHAR str[LINESIZE];

    // KmnCompilerMessages::ERROR_NoTokensFound
    msgproc_errors.clear();
    u16cpy(str, u"");
    EXPECT_EQ(FALSE, ProcessBeginLine(&fileKeyboard, str));
    EXPECT_EQ(1, msgproc_errors.size());
    EXPECT_EQ(KmnCompilerMessages::ERROR_NoTokensFound, msgproc_errors[0].errorCode);

    // KmnCompilerMessages::ERROR_InvalidToken
    msgproc_errors.clear();
    u16cpy(str, u"abc >");
    EXPECT_EQ(FALSE, ProcessBeginLine(&fileKeyboard, str));
    EXPECT_EQ(1, msgproc_errors.size());
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, msgproc_errors[0].errorCode);

    // KmnCompilerMessages::ERROR_RepeatedBegin, BEGIN_UNICODE
    msgproc_errors.clear();
    kmcmp::BeginLine[BEGIN_UNICODE] = 0; // not -1
    u16cpy(str, u" unicode>");
    EXPECT_EQ(FALSE, ProcessBeginLine(&fileKeyboard, str));
    EXPECT_EQ(1, msgproc_errors.size());
    EXPECT_EQ(KmnCompilerMessages::ERROR_RepeatedBegin, msgproc_errors[0].errorCode);
    kmcmp::BeginLine[BEGIN_UNICODE] = -1;

    // KmnCompilerMessages::ERROR_RepeatedBegin, BEGIN_ANSI
    msgproc_errors.clear();
    kmcmp::BeginLine[BEGIN_ANSI] = 0; // not -1
    u16cpy(str, u" ansi>");
    EXPECT_EQ(FALSE, ProcessBeginLine(&fileKeyboard, str));
    EXPECT_EQ(1, msgproc_errors.size());
    EXPECT_EQ(KmnCompilerMessages::ERROR_RepeatedBegin, msgproc_errors[0].errorCode);
    kmcmp::BeginLine[BEGIN_ANSI] = -1;

    // KmnCompilerMessages::ERROR_RepeatedBegin, BEGIN_NEWCONTEXT
    msgproc_errors.clear();
    kmcmp::BeginLine[BEGIN_NEWCONTEXT] = 0; // not -1
    u16cpy(str, u" newContext>");
    EXPECT_EQ(FALSE, ProcessBeginLine(&fileKeyboard, str));
    EXPECT_EQ(1, msgproc_errors.size());
    EXPECT_EQ(KmnCompilerMessages::ERROR_RepeatedBegin, msgproc_errors[0].errorCode);
    kmcmp::BeginLine[BEGIN_NEWCONTEXT] = -1;

    // KmnCompilerMessages::ERROR_RepeatedBegin, BEGIN_POSTKEYSTROKE
    msgproc_errors.clear();
    kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = 0; // not -1
    u16cpy(str, u" postKeystroke>");
    EXPECT_EQ(FALSE, ProcessBeginLine(&fileKeyboard, str));
    EXPECT_EQ(1, msgproc_errors.size());
    EXPECT_EQ(KmnCompilerMessages::ERROR_RepeatedBegin, msgproc_errors[0].errorCode);
    kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = -1;
};

TEST_F(CompilerTest, ValidateMatchNomatchOutput_test) {
    EXPECT_EQ(STATUS_Success, ValidateMatchNomatchOutput(NULL));
    EXPECT_EQ(STATUS_Success, ValidateMatchNomatchOutput((PKMX_WCHAR)u""));
    const KMX_WCHAR context[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXT, 'd', 'e', 'f', 0 };
    EXPECT_EQ(KmnCompilerMessages::ERROR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)context));
    const KMX_WCHAR contextex[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXTEX, 'd', 'e', 'f', 0 };
    EXPECT_EQ(KmnCompilerMessages::ERROR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)contextex));
    const KMX_WCHAR index[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_INDEX, 'd', 'e', 'f', 0 };
    EXPECT_EQ(KmnCompilerMessages::ERROR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)index));
    const KMX_WCHAR sentinel[] = { 'a', 'b', 'c', UC_SENTINEL, 'd', 'e', 'f', 0 };
    EXPECT_EQ(STATUS_Success, ValidateMatchNomatchOutput((PKMX_WCHAR)sentinel));
};

// KMX_BOOL ParseLine(PFILE_KEYBOARD fk, PKMX_WCHAR str)
// KMX_BOOL ProcessGroupLine(PFILE_KEYBOARD fk, PKMX_WCHAR p)
// int kmcmp::cmpkeys(const void *key, const void *elem)
// KMX_BOOL ProcessGroupFinish(PFILE_KEYBOARD fk)
// KMX_BOOL ProcessStoreLine(PFILE_KEYBOARD fk, PKMX_WCHAR p)
// bool resizeStoreArray(PFILE_KEYBOARD fk)
// bool resizeKeyArray(PFILE_GROUP gp, int increment)
// KMX_BOOL AddStore(PFILE_KEYBOARD fk, KMX_DWORD SystemID, const KMX_WCHAR * str, KMX_DWORD *dwStoreID)
// KMX_DWORD AddDebugStore(PFILE_KEYBOARD fk, KMX_WCHAR const * str)
// KMX_BOOL ProcessSystemStore(PFILE_KEYBOARD fk, KMX_DWORD SystemID, PFILE_STORE sp)
// int GetCompileTargetsFromTargetsStore(const KMX_WCHAR* store)

TEST_F(CompilerTest, IsValidKeyboardVersion_test) {
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u""));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u" "));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"\t"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u" 1.1"));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.1"));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.0"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"1."));
    EXPECT_TRUE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.2.3"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"a"));
    EXPECT_FALSE(IsValidKeyboardVersion((KMX_WCHAR *)u"1.a"));
};

// KMX_DWORD kmcmp::AddCompilerVersionStore(PFILE_KEYBOARD fk)
// KMX_DWORD CheckStatementOffsets(PFILE_KEYBOARD fk, PFILE_GROUP gp, PKMX_WCHAR context, PKMX_WCHAR output, PKMX_WCHAR key)
// void CheckContextStatementPositions(PKMX_WCHAR context)
// KMX_DWORD CheckUseStatementsInOutput(PKMX_WCHAR output)
// KMX_DWORD CheckVirtualKeysInOutput(PKMX_WCHAR output)
// KMX_DWORD InjectContextToReadonlyOutput(PKMX_WCHAR pklOut)
// KMX_DWORD CheckOutputIsReadonly(const PFILE_KEYBOARD fk, const PKMX_WCHAR output)
// KMX_DWORD ProcessKeyLine(PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_BOOL IsUnicode)

TEST_F(CompilerTest, ProcessKeyLineImpl_test) {
    initFileGroupArray(fileKeyboard, TRUE);

    PKMX_WCHAR pklIn, pklKey, pklOut;
    KMX_WCHAR str[128];

    pklIn  = new KMX_WCHAR[GLOBAL_BUFSIZE];
    pklKey = new KMX_WCHAR[GLOBAL_BUFSIZE];
    pklOut = new KMX_WCHAR[GLOBAL_BUFSIZE];

    // #11643: non-BMP characters do not makes sense for key codes
    u16cpy(str, u"+ 'A' > 'test'\n"); // baseline
    EXPECT_EQ(STATUS_Success, ProcessKeyLineImpl(&fileKeyboard, str, TRUE, pklIn, pklKey, pklOut));

    u16cpy(str, u"+ '\U00010000' > 'test'\n"); // surrogate pair
    EXPECT_EQ(KmnCompilerMessages::ERROR_NonBMPCharactersNotSupportedInKeySection, ProcessKeyLineImpl(&fileKeyboard, str, TRUE, pklIn, pklKey, pklOut));

    delete[] pklIn;
    delete[] pklKey;
    delete[] pklOut;

    // TODO: other tests for this function

}

// KMX_DWORD ExpandKp_ReplaceIndex(PFILE_KEYBOARD fk, PFILE_KEY k, KMX_DWORD keyIndex, int nAnyIndex)
// KMX_DWORD ExpandKp(PFILE_KEYBOARD fk, PFILE_KEY kpp, KMX_DWORD storeIndex)

TEST_F(CompilerTest, GetDelimitedString_test) {
    KMX_WCHAR str[LINESIZE];
    PKMX_WCHAR p = str;
    PKMX_WCHAR q = nullptr;

    // no open delimiter, cut spaces after open and before close delimiter
    u16cpy(str, u"");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_FALSE(q);

    // no close delimiter, cut spaces after open and before close delimiter
    u16cpy(str, u"(");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_FALSE(q);

    // no argument, cut spaces after open and before close delimiter
    u16cpy(str, u"()");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(1, p-str); // deleted close delimiter

    // no argument, single space, no flags
    u16cpy(str, u"( )");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u" ", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(2, p-str); // deleted close delimiter

    // no argument, single space, cut spaces after open delimiter
    u16cpy(str, u"( )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD);
    EXPECT_EQ(0, u16cmp(u"", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(2, p-str); // deleted close delimiter

    // no argument, single space, cut spaces before close delimiter
    u16cpy(str, u"( )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(2, p-str); // deleted close delimiter

    // no argument, single space, cut spaces after open and before close delimiter
    u16cpy(str, u"( )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(2, p-str); // deleted close delimiter

    // no argument, two spaces, no flags
    u16cpy(str, u"(  )");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u"  ", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // no argument, two spaces, cut spaces after open delimiter
    u16cpy(str, u"(  )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD);
    EXPECT_EQ(0, u16cmp(u"", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // no argument, two spaces, cut spaces before close delimiter
    u16cpy(str, u"(  )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // no argument, two spaces, cut spaces after open and before close delimiter
    u16cpy(str, u"(  )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // single-character argument, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(b)");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(2, p-str); // deleted close delimiter

    // multi-character argument, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(abc)");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"abc", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(4, p-str); // deleted close delimiter

    // multi-word argument, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(abc def)");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"abc def", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(8, p-str); // deleted close delimiter

    // single-character argument, leading single space, cut spaces after open and before close delimiter, valid
    u16cpy(str, u" (b)");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // single-character argument, leading double space, cut open and close delimiter, valid
    u16cpy(str, u"  (b)");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(4, p-str); // deleted close delimiter

    // single-character argument, space before argument, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"( b)");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // single-character argument, space before argument, no flags, valid
    u16cpy(str, u"( b)");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u" b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // single-character argument, double space before argument, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(  b)");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(4, p-str); // deleted close delimiter

    // single-character argument, double space before argument, no flags, valid
    u16cpy(str, u"(  b)");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u"  b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(4, p-str); // deleted close delimiter

    // single-character argument, space after argument, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(b )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // single-character argument, space after argument, no flags, valid
    u16cpy(str, u"(b )");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u"b ", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(3, p-str); // deleted close delimiter

    // single-character argument, two spaces after argument, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(b  )");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(4, p-str); // deleted close delimiter

    // single-character argument, two spaces after argument, no flags, valid
    u16cpy(str, u"(b  )");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u"b  ", q));
    EXPECT_FALSE(*p);
    EXPECT_EQ(4, p-str); // deleted close delimiter

    // single-character argument, space after close delimiter, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(b) ");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_EQ(' ', *p);
    EXPECT_EQ(3, p-str); // space after the close delimiter

    // single-character argument, two spaces after close delimiter, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(b)  ");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_EQ(' ', *p);
    EXPECT_EQ(4, p-str); // last space after the close delimiter

    // single-character argument, two spaces after argument and two spaces after close delimiter,
    // cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(b  )  ");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_EQ(' ', *p);
    EXPECT_EQ(6, p-str); // last space after the close delimiter

    // single-character argument, two spaces after argument and two spaces after close delimiter, no flags, valid
    u16cpy(str, u"(b  )  ");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u"b  ", q));
    EXPECT_EQ(' ', *p);
    EXPECT_EQ(6, p-str); // last space after the close delimiter

    // single-character argument, two spaces and text after close delimiter, cut spaces after open and before close delimiter, valid
    u16cpy(str, u"(b)  def");
    p = str;
    q = GetDelimitedString(&p, u"()", GDS_CUTLEAD | GDS_CUTFOLL);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_EQ('d', *p);
    EXPECT_EQ(5, p-str); // first text character after the close delimiter

    // single-character argument, two spaces and text after close delimiter, no flags, valid
    u16cpy(str, u"(b)  def");
    p = str;
    q = GetDelimitedString(&p, u"()", 0x00);
    EXPECT_EQ(0, u16cmp(u"b", q));
    EXPECT_EQ('d', *p);
    EXPECT_EQ(5, p-str); // first text character after the close delimiter
}

// LinePrefixType GetLinePrefixType(PKMX_WCHAR *p)

TEST_F(CompilerTest, LineTokenType_test) {
    KMX_WCHAR str[LINESIZE];
    PKMX_WCHAR p = nullptr;

    // T_BLANK, lptOther, empty string
    u16cpy(str, u"");
    p = str;
    EXPECT_EQ(T_BLANK, LineTokenType(&p));

    // T_BLANK, lptOther, one space
    u16cpy(str, u" ");
    p = str;
    EXPECT_EQ(T_BLANK, LineTokenType(&p));

    // T_BLANK, mismatched prefix, CKF_KEYMAN, lptKeymanWebOnly
    u16cpy(str, u"$keymanweb:");
    p = str;
    kmcmp::CompileTarget = CKF_KEYMAN;
    EXPECT_EQ(T_BLANK, LineTokenType(&p));

    // T_BLANK, mismatched prefix, CKF_KEYMANWEB, lptKeymanOnly
    u16cpy(str, u"$keymanonly:");
    p = str;
    kmcmp::CompileTarget = CKF_KEYMANWEB;
    EXPECT_EQ(T_BLANK, LineTokenType(&p));

    // T_BLANK, nothing after prefix
    u16cpy(str, u"$keyman:");
    p = str;
    kmcmp::CompileTarget = CKF_KEYMAN;
    EXPECT_EQ(T_BLANK, LineTokenType(&p));

    // T_STORE (=T_W_START)
    u16cpy(str, u"store(b)");
    p = str;
    EXPECT_EQ(T_STORE, LineTokenType(&p));
    EXPECT_EQ(u16len(u"store"), p - str);
    EXPECT_TRUE(!u16cmp(p, u"(b)"));

    // T_BITMAPS (=T_W_END)
    u16cpy(str, u"bitmaps \"b\"");
    p = str;
    EXPECT_EQ(T_BITMAPS, LineTokenType(&p));
    EXPECT_EQ(u16len(u"bitmaps "), p - str);
    EXPECT_TRUE(!u16cmp(p, u"\"b\""));

    // T_STORE, preceeded by one space
    u16cpy(str, u" store(b)");
    p = str;
    EXPECT_EQ(T_STORE, LineTokenType(&p));
    EXPECT_EQ(u16len(u" store"), p - str);
    EXPECT_TRUE(!u16cmp(p, u"(b)"));

    // T_STORE, preceeded by two spaces
    u16cpy(str, u"  store(b)");
    p = str;
    EXPECT_EQ(T_STORE, LineTokenType(&p));
    EXPECT_EQ(u16len(u"  store"), p - str);
    EXPECT_TRUE(!u16cmp(p, u"(b)"));

    // T_STORE, followed by one space
    u16cpy(str, u"store (b)");
    p = str;
    EXPECT_EQ(T_STORE, LineTokenType(&p));
    EXPECT_EQ(u16len(u"store "), p - str);
    EXPECT_TRUE(!u16cmp(p, u"(b)"));

    // T_STORE, followed by two spaces
    u16cpy(str, u"store  (b)");
    p = str;
    EXPECT_EQ(T_STORE, LineTokenType(&p));
    EXPECT_EQ(u16len(u"store  "), p - str);
    EXPECT_TRUE(!u16cmp(p, u"(b)"));

    // T_COMMENT
    u16cpy(str, u"c ");
    p = str;
    EXPECT_EQ(T_COMMENT, LineTokenType(&p));
    EXPECT_EQ(0, p - str);

    // comment without following space ... potential bug, but ReadLine() currently ensures following space
    u16cpy(str, u"c");
    p = str;
    EXPECT_EQ(T_UNKNOWN, LineTokenType(&p));
    EXPECT_EQ(0, p - str);

    // T_KEYTOKEY
    u16cpy(str, u"abc");
    p = str;
    EXPECT_EQ(T_KEYTOKEY, LineTokenType(&p));
    EXPECT_EQ(0, p - str);

     // T_UNKNOWN
    u16cpy(str, u"z");
    p = str;
    EXPECT_EQ(T_UNKNOWN, LineTokenType(&p));
    EXPECT_EQ(0, p - str);
}

// KMX_BOOL StrValidChrs(PKMX_WCHAR q, KMX_WCHAR const * chrs)
// KMX_DWORD GetXString(PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_WCHAR const * token,
//  PKMX_WCHAR output, int max, int offset, PKMX_WCHAR *newp, int /*isVKey*/, int isUnicode
// )

TEST_F(CompilerTest, GetXStringImpl_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp  = nullptr;
    KMX_WCHAR token[128];

    // KmnCompilerMessages::FATAL_BufferOverflow, max=0
    EXPECT_EQ(KmnCompilerMessages::FATAL_BufferOverflow, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 0, 0, &newp, FALSE));

    // STATUS_Success, no token
    u16cpy(str, u"");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_NoTokensFound, empty
    u16cpy(str, u"");
    u16cpy(token, u"c");
    EXPECT_EQ(KmnCompilerMessages::ERROR_NoTokensFound, GetXStringImpl(tstr, &fileKeyboard, str,token, output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_NoTokensFound, whitespace
    u16cpy(str, u" ");
    u16cpy(token, u"c");
    EXPECT_EQ(KmnCompilerMessages::ERROR_NoTokensFound, GetXStringImpl(tstr, &fileKeyboard, str, token, output, 80, 0, &newp, FALSE));
}

// tests strings starting with 'x' or 'd'
TEST_F(CompilerTest, GetXStringImpl_type_xd_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;

    // hex 32-bit
    u16cpy(str, u"x10330"); // Gothic A
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_GothicA[] = { 0xD800, 0xDF30, 0 }; // see UTF32ToUTF16
    EXPECT_EQ(0, u16cmp(tstr_GothicA, tstr));

    // decimal 8-bit
    u16cpy(str, u"d18");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"\u0012", tstr));

    // hex capital 8-bit
    u16cpy(str, u"X12");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"\u0012", tstr));

    // hex 32-bit, KmnCompilerMessages::ERROR_InvalidCharacter
    u16cpy(str, u"x110000");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidCharacter, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // dk, valid
    u16cpy(str, u"dk(A)");
    EXPECT_EQ(0, (int)fileKeyboard.cxDeadKeyArray);
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_dk_valid[] = { UC_SENTINEL, CODE_DEADKEY, 1, 0 }; // setup deadkeys
    EXPECT_EQ(0, u16cmp(tstr_dk_valid, tstr));
    fileKeyboard.cxDeadKeyArray = 0;

    // deadkey, valid
    u16cpy(str, u"deadkey(A)");
    EXPECT_EQ(0, (int)fileKeyboard.cxDeadKeyArray);
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_deadkey_valid[] = { UC_SENTINEL, CODE_DEADKEY, 1, 0 }; // setup deadkeys
    EXPECT_EQ(0, u16cmp(tstr_deadkey_valid, tstr));
    fileKeyboard.cxDeadKeyArray = 0;

    // dk, KmnCompilerMessages::ERROR_InvalidDeadkey, bad character
    u16cpy(str, u"dk(%)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidDeadkey, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // dk, KmnCompilerMessages::ERROR_InvalidDeadkey, no close delimiter => NULL
    u16cpy(str, u"dk(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidDeadkey, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // dk, KmnCompilerMessages::ERROR_InvalidDeadkey, empty delimiters => empty string
    u16cpy(str, u"dk()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidDeadkey, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
}

// tests strings starting with double quote
TEST_F(CompilerTest, GetXStringImpl_type_double_quote_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;

    // valid
    u16cpy(str, u"\"abc\"");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"abc", tstr));

    // KmnCompilerMessages::ERROR_UnterminatedString
    u16cpy(str, u"\"abc");
    EXPECT_EQ(KmnCompilerMessages::ERROR_UnterminatedString, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_ExtendedStringTooLong
    u16cpy(str, u"\"abc\"");
    EXPECT_EQ(KmnCompilerMessages::ERROR_ExtendedStringTooLong, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 2, 0, &newp, FALSE)); // max reduced to force error

    // KmnCompilerMessages::ERROR_StringInVirtualKeySection *** TODO ***
}

// tests strings starting with single quote
TEST_F(CompilerTest, GetXStringImpl_type_single_quote_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;

    // valid
    u16cpy(str, u"\'abc\'");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"abc", tstr));

    // KmnCompilerMessages::ERROR_UnterminatedString
    u16cpy(str, u"\'abc");
    EXPECT_EQ(KmnCompilerMessages::ERROR_UnterminatedString, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_ExtendedStringTooLong
    u16cpy(str, u"\'abc\'");
    EXPECT_EQ(KmnCompilerMessages::ERROR_ExtendedStringTooLong, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 2, 0, &newp, FALSE)); // max reduced to force error

    // KmnCompilerMessages::ERROR_StringInVirtualKeySection *** TODO ***
}

// tests strings starting with 'a'
TEST_F(CompilerTest, GetXStringImpl_type_a_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;
    PFILE_STORE file_store = new FILE_STORE[100];
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    u16cpy(file_store[0].szName, u"a");
    u16cpy(file_store[1].szName, u"b");
    u16cpy(file_store[2].szName, u"c");

    // KmnCompilerMessages::ERROR_InvalidToken
    u16cpy(str, u"abc");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_AnyInVirtualKeySection *** TODO ***

    // KmnCompilerMessages::ERROR_InvalidAny, no close delimiter => NULL
    u16cpy(str, u"any(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidAny, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_InvalidAny, empty delimiters => empty string
    u16cpy(str, u"any()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidAny, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_InvalidAny, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    u16cpy(str, u"any( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidAny, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_StoreDoesNotExist
    u16cpy(str, u"any(d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_StoreDoesNotExist, space before store
    u16cpy(str, u"any( d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_StoreDoesNotExist, space after store
    u16cpy(str, u"any(d )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // KmnCompilerMessages::ERROR_ZeroLengthString
    u16cpy(str, u"any(b)");
    file_store[1].dpString = (PKMX_WCHAR)u"";
    EXPECT_EQ(KmnCompilerMessages::ERROR_ZeroLengthString, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // valid
    u16cpy(str, u"any(b)");
    file_store[1].dpString = (PKMX_WCHAR)u"abc"; // non-empty
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_any_valid[] = { UC_SENTINEL, CODE_ANY, 2, 0 };
    EXPECT_EQ(0, u16cmp(tstr_any_valid, tstr));

    // space before store, valid
    u16cpy(str, u"any( b)");
    file_store[1].dpString = (PKMX_WCHAR)u"abc"; // non-empty
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_any_valid, tstr));

    // space after store, valid (see #11937, #11938)
    u16cpy(str, u"any(b )");
    file_store[1].dpString = (PKMX_WCHAR)u"abc"; // non-empty
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_any_valid, tstr));
}

// tests strings starting with 'b'
TEST_F(CompilerTest, GetXStringImpl_type_b_test) {
    KMX_WCHAR tstr[128];
    fileKeyboard.version = VERSION_90;
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;

    // KmnCompilerMessages::ERROR_InvalidToken
    u16cpy(str, u"bcd");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // beep, KmnCompilerMessages::ERROR_BeepInVirtualKeySection *** TODO ***

    // beep, valid
    u16cpy(str, u"beep");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_beep_valid[] = { UC_SENTINEL, CODE_BEEP, 0 };
    EXPECT_EQ(0, u16cmp(tstr_beep_valid, tstr));

    // baselayout, KmnCompilerMessages::ERROR_90FeatureOnly_IfSystemStores
    fileKeyboard.version = VERSION_80;
    fileKeyboard.dwFlags = 0u;
    u16cpy(str, u"baselayout");
    EXPECT_EQ(KmnCompilerMessages::ERROR_90FeatureOnly_IfSystemStores, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    fileKeyboard.version = VERSION_90;

    // baselayout, KmnCompilerMessages::ERROR_InvalidInVirtualKeySection *** TODO ***

    // baselayout, no close delimiter => NULL
    fileKeyboard.version = VERSION_90;
    u16cpy(str, u"baselayout(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // baselayout, empty delimiters => empty string
    fileKeyboard.version = VERSION_90;
    u16cpy(str, u"baselayout()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // baselayout, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    fileKeyboard.version = VERSION_90;
    u16cpy(str, u"baselayout( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // baselayout, KmnCompilerMessages::ERROR_InvalidToken from process_baselayout
    fileKeyboard.version = VERSION_90;
    u16cpy(str, u"baselayout(abc)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // baselayout, valid
    fileKeyboard.version = VERSION_90;
    fileKeyboard.cxStoreArray = 0;
    fileKeyboard.dpStoreArray = nullptr;
    u16cpy(str, u"baselayout(beep)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_baselayout_valid[] = { UC_SENTINEL, CODE_IFSYSTEMSTORE, TSS_BASELAYOUT+1, 2, 1, 0 };
    EXPECT_EQ(0, u16cmp(tstr_baselayout_valid, tstr));

    // baselayout, space before argument, valid
    fileKeyboard.version = VERSION_90;
    fileKeyboard.cxStoreArray = 0;
    fileKeyboard.dpStoreArray = nullptr;
    u16cpy(str, u"baselayout( beep)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_baselayout_valid, tstr));

    // baselayout, space after argument, valid (see #11937, #11938)
    fileKeyboard.version = VERSION_90;
    fileKeyboard.cxStoreArray = 0;
    fileKeyboard.dpStoreArray = nullptr;
    u16cpy(str, u"baselayout(beep )");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_baselayout_valid, tstr));
}

// tests strings starting with 'i'
TEST_F(CompilerTest, GetXStringImpl_type_i_test) {
    KMX_WCHAR tstr[128];
    fileKeyboard.version = VERSION_80;
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;
    initFileStoreArray(fileKeyboard, {u"a", u"b", u"c"});

    // KmnCompilerMessages::ERROR_InvalidToken
    u16cpy(str, u"ijk");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, KmnCompilerMessages::ERROR_80FeatureOnly
    fileKeyboard.version = VERSION_70;
    fileKeyboard.dwFlags = 0u;
    u16cpy(str, u"if");
    EXPECT_EQ(KmnCompilerMessages::ERROR_80FeatureOnly, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    fileKeyboard.version = VERSION_80;

    // if, KmnCompilerMessages::ERROR_InvalidInVirtualKeySection *** TODO ***

    // if, no close delimiter => NULL
    fileKeyboard.version = VERSION_80;
    u16cpy(str, u"if(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIf, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, empty delimiters => empty string
    fileKeyboard.version = VERSION_80;
    fileKeyboard.dwFlags = 0u;
    u16cpy(str, u"if()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIf, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    fileKeyboard.version = VERSION_80;
    fileKeyboard.dwFlags = 0u;
    u16cpy(str, u"if( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIf, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, invalid
    fileKeyboard.version = VERSION_80;
    u16cpy(str, u"if(abc)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIf, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, KmnCompilerMessages::ERROR_90FeatureOnly_IfSystemStores
    fileKeyboard.version = VERSION_80;
    u16cpy(str, u"if(&BITMAP=)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_90FeatureOnly_IfSystemStores, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, KmnCompilerMessages::ERROR_IfSystemStore_NotFound
    fileKeyboard.version = VERSION_90;
    u16cpy(str, u"if(&abc=)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_IfSystemStore_NotFound, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, system store, equal, valid
    fileKeyboard.version = VERSION_90;
    fileKeyboard.cxStoreArray = 3u;
    u16cpy(str, u"if(&BITMAP=beep)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_if_equal_system_store_valid[] = { UC_SENTINEL, CODE_IFSYSTEMSTORE, 2, 2, 4, 0 };
    EXPECT_EQ(0, u16cmp(tstr_if_equal_system_store_valid, tstr));

    // if, system store, not equal, valid
    fileKeyboard.version = VERSION_90;
    fileKeyboard.cxStoreArray = 3u;
    u16cpy(str, u"if(&BITMAP!=beep)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_if_not_equal_system_store_valid[] = { UC_SENTINEL, CODE_IFSYSTEMSTORE, 2, 1, 4, 0 };
    EXPECT_EQ(0, u16cmp(tstr_if_not_equal_system_store_valid, tstr));

    // if, option, KmnCompilerMessages::ERROR_StoreDoesNotExist
    fileKeyboard.version = VERSION_80;
    u16cpy(str, u"if(d=beep)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // if, option, equal, valid
    fileKeyboard.version = VERSION_80;
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray[1].fIsOption = TRUE;
    u16cpy(str, u"if(b=beep)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_if_option_valid[] = { UC_SENTINEL, CODE_IFOPT, 2, 2, 4, 0 };
    EXPECT_EQ(0, u16cmp(tstr_if_option_valid, tstr));

    // if, option, equal, space before assign, valid
    fileKeyboard.version = VERSION_80;
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray[1].fIsOption = TRUE;
    u16cpy(str, u"if(b =beep)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_if_option_valid, tstr));

    // if, option, equal, space before rhs, valid
    fileKeyboard.version = VERSION_80;
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray[1].fIsOption = TRUE;
    u16cpy(str, u"if(b= beep)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_if_option_valid, tstr));

    // if, option, equal, space after rhs, valid (see #11937, #11938)
    fileKeyboard.version = VERSION_80;
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray[1].fIsOption = TRUE;
    u16cpy(str, u"if(b=beep )");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_if_option_valid, tstr));

    delete[] fileKeyboard.dpStoreArray;
    initFileStoreArray(fileKeyboard, {u"a", u"b", u"c"});
    fileKeyboard.dpStoreArray[1].fIsStore = TRUE;

    // index, KmnCompilerMessages::ERROR_InvalidInVirtualKeySection *** TODO ***

    // index, no close delimiter => NULL
    u16cpy(str, u"index(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, empty delimiters => empty string
    u16cpy(str, u"index()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    u16cpy(str, u"index( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, no comma or space
    u16cpy(str, u"index(b)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, no comma, space before store
    u16cpy(str, u"index( b)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, no comma, space after store
    u16cpy(str, u"index(b )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, KmnCompilerMessages::ERROR_StoreDoesNotExist
    u16cpy(str, u"index(d,4)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, comma, offset=0
    u16cpy(str, u"index(b,0)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, comma, negative offset
    u16cpy(str, u"index(b,-1)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, comma, valid
    u16cpy(str, u"index(b,4)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_index_valid[] = { UC_SENTINEL, CODE_INDEX, 2, 4, 0 };
    EXPECT_EQ(0, u16cmp(tstr_index_valid, tstr));

    // index, space before store, comma, valid
    u16cpy(str, u"index( b,4)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_index_valid, tstr));

    // index, space after store, comma, valid
    u16cpy(str, u"index(b ,4)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_index_valid, tstr));

    // index, comma and space, valid
    u16cpy(str, u"index(b, 4)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_index_valid, tstr));

    // index, comma, space after offset, valid
    u16cpy(str, u"index(b,4 )");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_index_valid, tstr));

    // index, space, KmnCompilerMessages::ERROR_StoreDoesNotExist (see issue #11833)
    u16cpy(str, u"index(b 4)"); // store name appears to be 'b 4'
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, two commas and extra parameter, KmnCompilerMessages::ERROR_InvalidIndex
    u16cpy(str, u"index(b,4,5)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, two-digit offset, valid
    u16cpy(str, u"index(b,42)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_index_two_digit_valid[] = { UC_SENTINEL, CODE_INDEX, 2, 42, 0 };
    EXPECT_EQ(0, u16cmp(tstr_index_two_digit_valid, tstr));

    // index, comma, non-digit offset, KmnCompilerMessages::ERROR_InvalidIndex
    u16cpy(str, u"index(b,g)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, comma, no offset, KmnCompilerMessages::ERROR_InvalidIndex
    u16cpy(str, u"index(b,)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, space and comma, no offset, KmnCompilerMessages::ERROR_InvalidIndex
    u16cpy(str, u"index(b ,)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, comma, no offset but space, KmnCompilerMessages::ERROR_InvalidIndex
    u16cpy(str, u"index(b, )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
}

// tests strings starting with 'o'
TEST_F(CompilerTest, GetXStringImpl_type_o_test) {
    KMX_WCHAR tstr[128];
    fileKeyboard.version = VERSION_80;
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;
    initFileStoreArray(fileKeyboard, {u"a", u"b", u"c"});
    fileKeyboard.dpStoreArray[1].fIsStore = TRUE;

    // KmnCompilerMessages::ERROR_InvalidToken
    u16cpy(str, u"opq");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // outs, KmnCompilerMessages::ERROR_OutsInVirtualKeySection *** TODO ***

    // outs, no close delimiter => NULL
    u16cpy(str, u"outs(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidOuts, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // outs, empty delimiters => empty string
    u16cpy(str, u"outs()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidOuts, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // outs, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    u16cpy(str, u"outs( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidOuts, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // outs, KmnCompilerMessages::ERROR_StoreDoesNotExist
    u16cpy(str, u"outs(d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // outs, KmnCompilerMessages::ERROR_StoreDoesNotExist, space before store
    u16cpy(str, u"outs( d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // outs, KmnCompilerMessages::ERROR_StoreDoesNotExist, space after store
    u16cpy(str, u"outs(d )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // outs, KmnCompilerMessages::ERROR_OutsTooLong
    PKMX_WCHAR dpString = (PKMX_WCHAR)u"abc";
    fileKeyboard.dpStoreArray[1].dpString = dpString; // length 4 => max should be > 4, otherwise a ERROR_OutsTooLong is emitted
    int max = u16len(dpString) + 1; // 4, including terminating '\0'
    u16cpy(str, u"outs(b)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_OutsTooLong, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, max, 0, &newp, FALSE)); // max reduced to force error

    // outs, valid
    fileKeyboard.dpStoreArray[1].dpString = (PKMX_WCHAR)u"abc";
    u16cpy(str, u"outs(b)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_outs_valid[] = { 'a', 'b', 'c', 0 };
    EXPECT_EQ(0, u16cmp(tstr_outs_valid, tstr));

    // outs, space before store, valid
    fileKeyboard.dpStoreArray[1].dpString = (PKMX_WCHAR)u"abc";
    u16cpy(str, u"outs( b)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_outs_valid, tstr));

    // outs, space after store, valid (see #11937, #11938)
    fileKeyboard.dpStoreArray[1].dpString = (PKMX_WCHAR)u"abc";
    u16cpy(str, u"outs(b )");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_outs_valid, tstr));
}

// tests strings starting with 'c'
TEST_F(CompilerTest, GetXStringImpl_type_c_test) {
    KMX_WCHAR tstr[128];
    fileKeyboard.version = VERSION_60;
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;
    initFileStoreArray(fileKeyboard, {u"a", u"b", u"c"});
    fileKeyboard.dpStoreArray[1].fIsCall    = TRUE;
    fileKeyboard.dpStoreArray[1].dwSystemID = TSS_NONE;

    // are comments stripped before this point?
    // if so, why the test on whitespace after 'c'?

    // KmnCompilerMessages::ERROR_InvalidToken
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"cde");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // context, KmnCompilerMessages::ERROR_ContextInVirtualKeySection *** TODO ***

    // context, no offset, valid
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_context_no_offset_valid[] = { UC_SENTINEL, CODE_CONTEXT, 0 };
    EXPECT_EQ(0, u16cmp(tstr_context_no_offset_valid, tstr));

    // context, KmnCompilerMessages::ERROR_InvalidToken, no close delimiter => NULL
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // context, empty delimiters => empty string, valid
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context()");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_context_empty_offset_valid[] = { UC_SENTINEL, CODE_CONTEXT, 0 };
    EXPECT_EQ(0, u16cmp(tstr_context_empty_offset_valid, tstr));

    // context, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context( )");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // context, offset, valid
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context(1)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_context_offset_valid[] = { UC_SENTINEL, CODE_CONTEXTEX, 1, 0 };
    EXPECT_EQ(0, u16cmp(tstr_context_offset_valid, tstr));

    // context, CERR_InvalidToke, offset < 1
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context(0)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // context, large offset < 0xF000, valid
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context(61439)"); //0xF000 - 1
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_context_large_offset_valid[] = { UC_SENTINEL, CODE_CONTEXTEX, 61439, 0 };
    EXPECT_EQ(0, u16cmp(tstr_context_large_offset_valid, tstr));

    // context, KmnCompilerMessages::ERROR_InvalidToken, too large offset == 0xF000
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context(61440)"); //0xF000
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // context, KmnCompilerMessages::ERROR_60FeatureOnly_Contextn
    fileKeyboard.version = VERSION_50;
    u16cpy(str, u"context(1)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_60FeatureOnly_Contextn, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // context, valid
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context(1)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_context_valid[] = { UC_SENTINEL, CODE_CONTEXTEX, 1, 0 };
    EXPECT_EQ(0, u16cmp(tstr_context_valid, tstr));

    // context, space before offset, valid
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context( 1)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_context_valid, tstr));

    // context, space after offset, valid (see #11937, #11938)
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"context(1 )");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_context_valid, tstr));

    // clearcontext, valid
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"clearcontext");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_clearcontext_valid[] = { UC_SENTINEL, CODE_CLEARCONTEXT, 0 };
    EXPECT_EQ(0, u16cmp(tstr_clearcontext_valid, tstr));

    // call, KmnCompilerMessages::ERROR_501FeatureOnly_Call
    fileKeyboard.version = VERSION_50;
    u16cpy(str, u"call");
    EXPECT_EQ(KmnCompilerMessages::ERROR_501FeatureOnly_Call, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, KmnCompilerMessages::ERROR_CallInVirtualKeySection *** TODO ***

    // call, no close delimiter => NULL
    fileKeyboard.version = VERSION_501;
    u16cpy(str, u"call(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidCall, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, empty delimiters => empty string
    fileKeyboard.version = VERSION_501;
    u16cpy(str, u"call()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidCall, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    fileKeyboard.version = VERSION_501;
    u16cpy(str, u"call( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidCall, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, KmnCompilerMessages::ERROR_StoreDoesNotExist
    fileKeyboard.version = VERSION_501;
    u16cpy(str, u"call(d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, KmnCompilerMessages::ERROR_StoreDoesNotExist, space before store
    fileKeyboard.version = VERSION_501;
    u16cpy(str, u"call( d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, KmnCompilerMessages::ERROR_StoreDoesNotExist, space after store
    fileKeyboard.version = VERSION_501;
    u16cpy(str, u"call(d )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, KmnCompilerMessages::ERROR_InvalidCall
    fileKeyboard.version = VERSION_501;
    fileKeyboard.dpStoreArray[1].dpString = (PKMX_WCHAR)u"*"; // cause IsValidCallStore() to fail
    u16cpy(str, u"call(b)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidCall, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // call, valid
    fileKeyboard.version = VERSION_501;
    fileKeyboard.dpStoreArray[1].dpString   = (PKMX_WCHAR)u"a.dll:A";
    fileKeyboard.dpStoreArray[1].dwSystemID = TSS_NONE;
    u16cpy(str, u"call(b)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_call_valid[] = { UC_SENTINEL, CODE_CALL, 2, 0 };
    EXPECT_EQ(0, u16cmp(tstr_call_valid, tstr));
    EXPECT_EQ(TSS_CALLDEFINITION, fileKeyboard.dpStoreArray[1].dwSystemID);

    // call, space before store, valid
    fileKeyboard.version = VERSION_501;
    fileKeyboard.dpStoreArray[1].dpString   = (PKMX_WCHAR)u"a.dll:A";
    fileKeyboard.dpStoreArray[1].dwSystemID = TSS_NONE;
    u16cpy(str, u"call( b)");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_call_valid, tstr));
    EXPECT_EQ(TSS_CALLDEFINITION, fileKeyboard.dpStoreArray[1].dwSystemID);

    // call, space after store, valid (see #11937, #11938)
    fileKeyboard.version = VERSION_501;
    fileKeyboard.dpStoreArray[1].dpString = (PKMX_WCHAR)u"a.dll:A";
    fileKeyboard.dpStoreArray[1].dwSystemID = TSS_NONE;
    u16cpy(str, u"call(b )");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_call_valid, tstr));
    EXPECT_EQ(TSS_CALLDEFINITION, fileKeyboard.dpStoreArray[1].dwSystemID);
}

// tests strings starting with 'n'
TEST_F(CompilerTest, GetXStringImpl_type_n_test) {
    KMX_WCHAR tstr[128];
    fileKeyboard.version = VERSION_70;
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;
    PFILE_STORE file_store = new FILE_STORE[100];
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    u16cpy(file_store[0].szName, u"a");
    u16cpy(file_store[1].szName, u"b");
    u16cpy(file_store[2].szName, u"c");

    // KmnCompilerMessages::ERROR_InvalidToken
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"nmo");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, KmnCompilerMessages::ERROR_60FeatureOnly_Contextn
    fileKeyboard.version = VERSION_60;
    u16cpy(str, u"notany");
    EXPECT_EQ(KmnCompilerMessages::ERROR_70FeatureOnly, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, KmnCompilerMessages::ERROR_AnyInVirtualKeySection *** TODO ***

    // notany, no close delimiter => NULL
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidAny, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, empty delimiters => empty string
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidAny, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidAny, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, KmnCompilerMessages::ERROR_StoreDoesNotExist
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany(d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, KmnCompilerMessages::ERROR_StoreDoesNotExist, space before store
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany( d)");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, KmnCompilerMessages::ERROR_StoreDoesNotExist, space after store
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany(d )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // notany, valid
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany(b)");
    file_store[1].dpString = (PKMX_WCHAR)u"abc"; // non-empty
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_notany_valid[] = { UC_SENTINEL, CODE_NOTANY, 2, 0 };
    EXPECT_EQ(0, u16cmp(tstr_notany_valid, tstr));

    // notany, valid, empy store
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany(b)");
    file_store[1].dpString = (PKMX_WCHAR)u""; // empty
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_notany_valid, tstr));

    // notany, space before store, valid
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany( b)");
    file_store[1].dpString = (PKMX_WCHAR)u"abc"; // non-empty
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_notany_valid, tstr));

    // notany, space after store, valid (see #11937, #11938)
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"notany(b )");
    file_store[1].dpString = (PKMX_WCHAR)u"abc"; // non-empty
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_notany_valid, tstr));

    // null
    fileKeyboard.version = VERSION_70;
    u16cpy(str, u"nul");
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_null_valid[] = { UC_SENTINEL, CODE_NUL, 0 };
    EXPECT_EQ(0, u16cmp(tstr_null_valid, tstr));
}

// tests strings starting with 'u'
TEST_F(CompilerTest, GetXStringImpl_type_u_test) {
    KMX_WCHAR tstr[128];
    fileKeyboard.version = VERSION_90;
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;

    // KmnCompilerMessages::ERROR_InvalidToken
    u16cpy(str, u"uvw");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidToken, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // unicode, n1 and n2, valid
    u16cpy(str, u"u+10330"); // Gothic A
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, TRUE));
    const KMX_WCHAR tstr_unicode_valid[] = { 0xD800, 0xDF30, 0 }; // see UTF32ToUTF16
    EXPECT_EQ(0, u16cmp(tstr_unicode_valid, tstr));
    EXPECT_EQ(0, msgproc_errors.size());

    // unicode, ERROR_InvalidValue
    u16cpy(str, u"u+10330z"); // Gothic A, unexpected character 'z'
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidValue, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, TRUE));

    // unicode, space after, valid
    u16cpy(str, u"u+10330 "); // Gothic A
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, TRUE));
    EXPECT_EQ(0, u16cmp(tstr_unicode_valid, tstr));
    EXPECT_EQ(0, msgproc_errors.size());

    // unicode, KmnCompilerMessages::ERROR_InvalidCharacter
    u16cpy(str, u"u+110000");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidCharacter, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, TRUE));

    // unicode, n1 only, valid
    u16cpy(str, u"u+0061"); // a
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, TRUE));
    const KMX_WCHAR tstr_unicode_n1_only_valid[] = { 0x0061, 0 }; // see UTF32ToUTF16
    EXPECT_EQ(0, u16cmp(tstr_unicode_n1_only_valid, tstr));
    EXPECT_EQ(0, msgproc_errors.size());

    // unicode, n1 and n2, valid, KmnCompilerMessages::WARN_UnicodeInANSIGroup
    u16cpy(str, u"u+10330"); // Gothic A
    EXPECT_EQ(STATUS_Success, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(tstr_unicode_valid, tstr));
    EXPECT_EQ(1, msgproc_errors.size());
    EXPECT_EQ(KmnCompilerMessages::WARN_UnicodeInANSIGroup, msgproc_errors[0].errorCode);

    // use, no delimiters => NULL
    u16cpy(str, u"use");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidUse, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // use, no close delimiter => NULL
    u16cpy(str, u"use(");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidUse, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // use, empty delimiters => empty string
    u16cpy(str, u"use()");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidUse, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // use, space in delimiters (see #11814, #11937, #11910, #11894, #11938)
    u16cpy(str, u"use( )");
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidUse, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
}

// KMX_DWORD process_baselayout(PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// KMX_DWORD process_platform(PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// KMX_DWORD process_if_synonym(KMX_DWORD dwSystemID, PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// KMX_DWORD process_if(PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// KMX_DWORD process_reset(PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// KMX_DWORD process_expansion(PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx, int max)
// KMX_DWORD process_set_synonym(KMX_DWORD dwSystemID, PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// KMX_DWORD process_set(PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// KMX_DWORD process_save(PFILE_KEYBOARD fk, PKMX_WCHAR q, PKMX_WCHAR tstr, int *mx)
// int xatoi(PKMX_WCHAR *p)
// int GetGroupNum(PFILE_KEYBOARD fk, PKMX_WCHAR p)

TEST_F(CompilerTest, ProcessEthnologueStore_test) {
    EXPECT_EQ(STATUS_Success, ProcessEthnologueStore((PKMX_WCHAR)u"abc"));
    EXPECT_EQ(KmnCompilerMessages::WARN_PunctuationInEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u";abc"));
    EXPECT_EQ(KmnCompilerMessages::WARN_PunctuationInEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u",abc"));
    EXPECT_EQ(STATUS_Success, ProcessEthnologueStore((PKMX_WCHAR)u" abc"));
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u"abc "));
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u"abcd"));
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u"ab"));
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u"a"));
    EXPECT_EQ(KmnCompilerMessages::ERROR_InvalidEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u"a2b"));
    EXPECT_EQ(STATUS_Success, ProcessEthnologueStore((PKMX_WCHAR)u"")); // needs correcting ... see #11955
    EXPECT_EQ(STATUS_Success, ProcessEthnologueStore((PKMX_WCHAR)u"abc def"));
    EXPECT_EQ(STATUS_Success, ProcessEthnologueStore((PKMX_WCHAR)u"abc  def"));
    EXPECT_EQ(KmnCompilerMessages::WARN_PunctuationInEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u"abc,def"));
    EXPECT_EQ(KmnCompilerMessages::WARN_PunctuationInEthnologueCode, ProcessEthnologueStore((PKMX_WCHAR)u"abc;def"));
}

// KMX_DWORD ProcessHotKey(PKMX_WCHAR p, KMX_DWORD *hk)
// void SetChecksum(PKMX_BYTE buf, PKMX_DWORD CheckSum, KMX_DWORD sz)
// void kmcmp::CheckStoreUsage(PFILE_KEYBOARD fk, int storeIndex, KMX_BOOL fIsStore, KMX_BOOL fIsOption, KMX_BOOL fIsCall)
// KMX_DWORD WriteCompiledKeyboard(PFILE_KEYBOARD fk, KMX_BYTE**data, size_t& dataSize)
// KMX_DWORD ReadLine(KMX_BYTE* infile, int sz, int& offset, PKMX_WCHAR wstr, KMX_BOOL PreProcess)

TEST_F(CompilerTest, GetRHS_test) {
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR tstr[128];

    // KmnCompilerMessages::ERROR_NoTokensFound, empty string
    u16cpy(str, u"");
    EXPECT_EQ(KmnCompilerMessages::ERROR_NoTokensFound, GetRHS(&fileKeyboard, str, tstr, 80, 0, FALSE));

    // KmnCompilerMessages::ERROR_NoTokensFound, no '>'
    u16cpy(str, u"abc");
    EXPECT_EQ(KmnCompilerMessages::ERROR_NoTokensFound, GetRHS(&fileKeyboard, str, tstr, 80, 0, FALSE));

    // STATUS_Success
    u16cpy(str, u"> nul c\n");
    EXPECT_EQ(STATUS_Success, GetRHS(&fileKeyboard, str, tstr, 80, 0, FALSE));
}

// void safe_wcsncpy(PKMX_WCHAR out, PKMX_WCHAR in, int cbMax)
// KMX_BOOL IsSameToken(PKMX_WCHAR *p, KMX_WCHAR const * token)
// static bool endsWith(const std::string& str, const std::string& suffix)
// KMX_DWORD ImportBitmapFile(PFILE_KEYBOARD fk, PKMX_WCHAR szName, PKMX_DWORD FileSize, PKMX_BYTE *Buf)
// int atoiW(PKMX_WCHAR p)
// KMX_DWORD kmcmp::CheckUTF16(int n)

TEST_F(CompilerTest, isIntegerWstring_test) {
    EXPECT_FALSE(isIntegerWstring(nullptr));
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u""));
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"a"));
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"-1"));
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"1"));
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u" 1"));
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"1 "));
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"42"));
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"2147483647")); // INT_MAX
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"2147483648")); // INT_MAX + 1
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"9999999999")); // > INT_MAX
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"-2147483648")); // -INT_MAX - 1
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"-2147483649")); // -INT_MAX - 2
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"-9999999999")); // < -INT_MAX - 1
}

// KMX_DWORD kmcmp::UTF32ToUTF16(int n, int *n1, int *n2)
// KMX_BOOL BuildVKDictionary(PFILE_KEYBOARD fk)
// int GetVKCode(PFILE_KEYBOARD fk, PKMX_WCHAR p)
// int GetDeadKey(PFILE_KEYBOARD fk, PKMX_WCHAR p)
// void kmcmp::RecordDeadkeyNames(PFILE_KEYBOARD fk)
// KMX_BOOL kmcmp::IsValidCallStore(PFILE_STORE fs)

TEST_F(CompilerTest, hasPreamble_test) {
    EXPECT_FALSE(hasPreamble(u""));
    EXPECT_FALSE(hasPreamble(u"\uFEFE")); // not \uFEFF
    EXPECT_TRUE(hasPreamble(u"\uFEFF"));
    EXPECT_FALSE(hasPreamble(u"a\uFEFF"));
}

// bool UTF16TempFromUTF8(KMX_BYTE* infile, int sz, KMX_BYTE** tempfile, int *sz16)
// PFILE_STORE FindSystemStore(PFILE_KEYBOARD fk, KMX_DWORD dwSystemID)
