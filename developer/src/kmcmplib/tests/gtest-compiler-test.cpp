#include <gtest/gtest.h>
#include "../include/kmcompx.h"
#include "../include/kmcmplibapi.h"
#include "../src/kmx_u16.h"
#include "../src/compfile.h"
#include "../src/CompMsg.h"
#include "../../common/include/kmn_compiler_errors.h"
#include "../../../../common/include/km_types.h"
#include "../../../../common/include/kmx_file.h"

PKMX_WCHAR strtowstr(PKMX_STR in);
PKMX_STR wstrtostr(PKMX_WCHAR in);
KMX_BOOL AddCompileError(KMX_DWORD msg);
KMX_DWORD ProcessBeginLine(PFILE_KEYBOARD fk, PKMX_WCHAR p);
KMX_DWORD ValidateMatchNomatchOutput(PKMX_WCHAR p);
KMX_BOOL IsValidKeyboardVersion(KMX_WCHAR *dpString);
KMX_DWORD GetXStringImpl(PKMX_WCHAR tstr, PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_WCHAR const * token,
  PKMX_WCHAR output, int max, int offset, PKMX_WCHAR *newp, int isUnicode
);
KMX_DWORD GetRHS(PFILE_KEYBOARD fk, PKMX_WCHAR p, PKMX_WCHAR buf, int bufsize, int offset, int IsUnicode);
bool isIntegerWstring(PKMX_WCHAR p);
bool hasPreamble(std::u16string result);

extern kmcmp_CompilerMessageProc msgproc;

namespace kmcmp {
    extern int nErrors;
    extern int ErrChr;
    extern int BeginLine[4];
    KMX_BOOL AddCompileWarning(char* buf);
}

#define ERR_EXTRA_LIB_LEN 256
extern char ErrExtraLIB[ERR_EXTRA_LIB_LEN];

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
            msgproc = NULL;
            szText_stub[0] = '\0';
            kmcmp::nErrors = 0;
            kmcmp::ErrChr = 0;
            ErrExtraLIB[0] = '\0';
            kmcmp::BeginLine[BEGIN_ANSI] = -1;
            kmcmp::BeginLine[BEGIN_UNICODE] = -1;
            kmcmp::BeginLine[BEGIN_NEWCONTEXT] = -1;
            kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = -1;
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

        void deleteFileKeyboard(FILE_KEYBOARD &fk) {
            if (fk.dpStoreArray)   { delete[] fk.dpStoreArray;   }
            if (fk.dpGroupArray)   { delete[] fk.dpGroupArray;   }
            if (fk.lpBitmap)       { delete   fk.lpBitmap;       }
            if (fk.dpDeadKeyArray) { delete[] fk.dpDeadKeyArray; }
            if (fk.dpVKDictionary) { delete   fk.dpVKDictionary; }
            if (fk.extra)          { delete   fk.extra;          }
        }

    public:
        static KMX_CHAR szText_stub[];

        static int msgproc_true_stub(int line, uint32_t dwMsgCode, const char* szText, void* context) {
            strcpy(szText_stub, szText);
            return 1;
        };

        static int msgproc_false_stub(int line, uint32_t dwMsgCode, const char* szText, void* context) {
            strcpy(szText_stub, szText);
            return 0;
        };
};

#define COMPILE_ERROR_MAX_LEN (SZMAX_ERRORTEXT + 1 + 280)
KMX_CHAR CompilerTest::szText_stub[COMPILE_ERROR_MAX_LEN];

TEST_F(CompilerTest, strtowstr_test) {
    EXPECT_EQ(0, u16cmp(u"hello", strtowstr((PKMX_STR)"hello")));
    EXPECT_EQ(0, u16cmp(u"", strtowstr((PKMX_STR)"")));
};

TEST_F(CompilerTest, wstrtostr_test) {
    EXPECT_EQ(0, strcmp("hello", wstrtostr((PKMX_WCHAR)u"hello")));
    EXPECT_EQ(0, strcmp("", wstrtostr((PKMX_WCHAR)u"")));
};

TEST_F(CompilerTest, AddCompileWarning_test) {
    msgproc = msgproc_false_stub;
    const char *const WARNING_TEXT = "warning";
    EXPECT_EQ(0, kmcmp::nErrors);
    EXPECT_FALSE(kmcmp::AddCompileWarning((PKMX_CHAR)WARNING_TEXT));
    EXPECT_EQ(0, strcmp(WARNING_TEXT, szText_stub));
    EXPECT_EQ(0, kmcmp::nErrors);
};

TEST_F(CompilerTest, AddCompileError_test) {
    msgproc = msgproc_true_stub;
    kmcmp::ErrChr = 0;
    ErrExtraLIB[0] = '\0';
    KMX_CHAR expected[COMPILE_ERROR_MAX_LEN];

    // CERR_FATAL
    EXPECT_EQ(0, kmcmp::nErrors);
    EXPECT_EQ(CERR_FATAL, CERR_CannotCreateTempfile & CERR_FATAL);
    EXPECT_TRUE(AddCompileError(CERR_CannotCreateTempfile));
    EXPECT_EQ(0, strcmp(GetCompilerErrorString(CERR_CannotCreateTempfile), szText_stub));
    EXPECT_EQ(1, kmcmp::nErrors);

    // CERR_ERROR
    EXPECT_EQ(CERR_ERROR, CERR_InvalidLayoutLine & CERR_ERROR);
    EXPECT_FALSE(AddCompileError(CERR_InvalidLayoutLine));
    EXPECT_EQ(0, strcmp(GetCompilerErrorString(CERR_InvalidLayoutLine), szText_stub));
    EXPECT_EQ(2, kmcmp::nErrors);

    // Unknown
    const KMX_DWORD UNKNOWN_ERROR = 0x00004FFF; // top of range ERROR
    EXPECT_EQ(CERR_ERROR, UNKNOWN_ERROR & CERR_ERROR);
    EXPECT_FALSE(AddCompileError(UNKNOWN_ERROR));
    sprintf(expected, "Unknown error %x", UNKNOWN_ERROR);
    EXPECT_EQ(0, strcmp(expected, szText_stub));
    EXPECT_EQ(3, kmcmp::nErrors);

    // ErrChr
    const int ERROR_CHAR_INDEX = 42;
    kmcmp::ErrChr = ERROR_CHAR_INDEX ;
    EXPECT_EQ(CERR_ERROR, CERR_InvalidLayoutLine & CERR_ERROR);
    EXPECT_FALSE(AddCompileError(CERR_InvalidLayoutLine));
    sprintf(expected, "%s character offset: %d", GetCompilerErrorString(CERR_InvalidLayoutLine), ERROR_CHAR_INDEX);
    EXPECT_EQ(0, strcmp(expected, szText_stub));
    kmcmp::ErrChr = 0;
    EXPECT_EQ(4, kmcmp::nErrors);

    // ErrExtraLIB
    const char *const EXTRA_LIB_TEXT = " extra lib";
    strcpy(ErrExtraLIB, EXTRA_LIB_TEXT);
    EXPECT_EQ(CERR_ERROR, CERR_InvalidLayoutLine & CERR_ERROR);
    EXPECT_FALSE(AddCompileError(CERR_InvalidLayoutLine));
    sprintf(expected, "%s%s", GetCompilerErrorString(CERR_InvalidLayoutLine), EXTRA_LIB_TEXT);
    EXPECT_EQ(0, strcmp(expected, szText_stub));
    ErrExtraLIB[0] = '\0';
    EXPECT_EQ(5, kmcmp::nErrors);

    // msgproc returns FALSE
    msgproc = msgproc_false_stub;
    EXPECT_EQ(CERR_ERROR, CERR_InvalidLayoutLine & CERR_ERROR);
    EXPECT_TRUE(AddCompileError(CERR_InvalidLayoutLine));
    EXPECT_EQ(0, strcmp(GetCompilerErrorString(CERR_InvalidLayoutLine), szText_stub));
    EXPECT_EQ(6, kmcmp::nErrors);
};

TEST_F(CompilerTest, ProcessBeginLine_test) {
    KMX_WCHAR str[LINESIZE];

    // CERR_NoTokensFound
    str[0] = '\0';
    EXPECT_EQ(CERR_NoTokensFound, ProcessBeginLine(&fileKeyboard, str));

    // CERR_InvalidToken
    u16cpy(str, u"abc >");
    EXPECT_EQ(CERR_InvalidToken, ProcessBeginLine(&fileKeyboard, str));

    // CERR_RepeatedBegin, BEGIN_UNICODE
    kmcmp::BeginLine[BEGIN_UNICODE] = 0; // not -1
    u16cpy(str, u" unicode>");
    EXPECT_EQ(CERR_RepeatedBegin, ProcessBeginLine(&fileKeyboard, str));
    kmcmp::BeginLine[BEGIN_UNICODE] = -1;

    // CERR_RepeatedBegin, BEGIN_ANSI
    kmcmp::BeginLine[BEGIN_ANSI] = 0; // not -1
    u16cpy(str, u" ansi>");
    EXPECT_EQ(CERR_RepeatedBegin, ProcessBeginLine(&fileKeyboard, str));
    kmcmp::BeginLine[BEGIN_ANSI] = -1;

    // CERR_RepeatedBegin, BEGIN_NEWCONTEXT
    kmcmp::BeginLine[BEGIN_NEWCONTEXT] = 0; // not -1
    u16cpy(str, u" newContext>");
    EXPECT_EQ(CERR_RepeatedBegin, ProcessBeginLine(&fileKeyboard, str));
    kmcmp::BeginLine[BEGIN_NEWCONTEXT] = -1;

    // CERR_RepeatedBegin, BEGIN_POSTKEYSTROKE
    kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = 0; // not -1
    u16cpy(str, u" postKeystroke>");
    EXPECT_EQ(CERR_RepeatedBegin, ProcessBeginLine(&fileKeyboard, str));
    kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = -1;
};

TEST_F(CompilerTest, ValidateMatchNomatchOutput_test) {
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput(NULL));
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput((PKMX_WCHAR)u""));
    const KMX_WCHAR context[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXT, 'd', 'e', 'f', 0 };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)context));
    const KMX_WCHAR contextex[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXTEX, 'd', 'e', 'f', 0 };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)contextex));
    const KMX_WCHAR index[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_INDEX, 'd', 'e', 'f', 0 };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)index));
    const KMX_WCHAR sentinel[] = { 'a', 'b', 'c', UC_SENTINEL, 'd', 'e', 'f', 0 };
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput((PKMX_WCHAR)sentinel));
};

// KMX_DWORD ParseLine(PFILE_KEYBOARD fk, PKMX_WCHAR str)
// KMX_DWORD ProcessGroupLine(PFILE_KEYBOARD fk, PKMX_WCHAR p)
// int kmcmp::cmpkeys(const void *key, const void *elem)
// KMX_DWORD ProcessGroupFinish(PFILE_KEYBOARD fk)
// KMX_DWORD ProcessStoreLine(PFILE_KEYBOARD fk, PKMX_WCHAR p)
// bool resizeStoreArray(PFILE_KEYBOARD fk)
// bool resizeKeyArray(PFILE_GROUP gp, int increment)
// KMX_DWORD AddStore(PFILE_KEYBOARD fk, KMX_DWORD SystemID, const KMX_WCHAR * str, KMX_DWORD *dwStoreID)
// KMX_DWORD AddDebugStore(PFILE_KEYBOARD fk, KMX_WCHAR const * str)
// KMX_DWORD ProcessSystemStore(PFILE_KEYBOARD fk, KMX_DWORD SystemID, PFILE_STORE sp)
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
// KMX_BOOL CheckContextStatementPositions(PKMX_WCHAR context)
// KMX_DWORD CheckUseStatementsInOutput(PKMX_WCHAR output)
// KMX_DWORD CheckVirtualKeysInOutput(PKMX_WCHAR output)
// KMX_DWORD InjectContextToReadonlyOutput(PKMX_WCHAR pklOut)
// KMX_DWORD CheckOutputIsReadonly(const PFILE_KEYBOARD fk, const PKMX_WCHAR output)
// KMX_DWORD ProcessKeyLine(PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_BOOL IsUnicode)
// KMX_DWORD ProcessKeyLineImpl(PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_BOOL IsUnicode, PKMX_WCHAR pklIn, PKMX_WCHAR pklKey, PKMX_WCHAR pklOut)
// KMX_DWORD ExpandKp_ReplaceIndex(PFILE_KEYBOARD fk, PFILE_KEY k, KMX_DWORD keyIndex, int nAnyIndex)
// KMX_DWORD ExpandKp(PFILE_KEYBOARD fk, PFILE_KEY kpp, KMX_DWORD storeIndex)
// PKMX_WCHAR GetDelimitedString(PKMX_WCHAR *p, KMX_WCHAR const * Delimiters, KMX_WORD Flags)
// LinePrefixType GetLinePrefixType(PKMX_WCHAR *p)
// int LineTokenType(PKMX_WCHAR *str)
// KMX_BOOL StrValidChrs(PKMX_WCHAR q, KMX_WCHAR const * chrs)
// KMX_DWORD GetXString(PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_WCHAR const * token,
//  PKMX_WCHAR output, int max, int offset, PKMX_WCHAR *newp, int /*isVKey*/, int isUnicode
// )

TEST_F(CompilerTest, GetXStringImpl_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = NULL;

    // CERR_BufferOverflow, max=0
    EXPECT_EQ(CERR_BufferOverflow, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 0, 0, &newp, FALSE));

    // CERR_None, no token
    str[0] = '\0';
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // CERR_NoTokensFound, empty
    u16cpy(str, u"");
    EXPECT_EQ(CERR_NoTokensFound, GetXStringImpl(tstr, &fileKeyboard, str, u"c", output, 80, 0, &newp, FALSE));

    // CERR_NoTokensFound, whitespace
    u16cpy(str, u" ");
    EXPECT_EQ(CERR_NoTokensFound, GetXStringImpl(tstr, &fileKeyboard, str, u"c", output, 80, 0, &newp, FALSE));
}

// tests strings starting with 'x' or 'd'
TEST_F(CompilerTest, GetXStringImpl_type_xd_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = NULL;

    // hex 32-bit
    u16cpy(str, u"x10330"); // Gothic A
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_GothicA[] = { 0xD800, 0xDF30, 0 }; // see UTF32ToUTF16
    EXPECT_EQ(0, u16cmp(tstr_GothicA, tstr));

    // decimal 8-bit
    u16cpy(str, u"d18");
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"\u0012", tstr));

    // hex capital 8-bit
    u16cpy(str, u"X12");
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"\u0012", tstr));

    // hex 32-bit, CERR_InvalidCharacter
    u16cpy(str, u"x110000");
    EXPECT_EQ(CERR_InvalidCharacter, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // dk, valid
    u16cpy(str, u"dk(A)");
    EXPECT_EQ(0, (int)fileKeyboard.cxDeadKeyArray);
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_dk_valid[] = { UC_SENTINEL, CODE_DEADKEY, 1, 0 }; // setup deadkeys
    EXPECT_EQ(0, u16cmp(tstr_dk_valid, tstr));
    fileKeyboard.cxDeadKeyArray = 0;

    // deadkey, valid
    u16cpy(str, u"deadkey(A)");
    EXPECT_EQ(0, (int)fileKeyboard.cxDeadKeyArray);
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_deadkey_valid[] = { UC_SENTINEL, CODE_DEADKEY, 1, 0 }; // setup deadkeys
    EXPECT_EQ(0, u16cmp(tstr_deadkey_valid, tstr));
    fileKeyboard.cxDeadKeyArray = 0;

    // dk, CERR_InvalidDeadkey, bad character
    u16cpy(str, u"dk(%)");
    EXPECT_EQ(CERR_InvalidDeadkey, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // dk, CERR_InvalidDeadkey, no close delimiter => NULL
    u16cpy(str, u"dk(");
    EXPECT_EQ(CERR_InvalidDeadkey, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // dk, CERR_InvalidDeadkey, empty delimiters => empty string
    u16cpy(str, u"dk()");
    EXPECT_EQ(CERR_InvalidDeadkey, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
}

// tests strings starting with double quote
TEST_F(CompilerTest, GetXStringImpl_type_double_quote_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = NULL;

    // valid
    u16cpy(str, u"\"abc\"");
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"abc", tstr));

    // CERR_UnterminatedString
    u16cpy(str, u"\"abc");
    EXPECT_EQ(CERR_UnterminatedString, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // CERR_ExtendedStringTooLong
    u16cpy(str, u"\"abc\"");
    EXPECT_EQ(CERR_ExtendedStringTooLong, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 2, 0, &newp, FALSE)); // max reduced to force error

    // CERR_StringInVirtualKeySection *** TODO ***
}

// tests strings starting with single quote
TEST_F(CompilerTest, GetXStringImpl_type_single_quote_test) {
    KMX_WCHAR tstr[128];
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = NULL;

    // valid
    u16cpy(str, u"\'abc\'");
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    EXPECT_EQ(0, u16cmp(u"abc", tstr));

    // CERR_UnterminatedString
    u16cpy(str, u"\'abc");
    EXPECT_EQ(CERR_UnterminatedString, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // CERR_ExtendedStringTooLong
    u16cpy(str, u"\'abc\'");
    EXPECT_EQ(CERR_ExtendedStringTooLong, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 2, 0, &newp, FALSE)); // max reduced to force error

    // CERR_StringInVirtualKeySection *** TODO ***
}

// tests strings starting with 'i'
TEST_F(CompilerTest, GetXStringImpl_type_i_test) {
    KMX_WCHAR tstr[128];
    fileKeyboard.version = VERSION_80;
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR output[GLOBAL_BUFSIZE];
    PKMX_WCHAR newp = nullptr;
    PFILE_STORE file_store = new FILE_STORE[100];
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    file_store[1].fIsStore = TRUE;
    u16cpy(file_store[0].szName, u"a");
    u16cpy(file_store[1].szName, u"b");
    u16cpy(file_store[2].szName, u"c");

    // index, CERR_InvalidInVirtualKeySection *** TODO ***

    // index, no close delimiter => NULL
    u16cpy(str, u"index(");
    EXPECT_EQ(CERR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, empty delimiters => empty string
    u16cpy(str, u"index()");
    EXPECT_EQ(CERR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, no comma or space
    u16cpy(str, u"index(b)");
    EXPECT_EQ(CERR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, CERR_StoreDoesNotExist
    u16cpy(str, u"index(d 4)");
    EXPECT_EQ(CERR_StoreDoesNotExist, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, comma, valid
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    u16cpy(str, u"index(b,4)");
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_index_comma_valid[] = { UC_SENTINEL, CODE_INDEX, 2, 4, 0 };
    EXPECT_EQ(0, u16cmp(tstr_index_comma_valid, tstr));

    // index, space, valid
    u16cpy(str, u"index(b 4)");
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_index_space_valid[] = { UC_SENTINEL, CODE_INDEX, 2, 4, 0 };
    EXPECT_EQ(0, u16cmp(tstr_index_space_valid, tstr));

    // index, two-digit parameter, valid
    u16cpy(str, u"index(b,42)");
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    EXPECT_EQ(CERR_None, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
    const KMX_WCHAR tstr_index_two_digit_valid[] = { UC_SENTINEL, CODE_INDEX, 2, 42, 0 };
    EXPECT_EQ(0, u16cmp(tstr_index_two_digit_valid, tstr));

    // index, comma, non-digit parameter, CERR_InvalidIndex
    u16cpy(str, u"index(b,g)");
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    EXPECT_EQ(CERR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));

    // index, comma, no parameter, CERR_InvalidIndex
    u16cpy(str, u"index(b,)");
    fileKeyboard.cxStoreArray = 3u;
    fileKeyboard.dpStoreArray = file_store;
    EXPECT_EQ(CERR_InvalidIndex, GetXStringImpl(tstr, &fileKeyboard, str, u"", output, 80, 0, &newp, FALSE));
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
// KMX_DWORD ProcessEthnologueStore(PKMX_WCHAR p)
// KMX_DWORD ProcessHotKey(PKMX_WCHAR p, KMX_DWORD *hk)
// void SetChecksum(PKMX_BYTE buf, PKMX_DWORD CheckSum, KMX_DWORD sz)
// KMX_BOOL kmcmp::CheckStoreUsage(PFILE_KEYBOARD fk, int storeIndex, KMX_BOOL fIsStore, KMX_BOOL fIsOption, KMX_BOOL fIsCall)
// KMX_DWORD WriteCompiledKeyboard(PFILE_KEYBOARD fk, KMX_BYTE**data, size_t& dataSize)
// KMX_DWORD ReadLine(KMX_BYTE* infile, int sz, int& offset, PKMX_WCHAR wstr, KMX_BOOL PreProcess)

TEST_F(CompilerTest, GetRHS_test) {
    KMX_WCHAR str[LINESIZE];
    KMX_WCHAR tstr[128];

    // CERR_NoTokensFound, empty string
    str[0] = '\0';
    EXPECT_EQ(CERR_NoTokensFound, GetRHS(&fileKeyboard, str, tstr, 80, 0, FALSE));

    // CERR_NoTokensFound, no '>'
    u16cpy(str, u"abc");
    EXPECT_EQ(CERR_NoTokensFound, GetRHS(&fileKeyboard, str, tstr, 80, 0, FALSE));

    // CERR_None
    u16cpy(str, u"> nul c\n");
    EXPECT_EQ(CERR_None, GetRHS(&fileKeyboard, str, tstr, 80, 0, FALSE));
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
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"42"));
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"2147483647")); // INT_MAX
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"2147483648")); // INT_MAX + 1
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"9999999999")); // > INT_MAX
    EXPECT_TRUE(isIntegerWstring((PKMX_WCHAR)u"-2147483648")); // -INT_MAX - 1
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"-2147483649")); // -INT_MAX - 2
    EXPECT_FALSE(isIntegerWstring((PKMX_WCHAR)u"-9999999999")); // < -INT_MAX - 1
}

// KMX_DWORD kmcmp::UTF32ToUTF16(int n, int *n1, int *n2)
// KMX_DWORD BuildVKDictionary(PFILE_KEYBOARD fk)
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
