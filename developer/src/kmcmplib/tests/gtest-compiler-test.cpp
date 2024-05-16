#include <gtest/gtest.h>
#include "..\include\kmcompx.h"
#include "..\include\kmcmplibapi.h"
#include "..\src\kmx_u16.h"
#include "..\src\compfile.h"
#include "..\src\CompMsg.h"
#include "..\..\..\..\common\include\km_types.h"
#include "..\..\..\..\common\include\kmx_file.h"
#include "..\..\..\..\common\include\kmn_compiler_errors.h"

PKMX_WCHAR strtowstr(PKMX_STR in);
PKMX_STR wstrtostr(PKMX_WCHAR in);
KMX_BOOL AddCompileError(KMX_DWORD msg);
KMX_DWORD ValidateMatchNomatchOutput(PKMX_WCHAR p);
KMX_BOOL IsValidKeyboardVersion(KMX_WCHAR *dpString);
bool hasPreamble(std::u16string result);

extern kmcmp_CompilerMessageProc msgproc;

namespace kmcmp {
    extern int nErrors;
    extern int ErrChr;
}

#define ERR_EXTRA_LIB_LEN 256
extern char ErrExtraLIB[ERR_EXTRA_LIB_LEN];

class CompilerTest : public testing::Test {
    protected:
    	CompilerTest() {}
	    ~CompilerTest() override {}
	    void SetUp() override {}
	    void TearDown() override {
            msgproc = NULL;
            szText_stub[0] = '\0';
            kmcmp::nErrors = 0;
            kmcmp::ErrChr = 0;
            ErrExtraLIB[0] = '\0';
        }

    public:
        static KMX_CHAR szText_stub[];

        static int msgproc_stub(int line, uint32_t dwMsgCode, const char* szText, void* context) {
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

// KMX_BOOL kmcmp::AddCompileWarning(PKMX_CHAR buf)

TEST_F(CompilerTest, AddCompileError_test) {
    msgproc = msgproc_stub;
    kmcmp::ErrChr = 0;
    ErrExtraLIB[0] = '\0';
    KMX_CHAR expected[COMPILE_ERROR_MAX_LEN];

    // CERR_FATAL
    EXPECT_EQ(0, kmcmp::nErrors);
    EXPECT_EQ(CERR_FATAL, CERR_CannotCreateTempfile & CERR_FATAL);
    EXPECT_TRUE(AddCompileError(CERR_CannotCreateTempfile));
    strcpy(expected, GetCompilerErrorString(CERR_CannotCreateTempfile));
    EXPECT_EQ(0, strcmp(expected, szText_stub));
    EXPECT_EQ(1, kmcmp::nErrors);

    // CERR_ERROR
    EXPECT_EQ(CERR_ERROR, CERR_InvalidLayoutLine & CERR_ERROR);
    EXPECT_FALSE(AddCompileError(CERR_InvalidLayoutLine));
    strcpy(expected, GetCompilerErrorString(CERR_InvalidLayoutLine));
    EXPECT_EQ(0, strcmp(expected, szText_stub));
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
    strcpy(expected, GetCompilerErrorString(CERR_InvalidLayoutLine));
    EXPECT_EQ(0, strcmp(expected, szText_stub));
    EXPECT_EQ(6, kmcmp::nErrors);
};

// KMX_DWORD ProcessBeginLine(PFILE_KEYBOARD fk, PKMX_WCHAR p)

TEST_F(CompilerTest, ValidateMatchNomatchOutput_test) {
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput(NULL));
    EXPECT_EQ(CERR_None, ValidateMatchNomatchOutput((PKMX_WCHAR)u""));
    const KMX_WCHAR context[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXT, 'd', 'e', 'f' };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)context));
    const KMX_WCHAR contextex[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_CONTEXTEX, 'd', 'e', 'f' };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)contextex));
    const KMX_WCHAR index[] = { 'a', 'b', 'c', UC_SENTINEL, CODE_INDEX, 'd', 'e', 'f' };
    EXPECT_EQ(CERR_ContextAndIndexInvalidInMatchNomatch, ValidateMatchNomatchOutput((PKMX_WCHAR)index));
    const KMX_WCHAR sentinel[] = { 'a', 'b', 'c', UC_SENTINEL, 'd', 'e', 'f' };
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
// KMX_DWORD GetXStringImpl(PKMX_WCHAR tstr, PFILE_KEYBOARD fk, PKMX_WCHAR str, KMX_WCHAR const * token,
//  PKMX_WCHAR output, int max, int offset, PKMX_WCHAR *newp, int isUnicode
// )
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
// KMX_DWORD GetRHS(PFILE_KEYBOARD fk, PKMX_WCHAR p, PKMX_WCHAR buf, int bufsize, int offset, int IsUnicode)
// void safe_wcsncpy(PKMX_WCHAR out, PKMX_WCHAR in, int cbMax)
// KMX_BOOL IsSameToken(PKMX_WCHAR *p, KMX_WCHAR const * token)
// static bool endsWith(const std::string& str, const std::string& suffix)
// KMX_DWORD ImportBitmapFile(PFILE_KEYBOARD fk, PKMX_WCHAR szName, PKMX_DWORD FileSize, PKMX_BYTE *Buf)
// int atoiW(PKMX_WCHAR p)
// KMX_DWORD kmcmp::CheckUTF16(int n)
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
