#pragma once

#include <fstream>
#include <vector>

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#include <emscripten/bind.h>

#else
#define EMSCRIPTEN_KEEPALIVE
#endif

#ifdef __cplusplus
#define EXTERN extern "C" EMSCRIPTEN_KEEPALIVE
#else
#define EXTERN EMSCRIPTEN_KEEPALIVE
#endif

/* Compile target */

// TODO: deprecate these
#define CKF_KEYMAN    0
#define CKF_KEYMANWEB 1

struct KMCMP_COMPILER_OPTIONS {
  bool saveDebug;
  bool compilerWarningsAsErrors;
  bool warnDeprecatedCode;
  bool shouldAddCompilerVersion;
  int target;                     // CKF_KEYMAN, CKF_KEYMANWEB
};

//
// Additional metadata passed sideband from compiler result
//

#define STORETYPE_STORE       0x01
#define STORETYPE_RESERVED    0x02
#define STORETYPE_OPTION      0x04
#define STORETYPE_DEBUG       0x08
#define STORETYPE_CALL        0x10
#define STORETYPE__MASK       0x1F

struct KMCMP_COMPILER_RESULT_EXTRA_STORE {
  int storeType;    // STORETYPE__MASK
  std::string name; // when debug=false, the .kmx will not have store names
  int line;         // source line number where store is defined
};

struct KMCMP_COMPILER_RESULT_EXTRA_GROUP {
  bool isReadOnly;
  std::string name;
};

struct KMCMP_COMPILER_RESULT_MESSAGE {
  unsigned int errorCode;
  int lineNumber;
  int columnNumber;
  std::string filename;
  std::vector<std::string> parameters;
};

#define COMPILETARGETS_KMX     0x01
#define COMPILETARGETS_JS      0x02
#define COMPILETARGETS__MASK   0x03

struct KMCMP_COMPILER_RESULT_EXTRA {
  int targets; /// COMPILETARGETS__MASK = COMPILETARGETS_KMX | COMPILETARGETS_JS
  std::string kmnFilename;
  std::string kvksFilename;
  std::string displayMapFilename;
  std::vector<KMCMP_COMPILER_RESULT_EXTRA_STORE> stores;
  std::vector<KMCMP_COMPILER_RESULT_EXTRA_GROUP> groups;
};

struct KMCMP_COMPILER_RESULT {
  void* kmx;
  size_t kmxSize;
  struct KMCMP_COMPILER_RESULT_EXTRA extra;
};

/**
 * @param szText UTF-8 string
*/
typedef void (*kmcmp_CompilerMessageProc)(const KMCMP_COMPILER_RESULT_MESSAGE &message, void* context);
typedef const std::vector<uint8_t> (*kmcmp_LoadFileProc)(const std::string& loadFilename, const std::string& baseFilename);

/**
 * @param pszInfile  UTF-8 path to file.kmn
 */
EXTERN bool kmcmp_CompileKeyboard(
  const char* pszInfile,
  const KMCMP_COMPILER_OPTIONS& options,
  kmcmp_CompilerMessageProc messageProc,
  kmcmp_LoadFileProc loadFileProc,
  const void* procContext,
  KMCMP_COMPILER_RESULT& result
);

/**
 * kmcmp_parseUnicodeSet is successful if it returns >= USET_OK
 */
static const int KMCMP_USET_OK = 0;

/**
 * Error: Unknown syntax err, failed to parse
 */
static const int KMCMP_ERROR_SYNTAX_ERR = -1;
/**
 * Error: Invalid, contains strings (`{abc}` form)
 */
static const int KMCMP_ERROR_HAS_STRINGS = -2;
/**
 * Error: Invalid, uses properties \p{Mn} or [:Mn:]
 */
static const int KMCMP_ERROR_UNSUPPORTED_PROPERTY = -3;
/**
 * Fatal: output buffer too small
 */
static const int KMCMP_FATAL_OUT_OF_RANGE = -4;

/**
 * Parse a UnicodeSet into 32-bit ranges.
 * For example, "[]" will return 0 (KMCMP_USET_OK) as a zero-length set.
 * "[" will return KMCMP_ERROR_SYNTAX_ERR,
 * and "[x A-C]" will return 2 and [0x41, 0x43, 0x78, 0x78]
 * @param text input txt, null terminated, in UTF-8 format
 * @param outputBuffer output buffer, owned by caller: Pairs of ranges in order
 * @param outputBufferSize length of output buffer. Needs to be twice the number of expected ranges
 * @return If >= KMCMP_USET_OK, number of ranges, otherwise one of the negative error values.
 */
EXTERN int kmcmp_parseUnicodeSet(
  const std::string text,
  uintptr_t outputBuffer_,
  uint32_t outputBufferSize
);

