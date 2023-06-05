#include "pch.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "CheckNCapsConsistency.h"
#include "DeprecationChecks.h"
#include "versioning.h"
#include "CompileKeyboardBuffer.h"
#include "../../../../common/windows/cpp/include/keymanversion.h"

bool CompileKeyboardBuffer(KMX_BYTE* infile, int sz, PFILE_KEYBOARD fk)
{
  PKMX_WCHAR str, p;

  KMX_DWORD msg;

  kmcmp::FMnemonicLayout = FALSE;

  if (!fk) {
    AddCompileError(CERR_SomewhereIGotItWrong);
    return FALSE;
  }

  str = new KMX_WCHAR[LINESIZE];
  if (!str) {
    AddCompileError(CERR_CannotAllocateMemory);
    return FALSE;
  }

  fk->KeyboardID = 0;
  fk->version = 0;
  fk->dpStoreArray = NULL;
  fk->dpGroupArray = NULL;
  fk->cxStoreArray = 0;
  fk->cxGroupArray = 0;
  fk->StartGroup[0] = fk->StartGroup[1] = -1;
  fk->szName[0] = 0;
  fk->szCopyright[0] = 0;
  fk->dwFlags = KF_AUTOMATICVERSION;
  fk->currentGroup = 0xFFFFFFFF;
  fk->currentStore = 0;
  fk->cxDeadKeyArray = 0;
  fk->dpDeadKeyArray = NULL;
  fk->cxVKDictionary = 0;  // I3438
  fk->dpVKDictionary = NULL;  // I3438
  fk->extra->kvksFilename = u"";
/*	fk->szMessage[0] = 0;
  fk->szLanguageName[0] = 0;*/
  fk->dwBitmapSize = 0;
  fk->dwHotKey = 0;

  kmcmp::BeginLine[BEGIN_ANSI] = -1;
  kmcmp::BeginLine[BEGIN_UNICODE] = -1;
  kmcmp::BeginLine[BEGIN_NEWCONTEXT] = -1;
  kmcmp::BeginLine[BEGIN_POSTKEYSTROKE] = -1;


  /* Add a store for the Keyman 6.0 copyright information string */

  if(kmcmp::FShouldAddCompilerVersion) {
    u16sprintf(str,LINESIZE, L"Created with Keyman Developer version %d.%d.%d.%d", KEYMAN_VersionMajor, KEYMAN_VersionMinor, KEYMAN_VersionPatch, 0);
    AddStore(fk, TSS_KEYMANCOPYRIGHT, str);
  }

  /* Add a system store for the Keyman edition number */
  AddStore(fk, TSS_CUSTOMKEYMANEDITION, u"0");
  AddStore(fk, TSS_CUSTOMKEYMANEDITIONNAME, u"Keyman");

  int offset = 0;

  // must preprocess for group and store names -> this isn't really necessary, but never mind!
  while ((msg = ReadLine(infile, sz, offset, str, TRUE)) == CERR_None)
  {
    p = str;
    switch (LineTokenType(&p))
    {
      case T_VERSION:
        *(p + 4) = 0;
        if ((msg = AddStore(fk, TSS_VERSION, p)) != CERR_None) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      case T_GROUP:
        if ((msg = ProcessGroupLine(fk, p)) != CERR_None) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      case T_STORE:
        if ((msg = ProcessStoreLine(fk, p)) != CERR_None) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      default:
        break;
    }
  }

  if (msg != CERR_EndOfFile) {
    AddCompileError(msg);
    return FALSE;
  }

  offset = 0;
  kmcmp::currentLine = 0;

  /* Reindex the list of codeconstants after stores added */

  kmcmp::CodeConstants->reindex();

  /* ReadLine will automatically skip over $Keyman lines, and parse wrapped lines */
  while ((msg = ReadLine(infile, sz, offset, str, FALSE)) == CERR_None)
  {
    msg = ParseLine(fk, str);
    if (msg != CERR_None) {
      AddCompileError(msg);
      return FALSE;
    }
  }

  if (msg != CERR_EndOfFile) {
    AddCompileError(msg);
    return FALSE;
  }

  ProcessGroupFinish(fk);

  if (kmcmp::FSaveDebug) kmcmp::RecordDeadkeyNames(fk);

  /* Add the compiler version as a system store */
  if ((msg = kmcmp::AddCompilerVersionStore(fk)) != CERR_None) {
    AddCompileError(msg);
    return FALSE;
  }

  if ((msg = BuildVKDictionary(fk)) != CERR_None) {
    AddCompileError(msg);
    return FALSE;
  }

  delete str;

  if (!kmcmp::CheckKeyboardFinalVersion(fk)) {
    return FALSE;
  }

  /* Warn on inconsistent use of NCAPS */
  if (!kmcmp::FMnemonicLayout) {
    CheckNCapsConsistency(fk);
  }

  /* Flag presence of deprecated features */
  kmcmp::CheckForDeprecatedFeatures(fk);

  return TRUE;
}
