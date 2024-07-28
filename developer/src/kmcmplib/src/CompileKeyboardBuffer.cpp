#include "pch.h"
#include <kmn_compiler_errors.h>
#include "kmcmplib.h"
#include "CheckNCapsConsistency.h"
#include "DeprecationChecks.h"
#include "versioning.h"
#include "CompileKeyboardBuffer.h"
#include "../../../../common/windows/cpp/include/keymanversion.h"

namespace kmcmp {
  void CopyExtraData(PFILE_KEYBOARD fk);
};

bool CompileKeyboardBuffer(KMX_BYTE* infile, int sz, PFILE_KEYBOARD fk)
{
  PKMX_WCHAR str, p;

  KMX_DWORD msg;

  kmcmp::FMnemonicLayout = FALSE;

  if (!fk) {
    AddCompileError(KmnCompilerMessages::FATAL_SomewhereIGotItWrong);
    return FALSE;
  }

  str = new KMX_WCHAR[LINESIZE];
  if (!str) {
    AddCompileError(KmnCompilerMessages::FATAL_CannotAllocateMemory);
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
  fk->extra->targets = COMPILETARGETS_KMX;
  fk->extra->kvksFilename = "";
  fk->extra->displayMapFilename = "";
  fk->extra->stores.clear();
  fk->extra->groups.clear();
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
  while ((msg = ReadLine(infile, sz, offset, str, TRUE)) == STATUS_Success)
  {
    p = str;
    switch (LineTokenType(&p))
    {
      case T_VERSION:
        *(p + 4) = 0;
        if ((msg = AddStore(fk, TSS_VERSION, p)) != STATUS_Success) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      case T_GROUP:
        if ((msg = ProcessGroupLine(fk, p)) != STATUS_Success) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      case T_STORE:
        if ((msg = ProcessStoreLine(fk, p)) != STATUS_Success) {
          AddCompileError(msg);
          return FALSE;
        }
        break;

      default:
        break;
    }
  }

  if (msg != STATUS_EndOfFile) {
    AddCompileError(msg);
    return FALSE;
  }

  offset = 0;
  kmcmp::currentLine = 0;

  /* Reindex the list of codeconstants after stores added */

  kmcmp::CodeConstants->reindex();

  /* ReadLine will automatically skip over $Keyman lines, and parse wrapped lines */
  while ((msg = ReadLine(infile, sz, offset, str, FALSE)) == STATUS_Success)
  {
    msg = ParseLine(fk, str);
    if (msg != STATUS_Success) {
      AddCompileError(msg);
      return FALSE;
    }
  }

  if (msg != STATUS_EndOfFile) {
    AddCompileError(msg);
    return FALSE;
  }

  ProcessGroupFinish(fk);

  if (kmcmp::FSaveDebug) kmcmp::RecordDeadkeyNames(fk);

  /* Add the compiler version as a system store */
  if ((msg = kmcmp::AddCompilerVersionStore(fk)) != STATUS_Success) {
    AddCompileError(msg);
    return FALSE;
  }

  if ((msg = BuildVKDictionary(fk)) != STATUS_Success) {
    AddCompileError(msg);
    return FALSE;
  }

  delete[] str;

  if (!kmcmp::CheckKeyboardFinalVersion(fk)) {
    return FALSE;
  }

  /* Warn on inconsistent use of NCAPS */
  if (!kmcmp::FMnemonicLayout) {
    CheckNCapsConsistency(fk);
  }

  /* Flag presence of deprecated features */
  kmcmp::CheckForDeprecatedFeatures(fk);

  /* Extract extra metadata for callers */
  kmcmp::CopyExtraData(fk);

  return TRUE;
}

namespace kmcmp {
  void CopyExtraData(PFILE_KEYBOARD fk) {
    /* Copy stores */
    PFILE_STORE store = fk->dpStoreArray;
    for(KMX_DWORD i = 0; i < fk->cxStoreArray; i++, store++) {
      KMCMP_COMPILER_RESULT_EXTRA_STORE extraStore;
      extraStore.storeType =
        (store->fIsStore ? STORETYPE_STORE : 0) |
        (store->fIsReserved ? STORETYPE_RESERVED : 0) |
        (store->fIsOption ? STORETYPE_OPTION : 0) |
        (store->fIsDebug ? STORETYPE_DEBUG : 0) |
        (store->fIsCall ? STORETYPE_CALL : 0);
      extraStore.name = string_from_u16string(store->szName);
      extraStore.line = store->line;
      fk->extra->stores.push_back(extraStore);
    }

    PFILE_GROUP group = fk->dpGroupArray;
    for(KMX_DWORD i = 0; i < fk->cxGroupArray; i++, group++) {
      KMCMP_COMPILER_RESULT_EXTRA_GROUP extraGroup;
      extraGroup.isReadOnly = group->fReadOnly;
      extraGroup.name = string_from_u16string(group->szName);
      fk->extra->groups.push_back(extraGroup);
    }
  }
}
