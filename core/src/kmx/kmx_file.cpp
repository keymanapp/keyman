/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "kmx_processevent.h"
#include <assert.h>
#include "kmx_file_validator.hpp"

using namespace km::core;
using namespace kmx;

#if defined(_WIN32) || defined(_WIN64)
#include <share.h>
#endif

KMX_BOOL KMX_ProcessEvent::Load(PKMX_BYTE buf, size_t sz) {
  if(!LoadKeyboardFromBlob(buf, sz, &m_keyboard.Keyboard))
    return FALSE;   // I5136

  return TRUE;
}

const int km::core::kmx::CODE__SIZE[] = {
    -1,  // undefined                0x00
    1,   // CODE_ANY                 0x01
    2,   // CODE_INDEX               0x02
    0,   // CODE_CONTEXT             0x03
    0,   // CODE_NUL                 0x04
    1,   // CODE_USE                 0x05
    0,   // CODE_RETURN              0x06
    0,   // CODE_BEEP                0x07
    1,   // CODE_DEADKEY             0x08
    -1,  // unused                   0x09
    2,   // CODE_EXTENDED            0x0A
    -1,  // CODE_EXTENDEDEND         0x0B (unused)
    1,   // CODE_SWITCH              0x0C
    -1,  // CODE_KEY                 0x0D (never used)
    0,   // CODE_CLEARCONTEXT        0x0E
    1,   // CODE_CALL                0x0F
    -1,  // UC_SENTINEL_EXTENDEDEND  0x10 (not valid with UC_SENTINEL)
    1,   // CODE_CONTEXTEX           0x11
    1,   // CODE_NOTANY              0x12
    2,   // CODE_SETOPT              0x13
    3,   // CODE_IFOPT               0x14
    1,   // CODE_SAVEOPT             0x15
    1,   // CODE_RESETOPT            0x16
    3,   // CODE_IFSYSTEMSTORE       0x17
    2    // CODE_SETSYSTEMSTORE      0x18
};

// Ensure that all CODE_### sizes are defined
static_assert(sizeof(CODE__SIZE) / sizeof(CODE__SIZE[0]) == (CODE_LASTCODE + 1), "Size of array CODE__SIZE not correct");

KMX_BOOL KMX_ProcessEvent::LoadKeyboardFromBlob(
  PKMX_BYTE original_buf,
  size_t sz,
  LPKEYBOARD* lpKeyboard
) {
  LPKEYBOARD kbp;
  PKMX_BYTE buf;
  PKMX_BYTE filebase;


  if (!lpKeyboard || !original_buf) {
    DebugLog("Invalid parameter");
    return FALSE;
  }

  *lpKeyboard = NULL;

#ifdef KMX_64BIT
  // allocate enough memory for expanded data structure + original data.
  // Expanded data structure is double the size of data on disk (8-byte
  // pointers) - on disk the "pointers" are relative to the beginning of
  // the file.
  // We save the original data at the end of buf; we don't copy strings, so
  // those will remain in the location at the end of the buffer.
  buf = new KMX_BYTE[sz * 3];
#else
  buf = new KMX_BYTE[sz];
#endif

  if (!buf) {
    DebugLog("Not allocmem");
    return FALSE;
  }
#ifdef KMX_64BIT
  filebase = buf + sz * 2;
#else
  filebase = buf;
#endif
  memcpy(filebase, original_buf, sz);

  if (*PKMX_DWORD(filebase) != KMX_DWORD(FILEID_COMPILED)) {
    DebugLog("Invalid keyboard - signature is invalid");
    delete[] buf;
    return FALSE;
  }

  if (!VerifyKeyboard(filebase, sz)) {
    DebugLog("Verify keyboard failed");
    delete[] buf;
    return FALSE;
  }

#ifdef KMX_64BIT
  kbp = CopyKeyboard(buf, filebase);
#else
  kbp = FixupKeyboard(buf, filebase);
#endif

  if (!kbp) {
    DebugLog("Can't copy/fixup keyboard");
    delete[] buf;
    return FALSE;
  }

  if (kbp->dwIdentifier != FILEID_COMPILED) {
    DebugLog("errNotFileID");
    delete[] buf;
    return FALSE;
  }

  *lpKeyboard = kbp;
  return TRUE;
}

PKMX_WCHAR KMX_ProcessEvent::StringOffset(PKMX_BYTE base, KMX_DWORD offset)
{
  if(offset == 0)
    return NULL;
  return (PKMX_WCHAR)(base + offset);
}

#ifdef KMX_64BIT
/**
  CopyKeyboard will copy the data read into bufp from x86-sized structures into
  x64-sized structures starting at `base`
  * After this function finishes, we still need to keep the original data because
    we don't copy the strings
  This method is used on 64-bit architectures.
*/
LPKEYBOARD KMX_ProcessEvent::CopyKeyboard(PKMX_BYTE bufp, PKMX_BYTE base)
{
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) base;

  /* Copy keyboard structure */

  LPKEYBOARD kbp = (LPKEYBOARD) bufp;
  bufp += sizeof(KEYBOARD);

  kbp->dwIdentifier = ckbp->dwIdentifier;
  kbp->dwFileVersion = ckbp->dwFileVersion;
  kbp->dwCheckSum = ckbp->dwCheckSum;
  kbp->xxkbdlayout = ckbp->KeyboardID;
  kbp->IsRegistered = ckbp->IsRegistered;
  kbp->version = ckbp->version;
  kbp->cxStoreArray = ckbp->cxStoreArray;
  kbp->cxGroupArray = ckbp->cxGroupArray;
  kbp->StartGroup[0] = ckbp->StartGroup[0];
  kbp->StartGroup[1] = ckbp->StartGroup[1];
  kbp->dwFlags = ckbp->dwFlags;
  kbp->dwHotKey = ckbp->dwHotKey;

  kbp->dpStoreArray = (LPSTORE) bufp;
  bufp += sizeof(STORE) * kbp->cxStoreArray;

  kbp->dpGroupArray = (LPGROUP) bufp;
  bufp += sizeof(GROUP) * kbp->cxGroupArray;

  PCOMP_STORE csp;
  LPSTORE sp;
  KMX_DWORD i;

  for(
    csp = (PCOMP_STORE)(base + ckbp->dpStoreArray), sp = kbp->dpStoreArray, i = 0;
    i < kbp->cxStoreArray;
    i++, sp++, csp++)
  {
    sp->dwSystemID = csp->dwSystemID;
    sp->dpName = StringOffset(base, csp->dpName);
    sp->dpString = StringOffset(base, csp->dpString);
  }

  PCOMP_GROUP cgp;
  LPGROUP gp;

  for(
    cgp = (PCOMP_GROUP)(base + ckbp->dpGroupArray), gp = kbp->dpGroupArray, i = 0;
    i < kbp->cxGroupArray;
    i++, gp++, cgp++)
  {
    gp->dpName = StringOffset(base, cgp->dpName);
    gp->dpKeyArray = cgp->cxKeyArray > 0 ? (LPKEY) bufp : NULL;
    gp->cxKeyArray = cgp->cxKeyArray;
    bufp += sizeof(KEY) * gp->cxKeyArray;
    gp->dpMatch = StringOffset(base, cgp->dpMatch);
    gp->dpNoMatch = StringOffset(base, cgp->dpNoMatch);
    gp->fUsingKeys = cgp->fUsingKeys;

    PCOMP_KEY ckp;
    LPKEY kp;
    KMX_DWORD j;

    for(
      ckp = (PCOMP_KEY)(base + cgp->dpKeyArray), kp = gp->dpKeyArray, j = 0;
      j < gp->cxKeyArray;
      j++, kp++, ckp++)
    {
      kp->Key = ckp->Key;
      kp->Line = ckp->Line;
      kp->ShiftFlags = ckp->ShiftFlags;
      kp->dpOutput = StringOffset(base, ckp->dpOutput);
      kp->dpContext = StringOffset(base, ckp->dpContext);
    }
  }

  return kbp;
}

#else
/**
 Fixup the keyboard by expanding pointers. On disk the pointers are stored relative to the
 beginning of the file, but we need real pointers. This method is used on 32-bit architectures.
*/
LPKEYBOARD KMX_ProcessEvent::FixupKeyboard(PKMX_BYTE bufp, PKMX_BYTE base)
{
  KMX_DWORD i, j;
  PCOMP_KEYBOARD ckbp = (PCOMP_KEYBOARD) base;
  PCOMP_GROUP cgp;
  PCOMP_STORE csp;
  PCOMP_KEY ckp;
  LPKEYBOARD kbp = (LPKEYBOARD) bufp;
  LPSTORE sp;
  LPGROUP gp;
  LPKEY kp;

  kbp->dpStoreArray = (LPSTORE) (base + ckbp->dpStoreArray);
  kbp->dpGroupArray = (LPGROUP) (base + ckbp->dpGroupArray);

  for(sp = kbp->dpStoreArray, csp = (PCOMP_STORE) sp, i = 0; i < kbp->cxStoreArray; i++, sp++, csp++)
  {
    sp->dpName = StringOffset(base, csp->dpName);
    sp->dpString = StringOffset(base, csp->dpString);
  }

  for(gp = kbp->dpGroupArray, cgp = (PCOMP_GROUP) gp, i = 0; i < kbp->cxGroupArray; i++, gp++, cgp++)
  {
    gp->dpName = StringOffset(base, cgp->dpName);
    gp->dpKeyArray = cgp->cxKeyArray > 0 ? (LPKEY) (base + cgp->dpKeyArray) : NULL;
    gp->dpMatch = StringOffset(base, cgp->dpMatch);
    gp->dpNoMatch = StringOffset(base, cgp->dpNoMatch);

    for(kp = gp->dpKeyArray, ckp = (PCOMP_KEY) kp, j = 0; j < gp->cxKeyArray; j++, kp++, ckp++)
    {
      kp->dpOutput = StringOffset(base, ckp->dpOutput);
      kp->dpContext = StringOffset(base, ckp->dpContext);
    }
  }

  return kbp;
}

#endif


KMX_BOOL KMX_ProcessEvent::VerifyKeyboard(PKMX_BYTE filebase, size_t sz)
{
  KMX_FileValidator *ckbp = reinterpret_cast<KMX_FileValidator*>(filebase);

  return ckbp->VerifyKeyboard(sz);
}


KMX_BOOL KMX_FileValidator::VerifyKeyboard(std::size_t _kmn_unused(sz)) const
{
  KMX_DWORD i;
  PCOMP_STORE csp;
  const PKMX_BYTE filebase = (KMX_BYTE*)this;

  /* Check file version */

  if(dwFileVersion < VERSION_MIN ||
     dwFileVersion > VERSION_MAX)
  {
    /* Old or new version -- identify the desired program version */
    for(csp = (PCOMP_STORE)(filebase + dpStoreArray), i = 0; i < cxStoreArray; i++, csp++) {
      if(csp->dwSystemID == TSS_COMPILEDVERSION)
      {
        if(csp->dpString == 0)
          DebugLog("errWrongFileVersion:NULL");
        else
          DebugLog("errWrongFileVersion:%10.10ls", KMX_ProcessEvent::StringOffset(filebase, csp->dpString));
        return FALSE;
      }
    }
    DebugLog("errWrongFileVersion");
    return FALSE;
  }

  // Verify file structure

  if(StartGroup[0] != 0xFFFFFFFF && StartGroup[0] >= cxGroupArray) {
    DebugLog("Invalid ANSI start group index");
    return FALSE;
  }
  if(StartGroup[1] != 0xFFFFFFFF && StartGroup[1] >= cxGroupArray) {
    DebugLog("Invalid Unicode start group index");
    return FALSE;
  }

  // TODO: verify many other offsets such as stores, groups, keys, strings!

  return TRUE;
}
