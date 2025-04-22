/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Mnemonic layout support for mac
 */

#ifndef MCOMPILE_H
#define MCOMPILE_H
#include <vector>
#include "keymap.h"
#include "mc_kmxfile.h"
#include <map>
#include "mc_import_rules.h"

struct KMX_DeadkeyMapping {  // I4353
  KMX_WCHAR deadkey, dkid;
  KMX_DWORD shift;
  KMX_WORD vk;
};

extern std::vector<KMX_DeadkeyMapping> KMX_FDeadkeys;  // I4353

/** @brief  print (error) messages */
void mac_KMX_LogError(const wchar_t* fmt, ...);

#endif /*MCOMPILE_H*/
