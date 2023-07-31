
#include "pch.h"

#include <compfile.h>
#include <kmn_compiler_errors.h>
#include <kmcmplib.h>
#include "CharToKeyConversion.h"

/**
 * If any rule uses CAPS or NCAPS for a given key, then every rule that
 * uses that key must also use CAPS or NCAPS, as otherwise the results are
 * inconsistent. For example, if Caps Lock is on, then [K_F] may still be
 * matched:
 *
 *   + [K_F] > 'foo'
 *   + [CAPS K_F] > 'bar'
 *
 * Because of the way that KeymanWeb compiles rules, this may even apply when
 * we have a key rule that would otherwise be ignored due to context. In the
 * following example, [Caps Lock] + [y] would result in no output (or default
 * output) rather than 'bar', because the preceding rule would capture the
 * Y key, ignoring Caps Lock, meaning that the subsequent rule would never
 * even get tested. (Note that this was introduced in the KeymanWeb compiler
 * fix for extremely long if/else ladders in Keyman 12 in #1561, and this is
 * technically slightly inconsistent with Keyman Core, although in my analysis
 * only in this already ambiguous situation.
 *
 *   'x' + [K_Y] > 'foo'
 *   [CAPS K_Y] > 'bar'
 *
 * Given all this, we'll warn any time we find a key that has inconsistent use
 * of CAPS/NCAPS in its rules.
 *
 * @param fk     Keyboard to check
 */
bool CheckNCapsConsistency(PFILE_KEYBOARD fk) {
  struct CapsUsage {
    int ncaps_line, caps_line, neither_line;
  };

  const int oldCurrentLine = kmcmp::currentLine;

  // 256 virtual key codes + sizeof the virtual key dictionary is max key code possible
  const int nkeys = 256 + fk->cxVKDictionary;
  auto caps_ncaps_usage = new CapsUsage [nkeys];

  memset(caps_ncaps_usage, 0, nkeys * sizeof(CapsUsage));

  PFILE_GROUP gp;
  KMX_DWORD gn;
  for (gn = 0, gp = fk->dpGroupArray; gn < fk->cxGroupArray; gn++, gp++) {
    if (!gp->fUsingKeys) {
      continue;
    }

    PFILE_KEY kp;
    KMX_DWORD kn;
    for (kn = 0, kp = gp->dpKeyArray; kn < gp->cxKeyArray; kn++, kp++) {
      KMX_UINT key;
      KMX_UINT shift;
      if (kp->ShiftFlags & ISVIRTUALKEY) {
        if (kp->Key >= nkeys) {
          assert(false);
          continue;
        }
        key = kp->Key;
        shift = kp->ShiftFlags;
      }
      else if (!kmcmp::MapUSCharToVK(kp->Key, &key, &shift)) {
        // Not a valid key
        continue;
      }

      if (shift & NOTCAPITALFLAG) {
        if (!caps_ncaps_usage[key].ncaps_line) caps_ncaps_usage[key].ncaps_line = (kp->Line == 0 ? 1 : kp->Line);
      }
      else if (shift & CAPITALFLAG) {
        if (!caps_ncaps_usage[key].caps_line) caps_ncaps_usage[key].caps_line = (kp->Line == 0 ? 1 : kp->Line);
      }
      else {
        if (!caps_ncaps_usage[key].neither_line) caps_ncaps_usage[key].neither_line = (kp->Line == 0 ? 1 : kp->Line);
      }
    }
  }

  for (int i = 0; i < nkeys; i++) {
    if (caps_ncaps_usage[i].neither_line && (caps_ncaps_usage[i].caps_line || caps_ncaps_usage[i].ncaps_line)) {
      // We set the current line to one needing work: the developer should add the NCAPS flag
      kmcmp::currentLine = caps_ncaps_usage[i].neither_line;
      AddWarningBool(CWARN_KeyShouldIncludeNCaps);
    }
  }

  delete[] caps_ncaps_usage;

  kmcmp::currentLine = oldCurrentLine;

  return TRUE;
}
