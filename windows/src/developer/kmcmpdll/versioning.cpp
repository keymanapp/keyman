
#include <string.h>
#include <ctype.h>
#include <windows.h>

// Keyman includes

#include <keyman32.h>

#include <compfile.h>
#include <compiler.h>
#include <comperr.h>
#include <vkeys.h>


//TVersion FVersionInfo;

/* How to determine the minimum file version:

  1. Minimum file version is always 0x0500
  2. When first "call" statement is referenced, min file version moves to 0x0501
  3. If keyboard contains VERSION 5.1, then min file version moves to 0x0510

  Ensuring that features are not used if not available in VERSION x.xx statement:

  -- all older features available in VERSION 5.0 or earlier
  -- call available in VERSION 5.01 or later
  -- context(n), LHS context, LHS index, vkeys and deadkeys in stores,
     named code constants, mnemonic vs positional settings, ethnologue codes,
	 "old character position" system store, all only available in VERSION 5.1


  -- 0x0500:		Keyman 5.0.100.0
  -- 0x0501:		Keyman 5.0.112.0
  -- 0x0510:		Keyman 5.1.1xx.0
*/

void SetMinFileVersion(int version)
{
	FVersionInfo.MinVersion = version;
}

void UpdateKeyboardVersion(PWSTR p)
{
}
