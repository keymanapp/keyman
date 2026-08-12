/*
  Copyright:        Copyright (C) 2003-2018 SIL International.
  Authors:          mcdurdin
*/
#include "kmx_processevent.h"

using namespace km::core;
using namespace kmx;

#define MAX_RSHIFT 24
#define MAX_KSHIFT 18

static KMX_DWORD legalKeyStates[MAX_KSHIFT] = {
  0, LCTRLFLAG, RCTRLFLAG, LALTFLAG, LALTFLAG|LCTRLFLAG, LALTFLAG|RCTRLFLAG, RALTFLAG, RALTFLAG|LCTRLFLAG, RALTFLAG|RCTRLFLAG,
  K_SHIFTFLAG, K_SHIFTFLAG|LCTRLFLAG, K_SHIFTFLAG|RCTRLFLAG, K_SHIFTFLAG|LALTFLAG, K_SHIFTFLAG|LALTFLAG|LCTRLFLAG, K_SHIFTFLAG|LALTFLAG|RCTRLFLAG, K_SHIFTFLAG|RALTFLAG, K_SHIFTFLAG|RALTFLAG|LCTRLFLAG, K_SHIFTFLAG|RALTFLAG|RCTRLFLAG
};

static KMX_DWORD legalRuleStates[MAX_RSHIFT] = {
  0, LCTRLFLAG, RCTRLFLAG, LALTFLAG, LALTFLAG|LCTRLFLAG, LALTFLAG|RCTRLFLAG, RALTFLAG, RALTFLAG|LCTRLFLAG, RALTFLAG|RCTRLFLAG,
  K_SHIFTFLAG, K_SHIFTFLAG|LCTRLFLAG, K_SHIFTFLAG|RCTRLFLAG, K_SHIFTFLAG|LALTFLAG, K_SHIFTFLAG|LALTFLAG|LCTRLFLAG, K_SHIFTFLAG|LALTFLAG|RCTRLFLAG, K_SHIFTFLAG|RALTFLAG, K_SHIFTFLAG|RALTFLAG|LCTRLFLAG, K_SHIFTFLAG|RALTFLAG|RCTRLFLAG,
  K_CTRLFLAG, K_ALTFLAG, K_CTRLFLAG|K_ALTFLAG, K_SHIFTFLAG|K_CTRLFLAG, K_SHIFTFLAG|K_ALTFLAG, K_SHIFTFLAG|K_CTRLFLAG|K_ALTFLAG
};

// Simulate RALT -> LCtrl+RAlt -> columns marked with *
//
// Truth table: matching key states
// 1 = yes match always
// 2 = RAlt mapped as LCtrl+RAlt by keyboard driver
// 4 = Simulate RAlt with Ctrl+Alt
//
// x = key states, y = rule states
// CAPITAL LETTERS = RIGHT KEY, LOWER CASE LETTERS = LEFT KEY
static KMX_BYTE states[MAX_RSHIFT][MAX_KSHIFT] = {
  // Simulate RAlt ==>             *                             *
  //          0  c  C  a ac aC  A Ac AC  s sc sC sa sac saC sA sAc sAC    <-- Key states
  /* 0   */ { 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* c   */ { 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* C   */ { 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* a   */ { 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* ac  */ { 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* aC  */ { 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* A   */ { 0, 0, 0, 0, 4, 4, 1, 6, 4, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* Ac  */ { 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },   // Note: RALT will also trigger LCtrl+RAlt because we cannot distinguish reliably through TSF???
  /* AC  */ { 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* s   */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* sc  */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,  0,  0, 0,  0,  0 },
  /* sC  */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,  0,  0, 0,  0,  0 },
  /* sa  */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  0,  0, 0,  0,  0 },
  /* sac */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  0, 0,  0,  0 },
  /* saC */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  1, 0,  0,  0 },
  /* sA  */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  4,  4, 1,  6,  4 },
  /* sAc */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  1,  0 },
  /* sAC */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  1 },

  /* Either Ctrl/Alt rules */

  //          0  c  C  a ac aC  A Ac AC  s sc sC sa sac saC sA sAc sAC    <-- Key states
  /* C   */ { 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* A   */ { 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* CA  */ { 0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0, 0,  0,  0, 0,  0,  0 },
  /* SC  */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,  0,  0, 0,  0,  0 },
  /* SA  */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,  0,  0, 1,  0,  0 },
  /* SCA */ { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,  1,  1, 0,  1,  1 }

  /* ^ rule states */
};


/*
* KMX_BOOL IsEquivalentShift( KMX_UINT rshift, KMX_UINT kshift );
*
* Parameters: rshift  Rule shift state flag set to compare.
*       kshift  Current shift state flag set to compare.
*
* Returns:  TRUE if the shift states are equivalent.
*
* IsEquivalentShift will compare rshift and kshift and check the generic
* K_CTRLFLAG and K_ALTFLAG as well as specific keys correctly.
*/

KMX_BOOL KMX_ProcessEvent::IsEquivalentShift(KMX_UINT rshift, KMX_UINT kshift) {
  //
  // The rule shift must have ISVIRTUALKEY bit set for virt.keys
  //

  if(rshift == 0) return FALSE;

  //
  // Test CAPS-specific rules
  //

  if( (rshift & NOTCAPITALFLAG) && (kshift & CAPITALFLAG) ) return FALSE;
  if( (rshift & CAPITALFLAG) && !(kshift & CAPITALFLAG) ) return FALSE;

  //
  // Ignore CAPS, NUM and SCROLL
  //

  rshift &= K_MODIFIERFLAG;
  kshift &= K_MODIFIERFLAG;

  //
  // Find rule state index and key state index
  // If not found, it's not a supported combination, e.g. LCTRL|RCTRL
  // TODO: unwind these loops if you're feeling optimistic
  //

  int i;

  for(i = 0; i < MAX_RSHIFT; i++) {
    if(rshift == legalRuleStates[i]) {
      rshift = i;
      break;
    }
  }
  if(i == MAX_RSHIFT) return FALSE;

  for(i = 0; i < MAX_KSHIFT; i++) {
    if(kshift == legalKeyStates[i]) {
      kshift = i;
      break;
    }
  }
  if(i == MAX_KSHIFT) return FALSE;

  //
  // Time of truth: is it a valid state?
  //

  switch(states[rshift][kshift]) {
    case 0: return FALSE;
    case 1: return TRUE;
    case 2: return m_environment.baseLayoutGivesCtrlRAltForRAlt();  // This state is used when TSF gives us a bogus RALT instead of LCtrl+RAlt
    case 4: return m_environment.simulateAltGr();   // I4583
    case 6: return m_environment.baseLayoutGivesCtrlRAltForRAlt() || m_environment.simulateAltGr();   // I4583
  }

  return FALSE; // should never happen
}


