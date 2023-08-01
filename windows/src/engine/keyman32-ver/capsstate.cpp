/*
  Name:             capsstate
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 Dec 2006

  Modified Date:    31 Dec 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 Dec 2006 - mcdurdin - Fix CAPS ON ONLY / CAPS ALWAYS OFF
                    22 Jan 2007 - mcdurdin - Fix CAPS ALWAYS OFF
                    11 Mar 2009 - mcdurdin - I1894 - Fix threading bugs introduced in I1888
                    11 Dec 2009 - mcdurdin - I934 - x64 - Initial version
                    12 Mar 2010 - mcdurdin - I934 - x64 - Complete
                    12 Mar 2010 - mcdurdin - I2229 - Remove hints and warnings
                    04 May 2010 - mcdurdin - I2351 - Robustness - verify _td return value
                    23 Mar 2012 - mcdurdin - I3284 - Fix blocking of Ctrl+Shift passthrough to other hooks
                    04 Nov 2012 - mcdurdin - I3529 - V9.0 - Merge of I3284 - Fix blocking of Ctrl+Shift passthrough to other hooks
                    31 Dec 2014 - mcdurdin - I4548 - V9.0 - When Alt is down, release of Ctrl, Shift is not detectable within TIP in some languages
*/
#include "pch.h"

BOOL IsCapsLockOn(void) {
  return GetKeyState(VK_CAPITAL) & 1;
}
