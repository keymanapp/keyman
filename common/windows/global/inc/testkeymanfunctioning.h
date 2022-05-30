/*
  Name:             testkeymanfunctioning
  Copyright:        Copyright (C) SIL International.
  Documentation:    gl1
  Description:      Functionality to test whether Keyman hooks are functioning correctly
  Create Date:      14 Jul 2005

  Modified Date:    16 May 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Jul 2005 - mcdurdin - Add return codes for KeymanInitEx
                    16 May 2007 - mcdurdin - I819
*/

#ifndef _TESTKEYMANFUNCTIONING_H
#define _TESTKEYMANFUNCTIONING_H

#define TKF_PING		0	// Posted for getmessage hook to generate TKF_RESPONSE
#define TKF_RESPONSE	1	// Received from getmessage

#define ERROR_KEYMAN_ALREADYRUNNING		0x20000001L
#define ERROR_KEYMAN_PROGRAM_CORRUPT	0x20000002L
#define ERROR_KEYMAN_UNABLE_TO_INIT		0x20000003L
#define ERROR_KEYMAN_CANNOT_SET_HOOKS	0x20000004L

#endif
