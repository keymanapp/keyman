(*
  Name:             kmxfileconsts
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      27 Mar 2008

  Modified Date:    27 Aug 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          27 Mar 2008 - mcdurdin - Initial version (Refactored)
                    25 May 2009 - mcdurdin - Use names for functions rather than codes in KMW compiler error messages
                    17 Aug 2012 - mcdurdin - I3429 - V9.0 - Add support for if, set, reset, save to KeymanWeb compiler
                    17 Aug 2012 - mcdurdin - I3430 - V9.0 - Add support for if(&platform) and if(&baselayout) to compilers
                    27 Aug 2012 - mcdurdin - I3437 - V9.0 - Add support for set(&layer) and layer()
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys
*)
unit kmxfileconsts;

interface

const
  SZMAX_STORENAME	= 80;
  SZMAX_GROUPNAME = 80;
  SZMAX_DEADKEYNAME = 80;
  SZMAX_VKDICTIONARYNAME = 80;  // I3438
  SZMAX_ERRORTEXT = 512;

  SZMAX_LANGUAGENAME = 80;
  SZMAX_KEYBOARDNAME = 80;
  SZMAX_COPYRIGHT = 256;
  SZMAX_MESSAGE = 1024;

  BEGIN_ANSI = 0;
  BEGIN_UNICODE = 1;

  UC_SENTINEL = $FFFF;
  UC_SENTINEL_EXTENDEDEND	= $0010; // was ((CODE_LASTCODE)+1)... what was I thinking?

  CODE_FIRSTCODE = $01;

  CODE_ANY			= $01;
  CODE_INDEX			= $02;
  CODE_CONTEXT		= $03;
  CODE_NUL			= $04;
  CODE_USE			= $05;
  CODE_RETURN			= $06;
  CODE_BEEP			= $07;
  CODE_DEADKEY		= $08;
//	= $09 = bkspace.-- we don't need to keep this separate though with UC_SENTINEL
  CODE_EXTENDED		= $0A;
//#define CODE_EXTENDEDEND	= $0B  deprecated
  CODE_SWITCH			= $0C;
  CODE_KEY			= $0D;
  CODE_CLEARCONTEXT	= $0E;
  CODE_CALL		 =	$0F;
// UC_SENTINEL_EXTENDEDEND  0x10
  CODE_CONTEXTEX = $11;

  CODE_NOTANY   = $12;

  // Keyman 8
  CODE_SETOPT   = $13;  // I3429
  CODE_IFOPT    = $14;  // I3429
  CODE_SAVEOPT  = $15;  // I3429
  CODE_RESETOPT = $16;  // I3429

  // Keyman 9
  CODE_IFSYSTEMSTORE = $17;  // I3430
  CODE_SETSYSTEMSTORE = $18;  // I3437
  CODE_LASTCODE = $18;

const
  KMXCodeNames: array[CODE_FIRSTCODE..CODE_LASTCODE] of string = (
    'any', 'index', 'context', 'nul', 'use', 'return', 'beep', 'deadkey',
    '',
    'extended', '', 'switch', 'key', 'clearcontext', 'call',
    '', 'contextex', 'notany',
    'set', 'if', 'save', 'reset',  // I3429
    'if(&system)', 'set(&system)');  // I3430    // I3437
    
implementation

end.
