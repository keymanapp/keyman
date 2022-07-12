{******************************************************************************}
{                                                                              }
{ Security API interface Unit for Object Pascal                                }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: security.h, released June 2000. The original Pascal    }
{ code is: Security.pas, released December 2000. The initial developer of the  }
{ Pascal code is Marcel van Brakel (brakelm att chello dott nl).               }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

// $Id: JwaSecurity.pas,v 1.7 2007/09/14 06:48:47 marquardt Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSecurity;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "security.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

// This file will go out and pull in all the header files that you need,
// based on defines that you issue.  The following macros are used.
//
// SECURITY_KERNEL      Use the kernel interface, not the usermode
//

//
// These are name that can be used to refer to the builtin packages
//

const
  NTLMSP_NAME_A = 'NTLM';
  {$EXTERNALSYM NTLMSP_NAME_A}
  NTLMSP_NAME = WideString('NTLM');
  {$EXTERNALSYM NTLMSP_NAME}

  {$IFNDEF JWA_INCLUDEMODE}
  MICROSOFT_KERBEROS_NAME_A = 'Kerberos';
  {$EXTERNALSYM MICROSOFT_KERBEROS_NAME_A}
  MICROSOFT_KERBEROS_NAME_W = WideString('Kerberos');
  {$EXTERNALSYM MICROSOFT_KERBEROS_NAME_W}

  // (rom) WIN32_CHICAGO replaced by UNICODE
  {$IFDEF UNICODE}
  MICROSOFT_KERBEROS_NAME = MICROSOFT_KERBEROS_NAME_W;
  {$EXTERNALSYM MICROSOFT_KERBEROS_NAME}
  {$ELSE}
  MICROSOFT_KERBEROS_NAME = MICROSOFT_KERBEROS_NAME_A;
  {$EXTERNALSYM MICROSOFT_KERBEROS_NAME}
  {$ENDIF UNICODE}
  {$ENDIF JWA_INCLUDEMODE}  

  NEGOSSP_NAME_W = WideString('Negotiate');
  {$EXTERNALSYM NEGOSSP_NAME_W}
  NEGOSSP_NAME_A = 'Negotiate';
  {$EXTERNALSYM NEGOSSP_NAME_A}

  {$IFDEF UNICODE}
  NEGOSSP_NAME = NEGOSSP_NAME_W;
  {$EXTERNALSYM NEGOSSP_NAME}
  {$ELSE}
  NEGOSSP_NAME = NEGOSSP_NAME_A;
  {$EXTERNALSYM NEGOSSP_NAME}
  {$ENDIF UNICODE}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}
//your implementation here
{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
