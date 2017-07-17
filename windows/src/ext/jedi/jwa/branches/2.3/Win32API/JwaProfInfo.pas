{******************************************************************************}
{                                                                              }
{ Profile Info Structures API interface Unit for Object Pascal                 }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: profinfo.h, released June 2000. The original Pascal    }
{ code is: ProfInfo.pas, released December 2000. The initial developer of the  }
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

// $Id: JwaProfInfo.pas,v 1.8 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaProfInfo;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "profinfo.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

type
  LPPROFILEINFOA = ^PROFILEINFOA;
  {$EXTERNALSYM LPPROFILEINFOA}
  _PROFILEINFOA = record
    dwSize: DWORD;         // Set to sizeof(PROFILEINFO) before calling
    dwFlags: DWORD;        // See flags above
    lpUserName: LPSTR;     // User name (required)
    lpProfilePath: LPSTR;  // Roaming profile path (optional, can be NULL)
    lpDefaultPath: LPSTR;  // Default user profile path (optional, can be NULL)
    lpServerName: LPSTR;   // Validating domain controller name in netbios format (optional, can be NULL but group NT4 style policy won't be applied)
    lpPolicyPath: LPSTR;   // Path to the NT4 style policy file (optional, can be NULL)
    hProfile: HANDLE;      // Filled in by the function.  Registry key handle open to the root.
  end;
  {$EXTERNALSYM _PROFILEINFOA}
  PROFILEINFOA = _PROFILEINFOA;
  {$EXTERNALSYM PROFILEINFOA}
  TProfileInfoA = PROFILEINFOA;
  PProfileInfoA = LPPROFILEINFOA;

  LPPROFILEINFOW = ^PROFILEINFOW;
  {$EXTERNALSYM LPPROFILEINFOW}
  _PROFILEINFOW = record
    dwSize: DWORD;         // Set to sizeof(PROFILEINFO) before calling
    dwFlags: DWORD;        // See flags above
    lpUserName: LPWSTR;    // User name (required)
    lpProfilePath: LPWSTR; // Roaming profile path (optional, can be NULL)
    lpDefaultPath: LPWSTR; // Default user profile path (optional, can be NULL)
    lpServerName: LPWSTR;  // Validating domain controller name in netbios format (optional, can be NULL but group NT4 style policy won't be applied)
    lpPolicyPath: LPWSTR;  // Path to the NT4 style policy file (optional, can be NULL)
    hProfile: HANDLE;      // Filled in by the function.  Registry key handle open to the root.
  end;
  {$EXTERNALSYM _PROFILEINFOW}
  PROFILEINFOW = _PROFILEINFOW;
  {$EXTERNALSYM PROFILEINFOW}
  TProfileInfoW = PROFILEINFOW;
  PProfileInfoW = LPPROFILEINFOW;

  {$IFDEF UNICODE}
  PROFILEINFO = PROFILEINFOW;
  {$EXTERNALSYM PROFILEINFO}
  LPPROFILEINFO = LPPROFILEINFOW;
  {$EXTERNALSYM LPPROFILEINFO}
  TProfileInfo = TProfileInfoW;
  PProfileInfo = PProfileInfoW;
  {$ELSE}
  PROFILEINFO = PROFILEINFOA;
  {$EXTERNALSYM PROFILEINFO}
  LPPROFILEINFO = LPPROFILEINFOA;
  {$EXTERNALSYM LPPROFILEINFO}
  TProfileInfo = TProfileInfoA;
  PProfileInfo = PProfileInfoA;
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
