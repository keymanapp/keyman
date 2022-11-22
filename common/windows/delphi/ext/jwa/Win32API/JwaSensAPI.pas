{******************************************************************************}
{                                                                              }
{ System Event Notification Services API interface Unit for Object Pascal      }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: sensapi.h, released March 2003. The original Pascal    }
{ code is: SensAPI.pas, released April 2003. The initial developer of the      }
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

// $Id: JwaSensAPI.pas,v 1.10 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSensAPI;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "SensAPI.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  NETWORK_ALIVE_LAN  = $00000001;
  {$EXTERNALSYM NETWORK_ALIVE_LAN}
  NETWORK_ALIVE_WAN  = $00000002;
  {$EXTERNALSYM NETWORK_ALIVE_WAN}
  NETWORK_ALIVE_AOL  = $00000004;
  {$EXTERNALSYM NETWORK_ALIVE_AOL}

type
  tagQOCINFO = record
    dwSize: DWORD;
    dwFlags: DWORD;
    dwInSpeed: DWORD;
    dwOutSpeed: DWORD;
  end;
  {$EXTERNALSYM tagQOCINFO}
  QOCINFO = tagQOCINFO;
  {$EXTERNALSYM QOCINFO}
  LPQOCINFO = ^QOCINFO;
  {$EXTERNALSYM LPQOCINFO}
  TQocInfo = QOCINFO;
  PQocInfo = LPQOCINFO;

function IsDestinationReachableA(lpszDestination: LPCSTR; lpQOCInfo: LPQOCINFO): BOOL; stdcall;
{$EXTERNALSYM IsDestinationReachableA}
function IsDestinationReachableW(lpszDestination: LPCWSTR; lpQOCInfo: LPQOCINFO): BOOL; stdcall;
{$EXTERNALSYM IsDestinationReachableW}
function IsDestinationReachable(lpszDestination: LPCTSTR; lpQOCInfo: LPQOCINFO): BOOL; stdcall;
{$EXTERNALSYM IsDestinationReachable}

function IsNetworkAlive(out lpdwFlags: DWORD): BOOL; stdcall;
{$EXTERNALSYM IsNetworkAlive}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  sensapilib = 'sensapi.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _IsDestinationReachableA: Pointer;

function IsDestinationReachableA;
begin
  GetProcedureAddress(_IsDestinationReachableA, sensapilib, 'IsDestinationReachableA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsDestinationReachableA]
  end;
end;

var
  _IsDestinationReachableW: Pointer;

function IsDestinationReachableW;
begin
  GetProcedureAddress(_IsDestinationReachableW, sensapilib, 'IsDestinationReachableW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsDestinationReachableW]
  end;
end;

var
  _IsDestinationReachable: Pointer;

function IsDestinationReachable;
begin
  GetProcedureAddress(_IsDestinationReachable, sensapilib, 'IsDestinationReachable' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsDestinationReachable]
  end;
end;

var
  _IsNetworkAlive: Pointer;

function IsNetworkAlive;
begin
  GetProcedureAddress(_IsNetworkAlive, sensapilib, 'IsNetworkAlive');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_IsNetworkAlive]
  end;
end;

{$ELSE}

function IsDestinationReachableA; external sensapilib name 'IsDestinationReachableA';
function IsDestinationReachableW; external sensapilib name 'IsDestinationReachableW';
function IsDestinationReachable; external sensapilib name 'IsDestinationReachable' + AWSuffix;
function IsNetworkAlive; external sensapilib name 'IsNetworkAlive';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
