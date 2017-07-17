{******************************************************************************}
{                                                                              }
{ Windows File Protection API interface Unit for Object Pascal                 }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: sfc.h, released August 2001. The original Pascal       }
{ code is: Sfc.pas, released December 2000. The initial developer of the       }
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

// $Id: JwaSfc.pas,v 1.9 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSfc;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "sfc.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  SFC_DISABLE_NORMAL        = 0;
  {$EXTERNALSYM SFC_DISABLE_NORMAL}
  SFC_DISABLE_ASK           = 1;
  {$EXTERNALSYM SFC_DISABLE_ASK}
  SFC_DISABLE_ONCE          = 2;
  {$EXTERNALSYM SFC_DISABLE_ONCE}
  SFC_DISABLE_SETUP         = 3;
  {$EXTERNALSYM SFC_DISABLE_SETUP}
  SFC_DISABLE_NOPOPUPS      = 4;
  {$EXTERNALSYM SFC_DISABLE_NOPOPUPS}

  SFC_SCAN_NORMAL           = 0;
  {$EXTERNALSYM SFC_SCAN_NORMAL}
  SFC_SCAN_ALWAYS           = 1;
  {$EXTERNALSYM SFC_SCAN_ALWAYS}
  SFC_SCAN_ONCE             = 2;
  {$EXTERNALSYM SFC_SCAN_ONCE}
  SFC_SCAN_IMMEDIATE        = 3;
  {$EXTERNALSYM SFC_SCAN_IMMEDIATE}

  SFC_QUOTA_DEFAULT         = 50;
  {$EXTERNALSYM SFC_QUOTA_DEFAULT}
  SFC_QUOTA_ALL_FILES       = ULONG(-1);
  {$EXTERNALSYM SFC_QUOTA_ALL_FILES}

  SFC_IDLE_TRIGGER          = WideString('WFP_IDLE_TRIGGER');
  {$EXTERNALSYM SFC_IDLE_TRIGGER}

type
  PPROTECTED_FILE_DATA = ^PROTECTED_FILE_DATA;
  {$EXTERNALSYM PPROTECTED_FILE_DATA}
  _PROTECTED_FILE_DATA = record
    FileName: array [0..MAX_PATH - 1] of WCHAR;
    FileNumber: DWORD;
  end;
  {$EXTERNALSYM _PROTECTED_FILE_DATA}
  PROTECTED_FILE_DATA = _PROTECTED_FILE_DATA;
  {$EXTERNALSYM PROTECTED_FILE_DATA}
  TProtectedFileData = PROTECTED_FILE_DATA;
  PProtectedFileData = PPROTECTED_FILE_DATA;

function SfcGetNextProtectedFile(RpcHandle: HANDLE;
  var ProtFileData: PROTECTED_FILE_DATA): BOOL; stdcall;
{$EXTERNALSYM SfcGetNextProtectedFile}
function SfcIsFileProtected(RpcHandle: HANDLE; ProtFileName: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM SfcIsFileProtected}

//
// new APIs which are not currently supported, but are stubbed out
//

function SfpVerifyFile(pszFileName: LPCSTR; pszError: LPSTR; dwErrSize: DWORD): BOOL; stdcall;

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  Sfclib = 'sfc.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _SfcGetNextProtectedFile: Pointer;

function SfcGetNextProtectedFile;
begin
  GetProcedureAddress(_SfcGetNextProtectedFile, Sfclib, 'SfcGetNextProtectedFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SfcGetNextProtectedFile]
  end;
end;

var
  _SfcIsFileProtected: Pointer;

function SfcIsFileProtected;
begin
  GetProcedureAddress(_SfcIsFileProtected, Sfclib, 'SfcIsFileProtected');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SfcIsFileProtected]
  end;
end;

var
  _SfpVerifyFile: Pointer;

function SfpVerifyFile;
begin
  GetProcedureAddress(_SfpVerifyFile, Sfclib, 'SfpVerifyFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SfpVerifyFile]
  end;
end;

{$ELSE}

function SfcGetNextProtectedFile; external Sfclib name 'SfcGetNextProtectedFile';
function SfcIsFileProtected; external Sfclib name 'SfcIsFileProtected';
function SfpVerifyFile; external Sfclib name 'SfpVerifyFile';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
