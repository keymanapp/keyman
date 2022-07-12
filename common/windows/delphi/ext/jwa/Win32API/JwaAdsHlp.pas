{******************************************************************************}
{                                                                              }
{ Active Directory Helper Functions API interface Unit for Object Pascal       }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: adshlp.h, released June 2000. The original Pascal      }
{ code is: AdsHlp.pas, released December 2000. The initial developer of the    }
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

// $Id: JwaAdsHlp.pas,v 1.12 2007/09/06 14:57:10 marquardt Exp $

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaAdsHlp;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "adshlp.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaActiveX, JwaAdsTLB, JwaWinType, JwaWinNT;

{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_INCLUDEMODE}

function ADsGetObject(lpszPathName: LPCWSTR; const riid: TGUID; out ppObject: Pointer): HRESULT; stdcall;
{$EXTERNALSYM ADsGetObject}

function ADsBuildEnumerator(pADsContainer: IADsContainer; out ppEnumVariant: IEnumVARIANT): HRESULT; stdcall;
{$EXTERNALSYM ADsBuildEnumerator}

function ADsFreeEnumerator(pEnumVariant: IEnumVARIANT): HRESULT; stdcall;
{$EXTERNALSYM ADsFreeEnumerator}

function ADsEnumerateNext(pEnumVariant: IEnumVARIANT; cElements: ULONG;
  var pvar: OleVariant; var pcElementsFetched: ULONG): HRESULT; stdcall;
{$EXTERNALSYM ADsEnumerateNext}

function ADsBuildVarArrayStr(lppPathNames: LPWSTR; dwPathNames: DWORD;
  var pVar: OleVariant): HRESULT; stdcall;
{$EXTERNALSYM ADsBuildVarArrayStr}

function ADsBuildVarArrayInt(lpdwObjectTypes: LPDWORD; dwObjectTypes: DWORD;
  var pVar: OleVariant): HRESULT; stdcall;
{$EXTERNALSYM ADsBuildVarArrayInt}

function ADsOpenObject(lpszPathName, lpszUserName, lpszPassword: LPCWSTR;
  dwReserved: DWORD; const riid: TGUID; out ppObject: Pointer): HRESULT; stdcall;
{$EXTERNALSYM ADsOpenObject}

//
// Helper functions for extended error support
//

function ADsGetLastError(var lpError: DWORD; lpErrorBuf: LPWSTR;
  dwErrorBufLen: DWORD; lpNameBuf: LPWSTR; dwNameBufLen: DWORD): HRESULT; stdcall;
{$EXTERNALSYM ADsGetLastError}

procedure ADsSetLastError(dwErr: DWORD; pszError, pszProvider: LPCWSTR); stdcall;
{$EXTERNALSYM ADsSetLastError}

//procedure ADsFreeAllErrorRecords; stdcall;
//{$EXTERNALSYM ADsFreeAllErrorRecords}

function AllocADsMem(cb: DWORD): LPVOID; stdcall;
{$EXTERNALSYM AllocADsMem}

function FreeADsMem(pMem: LPVOID): BOOL; stdcall;
{$EXTERNALSYM FreeADsMem}

function ReallocADsMem(pOldMem: LPVOID; cbOld, cbNew: DWORD): LPVOID; stdcall;
{$EXTERNALSYM ReallocADsMem}

function AllocADsStr(pStr: LPCWSTR): LPWSTR; stdcall;
{$EXTERNALSYM AllocADsStr}

function FreeADsStr(pStr: LPWSTR): BOOL; stdcall;
{$EXTERNALSYM FreeADsStr}

function ReallocADsStr(var ppStr: LPWSTR; pStr: LPWSTR): BOOL; stdcall;
{$EXTERNALSYM ReallocADsStr}

function ADsEncodeBinaryData(pbSrcData: PBYTE; dwSrcLen: DWORD;
  var ppszDestData: LPWSTR): HRESULT; stdcall;
{$EXTERNALSYM ADsEncodeBinaryData}

function ADsDecodeBinaryData(szSrcData: LPCWSTR; var ppbDestData: PBYTE;
  var pdwDestLen: ULONG): HRESULT; stdcall;
{$EXTERNALSYM ADsDecodeBinaryData}

type
  // imports of a type library sometimes are missing a few decls, these are just
  // a few of them to make this file compile at all. I really should do all of
  // them one day.

  PADSVALUE = ^_adsvalue;
  {$EXTERNALSYM PADSVALUE}
  PADS_ATTR_INFO = ^_ads_attr_info;
  {$EXTERNALSYM PADS_ATTR_INFO}

function PropVariantToAdsType(var pVariant: OleVariant; dwNumVariant: DWORD;
  var ppAdsValues: PADSVALUE; pdwNumValues: PDWORD): HRESULT; stdcall;
{$EXTERNALSYM PropVariantToAdsType}


function AdsTypeToPropVariant(pAdsValues: PADSVALUE; dwNumValues: DWORD;
  var pVariant: OleVariant): HRESULT; stdcall;
{$EXTERNALSYM AdsTypeToPropVariant}

procedure AdsFreeAdsValues(pAdsValues: PADSVALUE; dwNumValues: DWORD); stdcall;
{$EXTERNALSYM AdsFreeAdsValues}

//
// Helper routines to convert IADsSecurityDescriptor to a binary
// security descriptor and also to convert a binary SD to 
// IADsSecurityDescriptor.
//

// TODO VARIANT!

function BinarySDToSecurityDescriptor(pSecurityDescriptor: PSECURITY_DESCRIPTOR;
  var pVarsec: VARIANT; pszServerName, userName, passWord: LPCWSTR; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM BinarySDToSecurityDescriptor}

{$ENDIF JWA_INCLUDEMODE}

function SecurityDescriptorToBinarySD(vVarSecDes: VARIANT;
  var ppSecurityDescriptor: PSECURITY_DESCRIPTOR; pdwSDLength: PDWORD;
  pszServerName, userName, passWord: LPCWSTR; dwFlags: DWORD): HRESULT; stdcall;
{$EXTERNALSYM SecurityDescriptorToBinarySD}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  adslib = 'activeds.dll';
{$ENDIF JWA_INCLUDEMODE}

//procedure ADsFreeAllErrorRecords

{$IFDEF DYNAMIC_LINK}

{$IFNDEF JWA_INCLUDEMODE}

var
  _ADsGetObject: Pointer;

function ADsGetObject;
begin
  GetProcedureAddress(_ADsGetObject, adslib, 'ADsGetObject');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsGetObject]
  end;
end;

var
  _ADsBuildEnumerator: Pointer;

function ADsBuildEnumerator;
begin
  GetProcedureAddress(_ADsBuildEnumerator, adslib, 'ADsBuildEnumerator');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsBuildEnumerator]
  end;
end;

var
  _ADsFreeEnumerator: Pointer;

function ADsFreeEnumerator;
begin
  GetProcedureAddress(_ADsFreeEnumerator, adslib, 'ADsFreeEnumerator');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsFreeEnumerator]
  end;
end;

var
  _ADsEnumerateNext: Pointer;

function ADsEnumerateNext;
begin
  GetProcedureAddress(_ADsEnumerateNext, adslib, 'ADsEnumerateNext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsEnumerateNext]
  end;
end;

var
  _ADsBuildVarArrayStr: Pointer;

function ADsBuildVarArrayStr;
begin
  GetProcedureAddress(_ADsBuildVarArrayStr, adslib, 'ADsBuildVarArrayStr');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsBuildVarArrayStr]
  end;
end;

var
  _ADsBuildVarArrayInt: Pointer;

function ADsBuildVarArrayInt;
begin
  GetProcedureAddress(_ADsBuildVarArrayInt, adslib, 'ADsBuildVarArrayInt');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsBuildVarArrayInt]
  end;
end;

var
  _ADsOpenObject: Pointer;

function ADsOpenObject;
begin
  GetProcedureAddress(_ADsOpenObject, adslib, 'ADsOpenObject');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsOpenObject]
  end;
end;

var
  _ADsGetLastError: Pointer;

function ADsGetLastError;
begin
  GetProcedureAddress(_ADsGetLastError, adslib, 'ADsGetLastError');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsGetLastError]
  end;
end;

var
  _ADsSetLastError: Pointer;

procedure ADsSetLastError;
begin
  GetProcedureAddress(_ADsSetLastError, adslib, 'ADsSetLastError');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsSetLastError]
  end;
end;

var
  _AllocADsMem: Pointer;

function AllocADsMem;
begin
  GetProcedureAddress(_AllocADsMem, adslib, 'AllocADsMem');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AllocADsMem]
  end;
end;

var
  _FreeADsMem: Pointer;

function FreeADsMem;
begin
  GetProcedureAddress(_FreeADsMem, adslib, 'FreeADsMem');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeADsMem]
  end;
end;

var
  _ReallocADsMem: Pointer;

function ReallocADsMem;
begin
  GetProcedureAddress(_ReallocADsMem, adslib, 'ReallocADsMem');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReallocADsMem]
  end;
end;

var
  _AllocADsStr: Pointer;

function AllocADsStr;
begin
  GetProcedureAddress(_AllocADsStr, adslib, 'AllocADsStr');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AllocADsStr]
  end;
end;

var
  _FreeADsStr: Pointer;

function FreeADsStr;
begin
  GetProcedureAddress(_FreeADsStr, adslib, 'FreeADsStr');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeADsStr]
  end;
end;

var
  _ReallocADsStr: Pointer;

function ReallocADsStr;
begin
  GetProcedureAddress(_ReallocADsStr, adslib, 'ReallocADsStr');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReallocADsStr]
  end;
end;

var
  _ADsEncodeBinaryData: Pointer;

function ADsEncodeBinaryData;
begin
  GetProcedureAddress(_ADsEncodeBinaryData, adslib, 'ADsEncodeBinaryData');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsEncodeBinaryData]
  end;
end;

var
  _ADsDecodeBinaryData: Pointer;

function ADsDecodeBinaryData;
begin
  GetProcedureAddress(_ADsDecodeBinaryData, adslib, 'ADsDecodeBinaryData');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ADsDecodeBinaryData]
  end;
end;

var
  _PropVariantToAdsType: Pointer;

function PropVariantToAdsType;
begin
  GetProcedureAddress(_PropVariantToAdsType, adslib, 'PropVariantToAdsType');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_PropVariantToAdsType]
  end;
end;

var
  _AdsTypeToPropVariant: Pointer;

function AdsTypeToPropVariant;
begin
  GetProcedureAddress(_AdsTypeToPropVariant, adslib, 'AdsTypeToPropVariant');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AdsTypeToPropVariant]
  end;
end;

var
  _AdsFreeAdsValues: Pointer;

procedure AdsFreeAdsValues;
begin
  GetProcedureAddress(_AdsFreeAdsValues, adslib, 'AdsFreeAdsValues');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AdsFreeAdsValues]
  end;
end;

var
  _BinarySDToSecurityDescriptor: Pointer;

function BinarySDToSecurityDescriptor;
begin
  GetProcedureAddress(_BinarySDToSecurityDescriptor, adslib, 'BinarySDToSecurityDescriptor');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BinarySDToSecurityDescriptor]
  end;
end;

{$ENDIF JWA_INCLUDEMODE}

var
  _SecurityDescriptorToBinarySD: Pointer;

function SecurityDescriptorToBinarySD;
begin
  GetProcedureAddress(_SecurityDescriptorToBinarySD, adslib, 'SecurityDescriptorToBinarySD');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SecurityDescriptorToBinarySD]
  end;
end;

{$ELSE}

{$IFNDEF JWA_INCLUDEMODE}
function ADsGetObject; external adslib name 'ADsGetObject';
function ADsBuildEnumerator; external adslib name 'ADsBuildEnumerator';
function ADsFreeEnumerator; external adslib name 'ADsFreeEnumerator';
function ADsEnumerateNext; external adslib name 'ADsEnumerateNext';
function ADsBuildVarArrayStr; external adslib name 'ADsBuildVarArrayStr';
function ADsBuildVarArrayInt; external adslib name 'ADsBuildVarArrayInt';
function ADsOpenObject; external adslib name 'ADsOpenObject';
function ADsGetLastError; external adslib name 'ADsGetLastError';
procedure ADsSetLastError; external adslib name 'ADsSetLastError';
function AllocADsMem; external adslib name 'AllocADsMem';
function FreeADsMem; external adslib name 'FreeADsMem';
function ReallocADsMem; external adslib name 'ReallocADsMem';
function AllocADsStr; external adslib name 'AllocADsStr';
function FreeADsStr; external adslib name 'FreeADsStr';
function ReallocADsStr; external adslib name 'ReallocADsStr';
function ADsEncodeBinaryData; external adslib name 'ADsEncodeBinaryData';
function ADsDecodeBinaryData; external adslib name 'ADsDecodeBinaryData';
function PropVariantToAdsType; external adslib name 'PropVariantToAdsType';
function AdsTypeToPropVariant; external adslib name 'AdsTypeToPropVariant';
procedure AdsFreeAdsValues; external adslib name 'AdsFreeAdsValues';
function BinarySDToSecurityDescriptor; external adslib name 'BinarySDToSecurityDescriptor';
{$ENDIF JWA_INCLUDEMODE}

function SecurityDescriptorToBinarySD; external adslib name 'SecurityDescriptorToBinarySD';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
