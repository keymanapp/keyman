{******************************************************************************}
{                                                                              }
{ Web Publishing API interface Unit for Object Pascal                          }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: wpapi.h, released June 2000. The original Pascal       }
{ code is: WPApi.pas, released December 2000. The initial developer of the     }
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

// $Id: JwaWPApi.pas,v 1.10 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWPApi;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "wpapi.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWPTypes, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//
// Flags
//

const
  WPF_NO_RECURSIVE_POST     = $00000001; // WpPost
  {$EXTERNALSYM WPF_NO_RECURSIVE_POST}
  WPF_NO_WIZARD             = $00000002; // WpPost
  {$EXTERNALSYM WPF_NO_WIZARD}
  WPF_MINIMAL_UI            = $00000004; // WpPost - OBSOLETE
  {$EXTERNALSYM WPF_MINIMAL_UI}
  WPF_FIRST_FILE_AS_DEFAULT = $00000008; // WpPost/PostFiles
  {$EXTERNALSYM WPF_FIRST_FILE_AS_DEFAULT}
  WPF_NO_DIRECT_LOOKUP      = $00000010; // WpBindToSite
  {$EXTERNALSYM WPF_NO_DIRECT_LOOKUP}
  WPF_NO_URL_LOOKUP         = $00000020; // WpBindToSite
  {$EXTERNALSYM WPF_NO_URL_LOOKUP}
  WPF_USE_PROVIDER_PASSED   = $00000040; // WpCreateSite
  {$EXTERNALSYM WPF_USE_PROVIDER_PASSED}
  WPF_NO_VERIFY_POST        = $00000080; // WpPost
  {$EXTERNALSYM WPF_NO_VERIFY_POST}
  WPF_NO_PROGRESS_DLGS      = $00000200; // WpPost
  {$EXTERNALSYM WPF_NO_PROGRESS_DLGS}
  WPF_SHOWPAGE_WELCOME      = $00010000; // WpPost/Wizard
  {$EXTERNALSYM WPF_SHOWPAGE_WELCOME}
  WPF_SHOWPAGE_SRCFILE      = $00020000; // WpPost/Wizard
  {$EXTERNALSYM WPF_SHOWPAGE_SRCFILE}
  WPF_SHOWPAGE_DESTSITE     = $00040000; // WpPost/Wizard
  {$EXTERNALSYM WPF_SHOWPAGE_DESTSITE}
  WPF_SHOWPAGE_PROVIDER     = $00080000; // WpPost/Wizard/AddWizardPages
  {$EXTERNALSYM WPF_SHOWPAGE_PROVIDER}
  WPF_ENABLE_LOCAL_BASE_DIR = $00100000; // WpPost/Wizard
  {$EXTERNALSYM WPF_ENABLE_LOCAL_BASE_DIR}

  WPF_NO_UI = WPF_NO_WIZARD or WPF_NO_PROGRESS_DLGS;
  {$EXTERNALSYM WPF_NO_UI}

  WPF_SHOWPAGE_ALL = WPF_SHOWPAGE_WELCOME or WPF_SHOWPAGE_SRCFILE or WPF_SHOWPAGE_DESTSITE or WPF_SHOWPAGE_PROVIDER;
  {$EXTERNALSYM WPF_SHOWPAGE_ALL}

//
// Miscellaneous
//

  MAX_SITENAME_LEN = 128;
  {$EXTERNALSYM MAX_SITENAME_LEN}

//
// Unicode APIs
//

function WpPostW(hwnd: HWND; dwNumLocalPaths: DWORD; pwsLocalPaths: LPWSTR;
  var pdwSiteNameBufLen: DWORD; wsSiteName: LPWSTR; var pdwDestURLBufLen: DWORD;
  wsDestURL: LPWSTR; dwFlag: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpPostW}
function WpListSitesW(var pdwSitesBufLen: DWORD; pSitesBuffer: LPWPSITEINFOW;
  var pdwNumSites: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpListSitesW}
function WpDoesSiteExistW(wsSiteName: LPCWSTR; var pfSiteExists: BOOL): DWORD; stdcall;
{$EXTERNALSYM WpDoesSiteExistW}
function WpDeleteSiteW(wsSiteName: LPCWSTR): DWORD; stdcall;
{$EXTERNALSYM WpDeleteSiteW}
function WpBindToSiteW(hwnd: HWND; wsSiteName, wsSitePostingURL: LPCWSTR;
  dwFlag, dwReserved: DWORD; out ppvUnk: IUnknown): DWORD; stdcall;
{$EXTERNALSYM WpBindToSiteW}
function WpCreateSiteW(wsSiteName, wsSiteLocalBaseDir, wsSitePostingURL,
  wsProviderCLSID: LPCWSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpCreateSiteW}
function WpEnumProvidersW(var pdwProvidersBufLen: DWORD; pProvidersBuffer: LPWPPROVINFOW;
  var pdwNumProviders: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpEnumProvidersW}
function WpGetErrorStringW(uErrCode: UINT; wsOutputBuf: LPWSTR; var pdwBufLen: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpGetErrorStringW}

//
// ANSI APIs
//

function WpPostA(hwnd: HWND; dwNumLocalPaths: DWORD; pwsLocalPaths: LPSTR;
  var pdwSiteNameBufLen: DWORD; wsSiteName: LPSTR; var pdwDestURLBufLen: DWORD;
  wsDestURL: LPSTR; dwFlag: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpPostA}
function WpListSitesA(var pdwSitesBufLen: DWORD; pSitesBuffer: LPWPSITEINFOA;
  var pdwNumSites: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpListSitesA}
function WpDoesSiteExistA(wsSiteName: LPCSTR; var pfSiteExists: BOOL): DWORD; stdcall;
{$EXTERNALSYM WpDoesSiteExistA}
function WpDeleteSiteA(wsSiteName: LPCSTR): DWORD; stdcall;
{$EXTERNALSYM WpDeleteSiteA}
function WpBindToSiteA(hwnd: HWND; wsSiteName, wsSitePostingURL: LPCSTR;
  dwFlag, dwReserved: DWORD; out ppvUnk: IUnknown): DWORD; stdcall;
{$EXTERNALSYM WpBindToSiteA}
function WpCreateSiteA(wsSiteName, wsSiteLocalBaseDir, wsSitePostingURL,
  wsProviderCLSID: LPCSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpCreateSiteA}
function WpEnumProvidersA(var pdwProvidersBufLen: DWORD; pProvidersBuffer: LPWPPROVINFOA;
  var pdwNumProviders: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpEnumProvidersA}
function WpGetErrorStringA(uErrCode: UINT; wsOutputBuf: LPSTR; var pdwBufLen: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpGetErrorStringA}

function WpPost(hwnd: HWND; dwNumLocalPaths: DWORD; pwsLocalPaths: LPTSTR;
  var pdwSiteNameBufLen: DWORD; wsSiteName: LPTSTR; var pdwDestURLBufLen: DWORD;
  wsDestURL: LPTSTR; dwFlag: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpPost}
function WpListSites(var pdwSitesBufLen: DWORD; pSitesBuffer: LPWPSITEINFO;
  var pdwNumSites: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpListSites}
function WpDoesSiteExist(wsSiteName: LPCTSTR; var pfSiteExists: BOOL): DWORD; stdcall;
{$EXTERNALSYM WpDoesSiteExist}
function WpDeleteSite(wsSiteName: LPCTSTR): DWORD; stdcall;
{$EXTERNALSYM WpDeleteSite}
function WpBindToSite(hwnd: HWND; wsSiteName, wsSitePostingURL: LPCTSTR;
  dwFlag, dwReserved: DWORD; out ppvUnk: IUnknown): DWORD; stdcall;
{$EXTERNALSYM WpBindToSite}
function WpCreateSite(wsSiteName, wsSiteLocalBaseDir, wsSitePostingURL, wsProviderCLSID: LPCTSTR; dwFlags: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpCreateSite}
function WpEnumProviders(var pdwProvidersBufLen: DWORD; pProvidersBuffer: LPWPPROVINFO;
  var pdwNumProviders: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpEnumProviders}
function WpGetErrorString(uErrCode: UINT; wsOutputBuf: LPTSTR; var pdwBufLen: DWORD): DWORD; stdcall;
{$EXTERNALSYM WpGetErrorString}

{$ENDIF JWA_IMPLEMENTATIONSECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}


{$IFNDEF JWA_INCLUDEMODE}
const
  wpapilib = 'wpapi.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _WpPostA: Pointer;

function WpPostA;
begin
  GetProcedureAddress(_WpPostA, wpapilib, 'WpPostA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpPostA]
  end;
end;

var
  _WpListSitesA: Pointer;

function WpListSitesA;
begin
  GetProcedureAddress(_WpListSitesA, wpapilib, 'WpListSitesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpListSitesA]
  end;
end;

var
  _WpDoesSiteExistA: Pointer;

function WpDoesSiteExistA;
begin
  GetProcedureAddress(_WpDoesSiteExistA, wpapilib, 'WpDoesSiteExistA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpDoesSiteExistA]
  end;
end;

var
  _WpDeleteSiteA: Pointer;

function WpDeleteSiteA;
begin
  GetProcedureAddress(_WpDeleteSiteA, wpapilib, 'WpDeleteSiteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpDeleteSiteA]
  end;
end;

var
  _WpBindToSiteA: Pointer;

function WpBindToSiteA;
begin
  GetProcedureAddress(_WpBindToSiteA, wpapilib, 'WpBindToSiteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpBindToSiteA]
  end;
end;

var
  _WpCreateSiteA: Pointer;

function WpCreateSiteA;
begin
  GetProcedureAddress(_WpCreateSiteA, wpapilib, 'WpCreateSiteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpCreateSiteA]
  end;
end;

var
  _WpEnumProvidersA: Pointer;

function WpEnumProvidersA;
begin
  GetProcedureAddress(_WpEnumProvidersA, wpapilib, 'WpEnumProvidersA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpEnumProvidersA]
  end;
end;

var
  _WpGetErrorStringA: Pointer;

function WpGetErrorStringA;
begin
  GetProcedureAddress(_WpGetErrorStringA, wpapilib, 'WpGetErrorStringA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpGetErrorStringA]
  end;
end;

var
  _WpPostW: Pointer;

function WpPostW;
begin
  GetProcedureAddress(_WpPostW, wpapilib, 'WpPostW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpPostW]
  end;
end;

var
  _WpListSitesW: Pointer;

function WpListSitesW;
begin
  GetProcedureAddress(_WpListSitesW, wpapilib, 'WpListSitesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpListSitesW]
  end;
end;

var
  _WpDoesSiteExistW: Pointer;

function WpDoesSiteExistW;
begin
  GetProcedureAddress(_WpDoesSiteExistW, wpapilib, 'WpDoesSiteExistW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpDoesSiteExistW]
  end;
end;

var
  _WpDeleteSiteW: Pointer;

function WpDeleteSiteW;
begin
  GetProcedureAddress(_WpDeleteSiteW, wpapilib, 'WpDeleteSiteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpDeleteSiteW]
  end;
end;

var
  _WpBindToSiteW: Pointer;

function WpBindToSiteW;
begin
  GetProcedureAddress(_WpBindToSiteW, wpapilib, 'WpBindToSiteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpBindToSiteW]
  end;
end;

var
  _WpCreateSiteW: Pointer;

function WpCreateSiteW;
begin
  GetProcedureAddress(_WpCreateSiteW, wpapilib, 'WpCreateSiteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpCreateSiteW]
  end;
end;

var
  _WpEnumProvidersW: Pointer;

function WpEnumProvidersW;
begin
  GetProcedureAddress(_WpEnumProvidersW, wpapilib, 'WpEnumProvidersW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpEnumProvidersW]
  end;
end;

var
  _WpGetErrorStringW: Pointer;

function WpGetErrorStringW;
begin
  GetProcedureAddress(_WpGetErrorStringW, wpapilib, 'WpGetErrorStringW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpGetErrorStringW]
  end;
end;

var
  _WpPost: Pointer;

function WpPost;
begin
  GetProcedureAddress(_WpPost, wpapilib, 'WpPost' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpPost]
  end;
end;

var
  _WpListSites: Pointer;

function WpListSites;
begin
  GetProcedureAddress(_WpListSites, wpapilib, 'WpListSites' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpListSites]
  end;
end;

var
  _WpDoesSiteExist: Pointer;

function WpDoesSiteExist;
begin
  GetProcedureAddress(_WpDoesSiteExist, wpapilib, 'WpDoesSiteExist' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpDoesSiteExist]
  end;
end;

var
  _WpDeleteSite: Pointer;

function WpDeleteSite;
begin
  GetProcedureAddress(_WpDeleteSite, wpapilib, 'WpDeleteSite' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpDeleteSite]
  end;
end;

var
  _WpBindToSite: Pointer;

function WpBindToSite;
begin
  GetProcedureAddress(_WpBindToSite, wpapilib, 'WpBindToSite' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpBindToSite]
  end;
end;

var
  _WpCreateSite: Pointer;

function WpCreateSite;
begin
  GetProcedureAddress(_WpCreateSite, wpapilib, 'WpCreateSite' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpCreateSite]
  end;
end;

var
  _WpEnumProviders: Pointer;

function WpEnumProviders;
begin
  GetProcedureAddress(_WpEnumProviders, wpapilib, 'WpEnumProviders' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpEnumProviders]
  end;
end;

var
  _WpGetErrorString: Pointer;

function WpGetErrorString;
begin
  GetProcedureAddress(_WpGetErrorString, wpapilib, 'WpGetErrorString' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_WpGetErrorString]
  end;
end;

{$ELSE}

function WpPostA; external wpapilib name 'WpPostA';
function WpListSitesA; external wpapilib name 'WpListSitesA';
function WpDoesSiteExistA; external wpapilib name 'WpDoesSiteExistA';
function WpDeleteSiteA; external wpapilib name 'WpDeleteSiteA';
function WpBindToSiteA; external wpapilib name 'WpBindToSiteA';
function WpCreateSiteA; external wpapilib name 'WpCreateSiteA';
function WpEnumProvidersA; external wpapilib name 'WpEnumProvidersA';
function WpGetErrorStringA; external wpapilib name 'WpGetErrorStringA';
function WpPostW; external wpapilib name 'WpPostW';
function WpListSitesW; external wpapilib name 'WpListSitesW';
function WpDoesSiteExistW; external wpapilib name 'WpDoesSiteExistW';
function WpDeleteSiteW; external wpapilib name 'WpDeleteSiteW';
function WpBindToSiteW; external wpapilib name 'WpBindToSiteW';
function WpCreateSiteW; external wpapilib name 'WpCreateSiteW';
function WpEnumProvidersW; external wpapilib name 'WpEnumProvidersW';
function WpGetErrorStringW; external wpapilib name 'WpGetErrorStringW';
function WpPost; external wpapilib name 'WpPost' + AWSuffix;
function WpListSites; external wpapilib name 'WpListSites' + AWSuffix;
function WpDoesSiteExist; external wpapilib name 'WpDoesSiteExist' + AWSuffix;
function WpDeleteSite; external wpapilib name 'WpDeleteSite' + AWSuffix;
function WpBindToSite; external wpapilib name 'WpBindToSite' + AWSuffix;
function WpCreateSite; external wpapilib name 'WpCreateSite' + AWSuffix;
function WpEnumProviders; external wpapilib name 'WpEnumProviders' + AWSuffix;
function WpGetErrorString; external wpapilib name 'WpGetErrorString' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

//your implementation here
{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
