{******************************************************************************}
{                                                                              }
{ Windows Version API interface Unit for Object Pascal                         }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: winver.h, released June 2000. The original Pascal      }
{ code is: WinVer.pas, released December 2000. The initial developer of the    }
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

// $Id: JwaWinVer.pas,v 1.12 2007/09/14 06:48:48 marquardt Exp $

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinVer;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinVer.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinUser, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
const

//  RT_VERSION = MAKEINTRESOURCE(16);

// ----- Symbols -----

  VS_FILE_INFO    = RT_VERSION;
  {$EXTERNALSYM VS_FILE_INFO}
  VS_VERSION_INFO = 1;
  {$EXTERNALSYM VS_VERSION_INFO}
  VS_USER_DEFINED = 100;
  {$EXTERNALSYM VS_USER_DEFINED}

// ----- VS_VERSION.dwFileFlags -----

  VS_FFI_SIGNATURE     = $FEEF04BD;
  {$EXTERNALSYM VS_FFI_SIGNATURE}
  VS_FFI_STRUCVERSION  = $00010000;
  {$EXTERNALSYM VS_FFI_STRUCVERSION}
  VS_FFI_FILEFLAGSMASK = $0000003F;
  {$EXTERNALSYM VS_FFI_FILEFLAGSMASK}

// ----- VS_VERSION.dwFileFlags -----

  VS_FF_DEBUG        = $00000001;
  {$EXTERNALSYM VS_FF_DEBUG}
  VS_FF_PRERELEASE   = $00000002;
  {$EXTERNALSYM VS_FF_PRERELEASE}
  VS_FF_PATCHED      = $00000004;
  {$EXTERNALSYM VS_FF_PATCHED}
  VS_FF_PRIVATEBUILD = $00000008;
  {$EXTERNALSYM VS_FF_PRIVATEBUILD}
  VS_FF_INFOINFERRED = $00000010;
  {$EXTERNALSYM VS_FF_INFOINFERRED}
  VS_FF_SPECIALBUILD = $00000020;
  {$EXTERNALSYM VS_FF_SPECIALBUILD}

// ----- VS_VERSION.dwFileOS -----

  VOS_UNKNOWN = $00000000;
  {$EXTERNALSYM VOS_UNKNOWN}
  VOS_DOS     = $00010000;
  {$EXTERNALSYM VOS_DOS}
  VOS_OS216   = $00020000;
  {$EXTERNALSYM VOS_OS216}
  VOS_OS232   = $00030000;
  {$EXTERNALSYM VOS_OS232}
  VOS_NT      = $00040000;
  {$EXTERNALSYM VOS_NT}
  VOS_WINCE   = $00050000;
  {$EXTERNALSYM VOS_WINCE}

  VOS__BASE      = $00000000;
  {$EXTERNALSYM VOS__BASE}
  VOS__WINDOWS16 = $00000001;
  {$EXTERNALSYM VOS__WINDOWS16}
  VOS__PM16      = $00000002;
  {$EXTERNALSYM VOS__PM16}
  VOS__PM32      = $00000003;
  {$EXTERNALSYM VOS__PM32}
  VOS__WINDOWS32 = $00000004;
  {$EXTERNALSYM VOS__WINDOWS32}

  VOS_DOS_WINDOWS16 = $00010001;
  {$EXTERNALSYM VOS_DOS_WINDOWS16}
  VOS_DOS_WINDOWS32 = $00010004;
  {$EXTERNALSYM VOS_DOS_WINDOWS32}
  VOS_OS216_PM16    = $00020002;
  {$EXTERNALSYM VOS_OS216_PM16}
  VOS_OS232_PM32    = $00030003;
  {$EXTERNALSYM VOS_OS232_PM32}
  VOS_NT_WINDOWS32  = $00040004;
  {$EXTERNALSYM VOS_NT_WINDOWS32}

// ----- VS_VERSION.dwFileType -----

  VFT_UNKNOWN    = $00000000;
  {$EXTERNALSYM VFT_UNKNOWN}
  VFT_APP        = $00000001;
  {$EXTERNALSYM VFT_APP}
  VFT_DLL        = $00000002;
  {$EXTERNALSYM VFT_DLL}
  VFT_DRV        = $00000003;
  {$EXTERNALSYM VFT_DRV}
  VFT_FONT       = $00000004;
  {$EXTERNALSYM VFT_FONT}
  VFT_VXD        = $00000005;
  {$EXTERNALSYM VFT_VXD}
  VFT_STATIC_LIB = $00000007;
  {$EXTERNALSYM VFT_STATIC_LIB}

// ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_DRV -----

  VFT2_UNKNOWN         = $00000000;
  {$EXTERNALSYM VFT2_UNKNOWN}
  VFT2_DRV_PRINTER     = $00000001;
  {$EXTERNALSYM VFT2_DRV_PRINTER}
  VFT2_DRV_KEYBOARD    = $00000002;
  {$EXTERNALSYM VFT2_DRV_KEYBOARD}
  VFT2_DRV_LANGUAGE    = $00000003;
  {$EXTERNALSYM VFT2_DRV_LANGUAGE}
  VFT2_DRV_DISPLAY     = $00000004;
  {$EXTERNALSYM VFT2_DRV_DISPLAY}
  VFT2_DRV_MOUSE       = $00000005;
  {$EXTERNALSYM VFT2_DRV_MOUSE}
  VFT2_DRV_NETWORK     = $00000006;
  {$EXTERNALSYM VFT2_DRV_NETWORK}
  VFT2_DRV_SYSTEM      = $00000007;
  {$EXTERNALSYM VFT2_DRV_SYSTEM}
  VFT2_DRV_INSTALLABLE = $00000008;
  {$EXTERNALSYM VFT2_DRV_INSTALLABLE}
  VFT2_DRV_SOUND       = $00000009;
  {$EXTERNALSYM VFT2_DRV_SOUND}
  VFT2_DRV_COMM        = $0000000A;
  {$EXTERNALSYM VFT2_DRV_COMM}
  VFT2_DRV_INPUTMETHOD = $0000000B;
  {$EXTERNALSYM VFT2_DRV_INPUTMETHOD}
  VFT2_DRV_VERSIONED_PRINTER = $0000000C;
  {$EXTERNALSYM VFT2_DRV_VERSIONED_PRINTER}

// ----- VS_VERSION.dwFileSubtype for VFT_WINDOWS_FONT -----

  VFT2_FONT_RASTER   = $00000001;
  {$EXTERNALSYM VFT2_FONT_RASTER}
  VFT2_FONT_VECTOR   = $00000002;
  {$EXTERNALSYM VFT2_FONT_VECTOR}
  VFT2_FONT_TRUETYPE = $00000003;
  {$EXTERNALSYM VFT2_FONT_TRUETYPE}

// ----- VerFindFile() flags -----

  VFFF_ISSHAREDFILE = $0001;
  {$EXTERNALSYM VFFF_ISSHAREDFILE}

  VFF_CURNEDEST     = $0001;
  {$EXTERNALSYM VFF_CURNEDEST}
  VFF_FILEINUSE     = $0002;
  {$EXTERNALSYM VFF_FILEINUSE}
  VFF_BUFFTOOSMALL  = $0004;
  {$EXTERNALSYM VFF_BUFFTOOSMALL}

// ----- VerInstallFile() flags -----

  VIFF_FORCEINSTALL  = $0001;
  {$EXTERNALSYM VIFF_FORCEINSTALL}
  VIFF_DONTDELETEOLD = $0002;
  {$EXTERNALSYM VIFF_DONTDELETEOLD}

  VIF_TEMPFILE = $00000001;
  {$EXTERNALSYM VIF_TEMPFILE}
  VIF_MISMATCH = $00000002;
  {$EXTERNALSYM VIF_MISMATCH}
  VIF_SRCOLD   = $00000004;
  {$EXTERNALSYM VIF_SRCOLD}

  VIF_DIFFLANG   = $00000008;
  {$EXTERNALSYM VIF_DIFFLANG}
  VIF_DIFFCODEPG = $00000010;
  {$EXTERNALSYM VIF_DIFFCODEPG}
  VIF_DIFFTYPE   = $00000020;
  {$EXTERNALSYM VIF_DIFFTYPE}

  VIF_WRITEPROT        = $00000040;
  {$EXTERNALSYM VIF_WRITEPROT}
  VIF_FILEINUSE        = $00000080;
  {$EXTERNALSYM VIF_FILEINUSE}
  VIF_OUTOFSPACE       = $00000100;
  {$EXTERNALSYM VIF_OUTOFSPACE}
  VIF_ACCESSVIOLATION  = $00000200;
  {$EXTERNALSYM VIF_ACCESSVIOLATION}
  VIF_SHARINGVIOLATION = $00000400;
  {$EXTERNALSYM VIF_SHARINGVIOLATION}
  VIF_CANNOTCREATE     = $00000800;
  {$EXTERNALSYM VIF_CANNOTCREATE}
  VIF_CANNOTDELETE     = $00001000;
  {$EXTERNALSYM VIF_CANNOTDELETE}
  VIF_CANNOTRENAME     = $00002000;
  {$EXTERNALSYM VIF_CANNOTRENAME}
  VIF_CANNOTDELETECUR  = $00004000;
  {$EXTERNALSYM VIF_CANNOTDELETECUR}
  VIF_OUTOFMEMORY      = $00008000;
  {$EXTERNALSYM VIF_OUTOFMEMORY}

  VIF_CANNOTREADSRC = $00010000;
  {$EXTERNALSYM VIF_CANNOTREADSRC}
  VIF_CANNOTREADDST = $00020000;
  {$EXTERNALSYM VIF_CANNOTREADDST}

  VIF_BUFFTOOSMALL      = $00040000;
  {$EXTERNALSYM VIF_BUFFTOOSMALL}
  VIF_CANNOTLOADLZ32    = $00080000;
  {$EXTERNALSYM VIF_CANNOTLOADLZ32}
  VIF_CANNOTLOADCABINET = $00100000;
  {$EXTERNALSYM VIF_CANNOTLOADCABINET}

// ----- Types and structures -----

type
  PVSFixedFileInfo = ^VS_FIXEDFILEINFO;
  tagVS_FIXEDFILEINFO = record
    dwSignature: DWORD;        // e.g. 0xfeef04bd
    dwStrucVersion: DWORD;     // e.g. 0x00000042 = "0.42"
    dwFileVersionMS: DWORD;    // e.g. 0x00030075 = "3.75"
    dwFileVersionLS: DWORD;    // e.g. 0x00000031 = "0.31"
    dwProductVersionMS: DWORD; // e.g. 0x00030010 = "3.10"
    dwProductVersionLS: DWORD; // e.g. 0x00000031 = "0.31"
    dwFileFlagsMask: DWORD;    // = 0x3F for version "0.42"
    dwFileFlags: DWORD;        // e.g. VFF_DEBUG | VFF_PRERELEASE
    dwFileOS: DWORD;           // e.g. VOS_DOS_WINDOWS16
    dwFileType: DWORD;         // e.g. VFT_DRIVER
    dwFileSubtype: DWORD;      // e.g. VFT2_DRV_KEYBOARD
    dwFileDateMS: DWORD;       // e.g. 0
    dwFileDateLS: DWORD;       // e.g. 0
  end;
  {$EXTERNALSYM tagVS_FIXEDFILEINFO}
  VS_FIXEDFILEINFO = tagVS_FIXEDFILEINFO;
  {$EXTERNALSYM VS_FIXEDFILEINFO}
  TVSFixedFileInfo = VS_FIXEDFILEINFO;

// ----- Function prototypes -----

function VerFindFileA(uFlags: DWORD; szFileName, szWinDir, szAppDir,
  szCurDir: LPSTR; var lpuCurDirLen: UINT; szDestDir: LPSTR;
  var lpuDestDirLen: UINT): DWORD; stdcall;
{$EXTERNALSYM VerFindFileA}
function VerFindFileW(uFlags: DWORD; szFileName, szWinDir, szAppDir,
  szCurDir: LPWSTR; var lpuCurDirLen: UINT; szDestDir: LPWSTR;
  var lpuDestDirLen: UINT): DWORD; stdcall;
{$EXTERNALSYM VerFindFileW}
function VerFindFile(uFlags: DWORD; szFileName, szWinDir, szAppDir,
  szCurDir: LPTSTR; var lpuCurDirLen: UINT; szDestDir: LPTSTR;
  var lpuDestDirLen: UINT): DWORD; stdcall;
{$EXTERNALSYM VerFindFile}

function VerInstallFileA(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir,
  szDestDir, szCurDir, szTmpFile: LPSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
{$EXTERNALSYM VerInstallFileA}
function VerInstallFileW(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir,
  szDestDir, szCurDir, szTmpFile: LPWSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
{$EXTERNALSYM VerInstallFileW}
function VerInstallFile(uFlags: DWORD; szSrcFileName, szDestFileName, szSrcDir,
  szDestDir, szCurDir, szTmpFile: LPTSTR; var lpuTmpFileLen: UINT): DWORD; stdcall;
{$EXTERNALSYM VerInstallFile}

// Returns size of version info in bytes

function GetFileVersionInfoSizeA(lptstrFilename: LPCSTR; var lpdwHandle: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetFileVersionInfoSizeA}
function GetFileVersionInfoSizeW(lptstrFilename: LPCWSTR; var lpdwHandle: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetFileVersionInfoSizeW}
function GetFileVersionInfoSize(lptstrFilename: LPCTSTR; var lpdwHandle: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetFileVersionInfoSize}

// Read version info into buffer

function GetFileVersionInfoA(lptstrFilename: LPCSTR; dwHandle, dwLen: DWORD;
  lpData: LPVOID): BOOL; stdcall;
{$EXTERNALSYM GetFileVersionInfoA}
function GetFileVersionInfoW(lptstrFilename: LPCWSTR; dwHandle, dwLen: DWORD;
  lpData: LPVOID): BOOL; stdcall;
{$EXTERNALSYM GetFileVersionInfoW}
function GetFileVersionInfo(lptstrFilename: LPCTSTR; dwHandle, dwLen: DWORD;
  lpData: LPVOID): BOOL; stdcall;
{$EXTERNALSYM GetFileVersionInfo}

function VerLanguageNameA(wLang: DWORD; szLang: LPSTR; nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM VerLanguageNameA}
function VerLanguageNameW(wLang: DWORD; szLang: LPWSTR; nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM VerLanguageNameW}
function VerLanguageName(wLang: DWORD; szLang: LPTSTR; nSize: DWORD): DWORD; stdcall;
{$EXTERNALSYM VerLanguageName}

function VerQueryValueA(pBlock: LPVOID; lpSubBlock: LPSTR; var lplpBuffer: LPVOID;
  var puLen: UINT): BOOL; stdcall;
{$EXTERNALSYM VerQueryValueA}
function VerQueryValueW(pBlock: LPVOID; lpSubBlock: LPWSTR; var lplpBuffer: LPVOID;
  var puLen: UINT): BOOL; stdcall;
{$EXTERNALSYM VerQueryValueW}
function VerQueryValue(pBlock: LPVOID; lpSubBlock: LPTSTR; var lplpBuffer: LPVOID;
  var puLen: UINT): BOOL; stdcall;
{$EXTERNALSYM VerQueryValue}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}
  {$IFNDEF JWA_INCLUDEMODE}
const
  versionlib = 'version.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
  {$ENDIF JWA_INCLUDEMODE}  

{$IFDEF DYNAMIC_LINK}

var
  _VerFindFileA: Pointer;

function VerFindFileA;
begin
  GetProcedureAddress(_VerFindFileA, versionlib, 'VerFindFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerFindFileA]
  end;
end;

var
  _VerFindFileW: Pointer;

function VerFindFileW;
begin
  GetProcedureAddress(_VerFindFileW, versionlib, 'VerFindFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerFindFileW]
  end;
end;

var
  _VerFindFile: Pointer;

function VerFindFile;
begin
  GetProcedureAddress(_VerFindFile, versionlib, 'VerFindFile' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerFindFile]
  end;
end;

var
  _VerInstallFileA: Pointer;

function VerInstallFileA;
begin
  GetProcedureAddress(_VerInstallFileA, versionlib, 'VerInstallFileA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerInstallFileA]
  end;
end;

var
  _VerInstallFileW: Pointer;

function VerInstallFileW;
begin
  GetProcedureAddress(_VerInstallFileW, versionlib, 'VerInstallFileW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerInstallFileW]
  end;
end;

var
  _VerInstallFile: Pointer;

function VerInstallFile;
begin
  GetProcedureAddress(_VerInstallFile, versionlib, 'VerInstallFile' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerInstallFile]
  end;
end;

var
  _GetFileVersionInfoSizeA: Pointer;

function GetFileVersionInfoSizeA;
begin
  GetProcedureAddress(_GetFileVersionInfoSizeA, versionlib, 'GetFileVersionInfoSizeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileVersionInfoSizeA]
  end;
end;

var
  _GetFileVersionInfoSizeW: Pointer;

function GetFileVersionInfoSizeW;
begin
  GetProcedureAddress(_GetFileVersionInfoSizeW, versionlib, 'GetFileVersionInfoSizeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileVersionInfoSizeW]
  end;
end;

var
  _GetFileVersionInfoSize: Pointer;

function GetFileVersionInfoSize;
begin
  GetProcedureAddress(_GetFileVersionInfoSize, versionlib, 'GetFileVersionInfoSize' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileVersionInfoSize]
  end;
end;

var
  _GetFileVersionInfoA: Pointer;

function GetFileVersionInfoA;
begin
  GetProcedureAddress(_GetFileVersionInfoA, versionlib, 'GetFileVersionInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileVersionInfoA]
  end;
end;

var
  _GetFileVersionInfoW: Pointer;

function GetFileVersionInfoW;
begin
  GetProcedureAddress(_GetFileVersionInfoW, versionlib, 'GetFileVersionInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileVersionInfoW]
  end;
end;

var
  _GetFileVersionInfo: Pointer;

function GetFileVersionInfo;
begin
  GetProcedureAddress(_GetFileVersionInfo, versionlib, 'GetFileVersionInfo' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetFileVersionInfo]
  end;
end;

var
  _VerLanguageNameA: Pointer;

function VerLanguageNameA;
begin
  GetProcedureAddress(_VerLanguageNameA, versionlib, 'VerLanguageNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerLanguageNameA]
  end;
end;

var
  _VerLanguageNameW: Pointer;

function VerLanguageNameW;
begin
  GetProcedureAddress(_VerLanguageNameW, versionlib, 'VerLanguageNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerLanguageNameW]
  end;
end;

var
  _VerLanguageName: Pointer;

function VerLanguageName;
begin
  GetProcedureAddress(_VerLanguageName, versionlib, 'VerLanguageName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerLanguageName]
  end;
end;

var
  _VerQueryValueA: Pointer;

function VerQueryValueA;
begin
  GetProcedureAddress(_VerQueryValueA, versionlib, 'VerQueryValueA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerQueryValueA]
  end;
end;

var
  _VerQueryValueW: Pointer;

function VerQueryValueW;
begin
  GetProcedureAddress(_VerQueryValueW, versionlib, 'VerQueryValueW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerQueryValueW]
  end;
end;

var
  _VerQueryValue: Pointer;

function VerQueryValue;
begin
  GetProcedureAddress(_VerQueryValue, versionlib, 'VerQueryValue' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_VerQueryValue]
  end;
end;

{$ELSE}

function VerFindFileA; external versionlib name 'VerFindFileA';
function VerFindFileW; external versionlib name 'VerFindFileW';
function VerFindFile; external versionlib name 'VerFindFile' + AWSuffix;
function VerInstallFileA; external versionlib name 'VerInstallFileA';
function VerInstallFileW; external versionlib name 'VerInstallFileW';
function VerInstallFile; external versionlib name 'VerInstallFile' + AWSuffix;
function GetFileVersionInfoSizeA; external versionlib name 'GetFileVersionInfoSizeA';
function GetFileVersionInfoSizeW; external versionlib name 'GetFileVersionInfoSizeW';
function GetFileVersionInfoSize; external versionlib name 'GetFileVersionInfoSize' + AWSuffix;
function GetFileVersionInfoA; external versionlib name 'GetFileVersionInfoA';
function GetFileVersionInfoW; external versionlib name 'GetFileVersionInfoW';
function GetFileVersionInfo; external versionlib name 'GetFileVersionInfo' + AWSuffix;
function VerLanguageNameA; external versionlib name 'VerLanguageNameA';
function VerLanguageNameW; external versionlib name 'VerLanguageNameW';
function VerLanguageName; external versionlib name 'VerLanguageName' + AWSuffix;
function VerQueryValueA; external versionlib name 'VerQueryValueA';
function VerQueryValueW; external versionlib name 'VerQueryValueW';
function VerQueryValue; external versionlib name 'VerQueryValue' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
