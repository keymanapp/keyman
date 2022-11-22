{******************************************************************************}
{                                                                              }
{ Shell Folder Interface Unit for Object Pascal                     		   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2005 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The initial developer of the original translation is Rudy Velthuis		   }
{                                                                              }
{ Portions created by Rudy Velthuis are Copyright (C) 2005-2008                }
{ All Rights Reserved.                                      				   }
{                                                                              }
{ Adapted for JEDI API Library by Christian Wimmer                             }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ The original code is: shfolder.h, released 2004.                			   }
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
{                                                                              }
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSHFolder;
{$I ..\Includes\JediAPILib.inc}

interface

uses 
  JwaWinBase, JwaWinType;

// functions to get shell special folders/
// shfolder.dll supports these on all platforms including Win95, Win98, NT4 and IE4 shell

// all CSIDL values referred to here are supported natively by shfolder.dll, that is they
// will work on all platforms.

{$HPPEMIT '#include <shfolder.h>'}

{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  {$EXTERNALSYM CSIDL_PERSONAL}
  CSIDL_PERSONAL             = $0005;      // My Documents
  {$EXTERNALSYM CSIDL_MYMUSIC}
  CSIDL_MYMUSIC              = $000D;      // "My Music" folder
  {$EXTERNALSYM CSIDL_APPDATA}
  CSIDL_APPDATA              = $001A;      // Application Data, new for NT4
  {$EXTERNALSYM CSIDL_LOCAL_APPDATA}
  CSIDL_LOCAL_APPDATA        = $001C;      // non roaming, user\Local Settings\Application Data
  {$EXTERNALSYM CSIDL_INTERNET_CACHE}
  CSIDL_INTERNET_CACHE       = $0020;
  {$EXTERNALSYM CSIDL_COOKIES}
  CSIDL_COOKIES              = $0021;
  {$EXTERNALSYM CSIDL_HISTORY}
  CSIDL_HISTORY              = $0022;
  {$EXTERNALSYM CSIDL_COMMON_APPDATA}
  CSIDL_COMMON_APPDATA       = $0023;      // All Users\Application Data
  {$EXTERNALSYM CSIDL_WINDOWS}
  CSIDL_WINDOWS              = $0024;      // GetWindowsDirectory()
  {$EXTERNALSYM CSIDL_SYSTEM}
  CSIDL_SYSTEM               = $0025;      // GetSystemDirectory()
  {$EXTERNALSYM CSIDL_PROGRAM_FILES}
  CSIDL_PROGRAM_FILES        = $0026;      // C:\Program Files
  {$EXTERNALSYM CSIDL_MYPICTURES}
  CSIDL_MYPICTURES           = $0027;      // My Pictures, new for Win2K
  {$EXTERNALSYM CSIDL_PROGRAM_FILES_COMMON}
  CSIDL_PROGRAM_FILES_COMMON = $002B;      // C:\Program Files\Common
  {$EXTERNALSYM CSIDL_COMMON_DOCUMENTS}
  CSIDL_COMMON_DOCUMENTS     = $002E;      // All Users\Documents
  {$EXTERNALSYM CSIDL_RESOURCES}
  CSIDL_RESOURCES            = $0038;      // %windir%\Resources\, For theme and other windows resources.
  {$EXTERNALSYM CSIDL_RESOURCES_LOCALIZED}
  CSIDL_RESOURCES_LOCALIZED  = $0039;      // %windir%\Resources\<LangID>, for theme and other windows specific resources.
  {$EXTERNALSYM CSIDL_FLAG_CREATE}
  CSIDL_FLAG_CREATE          = $8000;      // new for Win2K, or this in to force creation of folder
  {$EXTERNALSYM CSIDL_COMMON_ADMINTOOLS}
  CSIDL_COMMON_ADMINTOOLS    = $002F;      // All Users\Start Menu\Programs\Administrative Tools
  {$EXTERNALSYM CSIDL_ADMINTOOLS}
  CSIDL_ADMINTOOLS           = $0030;      // <user name>\Start Menu\Programs\Administrative Tools


{$EXTERNALSYM SHGetFolderPathA}
function SHGetFolderPathA(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PAnsiChar): HRESULT; stdcall;
{$EXTERNALSYM SHGetFolderPathW}
function SHGetFolderPathW(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PWideChar): HRESULT; stdcall;
{$EXTERNALSYM SHGetFolderPath}
function SHGetFolderPath(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PTSTR): HRESULT; stdcall;

// protos so callers can GetProcAddress() from shfolder.dll

type
  {$EXTERNALSYM PFNSHGETFOLDERPATHA}
  PFNSHGETFOLDERPATHA = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PAnsiChar): HRESULT stdcall;
  {$EXTERNALSYM PFNSHGETFOLDERPATHW}
  PFNSHGETFOLDERPATHW = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PWideChar): HRESULT stdcall;
  {$EXTERNALSYM PFNSHGETFOLDERPATH}
  PFNSHGETFOLDERPATH = function(hwnd: HWND; csidl: Integer; hToken: THandle; dwFlags: DWORD; pszPath: PTSTR): HRESULT stdcall;

  TSHGetFolderPathA = PFNSHGETFOLDERPATHA;
  TSHGetFolderPathW = PFNSHGETFOLDERPATHW;
  TSHGetFolderPath = TSHGetFolderPathA;
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}


{$IFNDEF DYNAMIC_LINK}

function SHGetFolderPathA; external ShFolderDll name 'SHGetFolderPathA';
function SHGetFolderPathW; external ShFolderDll name 'SHGetFolderPathW';
function SHGetFolderPath; external ShFolderDll name 'SHGetFolderPath'+ AWSuffix;

{$ELSE}
var
  _SHGetFolderPathA: Pointer;

function SHGetFolderPathA;
begin
  GetProcedureAddress(_SHGetFolderPathA, ShFolderDll, 'SHGetFolderPathA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPathA]
  end;
end;

var
  _SHGetFolderPathW: Pointer;

function SHGetFolderPathW;
begin
  GetProcedureAddress(_SHGetFolderPathW, ShFolderDll, 'SHGetFolderPathW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPathW]
  end;
end;

var
  _SHGetFolderPath: Pointer;

function SHGetFolderPath;
begin
  GetProcedureAddress(_SHGetFolderPath, ShFolderDll, 'SHGetFolderPath'+ AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SHGetFolderPath]
  end;
end;

{$ENDIF DYNAMIC_LINK}


{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
