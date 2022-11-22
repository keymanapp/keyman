{******************************************************************************}
{                                                                              }
{ Single-Instance Store API interface Unit for Object Pascal                   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: sisbkup.h, released August 2001. The original Pascal   }
{ code is: SisBkUp.pas, released December 2001. The initial developer of the   }
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

// $Id: JwaSisBkUp.pas,v 1.10 2007/09/14 06:48:47 marquardt Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaSisBkUp;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "sisbkup.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_INCLUDEMODE}

function SisCreateBackupStructure(volumeRoot: PWCHAR; var sisBackupStructure: PVOID; var commonStoreRootPathname: PWCHAR;
  countOfCommonStoreFilesToBackup: PULONG; var commonStoreFilesToBackup: PWCHAR): BOOL; stdcall;
{$EXTERNALSYM SisCreateBackupStructure}

function SisCSFilesToBackupForLink(sisBackupStructure, reparseData: PVOID; reparseDataSize: ULONG; thisFileContext: PVOID;
  matchingFileContext: PPVOID; countOfCommonStoreFilesToBackup: PULONG; var commonStoreFilesToBackup: PWCHAR): BOOL; stdcall;
{$EXTERNALSYM SisCSFilesToBackupForLink}

function SisFreeBackupStructure(sisBackupStructure: PVOID): BOOL; stdcall;
{$EXTERNALSYM SisFreeBackupStructure}

function SisCreateRestoreStructure(volumeRoot: PWCHAR; var sisRestoreStructure: PVOID; var commonStoreRootPathname: PWCHAR;
  countOfCommonStoreFilesToRestore: PULONG; var commonStoreFilesToRestore: PWCHAR): BOOL; stdcall;
{$EXTERNALSYM SisCreateRestoreStructure}

function SisRestoredLink(sisRestoreStructure: PVOID; restoredFileName: PWCHAR; reparseData: PVOID; reparseDataSize: ULONG;
  countOfCommonStoreFilesToRestore: PULONG; var commonStoreFilesToRestore: PWCHAR): BOOL; stdcall;
{$EXTERNALSYM SisRestoredLink}

function SisRestoredCommonStoreFile(sisRestoreStructure: PVOID; commonStoreFileName: PWCHAR): BOOL; stdcall;
{$EXTERNALSYM SisRestoredCommonStoreFile}

function SisFreeRestoreStructure(sisRestoreStructure: PVOID): BOOL; stdcall;
{$EXTERNALSYM SisFreeRestoreStructure}

function SisFreeAllocatedMemory(allocatedSpace: PVOID): BOOL; stdcall;
{$EXTERNALSYM SisFreeAllocatedMemory}

{$ENDIF JWA_INCLUDEMODE}

//
// SIS entry function typedefs
//

{$IFNDEF JWA_INCLUDEMODE}
type
  PF_SISCREATEBACKUPSTRUCTURE = function(volumeRoot: PWCHAR; var sisBackupStructure: PVOID; var commonStoreRootPathname: PWCHAR;
    countOfCommonStoreFilesToBackup: PULONG; var commonStoreFilesToBackup: PWCHAR): BOOL; stdcall;
  {$EXTERNALSYM PF_SISCREATEBACKUPSTRUCTURE}
  TSisCreateBackupStructure = PF_SISCREATEBACKUPSTRUCTURE;

  PF_SISCSFILESTOBACKUPFORLINK = function(sisBackupStructure, reparseData: PVOID; reparseDataSize: ULONG; thisFileContext: PVOID;
    matchingFileContext: PPVOID; countOfCommonStoreFilesToBackup: PULONG; var commonStoreFilesToBackup: PWCHAR): BOOL; stdcall;
  {$EXTERNALSYM PF_SISCSFILESTOBACKUPFORLINK}
  TSisCSFilesToBackupForLink = PF_SISCSFILESTOBACKUPFORLINK;

  PF_SISFREEBACKUPSTRUCTURE = function(sisBackupStructure: PVOID): BOOL; stdcall;
  {$EXTERNALSYM PF_SISFREEBACKUPSTRUCTURE}
  TSusFreeBackupStructure = PF_SISFREEBACKUPSTRUCTURE;

  PF_SISCREATERESTORESTRUCTURE = function(volumeRoot: PWCHAR; var sisRestoreStructure: PVOID; var commonStoreRootPathname: PWCHAR;
    countOfCommonStoreFilesToRestore: PULONG; var commonStoreFilesToRestore: PWCHAR): BOOL; stdcall;
  {$EXTERNALSYM PF_SISCREATERESTORESTRUCTURE}
  TSisCreateRestoreStructure = PF_SISCREATERESTORESTRUCTURE;

  PF_SISRESTOREDLINK = function(sisRestoreStructure: PVOID; restoredFileName: PWCHAR; reparseData: PVOID; reparseDataSize: ULONG;
    countOfCommonStoreFilesToRestore: PULONG; var commonStoreFilesToRestore: PWCHAR): BOOL; stdcall;
  {$EXTERNALSYM PF_SISRESTOREDLINK}
  TSisRestoredLink = PF_SISRESTOREDLINK;

  PF_SISRESTOREDCOMMONSTOREFILE = function(sisRestoreStructure: PVOID; commonStoreFileName: PWCHAR): BOOL; stdcall;
  {$EXTERNALSYM PF_SISRESTOREDCOMMONSTOREFILE}
  TSisRestoredCommonStoreFile = PF_SISRESTOREDCOMMONSTOREFILE;

  PF_SISFREERESTORESTRUCTURE = function(sisRestoreStructure: PVOID): BOOL; stdcall;
  {$EXTERNALSYM PF_SISFREERESTORESTRUCTURE}
  TSisFreeRestoreStructure = PF_SISFREERESTORESTRUCTURE;

  PF_SISFREEALLOCATEDMEMORY = function(allocatedSpace: PVOID): BOOL; stdcall;
  {$EXTERNALSYM PF_SISFREEALLOCATEDMEMORY}
  TSisFreeAllocatedMemory = PF_SISFREEALLOCATEDMEMORY;
{$ENDIF JWA_INCLUDEMODE}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  sisbkuplib = 'sisbkup.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

{$IFNDEF JWA_INCLUDEMODE}
var
  _SisCreateBackupStructure: Pointer;

function SisCreateBackupStructure;
begin
  GetProcedureAddress(_SisCreateBackupStructure, sisbkuplib, 'SisCreateBackupStructure');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisCreateBackupStructure]
  end;
end;

var
  _SisCSFilesToBackupForLink: Pointer;

function SisCSFilesToBackupForLink;
begin
  GetProcedureAddress(_SisCSFilesToBackupForLink, sisbkuplib, 'SisCSFilesToBackupForLink');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisCSFilesToBackupForLink]
  end;
end;

var
  _SisFreeBackupStructure: Pointer;

function SisFreeBackupStructure;
begin
  GetProcedureAddress(_SisFreeBackupStructure, sisbkuplib, 'SisFreeBackupStructure');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisFreeBackupStructure]
  end;
end;

var
  _SisCreateRestoreStructure: Pointer;

function SisCreateRestoreStructure;
begin
  GetProcedureAddress(_SisCreateRestoreStructure, sisbkuplib, 'SisCreateRestoreStructure');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisCreateRestoreStructure]
  end;
end;

var
  _SisRestoredLink: Pointer;

function SisRestoredLink;
begin
  GetProcedureAddress(_SisRestoredLink, sisbkuplib, 'SisRestoredLink');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisRestoredLink]
  end;
end;

var
  _SisRestoredCommonStoreFile: Pointer;

function SisRestoredCommonStoreFile;
begin
  GetProcedureAddress(_SisRestoredCommonStoreFile, sisbkuplib, 'SisRestoredCommonStoreFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisRestoredCommonStoreFile]
  end;
end;

var
  _SisFreeRestoreStructure: Pointer;

function SisFreeRestoreStructure;
begin
  GetProcedureAddress(_SisFreeRestoreStructure, sisbkuplib, 'SisFreeRestoreStructure');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisFreeRestoreStructure]
  end;
end;

var
  _SisFreeAllocatedMemory: Pointer;

function SisFreeAllocatedMemory;
begin
  GetProcedureAddress(_SisFreeAllocatedMemory, sisbkuplib, 'SisFreeAllocatedMemory');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SisFreeAllocatedMemory]
  end;
end;

{$ENDIF JWA_INCLUDEMODE}

{$ELSE}

{$IFNDEF JWA_INCLUDEMODE}
function SisCreateBackupStructure; external sisbkuplib name 'SisCreateBackupStructure';
function SisCSFilesToBackupForLink; external sisbkuplib name 'SisCSFilesToBackupForLink';
function SisFreeBackupStructure; external sisbkuplib name 'SisFreeBackupStructure';
function SisCreateRestoreStructure; external sisbkuplib name 'SisCreateRestoreStructure';
function SisRestoredLink; external sisbkuplib name 'SisRestoredLink';
function SisRestoredCommonStoreFile; external sisbkuplib name 'SisRestoredCommonStoreFile';
function SisFreeRestoreStructure; external sisbkuplib name 'SisFreeRestoreStructure';
function SisFreeAllocatedMemory; external sisbkuplib name 'SisFreeAllocatedMemory';

{$ENDIF JWA_INCLUDEMODE}

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
