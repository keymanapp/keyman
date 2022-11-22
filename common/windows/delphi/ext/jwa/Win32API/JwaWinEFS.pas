{******************************************************************************}
{                                                                              }
{ Encrypting File System API interface Unit for Object Pascal                  }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: winefs.h, released June 2000. The original Pascal      }
{ code is: WinEFS.pas, released December 2000. The initial developer of the    }
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

// $Id: JwaWinEFS.pas,v 1.11 2007/09/14 06:48:48 marquardt Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaWinEFS;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "WinEFS.h"'}
{$HPPEMIT ''}
{$HPPEMIT 'typedef PENCRYPTION_CERTIFICATE_HASH *PPENCRYPTION_CERTIFICATE_HASH'}
{$HPPEMIT 'typedef PENCRYPTION_CERTIFICATE *PPENCRYPTION_CERTIFICATE'}
{$HPPEMIT 'typedef PENCRYPTION_CERTIFICATE_HASH_LIST *PPENCRYPTION_CERTIFICATE_HASH_LIST'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinBase, JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_INCLUDEMODE}
type
  ALG_ID = Cardinal;
  {$EXTERNALSYM ALG_ID}
{$ENDIF JWA_INCLUDEMODE}  

//
//  Encoded Certificate
//

type
  PEFS_CERTIFICATE_BLOB = ^EFS_CERTIFICATE_BLOB;
  {$EXTERNALSYM PEFS_CERTIFICATE_BLOB}
  _CERTIFICATE_BLOB = record
    dwCertEncodingType: DWORD;
    cbData: DWORD;
    pbData: PBYTE;
  end;
  {$EXTERNALSYM _CERTIFICATE_BLOB}
  EFS_CERTIFICATE_BLOB = _CERTIFICATE_BLOB;
  {$EXTERNALSYM EFS_CERTIFICATE_BLOB}
  TEfsCertificateBlob = EFS_CERTIFICATE_BLOB;
  PEfsCertificateBlob = PEFS_CERTIFICATE_BLOB;

//
//  Certificate Hash
//

  PEFS_HASH_BLOB = ^EFS_HASH_BLOB;
  {$EXTERNALSYM PEFS_HASH_BLOB}
  _EFS_HASH_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;
  {$EXTERNALSYM _EFS_HASH_BLOB}
  EFS_HASH_BLOB = _EFS_HASH_BLOB;
  {$EXTERNALSYM EFS_HASH_BLOB}
  TEfsHashBlob = EFS_HASH_BLOB;
  PEfsHashBlob = PEFS_HASH_BLOB;

//
//  RPC blob
//

  _EFS_RPC_BLOB = record
    cbData: DWORD;
    pbData: PBYTE;
  end;
  {$EXTERNALSYM _EFS_RPC_BLOB}
  EFS_RPC_BLOB = _EFS_RPC_BLOB;
  {$EXTERNALSYM EFS_RPC_BLOB}
  PEFS_RPC_BLOB = ^EFS_RPC_BLOB;
  {$EXTERNALSYM PEFS_RPC_BLOB}
  TEfsRpcBlob = EFS_RPC_BLOB;
  PEfsRpcBlob = PEFS_RPC_BLOB;

  _EFS_KEY_INFO = record
    dwVersion: DWORD;
    Entropy: ULONG;
    Algorithm: ALG_ID;
    KeyLength: ULONG;
  end;
  {$EXTERNALSYM _EFS_KEY_INFO}
  EFS_KEY_INFO = _EFS_KEY_INFO;
  {$EXTERNALSYM EFS_KEY_INFO}
  PEFS_KEY_INFO = ^EFS_KEY_INFO;
  {$EXTERNALSYM PEFS_KEY_INFO}
  TEfsKeyInfo = EFS_KEY_INFO;
  PEfsKeyInfo = PEFS_KEY_INFO;

//
// Input to add a user to an encrypted file
//

  PENCRYPTION_CERTIFICATE = ^ENCRYPTION_CERTIFICATE;
  {$EXTERNALSYM PENCRYPTION_CERTIFICATE}
  _ENCRYPTION_CERTIFICATE = record
    cbTotalLength: DWORD;
    pUserSid: PSID;
    pCertBlob: PEFS_CERTIFICATE_BLOB;
  end;
  {$EXTERNALSYM _ENCRYPTION_CERTIFICATE}
  ENCRYPTION_CERTIFICATE = _ENCRYPTION_CERTIFICATE;
  {$EXTERNALSYM ENCRYPTION_CERTIFICATE}
  TEncryptionCertificate = ENCRYPTION_CERTIFICATE;
  PEncryptionCertificate = PENCRYPTION_CERTIFICATE;

const
  MAX_SID_SIZE = 256;
  {$EXTERNALSYM MAX_SID_SIZE}

type
  PENCRYPTION_CERTIFICATE_HASH = ^ENCRYPTION_CERTIFICATE_HASH;
  {$EXTERNALSYM PENCRYPTION_CERTIFICATE_HASH}
  _ENCRYPTION_CERTIFICATE_HASH = record
    cbTotalLength: DWORD;
    pUserSid: PSID;
    pHash: PEFS_HASH_BLOB;
    lpDisplayInformation: LPWSTR;
  end;
  {$EXTERNALSYM _ENCRYPTION_CERTIFICATE_HASH}
  ENCRYPTION_CERTIFICATE_HASH = _ENCRYPTION_CERTIFICATE_HASH;
  {$EXTERNALSYM ENCRYPTION_CERTIFICATE_HASH}
  TEncryptionCertificateHash = ENCRYPTION_CERTIFICATE_HASH;
  PEncryptionCertificateHash = PENCRYPTION_CERTIFICATE_HASH;

  PPENCRYPTION_CERTIFICATE_HASH = ^PENCRYPTION_CERTIFICATE_HASH;
  {$NODEFINE PPENCRYPTION_CERTIFICATE_HASH}

  PENCRYPTION_CERTIFICATE_HASH_LIST = ^ENCRYPTION_CERTIFICATE_HASH_LIST;
  {$EXTERNALSYM PENCRYPTION_CERTIFICATE_HASH_LIST}
  _ENCRYPTION_CERTIFICATE_HASH_LIST = record
    nCert_Hash: DWORD;
    pUsers: PPENCRYPTION_CERTIFICATE_HASH;
  end;
  {$EXTERNALSYM _ENCRYPTION_CERTIFICATE_HASH_LIST}
  ENCRYPTION_CERTIFICATE_HASH_LIST = _ENCRYPTION_CERTIFICATE_HASH_LIST;
  {$EXTERNALSYM ENCRYPTION_CERTIFICATE_HASH_LIST}
  TEncryptionCertificateHashList = ENCRYPTION_CERTIFICATE_HASH_LIST;
  PEncryptionCertificateHashList = PENCRYPTION_CERTIFICATE_HASH_LIST;

  PPENCRYPTION_CERTIFICATE = ^PENCRYPTION_CERTIFICATE;
  {$NODEFINE PPENCRYPTION_CERTIFICATE}

  PENCRYPTION_CERTIFICATE_LIST = ^ENCRYPTION_CERTIFICATE_LIST;
  {$EXTERNALSYM PENCRYPTION_CERTIFICATE_LIST}
  _ENCRYPTION_CERTIFICATE_LIST = record
    nUsers: DWORD;
    pUsers: PPENCRYPTION_CERTIFICATE;
  end;
  {$EXTERNALSYM _ENCRYPTION_CERTIFICATE_LIST}
  ENCRYPTION_CERTIFICATE_LIST = _ENCRYPTION_CERTIFICATE_LIST;
  {$EXTERNALSYM ENCRYPTION_CERTIFICATE_LIST}
  TEncryptionCertificateList = ENCRYPTION_CERTIFICATE_LIST;
  PEncryptionCertificateList = PENCRYPTION_CERTIFICATE_LIST;

  PPENCRYPTION_CERTIFICATE_HASH_LIST = ^PENCRYPTION_CERTIFICATE_HASH_LIST;
  {$NODEFINE PPENCRYPTION_CERTIFICATE_HASH_LIST}

function QueryUsersOnEncryptedFile(lpFileName: LPCWSTR;
  var pUsers: PENCRYPTION_CERTIFICATE_HASH_LIST): DWORD; stdcall;
{$EXTERNALSYM QueryUsersOnEncryptedFile}

function QueryRecoveryAgentsOnEncryptedFile(lpFileName: LPCWSTR;
  var pRecoveryAgents: PENCRYPTION_CERTIFICATE_HASH_LIST): DWORD; stdcall;
{$EXTERNALSYM QueryRecoveryAgentsOnEncryptedFile}

function RemoveUsersFromEncryptedFile(lpFileName: LPCWSTR;
  pHashes: PENCRYPTION_CERTIFICATE_HASH_LIST): DWORD; stdcall;
{$EXTERNALSYM RemoveUsersFromEncryptedFile}

function AddUsersToEncryptedFile(lpFileName: LPCWSTR;
  pUsers: PENCRYPTION_CERTIFICATE_LIST): DWORD; stdcall;
{$EXTERNALSYM AddUsersToEncryptedFile}

function SetUserFileEncryptionKey(pEncryptionCertificate: PENCRYPTION_CERTIFICATE): DWORD; stdcall;
{$EXTERNALSYM SetUserFileEncryptionKey}

procedure FreeEncryptionCertificateHashList(pHashes: PENCRYPTION_CERTIFICATE_HASH_LIST); stdcall;
{$EXTERNALSYM FreeEncryptionCertificateHashList}

function EncryptionDisable(DirPath: LPCWSTR; Disable: BOOL): BOOL; stdcall;
{$EXTERNALSYM EncryptionDisable}

function DuplicateEncryptionInfoFile(SrcFileName, DstFileName: LPCWSTR; dwCreationDistribution,
  dwAttributes: DWORD; lpSecurityAttributes: LPSECURITY_ATTRIBUTES): DWORD; stdcall;
{$EXTERNALSYM DuplicateEncryptionInfoFile}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INCLUDEMODE}
const
  advapi32 = 'advapi32.dll';
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INTERFACESECTION}

{$IFDEF DYNAMIC_LINK}

var
  _QueryUsersOnEncryptedFile: Pointer;

function QueryUsersOnEncryptedFile;
begin
  GetProcedureAddress(_QueryUsersOnEncryptedFile, advapi32, 'QueryUsersOnEncryptedFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryUsersOnEncryptedFile]
  end;
end;

var
  _QueryRecoveryAgentsOnEncrFile: Pointer;

function QueryRecoveryAgentsOnEncryptedFile;
begin
  GetProcedureAddress(_QueryRecoveryAgentsOnEncrFile, advapi32, 'QueryRecoveryAgentsOnEncryptedFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_QueryRecoveryAgentsOnEncrFile]
  end;
end;

var
  _RemoveUsersFromEncryptedFile: Pointer;

function RemoveUsersFromEncryptedFile;
begin
  GetProcedureAddress(_RemoveUsersFromEncryptedFile, advapi32, 'RemoveUsersFromEncryptedFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RemoveUsersFromEncryptedFile]
  end;
end;

var
  _AddUsersToEncryptedFile: Pointer;

function AddUsersToEncryptedFile;
begin
  GetProcedureAddress(_AddUsersToEncryptedFile, advapi32, 'AddUsersToEncryptedFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddUsersToEncryptedFile]
  end;
end;

var
  _SetUserFileEncryptionKey: Pointer;

function SetUserFileEncryptionKey;
begin
  GetProcedureAddress(_SetUserFileEncryptionKey, advapi32, 'SetUserFileEncryptionKey');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetUserFileEncryptionKey]
  end;
end;

var
  _FreeEncrCertificateHashList: Pointer;

procedure FreeEncryptionCertificateHashList;
begin
  GetProcedureAddress(_FreeEncrCertificateHashList, advapi32, 'FreeEncryptionCertificateHashList');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeEncrCertificateHashList]
  end;
end;

var
  _EncryptionDisable: Pointer;

function EncryptionDisable;
begin
  GetProcedureAddress(_EncryptionDisable, advapi32, 'EncryptionDisable');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_EncryptionDisable]
  end;
end;

var
  _DuplicateEncryptionInfoFile: Pointer;

function DuplicateEncryptionInfoFile;
begin
  GetProcedureAddress(_DuplicateEncryptionInfoFile, advapi32, 'DuplicateEncryptionInfoFile');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_DuplicateEncryptionInfoFile]
  end;
end;

{$ELSE}

function QueryUsersOnEncryptedFile; external advapi32 name 'QueryUsersOnEncryptedFile';
function QueryRecoveryAgentsOnEncryptedFile; external advapi32 name 'QueryRecoveryAgentsOnEncryptedFile';
function RemoveUsersFromEncryptedFile; external advapi32 name 'RemoveUsersFromEncryptedFile';
function AddUsersToEncryptedFile; external advapi32 name 'AddUsersToEncryptedFile';
function SetUserFileEncryptionKey; external advapi32 name 'SetUserFileEncryptionKey';
procedure FreeEncryptionCertificateHashList; external advapi32 name 'FreeEncryptionCertificateHashList';
function EncryptionDisable; external advapi32 name 'EncryptionDisable';
function DuplicateEncryptionInfoFile; external advapi32 name 'DuplicateEncryptionInfoFile';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}


{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
