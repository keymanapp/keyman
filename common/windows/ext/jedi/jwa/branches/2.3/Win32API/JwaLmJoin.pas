{******************************************************************************}
{                                                                              }
{ Lan Manager Join API interface Unit for Object Pascal                        }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: lmjoin.h, released November 2001. The original Pascal  }
{ code is: LmJoin.pas, released Februari 2002. The initial developer of the    }
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

// $Id: JwaLmJoin.pas,v 1.13 2007/09/05 11:58:50 dezipaitor Exp $

{$IFNDEF JWA_OMIT_SECTIONS_LM}
unit JwaLmJoin;

{$WEAKPACKAGEUNIT}

{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaLmCons, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS_LM}

{$HPPEMIT ''}
{$HPPEMIT '#include "lmjoin.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}
//
// Types of name that can be validated
//

type
  _NETSETUP_NAME_TYPE = (
    NetSetupUnknown,
    NetSetupMachine,
    NetSetupWorkgroup,
    NetSetupDomain,
    NetSetupNonExistentDomain
    {$IFDEF WIN2000_UP}
    , NetSetupDnsMachine
    {$ENDIF WIN2000_UP}
    );
  {$EXTERNALSYM _NETSETUP_NAME_TYPE}
  NETSETUP_NAME_TYPE = _NETSETUP_NAME_TYPE;
  {$EXTERNALSYM NETSETUP_NAME_TYPE}
  PNETSETUP_NAME_TYPE = ^NETSETUP_NAME_TYPE;
  {$EXTERNALSYM PNETSETUP_NAME_TYPE}
  TNetSetupNameType = NETSETUP_NAME_TYPE;
  PNetSetupNameType = PNETSETUP_NAME_TYPE;

//
// Status of a workstation
//

  _NETSETUP_JOIN_STATUS = (
    NetSetupUnknownStatus,
    NetSetupUnjoined,
    NetSetupWorkgroupName,
    NetSetupDomainName);
  {$EXTERNALSYM _NETSETUP_JOIN_STATUS}
  NETSETUP_JOIN_STATUS = _NETSETUP_JOIN_STATUS;
  {$EXTERNALSYM NETSETUP_JOIN_STATUS}
  PNETSETUP_JOIN_STATUS = ^NETSETUP_JOIN_STATUS;
  {$EXTERNALSYM PNETSETUP_JOIN_STATUS}
  TNetSetupJoinStatus = NETSETUP_JOIN_STATUS;
  PNetSetupJoinStatus = PNETSETUP_JOIN_STATUS;

//
// Flags to determine the behavior of the join/unjoin APIs
//

const
  NETSETUP_JOIN_DOMAIN   = $00000001; // If not present, workgroup is joined
  {$EXTERNALSYM NETSETUP_JOIN_DOMAIN}
  NETSETUP_ACCT_CREATE   = $00000002; // Do the server side account creation/rename
  {$EXTERNALSYM NETSETUP_ACCT_CREATE}
  NETSETUP_ACCT_DELETE   = $00000004; // Delete the account when a domain is left
  {$EXTERNALSYM NETSETUP_ACCT_DELETE}
  NETSETUP_WIN9X_UPGRADE = $00000010; // Invoked during upgrade of Windows 9x to
  {$EXTERNALSYM NETSETUP_WIN9X_UPGRADE}
                                                // Windows NT
  NETSETUP_DOMAIN_JOIN_IF_JOINED = $00000020; // Allow the client to join a new domain
  {$EXTERNALSYM NETSETUP_DOMAIN_JOIN_IF_JOINED}
                                                // even if it is already joined to a domain
  NETSETUP_JOIN_UNSECURE      = $00000040; // Performs an unsecure join
  {$EXTERNALSYM NETSETUP_JOIN_UNSECURE}
  NETSETUP_MACHINE_PWD_PASSED = $00000080; // Indicates that the machine (not user) password
  {$EXTERNALSYM NETSETUP_MACHINE_PWD_PASSED}
                                                //  is passed. Valid only for unsecure joins
  NETSETUP_DEFER_SPN_SET = $00000100; // Specifies that writting SPN and DnsHostName
  {$EXTERNALSYM NETSETUP_DEFER_SPN_SET}
                                                //  attributes on the computer object should be
                                                //  defered until rename that will follow join

  NETSETUP_INSTALL_INVOCATION = $00040000; // The APIs were invoked during install
  {$EXTERNALSYM NETSETUP_INSTALL_INVOCATION}

  NETSETUP_IGNORE_UNSUPPORTED_FLAGS = $10000000;  // If this bit is set, unrecognized flags
                                                       //  will be ignored by the NetJoin API and
                                                       //  the API will behave as if the flags
                                                       //  were not set.
  {$EXTERNALSYM NETSETUP_IGNORE_UNSUPPORTED_FLAGS}

  NETSETUP_VALID_UNJOIN_FLAGS = NETSETUP_ACCT_DELETE or NETSETUP_IGNORE_UNSUPPORTED_FLAGS;
  {$EXTERNALSYM NETSETUP_VALID_UNJOIN_FLAGS}

//
// 0x80000000 is reserved for internal use only
//

//
// Joins a machine to the domain.
//

function NetJoinDomain(lpServer, lpDomain, lpAccountOU, lpAccount, lpPassword: LPCWSTR; fJoinOptions: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetJoinDomain}

function NetUnjoinDomain(lpServer, lpAccount, lpPassword: LPCWSTR; fUnjoinOptions: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUnjoinDomain}

function NetRenameMachineInDomain(lpServer, lpNewMachineName, lpAccount, lpPassword: LPCWSTR; fRenameOptions: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetRenameMachineInDomain}

//
// Determine the validity of a name
//

function NetValidateName(lpServer, lpName, lpAccount, lpPassword: LPCWSTR; NameType: NETSETUP_NAME_TYPE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetValidateName}

//
// Determines whether a workstation is joined to a domain or not
//

function NetGetJoinInformation(lpServer: LPCWSTR; var lpNameBuffer: LPWSTR; BufferType: PNETSETUP_JOIN_STATUS): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGetJoinInformation}

//
// Determines the list of OUs that the client can create a machine account in
//

function NetGetJoinableOUs(lpServer, lpDomain, lpAccount, lpPassword: LPCWSTR; OUCount: LPDWORD; var OUs: LPLPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetGetJoinableOUs}

//
// Computer rename preparation APIs
//

const
  NET_IGNORE_UNSUPPORTED_FLAGS = $01;
  {$EXTERNALSYM NET_IGNORE_UNSUPPORTED_FLAGS}

function NetAddAlternateComputerName(Server, AlternateName, DomainAccount, DomainAccountPassword: LPCWSTR; Reserved: ULONG): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetAddAlternateComputerName}

function NetRemoveAlternateComputerName(Server, AlternateName, DomainAccount, DomainAccountPassword: LPCWSTR; Reserved: ULONG): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetRemoveAlternateComputerName}

function NetSetPrimaryComputerName(Server, PrimaryName, DomainAccount, DomainAccountPassword: LPCWSTR; Reserved: ULONG): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetSetPrimaryComputerName}

//
// The following enumeration must be kept
// in sync with COMPUTER_NAME_TYPE defined
// in winbase.h
//

type
  _NET_COMPUTER_NAME_TYPE = (
    NetPrimaryComputerName,
    NetAlternateComputerNames,
    NetAllComputerNames,
    NetComputerNameTypeMax);
  {$EXTERNALSYM _NET_COMPUTER_NAME_TYPE}
  NET_COMPUTER_NAME_TYPE = _NET_COMPUTER_NAME_TYPE;
  {$EXTERNALSYM NET_COMPUTER_NAME_TYPE}
  PNET_COMPUTER_NAME_TYPE = ^NET_COMPUTER_NAME_TYPE;
  {$EXTERNALSYM PNET_COMPUTER_NAME_TYPE}
  TNetComputerNameType = NET_COMPUTER_NAME_TYPE;
  PNetComputerNameType = PNET_COMPUTER_NAME_TYPE;

function NetEnumerateComputerNames(Server: LPCWSTR; NameType: NET_COMPUTER_NAME_TYPE; Reserved: ULONG; EntryCount: PDWORD; var ComputerNames: LPLPWSTR): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetEnumerateComputerNames}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS_LM}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS_LM}

{$IFNDEF JWA_INTERFACESECTION}

{$IFDEF DYNAMIC_LINK}

var
  _NetJoinDomain: Pointer;

function NetJoinDomain;
begin
  GetProcedureAddress(_NetJoinDomain, netapi32, 'NetJoinDomain');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetJoinDomain]
  end;
end;

var
  _NetUnjoinDomain: Pointer;

function NetUnjoinDomain;
begin
  GetProcedureAddress(_NetUnjoinDomain, netapi32, 'NetUnjoinDomain');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetUnjoinDomain]
  end;
end;

var
  _NetRenameMachineInDomain: Pointer;

function NetRenameMachineInDomain;
begin
  GetProcedureAddress(_NetRenameMachineInDomain, netapi32, 'NetRenameMachineInDomain');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetRenameMachineInDomain]
  end;
end;

var
  _NetValidateName: Pointer;

function NetValidateName;
begin
  GetProcedureAddress(_NetValidateName, netapi32, 'NetValidateName');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetValidateName]
  end;
end;

var
  _NetGetJoinInformation: Pointer;

function NetGetJoinInformation;
begin
  GetProcedureAddress(_NetGetJoinInformation, netapi32, 'NetGetJoinInformation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetGetJoinInformation]
  end;
end;

var
  _NetGetJoinableOUs: Pointer;

function NetGetJoinableOUs;
begin
  GetProcedureAddress(_NetGetJoinableOUs, netapi32, 'NetGetJoinableOUs');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetGetJoinableOUs]
  end;
end;

var
  _NetAddAlternateComputerName: Pointer;

function NetAddAlternateComputerName;
begin
  GetProcedureAddress(_NetAddAlternateComputerName, netapi32, 'NetAddAlternateComputerName');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetAddAlternateComputerName]
  end;
end;

var
  _NetRemoveAlternateComputerName: Pointer;

function NetRemoveAlternateComputerName;
begin
  GetProcedureAddress(_NetRemoveAlternateComputerName, netapi32, 'NetRemoveAlternateComputerName');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetRemoveAlternateComputerName]
  end;
end;

var
  _NetSetPrimaryComputerName: Pointer;

function NetSetPrimaryComputerName;
begin
  GetProcedureAddress(_NetSetPrimaryComputerName, netapi32, 'NetSetPrimaryComputerName');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetSetPrimaryComputerName]
  end;
end;

var
  _NetEnumerateComputerNames: Pointer;

function NetEnumerateComputerNames;
begin
  GetProcedureAddress(_NetEnumerateComputerNames, netapi32, 'NetEnumerateComputerNames');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetEnumerateComputerNames]
  end;
end;

{$ELSE}

function NetJoinDomain; external netapi32 name 'NetJoinDomain';
function NetUnjoinDomain; external netapi32 name 'NetUnjoinDomain';
function NetRenameMachineInDomain; external netapi32 name 'NetRenameMachineInDomain';
function NetValidateName; external netapi32 name 'NetValidateName';
function NetGetJoinInformation; external netapi32 name 'NetGetJoinInformation';
function NetGetJoinableOUs; external netapi32 name 'NetGetJoinableOUs';
function NetAddAlternateComputerName; external netapi32 name 'NetAddAlternateComputerName';
function NetRemoveAlternateComputerName; external netapi32 name 'NetRemoveAlternateComputerName';
function NetSetPrimaryComputerName; external netapi32 name 'NetSetPrimaryComputerName';
function NetEnumerateComputerNames; external netapi32 name 'NetEnumerateComputerNames';

{$ENDIF DYNAMIC_LINK}
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS_LM}
end.
{$ENDIF JWA_OMIT_SECTIONS_LM}
