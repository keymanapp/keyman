{******************************************************************************}
{                                                                              }
{ WinCypt dependent RPC API interface Unit for Object Pascal                   }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: rpcssl.h, released June 2000. The original Pascal      }
{ code is: RpcSsl.pas, released April 2003. The initial developer of the       }
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

// $Id: JwaRpcSsl.pas,v 1.11 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaRpcSsl;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "rpcssl.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaRpc, JwaWinCrypt, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

function RpcCertGeneratePrincipalNameW(Context: PCCERT_CONTEXT; Flags: DWORD; out pBuffer: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcCertGeneratePrincipalNameW}
function RpcCertGeneratePrincipalNameA(Context: PCCERT_CONTEXT; Flags: DWORD; out pBuffer: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcCertGeneratePrincipalNameA}
function RpcCertGeneratePrincipalName(Context: PCCERT_CONTEXT; Flags: DWORD; out pBuffer: PTCHAR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcCertGeneratePrincipalName}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  rpclib = 'rpcrt4.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _RpcCertGeneratePrincipalNameW: Pointer;

function RpcCertGeneratePrincipalNameW;
begin
  GetProcedureAddress(_RpcCertGeneratePrincipalNameW, rpclib, 'RpcCertGeneratePrincipalNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcCertGeneratePrincipalNameW]
  end;
end;

var
  _RpcCertGeneratePrincipalNameA: Pointer;

function RpcCertGeneratePrincipalNameA;
begin
  GetProcedureAddress(_RpcCertGeneratePrincipalNameA, rpclib, 'RpcCertGeneratePrincipalNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcCertGeneratePrincipalNameA]
  end;
end;

var
  _RpcCertGeneratePrincipalName: Pointer;

function RpcCertGeneratePrincipalName;
begin
  GetProcedureAddress(_RpcCertGeneratePrincipalName, rpclib, 'RpcCertGeneratePrincipalName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcCertGeneratePrincipalName]
  end;
end;

{$ELSE}

function RpcCertGeneratePrincipalNameW; external rpclib name 'RpcCertGeneratePrincipalNameW';
function RpcCertGeneratePrincipalNameA; external rpclib name 'RpcCertGeneratePrincipalNameA';
function RpcCertGeneratePrincipalName; external rpclib name 'RpcCertGeneratePrincipalName' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
