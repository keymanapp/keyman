{******************************************************************************}
{                                                                              }
{ Asynchronous RPC API interface Unit for Object Pascal                        }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: rpcnasync.h, released August 2001. The original Pascal }
{ code is: RpcAsync.pas, released December 2000. The initial developer of the  }
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

// $Id: JwaRpcASync.pas,v 1.13 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaRpcASync;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "RpcAsync.h"'}
{$HPPEMIT ''}
{$HPPEMIT 'typedef RPC_EXTENDED_ERROR_INFO* PRPC_EXTENDED_ERROR_INFO'}
{$HPPEMIT 'typedef RPC_ERROR_ENUM_HANDLE* PRPC_ERROR_ENUM_HANDLE'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaRpc, JwaRpcDce, JwaWinBase, JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

type
  _RPC_NOTIFICATION_TYPES = (
    RpcNotificationTypeNone,
    RpcNotificationTypeEvent,
    RpcNotificationTypeApc,
    RpcNotificationTypeIoc,
    RpcNotificationTypeHwnd,
    RpcNotificationTypeCallback);
  {$EXTERNALSYM _RPC_NOTIFICATION_TYPES}
  RPC_NOTIFICATION_TYPES = _RPC_NOTIFICATION_TYPES;
  {$EXTERNALSYM RPC_NOTIFICATION_TYPES}
  TRpcNotificationTypes = RPC_NOTIFICATION_TYPES;

  PFN_RPCNOTIFICATION_ROUTINE = ^RPCNOTIFICATION_ROUTINE;
  {$EXTERNALSYM PFN_RPCNOTIFICATION_ROUTINE}

  _RPC_ASYNC_EVENT = (
    RpcCallComplete,
    RpcSendComplete,
    RpcReceiveComplete);
  {$EXTERNALSYM _RPC_ASYNC_EVENT}
  RPC_ASYNC_EVENT = _RPC_ASYNC_EVENT;
  {$EXTERNALSYM RPC_ASYNC_EVENT}
  TRpcASynchEvent = RPC_ASYNC_EVENT;

  _RPC_ASYNC_STATE = record
    Size: Cardinal; // size of this structure
    Signature: Cardinal;
    Lock: Longint;
    Flags: Cardinal;
    StubInfo: Pointer;
    UserInfo: Pointer;
    RuntimeInfo: Pointer;
    Event: RPC_ASYNC_EVENT;

    NotificationType: RPC_NOTIFICATION_TYPES;
    u: record
    case Integer of
      //
      // Notification by APC
      //
      0: (
        NotificationRoutine: PFN_RPCNOTIFICATION_ROUTINE;
        hThread: HANDLE);

      //
      // Notification by IO completion port
      //
      1: (
        hIOPort: HANDLE;
        dwNumberOfBytesTransferred: DWORD;
        dwCompletionKey: DWORD_PTR;
        lpOverlapped: LPOVERLAPPED);

      //
      // Notification by window message
      //
      2: (
        hWnd: HWND;
        Msg: UINT);

      //
      // Notification by event
      //
      4: (
        hEvent: HANDLE);

      //
      // Notification by callback function
      //
      // This option is available only to OLE
      //
      //5: (
      //  NotificationRoutine: PFN_RPCNOTIFICATION_ROUTINE);
    end;
    Reserved: array [0..3] of LONG_PTR;
  end;
  {$EXTERNALSYM _RPC_ASYNC_STATE}
  RPC_ASYNC_STATE = _RPC_ASYNC_STATE;
  {$EXTERNALSYM RPC_ASYNC_STATE}
  PRPC_ASYNC_STATE = ^RPC_ASYNC_STATE;
  {$EXTERNALSYM PRPC_ASYNC_STATE}
  TRpcASynchState = RPC_ASYNC_STATE;
  PRpcASynchState = PRPC_ASYNC_STATE;

  RPCNOTIFICATION_ROUTINE = procedure(var pAsync: RPC_ASYNC_STATE;
    Context: Pointer; Event: RPC_ASYNC_EVENT); stdcall;
  {$EXTERNALSYM RPCNOTIFICATION_ROUTINE}
//  PFN_RPCNOTIFICATION_ROUTINE = ^RPCNOTIFICATION_ROUTINE;
//  {$EXTERNALSYM PFN_RPCNOTIFICATION_ROUTINE}
  TRpcNotificationRoutine = RPCNOTIFICATION_ROUTINE;
  PRpcNotificationRoutine = PFN_RPCNOTIFICATION_ROUTINE;

// Possible values for Flags

const
  RPC_C_NOTIFY_ON_SEND_COMPLETE    = $1;
  {$EXTERNALSYM RPC_C_NOTIFY_ON_SEND_COMPLETE}
  RPC_C_INFINITE_TIMEOUT           = INFINITE;
  {$EXTERNALSYM RPC_C_INFINITE_TIMEOUT}

function RpcAsyncGetCallHandle(var pAsync: RPC_ASYNC_STATE): Pointer;
{$EXTERNALSYM RpcAsyncGetCallHandle}
function RpcAsyncInitializeHandle(var pAsync: RPC_ASYNC_STATE; Size: Cardinal): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcAsyncInitializeHandle}
function RpcAsyncRegisterInfo(var pAsync: RPC_ASYNC_STATE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcAsyncRegisterInfo}
function RpcAsyncGetCallStatus(var pAsync: RPC_ASYNC_STATE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcAsyncGetCallStatus}
function RpcAsyncCompleteCall(var pAsync: RPC_ASYNC_STATE; Reply: Pointer): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcAsyncCompleteCall}
function RpcAsyncAbortCall(var pAsync: RPC_ASYNC_STATE; ExceptionCode: Cardinal): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcAsyncAbortCall}
function RpcAsyncCancelCall(var pAsync: RPC_ASYNC_STATE; fAbort: BOOL): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcAsyncCancelCall}

{This function is removed due to
http://www.freepascal.org/mantis/view.php?id=10364
and
http://sourceforge.net/tracker/index.php?func=detail&aid=1846986&group_id=121894&atid=694029
function RpcAsyncCleanupThread(dwTimeout: DWORD): RPC_STATUS; stdcall;}
{.$EXTERNALSYM RpcAsyncCleanupThread}

type
  tagExtendedErrorParamTypes = (
    eeptFiller0,
    eeptAnsiString,
    eeptUnicodeString,
    eeptLongVal,
    eeptShortVal,
    eeptPointerVal,
    eeptNone,
    eeptBinary);
  {$EXTERNALSYM tagExtendedErrorParamTypes}
  ExtendedErrorParamTypes = tagExtendedErrorParamTypes;
  {$EXTERNALSYM ExtendedErrorParamTypes}
  TExtendedErrorParamTypes = ExtendedErrorParamTypes;

const
  MaxNumberOfEEInfoParams = 4;
  {$EXTERNALSYM MaxNumberOfEEInfoParams}
  RPC_EEINFO_VERSION      = 1;
  {$EXTERNALSYM RPC_EEINFO_VERSION}

type
  tagBinaryParam = record
    Buffer: Pointer;
    Size: Word;
  end;
  {$EXTERNALSYM tagBinaryParam}
  BinaryParam = tagBinaryParam;
  {$EXTERNALSYM BinaryParam}
  TBinaryParam = BinaryParam;

  tagRPC_EE_INFO_PARAM = record
    ParameterType: ExtendedErrorParamTypes;
    case Integer of
      0: (AnsiString: LPSTR);
      1: (UnicodeString: LPWSTR);
      2: (LVal: Longint);
      3: (SVal: Word);
      4: (PVal: ULONGLONG);
      5: (BVal: BinaryParam);
  end;
  {$EXTERNALSYM tagRPC_EE_INFO_PARAM}
  RPC_EE_INFO_PARAM = tagRPC_EE_INFO_PARAM;
  {$EXTERNALSYM RPC_EE_INFO_PARAM}
  TRpcEeInfoParam = RPC_EE_INFO_PARAM;

const
  EEInfoPreviousRecordsMissing   = 1;
  {$EXTERNALSYM EEInfoPreviousRecordsMissing}
  EEInfoNextRecordsMissing       = 2;
  {$EXTERNALSYM EEInfoNextRecordsMissing}
  EEInfoUseFileTime              = 4;
  {$EXTERNALSYM EEInfoUseFileTime}

  EEInfoGCCOM                    = 11;
  {$EXTERNALSYM EEInfoGCCOM}
  EEInfoGCFRS                    = 12;
  {$EXTERNALSYM EEInfoGCFRS}

type
  tagRPC_EXTENDED_ERROR_INFO = record
    Version: ULONG;
    ComputerName: LPWSTR;
    ProcessID: ULONG;
    u: record
    case Integer of
      0: ( SystemTime: SYSTEMTIME);
      1: (FileTime: FILETIME);
    end;
    GeneratingComponent: ULONG;
    Status: ULONG;
    DetectionLocation: USHORT;
    Flags: USHORT;
    NumberOfParameters: Integer;
    Parameters: array [0..MaxNumberOfEEInfoParams - 1] of RPC_EE_INFO_PARAM;
  end;
  {$EXTERNALSYM tagRPC_EXTENDED_ERROR_INFO}
  RPC_EXTENDED_ERROR_INFO = tagRPC_EXTENDED_ERROR_INFO;
  {$EXTERNALSYM RPC_EXTENDED_ERROR_INFO}
  PRPC_EXTENDED_ERROR_INFO = ^RPC_EXTENDED_ERROR_INFO;
  {$NODEFINE PRPC_EXTENDED_ERROR_INFO}
  TRpcExtendedErrorInfo = RPC_EXTENDED_ERROR_INFO;
  PRpcExtendedErrorInfo = ^RPC_EXTENDED_ERROR_INFO;

  tagRPC_ERROR_ENUM_HANDLE = record
    Signature: ULONG;
    CurrentPos: Pointer;
    Head: Pointer;
  end;
  {$EXTERNALSYM tagRPC_ERROR_ENUM_HANDLE}
  RPC_ERROR_ENUM_HANDLE = tagRPC_ERROR_ENUM_HANDLE;
  {$EXTERNALSYM RPC_ERROR_ENUM_HANDLE}
  PRPC_ERROR_ENUM_HANDLE = ^RPC_ERROR_ENUM_HANDLE;
  {$NODEFINE PRPC_ERROR_ENUM_HANDLE}
  TRpcErrorEnumHandle = RPC_ERROR_ENUM_HANDLE;
  PRpcErrorEnumHandle = PRPC_ERROR_ENUM_HANDLE;

function RpcErrorStartEnumeration(var EnumHandle: RPC_ERROR_ENUM_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorStartEnumeration}
function RpcErrorGetNextRecord(EnumHandle: PRPC_ERROR_ENUM_HANDLE; CopyStrings: BOOL;
  var ErrorInfo: RPC_EXTENDED_ERROR_INFO): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorGetNextRecord}
function RpcErrorEndEnumeration(var EnumHandle: RPC_ERROR_ENUM_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorEndEnumeration}
function RpcErrorResetEnumeration(var EnumHandle: RPC_ERROR_ENUM_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorResetEnumeration}
function RpcErrorGetNumberOfRecords(var EnumHandle: RPC_ERROR_ENUM_HANDLE; var Records: Integer): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorGetNumberOfRecords}
function RpcErrorSaveErrorInfo(EnumHandle: PRPC_ERROR_ENUM_HANDLE; var ErrorBlob: PVOID; var BlobSize: size_t): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorSaveErrorInfo}
function RpcErrorLoadErrorInfo(ErrorBlob: PVOID; BlobSize: size_t; var EnumHandle: RPC_ERROR_ENUM_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorLoadErrorInfo}
function RpcErrorAddRecord(ErrorInfo: PRPC_EXTENDED_ERROR_INFO): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcErrorAddRecord}
procedure RpcErrorClearInformation; stdcall;
{$EXTERNALSYM RpcErrorClearInformation}
function RpcGetAuthorizationContextForClient(ClientBinding: RPC_BINDING_HANDLE; ImpersonateOnReturn: BOOL;
  Reserved1: PVOID; pExpirationTime: PLARGE_INTEGER; Reserved2: LUID; Reserved3: DWORD;
  Reserved4: PVOID; var pAuthzClientContext: PVOID): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcGetAuthorizationContextForClient}
function RpcFreeAuthorizationContext(var pAuthzClientContext: PVOID): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcFreeAuthorizationContext}
function RpcSsContextLockExclusive(ServerBindingHandle: RPC_BINDING_HANDLE; UserContext: PVOID): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcSsContextLockExclusive}
function RpcSsContextLockShared(ServerBindingHandle: RPC_BINDING_HANDLE; UserContext: PVOID): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcSsContextLockShared}

const
  RPC_CALL_ATTRIBUTES_VERSION                        = 1;
  {$EXTERNALSYM RPC_CALL_ATTRIBUTES_VERSION}
  RPC_QUERY_SERVER_PRINCIPAL_NAME                    = 2;
  {$EXTERNALSYM RPC_QUERY_SERVER_PRINCIPAL_NAME}
  RPC_QUERY_CLIENT_PRINCIPAL_NAME                    = 4;
  {$EXTERNALSYM RPC_QUERY_CLIENT_PRINCIPAL_NAME}

type
  tagRPC_CALL_ATTRIBUTES_V1_W = record
    Version: Cardinal;
    Flags: Cardinal;
    ServerPrincipalNameBufferLength: Cardinal;
    ServerPrincipalName: PWord;
    ClientPrincipalNameBufferLength: Cardinal;
    ClientPrincipalName: PWord;
    AuthenticationLevel: Cardinal;
    AuthenticationService: Cardinal;
    NullSession: BOOL;
  end;
  {$EXTERNALSYM tagRPC_CALL_ATTRIBUTES_V1_W}
  RPC_CALL_ATTRIBUTES_V1_W = tagRPC_CALL_ATTRIBUTES_V1_W;
  {$EXTERNALSYM RPC_CALL_ATTRIBUTES_V1_W}
  TRpcCallAttributesV1W = RPC_CALL_ATTRIBUTES_V1_W;

  tagRPC_CALL_ATTRIBUTES_V1_A = record
    Version: Cardinal;
    Flags: Cardinal;
    ServerPrincipalNameBufferLength: Cardinal;
    ServerPrincipalName: PAnsiChar;
    ClientPrincipalNameBufferLength: Cardinal;
    ClientPrincipalName: PAnsiChar;
    AuthenticationLevel: Cardinal;
    AuthenticationService: Cardinal;
    NullSession: BOOL;
  end;
  {$EXTERNALSYM tagRPC_CALL_ATTRIBUTES_V1_A}
  RPC_CALL_ATTRIBUTES_V1_A = tagRPC_CALL_ATTRIBUTES_V1_A;
  {$EXTERNALSYM RPC_CALL_ATTRIBUTES_V1_A}
  TRpcCallAttributesV1A = RPC_CALL_ATTRIBUTES_V1_A;
  {$IFDEF UNICODE}
  RPC_CALL_ATTRIBUTES_V1 = RPC_CALL_ATTRIBUTES_V1_W;
  {$EXTERNALSYM RPC_CALL_ATTRIBUTES_V1}
  TRpcCallAttributesV1 = TRpcCallAttributesV1W;
  {$ELSE}
  RPC_CALL_ATTRIBUTES_V1 = RPC_CALL_ATTRIBUTES_V1_A;
  {$EXTERNALSYM RPC_CALL_ATTRIBUTES_V1}
  TRpcCallAttributesV1 = TRpcCallAttributesV1A;
  {$ENDIF UNICODE}

function RpcServerInqCallAttributesW(ClientBinding: RPC_BINDING_HANDLE; RpcCallAttributes: Pointer): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcServerInqCallAttributesW}
function RpcServerInqCallAttributesA(ClientBinding: RPC_BINDING_HANDLE; RpcCallAttributes: Pointer): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcServerInqCallAttributesA}
function RpcServerInqCallAttributes(ClientBinding: RPC_BINDING_HANDLE; RpcCallAttributes: Pointer): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcServerInqCallAttributes}

type
  RPC_CALL_ATTRIBUTES = RPC_CALL_ATTRIBUTES_V1;
  {$EXTERNALSYM RPC_CALL_ATTRIBUTES}
  TRpcCallAttributes = RPC_CALL_ATTRIBUTES;

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  rpclib = 'rpc4rt.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

function RpcAsyncGetCallHandle(var pAsync: RPC_ASYNC_STATE): Pointer;
begin
 Result := pAsync.RuntimeInfo;
end;

{$IFDEF DYNAMIC_LINK}

var
  _RpcAsyncInitializeHandle: Pointer;

function RpcAsyncInitializeHandle;
begin
  GetProcedureAddress(_RpcAsyncInitializeHandle, rpclib, 'RpcAsyncInitializeHandle');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcAsyncInitializeHandle]
  end;
end;

var
  _RpcAsyncRegisterInfo: Pointer;

function RpcAsyncRegisterInfo;
begin
  GetProcedureAddress(_RpcAsyncRegisterInfo, rpclib, 'RpcAsyncRegisterInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcAsyncRegisterInfo]
  end;
end;

var
  _RpcAsyncGetCallStatus: Pointer;

function RpcAsyncGetCallStatus;
begin
  GetProcedureAddress(_RpcAsyncGetCallStatus, rpclib, 'RpcAsyncGetCallStatus');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcAsyncGetCallStatus]
  end;
end;

var
  _RpcAsyncCompleteCall: Pointer;

function RpcAsyncCompleteCall;
begin
  GetProcedureAddress(_RpcAsyncCompleteCall, rpclib, 'RpcAsyncCompleteCall');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcAsyncCompleteCall]
  end;
end;

var
  _RpcAsyncAbortCall: Pointer;

function RpcAsyncAbortCall;
begin
  GetProcedureAddress(_RpcAsyncAbortCall, rpclib, 'RpcAsyncAbortCall');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcAsyncAbortCall]
  end;
end;

var
  _RpcAsyncCancelCall: Pointer;

function RpcAsyncCancelCall;
begin
  GetProcedureAddress(_RpcAsyncCancelCall, rpclib, 'RpcAsyncCancelCall');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcAsyncCancelCall]
  end;
end;

{
{This function is removed due to
http://www.freepascal.org/mantis/view.php?id=10364
and
http://sourceforge.net/tracker/index.php?func=detail&aid=1846986&group_id=121894&atid=694029

var
  _RpcAsyncCleanupThread: Pointer;

function RpcAsyncCleanupThread;
begin
  GetProcedureAddress(_RpcAsyncCleanupThread, rpclib, 'RpcAsyncCleanupThread');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcAsyncCleanupThread]
  end;
end;
}

var
  _RpcErrorStartEnumeration: Pointer;

function RpcErrorStartEnumeration;
begin
  GetProcedureAddress(_RpcErrorStartEnumeration, rpclib, 'RpcErrorStartEnumeration');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorStartEnumeration]
  end;
end;

var
  _RpcErrorGetNextRecord: Pointer;

function RpcErrorGetNextRecord;
begin
  GetProcedureAddress(_RpcErrorGetNextRecord, rpclib, 'RpcErrorGetNextRecord');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorGetNextRecord]
  end;
end;

var
  _RpcErrorEndEnumeration: Pointer;

function RpcErrorEndEnumeration;
begin
  GetProcedureAddress(_RpcErrorEndEnumeration, rpclib, 'RpcErrorEndEnumeration');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorEndEnumeration]
  end;
end;

var
  _RpcErrorResetEnumeration: Pointer;

function RpcErrorResetEnumeration;
begin
  GetProcedureAddress(_RpcErrorResetEnumeration, rpclib, 'RpcErrorResetEnumeration');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorResetEnumeration]
  end;
end;

var
  _RpcErrorGetNumberOfRecords: Pointer;

function RpcErrorGetNumberOfRecords;
begin
  GetProcedureAddress(_RpcErrorGetNumberOfRecords, rpclib, 'RpcErrorGetNumberOfRecords');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorGetNumberOfRecords]
  end;
end;

var
  _RpcErrorSaveErrorInfo: Pointer;

function RpcErrorSaveErrorInfo;
begin
  GetProcedureAddress(_RpcErrorSaveErrorInfo, rpclib, 'RpcErrorSaveErrorInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorSaveErrorInfo]
  end;
end;

var
  _RpcErrorLoadErrorInfo: Pointer;

function RpcErrorLoadErrorInfo;
begin
  GetProcedureAddress(_RpcErrorLoadErrorInfo, rpclib, 'RpcErrorLoadErrorInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorLoadErrorInfo]
  end;
end;

var
  _RpcErrorAddRecord: Pointer;

function RpcErrorAddRecord;
begin
  GetProcedureAddress(_RpcErrorAddRecord, rpclib, 'RpcErrorAddRecord');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorAddRecord]
  end;
end;

var
  _RpcErrorClearInformation: Pointer;

procedure RpcErrorClearInformation;
begin
  GetProcedureAddress(_RpcErrorClearInformation, rpclib, 'RpcErrorClearInformation');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcErrorClearInformation]
  end;
end;

var
  _RpcGetAuthContextForClient: Pointer;

function RpcGetAuthorizationContextForClient;
begin
  GetProcedureAddress(_RpcGetAuthContextForClient, rpclib, 'RpcGetAuthorizationContextForClient');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcGetAuthContextForClient]
  end;
end;

var
  _RpcFreeAuthorizationContext: Pointer;

function RpcFreeAuthorizationContext;
begin
  GetProcedureAddress(_RpcFreeAuthorizationContext, rpclib, 'RpcFreeAuthorizationContext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcFreeAuthorizationContext]
  end;
end;

var
  _RpcSsContextLockExclusive: Pointer;

function RpcSsContextLockExclusive;
begin
  GetProcedureAddress(_RpcSsContextLockExclusive, rpclib, 'RpcSsContextLockExclusive');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcSsContextLockExclusive]
  end;
end;

var
  _RpcSsContextLockShared: Pointer;

function RpcSsContextLockShared;
begin
  GetProcedureAddress(_RpcSsContextLockShared, rpclib, 'RpcSsContextLockShared');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcSsContextLockShared]
  end;
end;

var
  _RpcServerInqCallAttributesA: Pointer;

function RpcServerInqCallAttributesA;
begin
  GetProcedureAddress(_RpcServerInqCallAttributesA, rpclib, 'RpcServerInqCallAttributesA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcServerInqCallAttributesA]
  end;
end;

var
  _RpcServerInqCallAttributesW: Pointer;

function RpcServerInqCallAttributesW;
begin
  GetProcedureAddress(_RpcServerInqCallAttributesW, rpclib, 'RpcServerInqCallAttributesW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcServerInqCallAttributesW]
  end;
end;

var
  _RpcServerInqCallAttributes: Pointer;

function RpcServerInqCallAttributes;
begin
  GetProcedureAddress(_RpcServerInqCallAttributes, rpclib, 'RpcServerInqCallAttributes' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcServerInqCallAttributes]
  end;
end;

{$ELSE}

function RpcAsyncInitializeHandle; external rpclib name 'RpcAsyncInitializeHandle';
function RpcAsyncRegisterInfo; external rpclib name 'RpcAsyncRegisterInfo';
function RpcAsyncGetCallStatus; external rpclib name 'RpcAsyncGetCallStatus';
function RpcAsyncCompleteCall; external rpclib name 'RpcAsyncCompleteCall';
function RpcAsyncAbortCall; external rpclib name 'RpcAsyncAbortCall';
function RpcAsyncCancelCall; external rpclib name 'RpcAsyncCancelCall';

{This function is removed due to
http://www.freepascal.org/mantis/view.php?id=10364
and
http://sourceforge.net/tracker/index.php?func=detail&aid=1846986&group_id=121894&atid=694029
}
//function RpcAsyncCleanupThread; external rpclib name 'RpcAsyncCleanupThread';
function RpcErrorStartEnumeration; external rpclib name 'RpcErrorStartEnumeration';
function RpcErrorGetNextRecord; external rpclib name 'RpcErrorGetNextRecord';
function RpcErrorEndEnumeration; external rpclib name 'RpcErrorEndEnumeration';
function RpcErrorResetEnumeration; external rpclib name 'RpcErrorResetEnumeration';
function RpcErrorGetNumberOfRecords; external rpclib name 'RpcErrorGetNumberOfRecords';
function RpcErrorSaveErrorInfo; external rpclib name 'RpcErrorSaveErrorInfo';
function RpcErrorLoadErrorInfo; external rpclib name 'RpcErrorLoadErrorInfo';
function RpcErrorAddRecord; external rpclib name 'RpcErrorAddRecord';
procedure RpcErrorClearInformation; external rpclib name 'RpcErrorClearInformation';
function RpcGetAuthorizationContextForClient; external rpclib name 'RpcGetAuthorizationContextForClient';
function RpcFreeAuthorizationContext; external rpclib name 'RpcFreeAuthorizationContext';
function RpcSsContextLockExclusive; external rpclib name 'RpcSsContextLockExclusive';
function RpcSsContextLockShared; external rpclib name 'RpcSsContextLockShared';
function RpcServerInqCallAttributesA; external rpclib name 'RpcServerInqCallAttributesA';
function RpcServerInqCallAttributesW; external rpclib name 'RpcServerInqCallAttributesW';
function RpcServerInqCallAttributes; external rpclib name 'RpcServerInqCallAttributes' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
