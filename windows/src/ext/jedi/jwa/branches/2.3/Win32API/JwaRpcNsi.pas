{******************************************************************************}
{                                                                              }
{ RPC NSI API interface Unit for Object Pascal                                 }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: rpcnsi.h, released June 2000. The original Pascal      }
{ code is: RpcNsi.pas, released December 2000. The initial developer of the    }
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

// $Id: JwaRpcNsi.pas,v 1.12 2007/09/05 11:58:52 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaRpcNsi;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "RpcNsi.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinType, JwaRpc, JwaRpcDce;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

type
  RPC_NS_HANDLE = Pointer;
  {$EXTERNALSYM RPC_NS_HANDLE}

const
  RPC_C_NS_SYNTAX_DEFAULT = 0;
  {$EXTERNALSYM RPC_C_NS_SYNTAX_DEFAULT}
  RPC_C_NS_SYNTAX_DCE = 3;
  {$EXTERNALSYM RPC_C_NS_SYNTAX_DCE}

  RPC_C_PROFILE_DEFAULT_ELT = 0;
  {$EXTERNALSYM RPC_C_PROFILE_DEFAULT_ELT}
  RPC_C_PROFILE_ALL_ELT = 1;
  {$EXTERNALSYM RPC_C_PROFILE_ALL_ELT}
  RPC_C_PROFILE_ALL_ELTS = RPC_C_PROFILE_ALL_ELT;
  {$EXTERNALSYM RPC_C_PROFILE_ALL_ELTS}
  RPC_C_PROFILE_MATCH_BY_IF = 2;
  {$EXTERNALSYM RPC_C_PROFILE_MATCH_BY_IF}
  RPC_C_PROFILE_MATCH_BY_MBR = 3;
  {$EXTERNALSYM RPC_C_PROFILE_MATCH_BY_MBR}
  RPC_C_PROFILE_MATCH_BY_BOTH = 4;
  {$EXTERNALSYM RPC_C_PROFILE_MATCH_BY_BOTH}

  RPC_C_NS_DEFAULT_EXP_AGE = -1;
  {$EXTERNALSYM RPC_C_NS_DEFAULT_EXP_AGE}

// Server APIs

function RpcNsBindingExportA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  IfSpec: RPC_IF_HANDLE; BindingVec: PRPC_BINDING_VECTOR;
  ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingExportA}
function RpcNsBindingExportW(EntryNameSyntax: Longword; EntryName: PWideChar;
  IfSpec: RPC_IF_HANDLE; BindingVec: PRPC_BINDING_VECTOR;
  ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingExportW}
function RpcNsBindingExport(EntryNameSyntax: Longword; EntryName: PTSTR;
  IfSpec: RPC_IF_HANDLE; BindingVec: PRPC_BINDING_VECTOR;
  ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingExport}

function RpcNsBindingUnexportA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  IfSpec: RPC_IF_HANDLE; ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingUnexportA}
function RpcNsBindingUnexportW(EntryNameSyntax: Longword; EntryName: PWideChar;
  IfSpec: RPC_IF_HANDLE; ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingUnexportW}
function RpcNsBindingUnexport(EntryNameSyntax: Longword; EntryName: PTSTR;
  IfSpec: RPC_IF_HANDLE; ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingUnexport}

// Server PnP APIs

function RpcNsBindingExportPnPA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  IfSpec: RPC_IF_HANDLE; ObjectVector: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingExportPnPA}
function RpcNsBindingExportPnPW(EntryNameSyntax: Longword; EntryName: PWideChar;
  IfSpec: RPC_IF_HANDLE; ObjectVector: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingExportPnPW}
function RpcNsBindingExportPnP(EntryNameSyntax: Longword; EntryName: PTSTR;
  IfSpec: RPC_IF_HANDLE; ObjectVector: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingExportPnP}

function RpcNsBindingUnexportPnPA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  IfSpec: RPC_IF_HANDLE; ObjectVector: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingUnexportPnPA}
function RpcNsBindingUnexportPnPW(EntryNameSyntax: Longword; EntryName: PWideChar;
  IfSpec: RPC_IF_HANDLE; ObjectVector: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingUnexportPnPW}
function RpcNsBindingUnexportPnP(EntryNameSyntax: Longword; EntryName: PTSTR;
  IfSpec: RPC_IF_HANDLE; ObjectVector: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingUnexportPnP}

// Client APIs

function RpcNsBindingLookupBeginA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  IfSpec: RPC_IF_HANDLE; ObjUuid: PUUID; BindingMaxCount: Longword;
  var LookupContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingLookupBeginA}
function RpcNsBindingLookupBeginW(EntryNameSyntax: Longword; EntryName: PWideChar;
  IfSpec: RPC_IF_HANDLE; ObjUuid: PUUID; BindingMaxCount: Longword;
  var LookupContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingLookupBeginW}
function RpcNsBindingLookupBegin(EntryNameSyntax: Longword; EntryName: PTSTR;
  IfSpec: RPC_IF_HANDLE; ObjUuid: PUUID; BindingMaxCount: Longword;
  var LookupContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingLookupBegin}

function RpcNsBindingLookupNext(LookupContext: RPC_NS_HANDLE;
  var BindingVec: PRPC_BINDING_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingLookupNext}

function RpcNsBindingLookupDone(var LookupContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingLookupDone}

// Group APIs

function RpcNsGroupDeleteA(GroupNameSyntax: Longword; GroupName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupDeleteA}
function RpcNsGroupDeleteW(GroupNameSyntax: Longword; GroupName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupDeleteW}
function RpcNsGroupDelete(GroupNameSyntax: Longword; GroupName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupDelete}

function RpcNsGroupMbrAddA(GroupNameSyntax: Longword; GroupName: PAnsiChar;
  MemberNameSyntax: Longword; MemberName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrAddA}
function RpcNsGroupMbrAddW(GroupNameSyntax: Longword; GroupName: PWideChar;
  MemberNameSyntax: Longword; MemberName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrAddW}
function RpcNsGroupMbrAdd(GroupNameSyntax: Longword; GroupName: PTSTR;
  MemberNameSyntax: Longword; MemberName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrAdd}

function RpcNsGroupMbrRemoveA(GroupNameSyntax: Longword; GroupName: PAnsiChar;
  MemberNameSyntax: Longword; MemberName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrRemoveA}
function RpcNsGroupMbrRemoveW(GroupNameSyntax: Longword; GroupName: PWideChar;
  MemberNameSyntax: Longword; MemberName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrRemoveW}
function RpcNsGroupMbrRemove(GroupNameSyntax: Longword; GroupName: PTSTR;
  MemberNameSyntax: Longword; MemberName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrRemove}

function RpcNsGroupMbrInqBeginA(GroupNameSyntax: Longword; GroupName: PAnsiChar;
  MemberNameSyntax: Longword; var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrInqBeginA}
function RpcNsGroupMbrInqBeginW(GroupNameSyntax: Longword; GroupName: PWideChar;
  MemberNameSyntax: Longword; var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrInqBeginW}
function RpcNsGroupMbrInqBegin(GroupNameSyntax: Longword; GroupName: PTSTR;
  MemberNameSyntax: Longword; var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrInqBegin}

function RpcNsGroupMbrInqNextA(InquiryContext: RPC_NS_HANDLE; MemberName: PPAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrInqNextA}
function RpcNsGroupMbrInqNextW(InquiryContext: RPC_NS_HANDLE; MemberName: PPWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrInqNextW}
function RpcNsGroupMbrInqNext(InquiryContext: RPC_NS_HANDLE; MemberName: PPTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrInqNext}

function RpcNsGroupMbrInqDone(var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsGroupMbrInqDone}

// Profile APIs

function RpcNsProfileDeleteA(ProfileNameSyntax: Longword; ProfileName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileDeleteA}
function RpcNsProfileDeleteW(ProfileNameSyntax: Longword; ProfileName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileDeleteW}
function RpcNsProfileDelete(ProfileNameSyntax: Longword; ProfileName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileDelete}

function RpcNsProfileEltAddA(ProfileNameSyntax: Longword; ProfileName: PAnsiChar;
  IfId: PRPC_IF_ID; MemberNameSyntax: Longword; MemberName: PAnsiChar;
  Priority: Longword; Annotation: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltAddA}
function RpcNsProfileEltAddW(ProfileNameSyntax: Longword; ProfileName: PWideChar;
  IfId: PRPC_IF_ID; MemberNameSyntax: Longword; MemberName: PWideChar;
  Priority: Longword; Annotation: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltAddW}
function RpcNsProfileEltAdd(ProfileNameSyntax: Longword; ProfileName: PTSTR;
  IfId: PRPC_IF_ID; MemberNameSyntax: Longword; MemberName: PTSTR;
  Priority: Longword; Annotation: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltAdd}

function RpcNsProfileEltRemoveA(ProfileNameSyntax: Longword; ProfileName: PAnsiChar;
  IfId: PRPC_IF_ID; MemberNameSyntax: Longword; MemberName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltRemoveA}
function RpcNsProfileEltRemoveW(ProfileNameSyntax: Longword; ProfileName: PWideChar;
  IfId: PRPC_IF_ID; MemberNameSyntax: Longword; MemberName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltRemoveW}
function RpcNsProfileEltRemove(ProfileNameSyntax: Longword; ProfileName: PTSTR;
  IfId: PRPC_IF_ID; MemberNameSyntax: Longword; MemberName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltRemove}

function RpcNsProfileEltInqBeginA(ProfileNameSyntax: Longword; ProfileName: PAnsiChar;
  InquiryType: Longword; IfId: PRPC_IF_ID; VersOption, MemberNameSyntax: Longword;
  MemberName: PAnsiChar; var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltInqBeginA}
function RpcNsProfileEltInqBeginW(ProfileNameSyntax: Longword; ProfileName: PWideChar;
  InquiryType: Longword; IfId: PRPC_IF_ID; VersOption, MemberNameSyntax: Longword;
  MemberName: PWideChar; var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltInqBeginW}
function RpcNsProfileEltInqBegin(ProfileNameSyntax: Longword; ProfileName: PTSTR;
  InquiryType: Longword; IfId: PRPC_IF_ID; VersOption, MemberNameSyntax: Longword;
  MemberName: PTSTR; var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltInqBegin}

function RpcNsProfileEltInqNextA(InquiryContext: RPC_NS_HANDLE; var IfId: RPC_IF_ID;
  MemberName: PPAnsiChar; var Priority: Longword; Annotation: PPAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltInqNextA}
function RpcNsProfileEltInqNextW(InquiryContext: RPC_NS_HANDLE; var IfId: RPC_IF_ID;
  MemberName: PPWideChar; var Priority: Longword; Annotation: PPWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltInqNextW}
function RpcNsProfileEltInqNext(InquiryContext: RPC_NS_HANDLE; var IfId: RPC_IF_ID;
  MemberName: PPTSTR; var Priority: Longword; Annotation: PPTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltInqNext}

function RpcNsProfileEltInqDone(var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsProfileEltInqDone}

// Entry object APIs

function RpcNsEntryObjectInqBeginA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryObjectInqBeginA}
function RpcNsEntryObjectInqBeginW(EntryNameSyntax: Longword; EntryName: PWideChar;
  var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryObjectInqBeginW}
function RpcNsEntryObjectInqBegin(EntryNameSyntax: Longword; EntryName: PTSTR;
  var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryObjectInqBegin}

function RpcNsEntryObjectInqNext(InquiryContext: RPC_NS_HANDLE; ObjUuid: PUUID): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryObjectInqNext}

function RpcNsEntryObjectInqDone(var InquiryContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryObjectInqDone}

// Management and MISC APIs

function RpcNsEntryExpandNameA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  var ExpandedName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryExpandNameA}
function RpcNsEntryExpandNameW(EntryNameSyntax: Longword; EntryName: PWideChar;
  var ExpandedName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryExpandNameW}
function RpcNsEntryExpandName(EntryNameSyntax: Longword; EntryName: PTSTR;
  var ExpandedName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsEntryExpandName}

function RpcNsMgmtBindingUnexportA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  IfId: PRPC_IF_ID; VersOption: Longword; ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtBindingUnexportA}
function RpcNsMgmtBindingUnexportW(EntryNameSyntax: Longword; EntryName: PWideChar;
  IfId: PRPC_IF_ID; VersOption: Longword; ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtBindingUnexportW}
function RpcNsMgmtBindingUnexport(EntryNameSyntax: Longword; EntryName: PTSTR;
  IfId: PRPC_IF_ID; VersOption: Longword; ObjectUuidVec: PUUID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtBindingUnexport}

function RpcNsMgmtEntryCreateA(EntryNameSyntax: Longword; EntryName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryCreateA}
function RpcNsMgmtEntryCreateW(EntryNameSyntax: Longword; EntryName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryCreateW}
function RpcNsMgmtEntryCreate(EntryNameSyntax: Longword; EntryName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryCreate}

function RpcNsMgmtEntryDeleteA(EntryNameSyntax: Longword; EntryName: PAnsiChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryDeleteA}
function RpcNsMgmtEntryDeleteW(EntryNameSyntax: Longword; EntryName: PWideChar): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryDeleteW}
function RpcNsMgmtEntryDelete(EntryNameSyntax: Longword; EntryName: PTSTR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryDelete}

function RpcNsMgmtEntryInqIfIdsA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  var IfIdVec: PRPC_IF_ID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryInqIfIdsA}
function RpcNsMgmtEntryInqIfIdsW(EntryNameSyntax: Longword; EntryName: PWideChar;
  var IfIdVec: PRPC_IF_ID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryInqIfIdsW}
function RpcNsMgmtEntryInqIfIds(EntryNameSyntax: Longword; EntryName: PTSTR;
  var IfIdVec: PRPC_IF_ID_VECTOR): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtEntryInqIfIds}

function RpcNsMgmtHandleSetExpAge(NsHandle: RPC_NS_HANDLE;
  ExpirationAge: Longword): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtHandleSetExpAge}

function RpcNsMgmtInqExpAge(var ExpirationAge: Longword): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtInqExpAge}

function RpcNsMgmtSetExpAge(ExpirationAge: Longword): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsMgmtSetExpAge}

// Client API's implemented in wrappers.

function RpcNsBindingImportBeginA(EntryNameSyntax: Longword; EntryName: PAnsiChar;
  IfSpec: RPC_IF_HANDLE; ObjUuid: PUUID; var ImportContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingImportBeginA}
function RpcNsBindingImportBeginW(EntryNameSyntax: Longword; EntryName: PWideChar;
  IfSpec: RPC_IF_HANDLE; ObjUuid: PUUID; var ImportContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingImportBeginW}
function RpcNsBindingImportBegin(EntryNameSyntax: Longword; EntryName: PTSTR;
  IfSpec: RPC_IF_HANDLE; ObjUuid: PUUID; var ImportContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingImportBegin}

function RpcNsBindingImportNext(ImportContext: RPC_NS_HANDLE;
  var Binding: RPC_BINDING_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingImportNext}

function RpcNsBindingImportDone(var ImportContext: RPC_NS_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingImportDone}

function RpcNsBindingSelect(BindingVec: PRPC_BINDING_VECTOR; var Binding: RPC_BINDING_HANDLE): RPC_STATUS; stdcall;
{$EXTERNALSYM RpcNsBindingSelect}

{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}



{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  rpcns4 = 'rpcns4.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _RpcNsBindingExportA: Pointer;

function RpcNsBindingExportA;
begin
  GetProcedureAddress(_RpcNsBindingExportA, rpcns4, 'RpcNsBindingExportA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingExportA]
  end;
end;

var
  _RpcNsBindingExportW: Pointer;

function RpcNsBindingExportW;
begin
  GetProcedureAddress(_RpcNsBindingExportW, rpcns4, 'RpcNsBindingExportW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingExportW]
  end;
end;

var
  _RpcNsBindingExport: Pointer;

function RpcNsBindingExport;
begin
  GetProcedureAddress(_RpcNsBindingExport, rpcns4, 'RpcNsBindingExport' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingExport]
  end;
end;

var
  _RpcNsBindingUnexportA: Pointer;

function RpcNsBindingUnexportA;
begin
  GetProcedureAddress(_RpcNsBindingUnexportA, rpcns4, 'RpcNsBindingUnexportA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingUnexportA]
  end;
end;

var
  _RpcNsBindingUnexportW: Pointer;

function RpcNsBindingUnexportW;
begin
  GetProcedureAddress(_RpcNsBindingUnexportW, rpcns4, 'RpcNsBindingUnexportW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingUnexportW]
  end;
end;

var
  _RpcNsBindingUnexport: Pointer;

function RpcNsBindingUnexport;
begin
  GetProcedureAddress(_RpcNsBindingUnexport, rpcns4, 'RpcNsBindingUnexport' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingUnexport]
  end;
end;

var
  _RpcNsBindingExportPnPA: Pointer;

function RpcNsBindingExportPnPA;
begin
  GetProcedureAddress(_RpcNsBindingExportPnPA, rpcns4, 'RpcNsBindingExportPnPA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingExportPnPA]
  end;
end;

var
  _RpcNsBindingExportPnPW: Pointer;

function RpcNsBindingExportPnPW;
begin
  GetProcedureAddress(_RpcNsBindingExportPnPW, rpcns4, 'RpcNsBindingExportPnPW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingExportPnPW]
  end;
end;

var
  _RpcNsBindingExportPnP: Pointer;

function RpcNsBindingExportPnP;
begin
  GetProcedureAddress(_RpcNsBindingExportPnP, rpcns4, 'RpcNsBindingExportPnP' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingExportPnP]
  end;
end;

var
  _RpcNsBindingUnexportPnPA: Pointer;

function RpcNsBindingUnexportPnPA;
begin
  GetProcedureAddress(_RpcNsBindingUnexportPnPA, rpcns4, 'RpcNsBindingUnexportPnPA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingUnexportPnPA]
  end;
end;

var
  _RpcNsBindingUnexportPnPW: Pointer;

function RpcNsBindingUnexportPnPW;
begin
  GetProcedureAddress(_RpcNsBindingUnexportPnPW, rpcns4, 'RpcNsBindingUnexportPnPW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingUnexportPnPW]
  end;
end;

var
  _RpcNsBindingUnexportPnP: Pointer;

function RpcNsBindingUnexportPnP;
begin
  GetProcedureAddress(_RpcNsBindingUnexportPnP, rpcns4, 'RpcNsBindingUnexportPnP' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingUnexportPnP]
  end;
end;

var
  _RpcNsBindingLookupBeginA: Pointer;

function RpcNsBindingLookupBeginA;
begin
  GetProcedureAddress(_RpcNsBindingLookupBeginA, rpcns4, 'RpcNsBindingLookupBeginA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingLookupBeginA]
  end;
end;

var
  _RpcNsBindingLookupBeginW: Pointer;

function RpcNsBindingLookupBeginW;
begin
  GetProcedureAddress(_RpcNsBindingLookupBeginW, rpcns4, 'RpcNsBindingLookupBeginW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingLookupBeginW]
  end;
end;

var
  _RpcNsBindingLookupBegin: Pointer;

function RpcNsBindingLookupBegin;
begin
  GetProcedureAddress(_RpcNsBindingLookupBegin, rpcns4, 'RpcNsBindingLookupBegin' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingLookupBegin]
  end;
end;

var
  _RpcNsBindingLookupNext: Pointer;

function RpcNsBindingLookupNext;
begin
  GetProcedureAddress(_RpcNsBindingLookupNext, rpcns4, 'RpcNsBindingLookupNext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingLookupNext]
  end;
end;

var
  _RpcNsBindingLookupDone: Pointer;

function RpcNsBindingLookupDone;
begin
  GetProcedureAddress(_RpcNsBindingLookupDone, rpcns4, 'RpcNsBindingLookupDone');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingLookupDone]
  end;
end;

var
  _RpcNsGroupDeleteA: Pointer;

function RpcNsGroupDeleteA;
begin
  GetProcedureAddress(_RpcNsGroupDeleteA, rpcns4, 'RpcNsGroupDeleteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupDeleteA]
  end;
end;

var
  _RpcNsGroupDeleteW: Pointer;

function RpcNsGroupDeleteW;
begin
  GetProcedureAddress(_RpcNsGroupDeleteW, rpcns4, 'RpcNsGroupDeleteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupDeleteW]
  end;
end;

var
  _RpcNsGroupDelete: Pointer;

function RpcNsGroupDelete;
begin
  GetProcedureAddress(_RpcNsGroupDelete, rpcns4, 'RpcNsGroupDelete' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupDelete]
  end;
end;

var
  _RpcNsGroupMbrAddA: Pointer;

function RpcNsGroupMbrAddA;
begin
  GetProcedureAddress(_RpcNsGroupMbrAddA, rpcns4, 'RpcNsGroupMbrAddA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrAddA]
  end;
end;

var
  _RpcNsGroupMbrAddW: Pointer;

function RpcNsGroupMbrAddW;
begin
  GetProcedureAddress(_RpcNsGroupMbrAddW, rpcns4, 'RpcNsGroupMbrAddW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrAddW]
  end;
end;

var
  _RpcNsGroupMbrAdd: Pointer;

function RpcNsGroupMbrAdd;
begin
  GetProcedureAddress(_RpcNsGroupMbrAdd, rpcns4, 'RpcNsGroupMbrAdd' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrAdd]
  end;
end;

var
  _RpcNsGroupMbrRemoveA: Pointer;

function RpcNsGroupMbrRemoveA;
begin
  GetProcedureAddress(_RpcNsGroupMbrRemoveA, rpcns4, 'RpcNsGroupMbrRemoveA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrRemoveA]
  end;
end;

var
  _RpcNsGroupMbrRemoveW: Pointer;

function RpcNsGroupMbrRemoveW;
begin
  GetProcedureAddress(_RpcNsGroupMbrRemoveW, rpcns4, 'RpcNsGroupMbrRemoveW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrRemoveW]
  end;
end;

var
  _RpcNsGroupMbrRemove: Pointer;

function RpcNsGroupMbrRemove;
begin
  GetProcedureAddress(_RpcNsGroupMbrRemove, rpcns4, 'RpcNsGroupMbrRemove' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrRemove]
  end;
end;

var
  _RpcNsGroupMbrInqBeginA: Pointer;

function RpcNsGroupMbrInqBeginA;
begin
  GetProcedureAddress(_RpcNsGroupMbrInqBeginA, rpcns4, 'RpcNsGroupMbrInqBeginA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrInqBeginA]
  end;
end;

var
  _RpcNsGroupMbrInqBeginW: Pointer;

function RpcNsGroupMbrInqBeginW;
begin
  GetProcedureAddress(_RpcNsGroupMbrInqBeginW, rpcns4, 'RpcNsGroupMbrInqBeginW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrInqBeginW]
  end;
end;

var
  _RpcNsGroupMbrInqBegin: Pointer;

function RpcNsGroupMbrInqBegin;
begin
  GetProcedureAddress(_RpcNsGroupMbrInqBegin, rpcns4, 'RpcNsGroupMbrInqBegin' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrInqBegin]
  end;
end;

var
  _RpcNsGroupMbrInqNextA: Pointer;

function RpcNsGroupMbrInqNextA;
begin
  GetProcedureAddress(_RpcNsGroupMbrInqNextA, rpcns4, 'RpcNsGroupMbrInqNextA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrInqNextA]
  end;
end;

var
  _RpcNsGroupMbrInqNextW: Pointer;

function RpcNsGroupMbrInqNextW;
begin
  GetProcedureAddress(_RpcNsGroupMbrInqNextW, rpcns4, 'RpcNsGroupMbrInqNextW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrInqNextW]
  end;
end;

var
  _RpcNsGroupMbrInqNext: Pointer;

function RpcNsGroupMbrInqNext;
begin
  GetProcedureAddress(_RpcNsGroupMbrInqNext, rpcns4, 'RpcNsGroupMbrInqNext' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrInqNext]
  end;
end;

var
  _RpcNsGroupMbrInqDone: Pointer;

function RpcNsGroupMbrInqDone;
begin
  GetProcedureAddress(_RpcNsGroupMbrInqDone, rpcns4, 'RpcNsGroupMbrInqDone');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsGroupMbrInqDone]
  end;
end;

var
  _RpcNsProfileDeleteA: Pointer;

function RpcNsProfileDeleteA;
begin
  GetProcedureAddress(_RpcNsProfileDeleteA, rpcns4, 'RpcNsProfileDeleteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileDeleteA]
  end;
end;

var
  _RpcNsProfileDeleteW: Pointer;

function RpcNsProfileDeleteW;
begin
  GetProcedureAddress(_RpcNsProfileDeleteW, rpcns4, 'RpcNsProfileDeleteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileDeleteW]
  end;
end;

var
  _RpcNsProfileDelete: Pointer;

function RpcNsProfileDelete;
begin
  GetProcedureAddress(_RpcNsProfileDelete, rpcns4, 'RpcNsProfileDelete' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileDelete]
  end;
end;

var
  _RpcNsProfileEltAddA: Pointer;

function RpcNsProfileEltAddA;
begin
  GetProcedureAddress(_RpcNsProfileEltAddA, rpcns4, 'RpcNsProfileEltAddA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltAddA]
  end;
end;

var
  _RpcNsProfileEltAddW: Pointer;

function RpcNsProfileEltAddW;
begin
  GetProcedureAddress(_RpcNsProfileEltAddW, rpcns4, 'RpcNsProfileEltAddW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltAddW]
  end;
end;

var
  _RpcNsProfileEltAdd: Pointer;

function RpcNsProfileEltAdd;
begin
  GetProcedureAddress(_RpcNsProfileEltAdd, rpcns4, 'RpcNsProfileEltAdd' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltAdd]
  end;
end;

var
  _RpcNsProfileEltRemoveA: Pointer;

function RpcNsProfileEltRemoveA;
begin
  GetProcedureAddress(_RpcNsProfileEltRemoveA, rpcns4, 'RpcNsProfileEltRemoveA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltRemoveA]
  end;
end;

var
  _RpcNsProfileEltRemoveW: Pointer;

function RpcNsProfileEltRemoveW;
begin
  GetProcedureAddress(_RpcNsProfileEltRemoveW, rpcns4, 'RpcNsProfileEltRemoveW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltRemoveW]
  end;
end;

var
  _RpcNsProfileEltRemove: Pointer;

function RpcNsProfileEltRemove;
begin
  GetProcedureAddress(_RpcNsProfileEltRemove, rpcns4, 'RpcNsProfileEltRemove' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltRemove]
  end;
end;

var
  _RpcNsProfileEltInqBeginA: Pointer;

function RpcNsProfileEltInqBeginA;
begin
  GetProcedureAddress(_RpcNsProfileEltInqBeginA, rpcns4, 'RpcNsProfileEltInqBeginA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltInqBeginA]
  end;
end;

var
  _RpcNsProfileEltInqBeginW: Pointer;

function RpcNsProfileEltInqBeginW;
begin
  GetProcedureAddress(_RpcNsProfileEltInqBeginW, rpcns4, 'RpcNsProfileEltInqBeginW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltInqBeginW]
  end;
end;

var
  _RpcNsProfileEltInqBegin: Pointer;

function RpcNsProfileEltInqBegin;
begin
  GetProcedureAddress(_RpcNsProfileEltInqBegin, rpcns4, 'RpcNsProfileEltInqBegin' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltInqBegin]
  end;
end;

var
  _RpcNsProfileEltInqNextA: Pointer;

function RpcNsProfileEltInqNextA;
begin
  GetProcedureAddress(_RpcNsProfileEltInqNextA, rpcns4, 'RpcNsProfileEltInqNextA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltInqNextA]
  end;
end;

var
  _RpcNsProfileEltInqNextW: Pointer;

function RpcNsProfileEltInqNextW;
begin
  GetProcedureAddress(_RpcNsProfileEltInqNextW, rpcns4, 'RpcNsProfileEltInqNextW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltInqNextW]
  end;
end;

var
  _RpcNsProfileEltInqNext: Pointer;

function RpcNsProfileEltInqNext;
begin
  GetProcedureAddress(_RpcNsProfileEltInqNext, rpcns4, 'RpcNsProfileEltInqNext' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltInqNext]
  end;
end;

var
  _RpcNsProfileEltInqDone: Pointer;

function RpcNsProfileEltInqDone;
begin
  GetProcedureAddress(_RpcNsProfileEltInqDone, rpcns4, 'RpcNsProfileEltInqDone');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsProfileEltInqDone]
  end;
end;

var
  _RpcNsEntryObjectInqBeginA: Pointer;

function RpcNsEntryObjectInqBeginA;
begin
  GetProcedureAddress(_RpcNsEntryObjectInqBeginA, rpcns4, 'RpcNsEntryObjectInqBeginA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryObjectInqBeginA]
  end;
end;

var
  _RpcNsEntryObjectInqBeginW: Pointer;

function RpcNsEntryObjectInqBeginW;
begin
  GetProcedureAddress(_RpcNsEntryObjectInqBeginW, rpcns4, 'RpcNsEntryObjectInqBeginW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryObjectInqBeginW]
  end;
end;

var
  _RpcNsEntryObjectInqBegin: Pointer;

function RpcNsEntryObjectInqBegin;
begin
  GetProcedureAddress(_RpcNsEntryObjectInqBegin, rpcns4, 'RpcNsEntryObjectInqBegin' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryObjectInqBegin]
  end;
end;

var
  _RpcNsEntryObjectInqNext: Pointer;

function RpcNsEntryObjectInqNext;
begin
  GetProcedureAddress(_RpcNsEntryObjectInqNext, rpcns4, 'RpcNsEntryObjectInqNext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryObjectInqNext]
  end;
end;

var
  _RpcNsEntryObjectInqDone: Pointer;

function RpcNsEntryObjectInqDone;
begin
  GetProcedureAddress(_RpcNsEntryObjectInqDone, rpcns4, 'RpcNsEntryObjectInqDone');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryObjectInqDone]
  end;
end;

var
  _RpcNsEntryExpandNameA: Pointer;

function RpcNsEntryExpandNameA;
begin
  GetProcedureAddress(_RpcNsEntryExpandNameA, rpcns4, 'RpcNsEntryExpandNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryExpandNameA]
  end;
end;

var
  _RpcNsEntryExpandNameW: Pointer;

function RpcNsEntryExpandNameW;
begin
  GetProcedureAddress(_RpcNsEntryExpandNameW, rpcns4, 'RpcNsEntryExpandNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryExpandNameW]
  end;
end;

var
  _RpcNsEntryExpandName: Pointer;

function RpcNsEntryExpandName;
begin
  GetProcedureAddress(_RpcNsEntryExpandName, rpcns4, 'RpcNsEntryExpandName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsEntryExpandName]
  end;
end;

var
  _RpcNsMgmtBindingUnexportA: Pointer;

function RpcNsMgmtBindingUnexportA;
begin
  GetProcedureAddress(_RpcNsMgmtBindingUnexportA, rpcns4, 'RpcNsMgmtBindingUnexportA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtBindingUnexportA]
  end;
end;

var
  _RpcNsMgmtBindingUnexportW: Pointer;

function RpcNsMgmtBindingUnexportW;
begin
  GetProcedureAddress(_RpcNsMgmtBindingUnexportW, rpcns4, 'RpcNsMgmtBindingUnexportW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtBindingUnexportW]
  end;
end;

var
  _RpcNsMgmtBindingUnexport: Pointer;

function RpcNsMgmtBindingUnexport;
begin
  GetProcedureAddress(_RpcNsMgmtBindingUnexport, rpcns4, 'RpcNsMgmtBindingUnexport' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtBindingUnexport]
  end;
end;

var
  _RpcNsMgmtEntryCreateA: Pointer;

function RpcNsMgmtEntryCreateA;
begin
  GetProcedureAddress(_RpcNsMgmtEntryCreateA, rpcns4, 'RpcNsMgmtEntryCreateA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryCreateA]
  end;
end;

var
  _RpcNsMgmtEntryCreateW: Pointer;

function RpcNsMgmtEntryCreateW;
begin
  GetProcedureAddress(_RpcNsMgmtEntryCreateW, rpcns4, 'RpcNsMgmtEntryCreateW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryCreateW]
  end;
end;

var
  _RpcNsMgmtEntryCreate: Pointer;

function RpcNsMgmtEntryCreate;
begin
  GetProcedureAddress(_RpcNsMgmtEntryCreate, rpcns4, 'RpcNsMgmtEntryCreate' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryCreate]
  end;
end;

var
  _RpcNsMgmtEntryDeleteA: Pointer;

function RpcNsMgmtEntryDeleteA;
begin
  GetProcedureAddress(_RpcNsMgmtEntryDeleteA, rpcns4, 'RpcNsMgmtEntryDeleteA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryDeleteA]
  end;
end;

var
  _RpcNsMgmtEntryDeleteW: Pointer;

function RpcNsMgmtEntryDeleteW;
begin
  GetProcedureAddress(_RpcNsMgmtEntryDeleteW, rpcns4, 'RpcNsMgmtEntryDeleteW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryDeleteW]
  end;
end;

var
  _RpcNsMgmtEntryDelete: Pointer;

function RpcNsMgmtEntryDelete;
begin
  GetProcedureAddress(_RpcNsMgmtEntryDelete, rpcns4, 'RpcNsMgmtEntryDelete' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryDelete]
  end;
end;

var
  _RpcNsMgmtEntryInqIfIdsA: Pointer;

function RpcNsMgmtEntryInqIfIdsA;
begin
  GetProcedureAddress(_RpcNsMgmtEntryInqIfIdsA, rpcns4, 'RpcNsMgmtEntryInqIfIdsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryInqIfIdsA]
  end;
end;

var
  _RpcNsMgmtEntryInqIfIdsW: Pointer;

function RpcNsMgmtEntryInqIfIdsW;
begin
  GetProcedureAddress(_RpcNsMgmtEntryInqIfIdsW, rpcns4, 'RpcNsMgmtEntryInqIfIdsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryInqIfIdsW]
  end;
end;

var
  _RpcNsMgmtEntryInqIfIds: Pointer;

function RpcNsMgmtEntryInqIfIds;
begin
  GetProcedureAddress(_RpcNsMgmtEntryInqIfIds, rpcns4, 'RpcNsMgmtEntryInqIfIds' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtEntryInqIfIds]
  end;
end;

var
  _RpcNsMgmtHandleSetExpAge: Pointer;

function RpcNsMgmtHandleSetExpAge;
begin
  GetProcedureAddress(_RpcNsMgmtHandleSetExpAge, rpcns4, 'RpcNsMgmtHandleSetExpAge');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtHandleSetExpAge]
  end;
end;

var
  _RpcNsMgmtInqExpAge: Pointer;

function RpcNsMgmtInqExpAge;
begin
  GetProcedureAddress(_RpcNsMgmtInqExpAge, rpcns4, 'RpcNsMgmtInqExpAge');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtInqExpAge]
  end;
end;

var
  _RpcNsMgmtSetExpAge: Pointer;

function RpcNsMgmtSetExpAge;
begin
  GetProcedureAddress(_RpcNsMgmtSetExpAge, rpcns4, 'RpcNsMgmtSetExpAge');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsMgmtSetExpAge]
  end;
end;

var
  _RpcNsBindingImportBeginA: Pointer;

function RpcNsBindingImportBeginA;
begin
  GetProcedureAddress(_RpcNsBindingImportBeginA, rpcns4, 'RpcNsBindingImportBeginA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingImportBeginA]
  end;
end;

var
  _RpcNsBindingImportBeginW: Pointer;

function RpcNsBindingImportBeginW;
begin
  GetProcedureAddress(_RpcNsBindingImportBeginW, rpcns4, 'RpcNsBindingImportBeginW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingImportBeginW]
  end;
end;

var
  _RpcNsBindingImportBegin: Pointer;

function RpcNsBindingImportBegin;
begin
  GetProcedureAddress(_RpcNsBindingImportBegin, rpcns4, 'RpcNsBindingImportBegin' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingImportBegin]
  end;
end;

var
  _RpcNsBindingImportNext: Pointer;

function RpcNsBindingImportNext;
begin
  GetProcedureAddress(_RpcNsBindingImportNext, rpcns4, 'RpcNsBindingImportNext');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingImportNext]
  end;
end;

var
  _RpcNsBindingImportDone: Pointer;

function RpcNsBindingImportDone;
begin
  GetProcedureAddress(_RpcNsBindingImportDone, rpcns4, 'RpcNsBindingImportDone');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingImportDone]
  end;
end;

var
  _RpcNsBindingSelect: Pointer;

function RpcNsBindingSelect;
begin
  GetProcedureAddress(_RpcNsBindingSelect, rpcns4, 'RpcNsBindingSelect');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_RpcNsBindingSelect]
  end;
end;

{$ELSE}

function RpcNsBindingExportA; external rpcns4 name 'RpcNsBindingExportA';
function RpcNsBindingExportW; external rpcns4 name 'RpcNsBindingExportW';
function RpcNsBindingExport; external rpcns4 name 'RpcNsBindingExport' + AWSuffix;
function RpcNsBindingUnexportA; external rpcns4 name 'RpcNsBindingUnexportA';
function RpcNsBindingUnexportW; external rpcns4 name 'RpcNsBindingUnexportW';
function RpcNsBindingUnexport; external rpcns4 name 'RpcNsBindingUnexport' + AWSuffix;
function RpcNsBindingExportPnPA; external rpcns4 name 'RpcNsBindingExportPnPA';
function RpcNsBindingExportPnPW; external rpcns4 name 'RpcNsBindingExportPnPW';
function RpcNsBindingExportPnP; external rpcns4 name 'RpcNsBindingExportPnP' + AWSuffix;
function RpcNsBindingUnexportPnPA; external rpcns4 name 'RpcNsBindingUnexportPnPA';
function RpcNsBindingUnexportPnPW; external rpcns4 name 'RpcNsBindingUnexportPnPW';
function RpcNsBindingUnexportPnP; external rpcns4 name 'RpcNsBindingUnexportPnP' + AWSuffix;
function RpcNsBindingLookupBeginA; external rpcns4 name 'RpcNsBindingLookupBeginA';
function RpcNsBindingLookupBeginW; external rpcns4 name 'RpcNsBindingLookupBeginW';
function RpcNsBindingLookupBegin; external rpcns4 name 'RpcNsBindingLookupBegin' + AWSuffix;
function RpcNsBindingLookupNext; external rpcns4 name 'RpcNsBindingLookupNext';
function RpcNsBindingLookupDone; external rpcns4 name 'RpcNsBindingLookupDone';
function RpcNsGroupDeleteA; external rpcns4 name 'RpcNsGroupDeleteA';
function RpcNsGroupDeleteW; external rpcns4 name 'RpcNsGroupDeleteW';
function RpcNsGroupDelete; external rpcns4 name 'RpcNsGroupDelete' + AWSuffix;
function RpcNsGroupMbrAddA; external rpcns4 name 'RpcNsGroupMbrAddA';
function RpcNsGroupMbrAddW; external rpcns4 name 'RpcNsGroupMbrAddW';
function RpcNsGroupMbrAdd; external rpcns4 name 'RpcNsGroupMbrAdd' + AWSuffix;
function RpcNsGroupMbrRemoveA; external rpcns4 name 'RpcNsGroupMbrRemoveA';
function RpcNsGroupMbrRemoveW; external rpcns4 name 'RpcNsGroupMbrRemoveW';
function RpcNsGroupMbrRemove; external rpcns4 name 'RpcNsGroupMbrRemove' + AWSuffix;
function RpcNsGroupMbrInqBeginA; external rpcns4 name 'RpcNsGroupMbrInqBeginA';
function RpcNsGroupMbrInqBeginW; external rpcns4 name 'RpcNsGroupMbrInqBeginW';
function RpcNsGroupMbrInqBegin; external rpcns4 name 'RpcNsGroupMbrInqBegin' + AWSuffix;
function RpcNsGroupMbrInqNextA; external rpcns4 name 'RpcNsGroupMbrInqNextA';
function RpcNsGroupMbrInqNextW; external rpcns4 name 'RpcNsGroupMbrInqNextW';
function RpcNsGroupMbrInqNext; external rpcns4 name 'RpcNsGroupMbrInqNext' + AWSuffix;
function RpcNsGroupMbrInqDone; external rpcns4 name 'RpcNsGroupMbrInqDone';
function RpcNsProfileDeleteA; external rpcns4 name 'RpcNsProfileDeleteA';
function RpcNsProfileDeleteW; external rpcns4 name 'RpcNsProfileDeleteW';
function RpcNsProfileDelete; external rpcns4 name 'RpcNsProfileDelete' + AWSuffix;
function RpcNsProfileEltAddA; external rpcns4 name 'RpcNsProfileEltAddA';
function RpcNsProfileEltAddW; external rpcns4 name 'RpcNsProfileEltAddW';
function RpcNsProfileEltAdd; external rpcns4 name 'RpcNsProfileEltAdd' + AWSuffix;
function RpcNsProfileEltRemoveA; external rpcns4 name 'RpcNsProfileEltRemoveA';
function RpcNsProfileEltRemoveW; external rpcns4 name 'RpcNsProfileEltRemoveW';
function RpcNsProfileEltRemove; external rpcns4 name 'RpcNsProfileEltRemove' + AWSuffix;
function RpcNsProfileEltInqBeginA; external rpcns4 name 'RpcNsProfileEltInqBeginA';
function RpcNsProfileEltInqBeginW; external rpcns4 name 'RpcNsProfileEltInqBeginW';
function RpcNsProfileEltInqBegin; external rpcns4 name 'RpcNsProfileEltInqBegin' + AWSuffix;
function RpcNsProfileEltInqNextA; external rpcns4 name 'RpcNsProfileEltInqNextA';
function RpcNsProfileEltInqNextW; external rpcns4 name 'RpcNsProfileEltInqNextW';
function RpcNsProfileEltInqNext; external rpcns4 name 'RpcNsProfileEltInqNext' + AWSuffix;
function RpcNsProfileEltInqDone; external rpcns4 name 'RpcNsProfileEltInqDone';
function RpcNsEntryObjectInqBeginA; external rpcns4 name 'RpcNsEntryObjectInqBeginA';
function RpcNsEntryObjectInqBeginW; external rpcns4 name 'RpcNsEntryObjectInqBeginW';
function RpcNsEntryObjectInqBegin; external rpcns4 name 'RpcNsEntryObjectInqBegin' + AWSuffix;
function RpcNsEntryObjectInqNext; external rpcns4 name 'RpcNsEntryObjectInqNext';
function RpcNsEntryObjectInqDone; external rpcns4 name 'RpcNsEntryObjectInqDone';
function RpcNsEntryExpandNameA; external rpcns4 name 'RpcNsEntryExpandNameA';
function RpcNsEntryExpandNameW; external rpcns4 name 'RpcNsEntryExpandNameW';
function RpcNsEntryExpandName; external rpcns4 name 'RpcNsEntryExpandName' + AWSuffix;
function RpcNsMgmtBindingUnexportA; external rpcns4 name 'RpcNsMgmtBindingUnexportA';
function RpcNsMgmtBindingUnexportW; external rpcns4 name 'RpcNsMgmtBindingUnexportW';
function RpcNsMgmtBindingUnexport; external rpcns4 name 'RpcNsMgmtBindingUnexport' + AWSuffix;
function RpcNsMgmtEntryCreateA; external rpcns4 name 'RpcNsMgmtEntryCreateA';
function RpcNsMgmtEntryCreateW; external rpcns4 name 'RpcNsMgmtEntryCreateW';
function RpcNsMgmtEntryCreate; external rpcns4 name 'RpcNsMgmtEntryCreate' + AWSuffix;
function RpcNsMgmtEntryDeleteA; external rpcns4 name 'RpcNsMgmtEntryDeleteA';
function RpcNsMgmtEntryDeleteW; external rpcns4 name 'RpcNsMgmtEntryDeleteW';
function RpcNsMgmtEntryDelete; external rpcns4 name 'RpcNsMgmtEntryDelete' + AWSuffix;
function RpcNsMgmtEntryInqIfIdsA; external rpcns4 name 'RpcNsMgmtEntryInqIfIdsA';
function RpcNsMgmtEntryInqIfIdsW; external rpcns4 name 'RpcNsMgmtEntryInqIfIdsW';
function RpcNsMgmtEntryInqIfIds; external rpcns4 name 'RpcNsMgmtEntryInqIfIds' + AWSuffix;
function RpcNsMgmtHandleSetExpAge; external rpcns4 name 'RpcNsMgmtHandleSetExpAge';
function RpcNsMgmtInqExpAge; external rpcns4 name 'RpcNsMgmtInqExpAge';
function RpcNsMgmtSetExpAge; external rpcns4 name 'RpcNsMgmtSetExpAge';
function RpcNsBindingImportBeginA; external rpcns4 name 'RpcNsBindingImportBeginA';
function RpcNsBindingImportBeginW; external rpcns4 name 'RpcNsBindingImportBeginW';
function RpcNsBindingImportBegin; external rpcns4 name 'RpcNsBindingImportBegin' + AWSuffix;
function RpcNsBindingImportNext; external rpcns4 name 'RpcNsBindingImportNext';
function RpcNsBindingImportDone; external rpcns4 name 'RpcNsBindingImportDone';
function RpcNsBindingSelect; external rpcns4 name 'RpcNsBindingSelect';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
