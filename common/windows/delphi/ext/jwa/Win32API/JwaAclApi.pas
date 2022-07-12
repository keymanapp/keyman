{******************************************************************************}
{                                                                              }
{ Access Control API interface Unit for Object Pascal                          }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: aclapi.h, released June 2000. The original Pascal      }
{ code is: AclApi.pas, released December 2000. The initial developer of the    }
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

// $Id: JwaAclApi.pas,v 1.11 2007/09/05 11:58:48 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaAclApi;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "aclapi.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaAccCtrl, JwaWinNT, JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}


{$IFNDEF JWA_INCLUDEMODE}
//
// Progress Function:
// Caller of tree operation implements this Progress function, then
// passes its function pointer to tree operation.
// Tree operation invokes Progress function to provide progress and error
// information to the caller during the potentially long execution
// of the tree operation.  Tree operation provides the name of the object
// last processed and the error status of the operation on that object.
// Tree operation also passes the current InvokeSetting value.
// Caller may change the InvokeSetting value, for example, from "Always"
// to "Only On Error."
//


type
  FN_PROGRESS = procedure(
    pObjectName: LPWSTR;    // name of object just processed
    Status: DWORD;          // status of operation on object
    var pInvokeSetting: PPROG_INVOKE_SETTING; // Never, always,
    Args: PVOID;            // Caller specific data
    SecuritySet: BOOL       // Whether security was set
    ); stdcall;
  {$EXTERNALSYM FN_PROGRESS}
  TFnProgress = FN_PROGRESS;
{$ENDIF JWA_INCLUDEMODE}  

function SetEntriesInAclA(cCountOfExplicitEntries: ULONG;
  pListOfExplicitEntries: PEXPLICIT_ACCESS_A; OldAcl: PACL;
  var NewAcl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetEntriesInAclA}
function SetEntriesInAclW(cCountOfExplicitEntries: ULONG;
  pListOfExplicitEntries: PEXPLICIT_ACCESS_W; OldAcl: PACL;
  var NewAcl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetEntriesInAclW}
function SetEntriesInAcl(cCountOfExplicitEntries: ULONG;
  pListOfExplicitEntries: PEXPLICIT_ACCESS; OldAcl: PACL;
  var NewAcl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetEntriesInAcl}

function GetExplicitEntriesFromAclA(pacl: PACL; var pcCountOfExplicitEntries: ULONG;
  var pListOfExplicitEntries: PEXPLICIT_ACCESS_A): DWORD; stdcall;
{$EXTERNALSYM GetExplicitEntriesFromAclA}
function GetExplicitEntriesFromAclW(pacl: PACL; var pcCountOfExplicitEntries: ULONG;
  var pListOfExplicitEntries: PEXPLICIT_ACCESS_W): DWORD; stdcall;
{$EXTERNALSYM GetExplicitEntriesFromAclW}
function GetExplicitEntriesFromAcl(pacl: PACL; var pcCountOfExplicitEntries: ULONG;
  var pListOfExplicitEntries: PEXPLICIT_ACCESS): DWORD; stdcall;
{$EXTERNALSYM GetExplicitEntriesFromAcl}

function GetEffectiveRightsFromAclA(pacl: PACL; pTrustee: PTRUSTEE_A;
  var pAccessRights: ACCESS_MASK): DWORD; stdcall;
{$EXTERNALSYM GetEffectiveRightsFromAclA}
function GetEffectiveRightsFromAclW(pacl: PACL; pTrustee: PTRUSTEE_W;
  var pAccessRights: ACCESS_MASK): DWORD; stdcall;
{$EXTERNALSYM GetEffectiveRightsFromAclW}
function GetEffectiveRightsFromAcl(pacl: PACL; pTrustee: PTRUSTEE;
  var pAccessRights: ACCESS_MASK): DWORD; stdcall;
{$EXTERNALSYM GetEffectiveRightsFromAcl}

function GetAuditedPermissionsFromAclA(pacl: PACL; pTrustee: PTRUSTEE_A;
  var pSuccessfulAuditedRights, pFailedAuditRights: ACCESS_MASK): DWORD; stdcall;
{$EXTERNALSYM GetAuditedPermissionsFromAclA}
function GetAuditedPermissionsFromAclW(pacl: PACL; pTrustee: PTRUSTEE_W;
  var pSuccessfulAuditedRights, pFailedAuditRights: ACCESS_MASK): DWORD; stdcall;
{$EXTERNALSYM GetAuditedPermissionsFromAclW}
function GetAuditedPermissionsFromAcl(pacl: PACL; pTrustee: PTRUSTEE;
  var pSuccessfulAuditedRights, pFailedAuditRights: ACCESS_MASK): DWORD; stdcall;
{$EXTERNALSYM GetAuditedPermissionsFromAcl}

function GetNamedSecurityInfoA(pObjectName: LPSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl,
  ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM GetNamedSecurityInfoA}
function GetNamedSecurityInfoW(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl,
  ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM GetNamedSecurityInfoW}
function GetNamedSecurityInfo(pObjectName: LPTSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID; ppDacl,
  ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM GetNamedSecurityInfo}

function GetSecurityInfo(handle: HANDLE; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; ppsidOwner: PPSID; ppsidGroup: PPSID;
  ppDacl, ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM GetSecurityInfo}

function SetNamedSecurityInfoA(pObjectName: LPSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
  pDacl, pSacl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetNamedSecurityInfoA}
function SetNamedSecurityInfoW(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
  pDacl, pSacl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetNamedSecurityInfoW}
function SetNamedSecurityInfo(pObjectName: LPTSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
  pDacl, pSacl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetNamedSecurityInfo}

function SetSecurityInfo(handle: HANDLE; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; psidOwner, psidGroup: PSID;
  pDacl, pSacl: PACL): DWORD; stdcall;
{$EXTERNALSYM SetSecurityInfo}

function GetInheritanceSourceA(pObjectName: LPSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; Container: BOOL; pObjectClassGuids: LPGUID;
  GuidCount: DWORD; pAcl: PACL; pfnArray: PFN_OBJECT_MGR_FUNCTS;
  pGenericMapping: PGENERIC_MAPPING; pInheritArray: PINHERITED_FROMA): DWORD; stdcall;
{$EXTERNALSYM GetInheritanceSourceA}
function GetInheritanceSourceW(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; Container: BOOL; pObjectClassGuids: LPGUID;
  GuidCount: DWORD; pAcl: PACL; pfnArray: PFN_OBJECT_MGR_FUNCTS;
  pGenericMapping: PGENERIC_MAPPING; pInheritArray: PINHERITED_FROMW): DWORD; stdcall;
{$EXTERNALSYM GetInheritanceSourceW}
function GetInheritanceSource(pObjectName: LPTSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; Container: BOOL; pObjectClassGuids: LPGUID;
  GuidCount: DWORD; pAcl: PACL; pfnArray: PFN_OBJECT_MGR_FUNCTS;
  pGenericMapping: PGENERIC_MAPPING; pInheritArray: PINHERITED_FROM): DWORD; stdcall;
{$EXTERNALSYM GetInheritanceSource}

function FreeInheritedFromArray(pInheritArray: PINHERITED_FROMW; AceCnt: USHORT;
  pfnArray: PFN_OBJECT_MGR_FUNCTS): DWORD; stdcall;
{$EXTERNALSYM FreeInheritedFromArray}

function TreeResetNamedSecurityInfoA(pObjectName: LPSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; pOwner, pGroup: PSID; pDacl, pSacl: PACL;
  KeepExplicit: BOOL; fnProgress: FN_PROGRESS; ProgressInvokeSetting: PROG_INVOKE_SETTING;
  Args: PVOID): DWORD; stdcall;
{$EXTERNALSYM TreeResetNamedSecurityInfoA}
function TreeResetNamedSecurityInfoW(pObjectName: LPWSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; pOwner, pGroup: PSID; pDacl, pSacl: PACL;
  KeepExplicit: BOOL; fnProgress: FN_PROGRESS; ProgressInvokeSetting: PROG_INVOKE_SETTING;
  Args: PVOID): DWORD; stdcall;
{$EXTERNALSYM TreeResetNamedSecurityInfoW}
function TreeResetNamedSecurityInfo(pObjectName: LPTSTR; ObjectType: SE_OBJECT_TYPE;
  SecurityInfo: SECURITY_INFORMATION; pOwner, pGroup: PSID; pDacl, pSacl: PACL;
  KeepExplicit: BOOL; fnProgress: FN_PROGRESS; ProgressInvokeSetting: PROG_INVOKE_SETTING;
  Args: PVOID): DWORD; stdcall;
{$EXTERNALSYM TreeResetNamedSecurityInfo}

//----------------------------------------------------------------------------
// The following API are provided for trusted servers to use to
// implement access control on their own objects.
//----------------------------------------------------------------------------

function BuildSecurityDescriptorA(pOwner: PTRUSTEE_A; pGroup: PTRUSTEE_A;
  cCountOfAccessEntries: ULONG; pListOfAccessEntries: PEXPLICIT_ACCESS_A;
  cCountOfAuditEntries: ULONG; pListOfAuditEntries: PEXPLICIT_ACCESS_A;
  pOldSD: PSECURITY_DESCRIPTOR; var pSizeNewSD: ULONG;
  var pNewSD: PSECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM BuildSecurityDescriptorA}
function BuildSecurityDescriptorW(pOwner: PTRUSTEE_W; pGroup: PTRUSTEE_W;
  cCountOfAccessEntries: ULONG; pListOfAccessEntries: PEXPLICIT_ACCESS_W;
  cCountOfAuditEntries: ULONG; pListOfAuditEntries: PEXPLICIT_ACCESS_W;
  pOldSD: PSECURITY_DESCRIPTOR; var pSizeNewSD: ULONG;
  var pNewSD: PSECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM BuildSecurityDescriptorW}
function BuildSecurityDescriptor(pOwner: PTRUSTEE; pGroup: PTRUSTEE;
  cCountOfAccessEntries: ULONG; pListOfAccessEntries: PEXPLICIT_ACCESS;
  cCountOfAuditEntries: ULONG; pListOfAuditEntries: PEXPLICIT_ACCESS;
  pOldSD: PSECURITY_DESCRIPTOR; var pSizeNewSD: ULONG;
  var pNewSD: PSECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM BuildSecurityDescriptor}

function LookupSecurityDescriptorPartsA(pOwner, pGroup: PPTRUSTEE_A;
  cCountOfAccessEntries: PULONG; pListOfAccessEntries: PEXPLICIT_ACCESS_A;
  cCountOfAuditEntries: PULONG; pListOfAuditEntries: PEXPLICIT_ACCESS_A;
  var pSD: SECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM LookupSecurityDescriptorPartsA}
function LookupSecurityDescriptorPartsW(pOwner, pGroup: PPTRUSTEE_W;
  cCountOfAccessEntries: PULONG; pListOfAccessEntries: PEXPLICIT_ACCESS_W;
  cCountOfAuditEntries: PULONG; pListOfAuditEntries: PEXPLICIT_ACCESS_W;
  var pSD: SECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM LookupSecurityDescriptorPartsW}
function LookupSecurityDescriptorParts(pOwner, pGroup: PPTRUSTEE;
  cCountOfAccessEntries: PULONG; pListOfAccessEntries: PEXPLICIT_ACCESS;
  cCountOfAuditEntries: PULONG; pListOfAuditEntries: PEXPLICIT_ACCESS;
  var pSD: SECURITY_DESCRIPTOR): DWORD; stdcall;
{$EXTERNALSYM LookupSecurityDescriptorParts}

//----------------------------------------------------------------------------
// The following helper API are provided for building
// access control structures.
//----------------------------------------------------------------------------

procedure BuildExplicitAccessWithNameA(pExplicitAccess: PEXPLICIT_ACCESS_A;
  pTrusteeName: LPSTR; AccessPermissions: DWORD; AccessMode: ACCESS_MODE;
  Inheritance: DWORD); stdcall;
{$EXTERNALSYM BuildExplicitAccessWithNameA}
procedure BuildExplicitAccessWithNameW(pExplicitAccess: PEXPLICIT_ACCESS_W;
  pTrusteeName: LPWSTR; AccessPermissions: DWORD; AccessMode: ACCESS_MODE;
  Inheritance: DWORD); stdcall;
{$EXTERNALSYM BuildExplicitAccessWithNameW}
procedure BuildExplicitAccessWithName(pExplicitAccess: PEXPLICIT_ACCESS;
  pTrusteeName: LPTSTR; AccessPermissions: DWORD; AccessMode: ACCESS_MODE;
  Inheritance: DWORD); stdcall;
{$EXTERNALSYM BuildExplicitAccessWithName}

procedure BuildImpersonateExplicitAccessWithNameA(pExplicitAccess: PEXPLICIT_ACCESS_A;
  pTrusteeName: LPSTR; pTrustee: PTRUSTEE_A; AccessPermissions: DWORD;
  AccessMode: ACCESS_MODE; Inheritance: DWORD); stdcall;
{$EXTERNALSYM BuildImpersonateExplicitAccessWithNameA}
procedure BuildImpersonateExplicitAccessWithNameW(pExplicitAccess: PEXPLICIT_ACCESS_W;
  pTrusteeName: LPWSTR; pTrustee: PTRUSTEE_W; AccessPermissions: DWORD;
  AccessMode: ACCESS_MODE; Inheritance: DWORD); stdcall;
{$EXTERNALSYM BuildImpersonateExplicitAccessWithNameW}
procedure BuildImpersonateExplicitAccessWithName(pExplicitAccess: PEXPLICIT_ACCESS;
  pTrusteeName: LPTSTR; pTrustee: PTRUSTEE; AccessPermissions: DWORD;
  AccessMode: ACCESS_MODE; Inheritance: DWORD); stdcall;
{$EXTERNALSYM BuildImpersonateExplicitAccessWithName}

procedure BuildTrusteeWithNameA(pTrustee: PTRUSTEE_A; pName: LPSTR); stdcall;
{$EXTERNALSYM BuildTrusteeWithNameA}
procedure BuildTrusteeWithNameW(pTrustee: PTRUSTEE_W; pName: LPWSTR); stdcall;
{$EXTERNALSYM BuildTrusteeWithNameW}
procedure BuildTrusteeWithName(pTrustee: PTRUSTEE; pName: LPTSTR); stdcall;
{$EXTERNALSYM BuildTrusteeWithName}

procedure BuildImpersonateTrusteeA(pTrustee: PTRUSTEE_A;
  pImpersonateTrustee: PTRUSTEE_A); stdcall;
{$EXTERNALSYM BuildImpersonateTrusteeA}
procedure BuildImpersonateTrusteeW(pTrustee: PTRUSTEE_W;
  pImpersonateTrustee: PTRUSTEE_W); stdcall;
{$EXTERNALSYM BuildImpersonateTrusteeW}
procedure BuildImpersonateTrustee(pTrustee: PTRUSTEE;
  pImpersonateTrustee: PTRUSTEE); stdcall;
{$EXTERNALSYM BuildImpersonateTrustee}

procedure BuildTrusteeWithSidA(pTrustee: PTRUSTEE_A; pSid: PSID); stdcall;
{$EXTERNALSYM BuildTrusteeWithSidA}
procedure BuildTrusteeWithSidW(pTrustee: PTRUSTEE_W; pSid: PSID); stdcall;
{$EXTERNALSYM BuildTrusteeWithSidW}
procedure BuildTrusteeWithSid(pTrustee: PTRUSTEE; pSid: PSID); stdcall;
{$EXTERNALSYM BuildTrusteeWithSid}

procedure BuildTrusteeWithObjectsAndSidA(pTrustee: PTRUSTEE_A;
  pObjSid: POBJECTS_AND_SID; pObjectGuid: PGUID; pInheritedObjectGuid: PGUID;
  pSid: PSID); stdcall;
{$EXTERNALSYM BuildTrusteeWithObjectsAndSidA}
procedure BuildTrusteeWithObjectsAndSidW(pTrustee: PTRUSTEE_W;
  pObjSid: POBJECTS_AND_SID; pObjectGuid: PGUID; pInheritedObjectGuid: PGUID;
  pSid: PSID); stdcall;
{$EXTERNALSYM BuildTrusteeWithObjectsAndSidW}
procedure BuildTrusteeWithObjectsAndSid(pTrustee: PTRUSTEE;
  pObjSid: POBJECTS_AND_SID; pObjectGuid: PGUID; pInheritedObjectGuid: PGUID;
  pSid: PSID); stdcall;
{$EXTERNALSYM BuildTrusteeWithObjectsAndSid}

procedure BuildTrusteeWithObjectsAndNameA(pTrustee: PTRUSTEE_A;
  pObjName: POBJECTS_AND_NAME_A; ObjectType: SE_OBJECT_TYPE;
  ObjectTypeName, InheritedObjectTypeName, Name: LPSTR); stdcall;
{$EXTERNALSYM BuildTrusteeWithObjectsAndNameA}
procedure BuildTrusteeWithObjectsAndNameW(pTrustee: PTRUSTEE_W;
  pObjName: POBJECTS_AND_NAME_W; ObjectType: SE_OBJECT_TYPE;
  ObjectTypeName, InheritedObjectTypeName, Name: LPWSTR); stdcall;
{$EXTERNALSYM BuildTrusteeWithObjectsAndNameW}
procedure BuildTrusteeWithObjectsAndName(pTrustee: PTRUSTEE;
  pObjName: POBJECTS_AND_NAME; ObjectType: SE_OBJECT_TYPE;
  ObjectTypeName, InheritedObjectTypeName, Name: LPTSTR); stdcall;
{$EXTERNALSYM BuildTrusteeWithObjectsAndName}

function GetTrusteeNameA(pTrustee: PTRUSTEE_A): LPSTR; stdcall;
{$EXTERNALSYM GetTrusteeNameA}
function GetTrusteeNameW(pTrustee: PTRUSTEE_W): LPWSTR; stdcall;
{$EXTERNALSYM GetTrusteeNameW}
function GetTrusteeName(pTrustee: PTRUSTEE): LPTSTR; stdcall;
{$EXTERNALSYM GetTrusteeName}

function GetTrusteeTypeA(pTrustee: PTRUSTEE_A): TRUSTEE_TYPE; stdcall;
{$EXTERNALSYM GetTrusteeTypeA}
function GetTrusteeTypeW(pTrustee: PTRUSTEE_W): TRUSTEE_TYPE; stdcall;
{$EXTERNALSYM GetTrusteeTypeW}
function GetTrusteeType(pTrustee: PTRUSTEE): TRUSTEE_TYPE; stdcall;
{$EXTERNALSYM GetTrusteeType}

function GetTrusteeFormA(pTrustee: PTRUSTEE_A): TRUSTEE_FORM; stdcall;
{$EXTERNALSYM GetTrusteeFormA}
function GetTrusteeFormW(pTrustee: PTRUSTEE_W): TRUSTEE_FORM; stdcall;
{$EXTERNALSYM GetTrusteeFormW}
function GetTrusteeForm(pTrustee: PTRUSTEE): TRUSTEE_FORM; stdcall;
{$EXTERNALSYM GetTrusteeForm}

function GetMultipleTrusteeOperationA(pTrustee: PTRUSTEE_A): MULTIPLE_TRUSTEE_OPERATION; stdcall;
{$EXTERNALSYM GetMultipleTrusteeOperationA}
function GetMultipleTrusteeOperationW(pTrustee: PTRUSTEE_W): MULTIPLE_TRUSTEE_OPERATION; stdcall;
{$EXTERNALSYM GetMultipleTrusteeOperationW}
function GetMultipleTrusteeOperation(pTrustee: PTRUSTEE): MULTIPLE_TRUSTEE_OPERATION; stdcall;
{$EXTERNALSYM GetMultipleTrusteeOperation}

function GetMultipleTrusteeA(pTrustee: PTRUSTEE_A): PTRUSTEE_A; stdcall;
{$EXTERNALSYM GetMultipleTrusteeA}
function GetMultipleTrusteeW(pTrustee: PTRUSTEE_W): PTRUSTEE_W; stdcall;
{$EXTERNALSYM GetMultipleTrusteeW}
function GetMultipleTrustee(pTrustee: PTRUSTEE): PTRUSTEE; stdcall;
{$EXTERNALSYM GetMultipleTrustee}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}
const
  aclapilib = 'advapi32.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFDEF DYNAMIC_LINK}

var
  _SetEntriesInAclA: Pointer;

function SetEntriesInAclA;
begin
  GetProcedureAddress(_SetEntriesInAclA, aclapilib, 'SetEntriesInAclA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetEntriesInAclA]
  end;
end;

var
  _SetEntriesInAclW: Pointer;

function SetEntriesInAclW;
begin
  GetProcedureAddress(_SetEntriesInAclW, aclapilib, 'SetEntriesInAclW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetEntriesInAclW]
  end;
end;

var
  _SetEntriesInAcl: Pointer;

function SetEntriesInAcl;
begin
  GetProcedureAddress(_SetEntriesInAcl, aclapilib, 'SetEntriesInAcl' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetEntriesInAcl]
  end;
end;

var
  _GetExplicitEntriesFromAclA: Pointer;

function GetExplicitEntriesFromAclA;
begin
  GetProcedureAddress(_GetExplicitEntriesFromAclA, aclapilib, 'GetExplicitEntriesFromAclA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetExplicitEntriesFromAclA]
  end;
end;

var
  _GetExplicitEntriesFromAclW: Pointer;

function GetExplicitEntriesFromAclW;
begin
  GetProcedureAddress(_GetExplicitEntriesFromAclW, aclapilib, 'GetExplicitEntriesFromAclW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetExplicitEntriesFromAclW]
  end;
end;

var
  _GetExplicitEntriesFromAcl: Pointer;

function GetExplicitEntriesFromAcl;
begin
  GetProcedureAddress(_GetExplicitEntriesFromAcl, aclapilib, 'GetExplicitEntriesFromAcl' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetExplicitEntriesFromAcl]
  end;
end;

var
  _GetEffectiveRightsFromAclA: Pointer;

function GetEffectiveRightsFromAclA;
begin
  GetProcedureAddress(_GetEffectiveRightsFromAclA, aclapilib, 'GetEffectiveRightsFromAclA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetEffectiveRightsFromAclA]
  end;
end;

var
  _GetEffectiveRightsFromAclW: Pointer;

function GetEffectiveRightsFromAclW;
begin
  GetProcedureAddress(_GetEffectiveRightsFromAclW, aclapilib, 'GetEffectiveRightsFromAclW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetEffectiveRightsFromAclW]
  end;
end;

var
  _GetEffectiveRightsFromAcl: Pointer;

function GetEffectiveRightsFromAcl;
begin
  GetProcedureAddress(_GetEffectiveRightsFromAcl, aclapilib, 'GetEffectiveRightsFromAcl' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetEffectiveRightsFromAcl]
  end;
end;

var
  _GetAuditedPermissionsFromAclA: Pointer;

function GetAuditedPermissionsFromAclA;
begin
  GetProcedureAddress(_GetAuditedPermissionsFromAclA, aclapilib, 'GetAuditedPermissionsFromAclA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetAuditedPermissionsFromAclA]
  end;
end;

var
  _GetAuditedPermissionsFromAclW: Pointer;

function GetAuditedPermissionsFromAclW;
begin
  GetProcedureAddress(_GetAuditedPermissionsFromAclW, aclapilib, 'GetAuditedPermissionsFromAclW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetAuditedPermissionsFromAclW]
  end;
end;

var
  _GetAuditedPermissionsFromAcl: Pointer;

function GetAuditedPermissionsFromAcl;
begin
  GetProcedureAddress(_GetAuditedPermissionsFromAcl, aclapilib, 'GetAuditedPermissionsFromAcl' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetAuditedPermissionsFromAcl]
  end;
end;

var
  _GetNamedSecurityInfoA: Pointer;

function GetNamedSecurityInfoA;
begin
  GetProcedureAddress(_GetNamedSecurityInfoA, aclapilib, 'GetNamedSecurityInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetNamedSecurityInfoA]
  end;
end;

var
  _GetNamedSecurityInfoW: Pointer;

function GetNamedSecurityInfoW;
begin
  GetProcedureAddress(_GetNamedSecurityInfoW, aclapilib, 'GetNamedSecurityInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetNamedSecurityInfoW]
  end;
end;

var
  _GetNamedSecurityInfo: Pointer;

function GetNamedSecurityInfo;
begin
  GetProcedureAddress(_GetNamedSecurityInfo, aclapilib, 'GetNamedSecurityInfo' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetNamedSecurityInfo]
  end;
end;

var
  _GetSecurityInfo: Pointer;

function GetSecurityInfo;
begin
  GetProcedureAddress(_GetSecurityInfo, aclapilib, 'GetSecurityInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetSecurityInfo]
  end;
end;

var
  _SetNamedSecurityInfoA: Pointer;

function SetNamedSecurityInfoA;
begin
  GetProcedureAddress(_SetNamedSecurityInfoA, aclapilib, 'SetNamedSecurityInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetNamedSecurityInfoA]
  end;
end;

var
  _SetNamedSecurityInfoW: Pointer;

function SetNamedSecurityInfoW;
begin
  GetProcedureAddress(_SetNamedSecurityInfoW, aclapilib, 'SetNamedSecurityInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetNamedSecurityInfoW]
  end;
end;

var
  _SetNamedSecurityInfo: Pointer;

function SetNamedSecurityInfo;
begin
  GetProcedureAddress(_SetNamedSecurityInfo, aclapilib, 'SetNamedSecurityInfo' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetNamedSecurityInfo]
  end;
end;

var
  _SetSecurityInfo: Pointer;

function SetSecurityInfo;
begin
  GetProcedureAddress(_SetSecurityInfo, aclapilib, 'SetSecurityInfo');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_SetSecurityInfo]
  end;
end;

var
  _GetInheritanceSourceA: Pointer;

function GetInheritanceSourceA;
begin
  GetProcedureAddress(_GetInheritanceSourceA, aclapilib, 'GetInheritanceSourceA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetInheritanceSourceA]
  end;
end;

var
  _GetInheritanceSourceW: Pointer;

function GetInheritanceSourceW;
begin
  GetProcedureAddress(_GetInheritanceSourceW, aclapilib, 'GetInheritanceSourceW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetInheritanceSourceW]
  end;
end;

var
  _GetInheritanceSource: Pointer;

function GetInheritanceSource;
begin
  GetProcedureAddress(_GetInheritanceSource, aclapilib, 'GetInheritanceSource' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetInheritanceSource]
  end;
end;

var
  _FreeInheritedFromArray: Pointer;

function FreeInheritedFromArray;
begin
  GetProcedureAddress(_FreeInheritedFromArray, aclapilib, 'FreeInheritedFromArray');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_FreeInheritedFromArray]
  end;
end;

var
  _TreeResetNamedSecurityInfoA: Pointer;

function TreeResetNamedSecurityInfoA;
begin
  GetProcedureAddress(_TreeResetNamedSecurityInfoA, aclapilib, 'TreeResetNamedSecurityInfoA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TreeResetNamedSecurityInfoA]
  end;
end;

var
  _TreeResetNamedSecurityInfoW: Pointer;

function TreeResetNamedSecurityInfoW;
begin
  GetProcedureAddress(_TreeResetNamedSecurityInfoW, aclapilib, 'TreeResetNamedSecurityInfoW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TreeResetNamedSecurityInfoW]
  end;
end;

var
  _TreeResetNamedSecurityInfo: Pointer;

function TreeResetNamedSecurityInfo;
begin
  GetProcedureAddress(_TreeResetNamedSecurityInfo, aclapilib, 'TreeResetNamedSecurityInfo' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_TreeResetNamedSecurityInfo]
  end;
end;

var
  _BuildSecurityDescriptorA: Pointer;

function BuildSecurityDescriptorA;
begin
  GetProcedureAddress(_BuildSecurityDescriptorA, aclapilib, 'BuildSecurityDescriptorA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildSecurityDescriptorA]
  end;
end;

var
  _BuildSecurityDescriptorW: Pointer;

function BuildSecurityDescriptorW;
begin
  GetProcedureAddress(_BuildSecurityDescriptorW, aclapilib, 'BuildSecurityDescriptorW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildSecurityDescriptorW]
  end;
end;

var
  _BuildSecurityDescriptor: Pointer;

function BuildSecurityDescriptor;
begin
  GetProcedureAddress(_BuildSecurityDescriptor, aclapilib, 'BuildSecurityDescriptor' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildSecurityDescriptor]
  end;
end;

var
  _LookupSecurityDescriptorPartsA: Pointer;

function LookupSecurityDescriptorPartsA;
begin
  GetProcedureAddress(_LookupSecurityDescriptorPartsA, aclapilib, 'LookupSecurityDescriptorPartsA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LookupSecurityDescriptorPartsA]
  end;
end;

var
  _LookupSecurityDescriptorPartsW: Pointer;

function LookupSecurityDescriptorPartsW;
begin
  GetProcedureAddress(_LookupSecurityDescriptorPartsW, aclapilib, 'LookupSecurityDescriptorPartsW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LookupSecurityDescriptorPartsW]
  end;
end;

var
  _LookupSecurityDescriptorParts: Pointer;

function LookupSecurityDescriptorParts;
begin
  GetProcedureAddress(_LookupSecurityDescriptorParts, aclapilib, 'LookupSecurityDescriptorParts' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_LookupSecurityDescriptorParts]
  end;
end;

var
  _BuildExplicitAccessWithNameA: Pointer;

procedure BuildExplicitAccessWithNameA;
begin
  GetProcedureAddress(_BuildExplicitAccessWithNameA, aclapilib, 'BuildExplicitAccessWithNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildExplicitAccessWithNameA]
  end;
end;

var
  _BuildExplicitAccessWithNameW: Pointer;

procedure BuildExplicitAccessWithNameW;
begin
  GetProcedureAddress(_BuildExplicitAccessWithNameW, aclapilib, 'BuildExplicitAccessWithNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildExplicitAccessWithNameW]
  end;
end;

var
  _BuildExplicitAccessWithName: Pointer;

procedure BuildExplicitAccessWithName;
begin
  GetProcedureAddress(_BuildExplicitAccessWithName, aclapilib, 'BuildExplicitAccessWithName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildExplicitAccessWithName]
  end;
end;

var
  _BuildImpersonateExplAccWNA: Pointer;

procedure BuildImpersonateExplicitAccessWithNameA;
begin
  GetProcedureAddress(_BuildImpersonateExplAccWNA, aclapilib, 'BuildImpersonateExplicitAccessWithNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildImpersonateExplAccWNA]
  end;
end;

var
  _BuildImpersonateExplAccWNW: Pointer;

procedure BuildImpersonateExplicitAccessWithNameW;
begin
  GetProcedureAddress(_BuildImpersonateExplAccWNW, aclapilib, 'BuildImpersonateExplicitAccessWithNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildImpersonateExplAccWNW]
  end;
end;

var
  _BuildImpersonateExplAccWN: Pointer;

procedure BuildImpersonateExplicitAccessWithName;
begin
  GetProcedureAddress(_BuildImpersonateExplAccWN, aclapilib, 'BuildImpersonateExplicitAccessWithName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildImpersonateExplAccWN]
  end;
end;

var
  _BuildTrusteeWithNameA: Pointer;

procedure BuildTrusteeWithNameA;
begin
  GetProcedureAddress(_BuildTrusteeWithNameA, aclapilib, 'BuildTrusteeWithNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithNameA]
  end;
end;

var
  _BuildTrusteeWithNameW: Pointer;

procedure BuildTrusteeWithNameW;
begin
  GetProcedureAddress(_BuildTrusteeWithNameW, aclapilib, 'BuildTrusteeWithNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithNameW]
  end;
end;

var
  _BuildTrusteeWithName: Pointer;

procedure BuildTrusteeWithName;
begin
  GetProcedureAddress(_BuildTrusteeWithName, aclapilib, 'BuildTrusteeWithName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithName]
  end;
end;

var
  _BuildImpersonateTrusteeA: Pointer;

procedure BuildImpersonateTrusteeA;
begin
  GetProcedureAddress(_BuildImpersonateTrusteeA, aclapilib, 'BuildImpersonateTrusteeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildImpersonateTrusteeA]
  end;
end;

var
  _BuildImpersonateTrusteeW: Pointer;

procedure BuildImpersonateTrusteeW;
begin
  GetProcedureAddress(_BuildImpersonateTrusteeW, aclapilib, 'BuildImpersonateTrusteeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildImpersonateTrusteeW]
  end;
end;

var
  _BuildImpersonateTrustee: Pointer;

procedure BuildImpersonateTrustee;
begin
  GetProcedureAddress(_BuildImpersonateTrustee, aclapilib, 'BuildImpersonateTrustee' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildImpersonateTrustee]
  end;
end;

var
  _BuildTrusteeWithSidA: Pointer;

procedure BuildTrusteeWithSidA;
begin
  GetProcedureAddress(_BuildTrusteeWithSidA, aclapilib, 'BuildTrusteeWithSidA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithSidA]
  end;
end;

var
  _BuildTrusteeWithSidW: Pointer;

procedure BuildTrusteeWithSidW;
begin
  GetProcedureAddress(_BuildTrusteeWithSidW, aclapilib, 'BuildTrusteeWithSidW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithSidW]
  end;
end;

var
  _BuildTrusteeWithSid: Pointer;

procedure BuildTrusteeWithSid;
begin
  GetProcedureAddress(_BuildTrusteeWithSid, aclapilib, 'BuildTrusteeWithSid' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithSid]
  end;
end;

var
  _BuildTrusteeWithObjectsAndSidA: Pointer;

procedure BuildTrusteeWithObjectsAndSidA;
begin
  GetProcedureAddress(_BuildTrusteeWithObjectsAndSidA, aclapilib, 'BuildTrusteeWithObjectsAndSidA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithObjectsAndSidA]
  end;
end;

var
  _BuildTrusteeWithObjectsAndSidW: Pointer;

procedure BuildTrusteeWithObjectsAndSidW;
begin
  GetProcedureAddress(_BuildTrusteeWithObjectsAndSidW, aclapilib, 'BuildTrusteeWithObjectsAndSidW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithObjectsAndSidW]
  end;
end;

var
  _BuildTrusteeWithObjectsAndSid: Pointer;

procedure BuildTrusteeWithObjectsAndSid;
begin
  GetProcedureAddress(_BuildTrusteeWithObjectsAndSid, aclapilib, 'BuildTrusteeWithObjectsAndSid' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithObjectsAndSid]
  end;
end;

var
  _BuildTrusteeWithObjectsAndNameA: Pointer;

procedure BuildTrusteeWithObjectsAndNameA;
begin
  GetProcedureAddress(_BuildTrusteeWithObjectsAndNameA, aclapilib, 'BuildTrusteeWithObjectsAndNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithObjectsAndNameA]
  end;
end;

var
  _BuildTrusteeWithObjectsAndNameW: Pointer;

procedure BuildTrusteeWithObjectsAndNameW;
begin
  GetProcedureAddress(_BuildTrusteeWithObjectsAndNameW, aclapilib, 'BuildTrusteeWithObjectsAndNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithObjectsAndNameW]
  end;
end;

var
  _BuildTrusteeWithObjectsAndName: Pointer;

procedure BuildTrusteeWithObjectsAndName;
begin
  GetProcedureAddress(_BuildTrusteeWithObjectsAndName, aclapilib, 'BuildTrusteeWithObjectsAndName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_BuildTrusteeWithObjectsAndName]
  end;
end;

var
  _GetTrusteeNameA: Pointer;

function GetTrusteeNameA;
begin
  GetProcedureAddress(_GetTrusteeNameA, aclapilib, 'GetTrusteeNameA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeNameA]
  end;
end;

var
  _GetTrusteeNameW: Pointer;

function GetTrusteeNameW;
begin
  GetProcedureAddress(_GetTrusteeNameW, aclapilib, 'GetTrusteeNameW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeNameW]
  end;
end;

var
  _GetTrusteeName: Pointer;

function GetTrusteeName;
begin
  GetProcedureAddress(_GetTrusteeName, aclapilib, 'GetTrusteeName' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeName]
  end;
end;

var
  _GetTrusteeTypeA: Pointer;

function GetTrusteeTypeA;
begin
  GetProcedureAddress(_GetTrusteeTypeA, aclapilib, 'GetTrusteeTypeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeTypeA]
  end;
end;

var
  _GetTrusteeTypeW: Pointer;

function GetTrusteeTypeW;
begin
  GetProcedureAddress(_GetTrusteeTypeW, aclapilib, 'GetTrusteeTypeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeTypeW]
  end;
end;

var
  _GetTrusteeType: Pointer;

function GetTrusteeType;
begin
  GetProcedureAddress(_GetTrusteeType, aclapilib, 'GetTrusteeType' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeType]
  end;
end;

var
  _GetTrusteeFormA: Pointer;

function GetTrusteeFormA;
begin
  GetProcedureAddress(_GetTrusteeFormA, aclapilib, 'GetTrusteeFormA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeFormA]
  end;
end;

var
  _GetTrusteeFormW: Pointer;

function GetTrusteeFormW;
begin
  GetProcedureAddress(_GetTrusteeFormW, aclapilib, 'GetTrusteeFormW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeFormW]
  end;
end;

var
  _GetTrusteeForm: Pointer;

function GetTrusteeForm;
begin
  GetProcedureAddress(_GetTrusteeForm, aclapilib, 'GetTrusteeForm' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetTrusteeForm]
  end;
end;

var
  _GetMultipleTrusteeOperationA: Pointer;

function GetMultipleTrusteeOperationA;
begin
  GetProcedureAddress(_GetMultipleTrusteeOperationA, aclapilib, 'GetMultipleTrusteeOperationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMultipleTrusteeOperationA]
  end;
end;

var
  _GetMultipleTrusteeOperationW: Pointer;

function GetMultipleTrusteeOperationW;
begin
  GetProcedureAddress(_GetMultipleTrusteeOperationW, aclapilib, 'GetMultipleTrusteeOperationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMultipleTrusteeOperationW]
  end;
end;

var
  _GetMultipleTrusteeOperation: Pointer;

function GetMultipleTrusteeOperation;
begin
  GetProcedureAddress(_GetMultipleTrusteeOperation, aclapilib, 'GetMultipleTrusteeOperation' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMultipleTrusteeOperation]
  end;
end;

var
  _GetMultipleTrusteeA: Pointer;

function GetMultipleTrusteeA;
begin
  GetProcedureAddress(_GetMultipleTrusteeA, aclapilib, 'GetMultipleTrusteeA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMultipleTrusteeA]
  end;
end;

var
  _GetMultipleTrusteeW: Pointer;

function GetMultipleTrusteeW;
begin
  GetProcedureAddress(_GetMultipleTrusteeW, aclapilib, 'GetMultipleTrusteeW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMultipleTrusteeW]
  end;
end;

var
  _GetMultipleTrustee: Pointer;

function GetMultipleTrustee;
begin
  GetProcedureAddress(_GetMultipleTrustee, aclapilib, 'GetMultipleTrustee' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetMultipleTrustee]
  end;
end;

{$ELSE}

function SetEntriesInAclA; external aclapilib name 'SetEntriesInAclA';
function SetEntriesInAclW; external aclapilib name 'SetEntriesInAclW';
function SetEntriesInAcl; external aclapilib name 'SetEntriesInAcl' + AWSuffix;
function GetExplicitEntriesFromAclA; external aclapilib name 'GetExplicitEntriesFromAclA';
function GetExplicitEntriesFromAclW; external aclapilib name 'GetExplicitEntriesFromAclW';
function GetExplicitEntriesFromAcl; external aclapilib name 'GetExplicitEntriesFromAcl' + AWSuffix;
function GetEffectiveRightsFromAclA; external aclapilib name 'GetEffectiveRightsFromAclA';
function GetEffectiveRightsFromAclW; external aclapilib name 'GetEffectiveRightsFromAclW';
function GetEffectiveRightsFromAcl; external aclapilib name 'GetEffectiveRightsFromAcl' + AWSuffix;
function GetAuditedPermissionsFromAclA; external aclapilib name 'GetAuditedPermissionsFromAclA';
function GetAuditedPermissionsFromAclW; external aclapilib name 'GetAuditedPermissionsFromAclW';
function GetAuditedPermissionsFromAcl; external aclapilib name 'GetAuditedPermissionsFromAcl' + AWSuffix;
function GetNamedSecurityInfoA; external aclapilib name 'GetNamedSecurityInfoA';
function GetNamedSecurityInfoW; external aclapilib name 'GetNamedSecurityInfoW';
function GetNamedSecurityInfo; external aclapilib name 'GetNamedSecurityInfo' + AWSuffix;
function GetSecurityInfo; external aclapilib name 'GetSecurityInfo';
function SetNamedSecurityInfoA; external aclapilib name 'SetNamedSecurityInfoA';
function SetNamedSecurityInfoW; external aclapilib name 'SetNamedSecurityInfoW';
function SetNamedSecurityInfo; external aclapilib name 'SetNamedSecurityInfo' + AWSuffix;
function SetSecurityInfo; external aclapilib name 'SetSecurityInfo';
function GetInheritanceSourceA; external aclapilib name 'GetInheritanceSourceA';
function GetInheritanceSourceW; external aclapilib name 'GetInheritanceSourceW';
function GetInheritanceSource; external aclapilib name 'GetInheritanceSource' + AWSuffix;
function FreeInheritedFromArray; external aclapilib name 'FreeInheritedFromArray';
function TreeResetNamedSecurityInfoA; external aclapilib name 'TreeResetNamedSecurityInfoA';
function TreeResetNamedSecurityInfoW; external aclapilib name 'TreeResetNamedSecurityInfoW';
function TreeResetNamedSecurityInfo; external aclapilib name 'TreeResetNamedSecurityInfo' + AWSuffix;
function BuildSecurityDescriptorA; external aclapilib name 'BuildSecurityDescriptorA';
function BuildSecurityDescriptorW; external aclapilib name 'BuildSecurityDescriptorW';
function BuildSecurityDescriptor; external aclapilib name 'BuildSecurityDescriptor' + AWSuffix;
function LookupSecurityDescriptorPartsA; external aclapilib name 'LookupSecurityDescriptorPartsA';
function LookupSecurityDescriptorPartsW; external aclapilib name 'LookupSecurityDescriptorPartsW';
function LookupSecurityDescriptorParts; external aclapilib name 'LookupSecurityDescriptorParts' + AWSuffix;
procedure BuildExplicitAccessWithNameA; external aclapilib name 'BuildExplicitAccessWithNameA';
procedure BuildExplicitAccessWithNameW; external aclapilib name 'BuildExplicitAccessWithNameW';
procedure BuildExplicitAccessWithName; external aclapilib name 'BuildExplicitAccessWithName' + AWSuffix;
procedure BuildImpersonateExplicitAccessWithNameA; external aclapilib name 'BuildImpersonateExplicitAccessWithNameA';
procedure BuildImpersonateExplicitAccessWithNameW; external aclapilib name 'BuildImpersonateExplicitAccessWithNameW';
procedure BuildImpersonateExplicitAccessWithName; external aclapilib name 'BuildImpersonateExplicitAccessWithName' + AWSuffix;
procedure BuildTrusteeWithNameA; external aclapilib name 'BuildTrusteeWithNameA';
procedure BuildTrusteeWithNameW; external aclapilib name 'BuildTrusteeWithNameW';
procedure BuildTrusteeWithName; external aclapilib name 'BuildTrusteeWithName' + AWSuffix;
procedure BuildImpersonateTrusteeA; external aclapilib name 'BuildImpersonateTrusteeA';
procedure BuildImpersonateTrusteeW; external aclapilib name 'BuildImpersonateTrusteeW';
procedure BuildImpersonateTrustee; external aclapilib name 'BuildImpersonateTrustee' + AWSuffix;
procedure BuildTrusteeWithSidA; external aclapilib name 'BuildTrusteeWithSidA';
procedure BuildTrusteeWithSidW; external aclapilib name 'BuildTrusteeWithSidW';
procedure BuildTrusteeWithSid; external aclapilib name 'BuildTrusteeWithSid' + AWSuffix;
procedure BuildTrusteeWithObjectsAndSidA; external aclapilib name 'BuildTrusteeWithObjectsAndSidA';
procedure BuildTrusteeWithObjectsAndSidW; external aclapilib name 'BuildTrusteeWithObjectsAndSidW';
procedure BuildTrusteeWithObjectsAndSid; external aclapilib name 'BuildTrusteeWithObjectsAndSid' + AWSuffix;
procedure BuildTrusteeWithObjectsAndNameA; external aclapilib name 'BuildTrusteeWithObjectsAndNameA';
procedure BuildTrusteeWithObjectsAndNameW; external aclapilib name 'BuildTrusteeWithObjectsAndNameW';
procedure BuildTrusteeWithObjectsAndName; external aclapilib name 'BuildTrusteeWithObjectsAndName' + AWSuffix;
function GetTrusteeNameA; external aclapilib name 'GetTrusteeNameA';
function GetTrusteeNameW; external aclapilib name 'GetTrusteeNameW';
function GetTrusteeName; external aclapilib name 'GetTrusteeName' + AWSuffix;
function GetTrusteeTypeA; external aclapilib name 'GetTrusteeTypeA';
function GetTrusteeTypeW; external aclapilib name 'GetTrusteeTypeW';
function GetTrusteeType; external aclapilib name 'GetTrusteeType' + AWSuffix;
function GetTrusteeFormA; external aclapilib name 'GetTrusteeFormA';
function GetTrusteeFormW; external aclapilib name 'GetTrusteeFormW';
function GetTrusteeForm; external aclapilib name 'GetTrusteeForm' + AWSuffix;
function GetMultipleTrusteeOperationA; external aclapilib name 'GetMultipleTrusteeOperationA';
function GetMultipleTrusteeOperationW; external aclapilib name 'GetMultipleTrusteeOperationW';
function GetMultipleTrusteeOperation; external aclapilib name 'GetMultipleTrusteeOperation' + AWSuffix;
function GetMultipleTrusteeA; external aclapilib name 'GetMultipleTrusteeA';
function GetMultipleTrusteeW; external aclapilib name 'GetMultipleTrusteeW';
function GetMultipleTrustee; external aclapilib name 'GetMultipleTrustee' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
