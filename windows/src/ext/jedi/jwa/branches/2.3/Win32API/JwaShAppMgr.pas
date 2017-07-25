{******************************************************************************}
{                                                                              }
{ ShAppMgr Interface Unit for Object Pascal                                    }
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
{ The original code is: shappmgr.h, appmgmt.h                     			   }
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
{ Known issues:                                                   			   }
{ - the INSTALLSPECTYPE and APPSTATE enumerations contained       			   }
{   values that clashed with Delphi identifiers, so they were     			   }
{   renamed with the prefixes "ist" and "as", respectively.       			   }
{                                                                              }
{******************************************************************************}
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaShAppMgr;
{$I ..\Includes\JediAPILib.inc}


interface

uses 
 JwaWinBase, JwaWinType;

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

{$HPPEMIT '#include "shappmgr.h"'}

//*************************************************************
//
// appmgmt.h
//
// APIs for operations on MSI applications which are deployed
// and managed in the NT Directory.
//
// Copyright (c) Microsoft Corporation 1998-1999
// All rights reserved
//
//*************************************************************
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}

type
  {$EXTERNALSYM _INSTALLSPECTYPE}
  _INSTALLSPECTYPE = DWORD;
  {$EXTERNALSYM INSTALLSPECTYPE}
  INSTALLSPECTYPE = _INSTALLSPECTYPE;
  TInstallSpecType = _INSTALLSPECTYPE;

const
  istAPPNAME  = 1;              // renamed: original name was APPNAME
  istFILEEXT  = 2;              // renamed: original name was FILEEXT
  istPROGID   = 3;              // renamed: original name was PROGID
  istCOMCLASS = 4;              // renamed: original name was COMCLASS

type
  PInstallSpec = ^TInstallSpec;
  {$EXTERNALSYM _INSTALLSPEC}
  _INSTALLSPEC = record
    case Byte of
      0: (AppName: record
            Name: PWideChar;
            GPOId: TGUID;
          end);
      1: (FileExt: PWideChar);
      2: (ProgId: PWideChar);
      3: (ComClass: record
            Clsid: TGUID;
            ClsCtx: DWORD;
          end);
  end;
  {$EXTERNALSYM INSTALLSPEC}
  INSTALLSPEC = _INSTALLSPEC;
  TInstallSpec = _INSTALLSPEC;

  PInstallData = ^TInstallData;
  {$EXTERNALSYM _INSTALLDATA}
  _INSTALLDATA = record
    _Type: TInstallSpecType;    // renamed: original name was Type
    Spec: TInstallSpec;
  end;
  {$EXTERNALSYM INSTALLDATA}
  INSTALLDATA = _INSTALLDATA;
  TInstallData = _INSTALLDATA;

  {$EXTERNALSYM APPSTATE}
  APPSTATE = DWORD;
  TAppState = DWORD;

const
  asABSENT    = 0;              // renamed: original name was ABSENT
  asASSIGNED  = 1;              // renamed: original name was ASSIGNED
  asPUBLISHED = 2;              // renamed: original name was PUBLISHED

  {$EXTERNALSYM LOCALSTATE_ASSIGNED}
  LOCALSTATE_ASSIGNED                 = $1;     // app is assigned
  {$EXTERNALSYM LOCALSTATE_PUBLISHED}
  LOCALSTATE_PUBLISHED                = $2;     // app is published
  {$EXTERNALSYM LOCALSTATE_UNINSTALL_UNMANAGED}
  LOCALSTATE_UNINSTALL_UNMANAGED      = $4;     // uninstall any unmanaged version before assigning
  {$EXTERNALSYM LOCALSTATE_POLICYREMOVE_ORPHAN}
  LOCALSTATE_POLICYREMOVE_ORPHAN      = $8;     // app is orphaned when policy removed
  {$EXTERNALSYM LOCALSTATE_POLICYREMOVE_UNINSTALL}
  LOCALSTATE_POLICYREMOVE_UNINSTALL   = $10;    // app is uninstalled when policy removed
  {$EXTERNALSYM LOCALSTATE_ORPHANED}
  LOCALSTATE_ORPHANED                 = $20;    // app is orphaned after being applied
  {$EXTERNALSYM LOCALSTATE_UNINSTALLED}
  LOCALSTATE_UNINSTALLED              = $40;    // app is uninstalled after being applied

type
  PLocalManagedApplication = ^TLocalManagedApplication;
  {$EXTERNALSYM _LOCALMANAGEDAPPLICATION}
  _LOCALMANAGEDAPPLICATION = record
    pszDeploymentName: PWideChar;
    pszPolicyName: PWideChar;
    pszProductId: PWideChar;
    dwState: DWORD;
  end;
  {$EXTERNALSYM LOCALMANAGEDAPPLICATION}
  LOCALMANAGEDAPPLICATION = _LOCALMANAGEDAPPLICATION;
  TLocalManagedApplication = _LOCALMANAGEDAPPLICATION;

const
  {$EXTERNALSYM MANAGED_APPS_USERAPPLICATIONS}
  MANAGED_APPS_USERAPPLICATIONS  = $1;
  {$EXTERNALSYM MANAGED_APPS_FROMCATEGORY}
  MANAGED_APPS_FROMCATEGORY      = $2;
  {$EXTERNALSYM MANAGED_APPS_INFOLEVEL_DEFAULT}
  MANAGED_APPS_INFOLEVEL_DEFAULT = $10000;

  {$EXTERNALSYM MANAGED_APPTYPE_WINDOWSINSTALLER}
  MANAGED_APPTYPE_WINDOWSINSTALLER = $1;
  {$EXTERNALSYM MANAGED_APPTYPE_SETUPEXE}
  MANAGED_APPTYPE_SETUPEXE         = $2;
  {$EXTERNALSYM MANAGED_APPTYPE_UNSUPPORTED}
  MANAGED_APPTYPE_UNSUPPORTED      = $3;

type
  PManagedApplication = ^TManagedApplication;
  {$EXTERNALSYM _MANAGEDAPPLICATION}
  _MANAGEDAPPLICATION = record
    pszPackageName: PWideChar;
    pszPublisher: PWideChar;
    dwVersionHi: DWORD;
    dwVersionLo: DWORD;
    dwRevision: DWORD;
    GpoId: TGUID;
    pszPolicyName: PWideChar;
    ProductId: TGUID;
    Language: LANGID;
    pszOwner: PWideChar;
    pszCompany: PWideChar;
    pszComments: PWideChar;
    pszContact: PWideChar;
    pszSupportUrl: PWideChar;
    dwPathType: DWORD;
    bInstalled: BOOL;
  end;
  {$EXTERNALSYM MANAGEDAPPLICATION}
  MANAGEDAPPLICATION = _MANAGEDAPPLICATION;
  TManagedApplication = _MANAGEDAPPLICATION;

  PAppCategoryInfo = ^TAppCategoryInfo;
  {$EXTERNALSYM _APPCATEGORYINFO}
  _APPCATEGORYINFO = record
    Locale: LCID;
    pszDescription: PWideChar;
    AppCategoryId: TGUID;
  end;
  {$EXTERNALSYM APPCATEGORYINFO}
  APPCATEGORYINFO = _APPCATEGORYINFO;
  TAppCategoryInfo = _APPCATEGORYINFO;

  PAppCategoryInfoList = ^TAppCategoryInfoList;
  {$EXTERNALSYM _APPCATEGORYINFOLIST}
  _APPCATEGORYINFOLIST = record
    cCategory: DWORD;
    pCategoryInfo: PAppCategoryInfo;
  end;
  {$EXTERNALSYM APPCATEGORYINFOLIST}
  APPCATEGORYINFOLIST = _APPCATEGORYINFOLIST;
  TAppCategoryInfoList = _APPCATEGORYINFOLIST;

{$EXTERNALSYM InstallApplication}
function InstallApplication(pInstallInfo: PInstallData): DWORD; stdcall;
{$EXTERNALSYM UninstallApplication}
function UninstallApplication(ProductCode: PWideChar;
  dwStatus: DWORD): DWORD; stdcall;
{$EXTERNALSYM CommandLineFromMsiDescriptor}
function CommandLineFromMsiDescriptor(Descriptor, CommandLine: PWideChar;
  var CommandLineLength: DWORD): DWORD; stdcall;
{$EXTERNALSYM GetManagedApplications}
function GetManagedApplications(const pCategory: TGUID;
  dwQueryFlags, dwInfoLevel: DWORD; out pdwApps: DWORD;
  out prgManagedApps: PManagedApplication): DWORD; stdcall;
{$EXTERNALSYM GetLocalManagedApplications}
function GetLocalManagedApplications(bUserApps: BOOL; out pdwApps: DWORD;
  out prgLocalApps: PLocalManagedApplication): DWORD; stdcall;
{$EXTERNALSYM GetLocalManagedApplicationData}
procedure GetLocalManagedApplicationData(ProductCode: PWideChar;
  out DisplayName, SupportUrl: PWideChar); stdcall;
{$EXTERNALSYM GetManagedApplicationCategories}
function GetManagedApplicationCategories(dwReserved: DWORD;
  out pAppCategory: TAppCategoryInfoList): DWORD; stdcall;

// shappmgr.h -----------------------------

{$HPPEMIT 'interface DECLSPEC_UUID("A3E14960-935F-11D1-B8B8-006008059382") IShellApp;'}
{$HPPEMIT 'interface DECLSPEC_UUID("1BC752E0-9046-11D1-B8B3-006008059382") IPublishedApp;'}
{$HPPEMIT 'interface DECLSPEC_UUID("0B124F8C-91F0-11D1-B8B5-006008059382") IEnumPublishedApps;'}
{$HPPEMIT 'interface DECLSPEC_UUID("07250A10-9CF9-11D1-9076-006008059382") IAppPublisher;'}

{$HPPEMIT 'typedef System::DelphiInterface<IShellApp> _di_IShellApp;'}
{$HPPEMIT 'typedef System::DelphiInterface<IPublishedApp> _di_IPublishedApp;'}
{$HPPEMIT 'typedef System::DelphiInterface<IEnumPublishedApps> _di_IEnumPublishedApps;'}
{$HPPEMIT 'typedef System::DelphiInterface<IAppPublisher> _di_IAppPublisher;'}

const
  {$EXTERNALSYM IID_IShellApp}
  IID_IShellApp: TGUID = '{A3E14960-935F-11D1-B8B8-006008059382}';
  {$EXTERNALSYM IID_IPublishedApp}
  IID_IPublishedApp: TGUID = '{1BC752E0-9046-11D1-B8B3-006008059382}';
  {$EXTERNALSYM IID_IEnumPublishedApps}
  IID_IEnumPublishedApps: TGUID = '{0B124F8C-91F0-11D1-B8B5-006008059382}';
  {$EXTERNALSYM IID_IAppPublisher}
  IID_IAppPublisher: TGUID = '{07250A10-9CF9-11D1-9076-006008059382}';

type
  {$EXTERNALSYM _tagAppInfoFlags}
  _tagAppInfoFlags = DWORD;
  {$EXTERNALSYM APPINFODATAFLAGS}
  APPINFODATAFLAGS = _tagAppInfoFlags;
  TAppInfoDataFlags = _tagAppInfoFlags;

const
  {$EXTERNALSYM AIM_DISPLAYNAME}
  AIM_DISPLAYNAME       = $1;
  {$EXTERNALSYM AIM_VERSION}
  AIM_VERSION           = $2;
  {$EXTERNALSYM AIM_PUBLISHER}
  AIM_PUBLISHER         = $4;
  {$EXTERNALSYM AIM_PRODUCTID}
  AIM_PRODUCTID         = $8;
  {$EXTERNALSYM AIM_REGISTEREDOWNER}
  AIM_REGISTEREDOWNER   = $10;
  {$EXTERNALSYM AIM_REGISTEREDCOMPANY}
  AIM_REGISTEREDCOMPANY = $20;
  {$EXTERNALSYM AIM_LANGUAGE}
  AIM_LANGUAGE          = $40;
  {$EXTERNALSYM AIM_SUPPORTURL}
  AIM_SUPPORTURL        = $80;
  {$EXTERNALSYM AIM_SUPPORTTELEPHONE}
  AIM_SUPPORTTELEPHONE  = $100;
  {$EXTERNALSYM AIM_HELPLINK}
  AIM_HELPLINK          = $200;
  {$EXTERNALSYM AIM_INSTALLLOCATION}
  AIM_INSTALLLOCATION   = $400;
  {$EXTERNALSYM AIM_INSTALLSOURCE}
  AIM_INSTALLSOURCE     = $800;
  {$EXTERNALSYM AIM_INSTALLDATE}
  AIM_INSTALLDATE       = $1000;
  {$EXTERNALSYM AIM_CONTACT}
  AIM_CONTACT           = $4000;
  {$EXTERNALSYM AIM_COMMENTS}
  AIM_COMMENTS          = $8000;
  {$EXTERNALSYM AIM_IMAGE}
  AIM_IMAGE             = $20000;
  {$EXTERNALSYM AIM_READMEURL}
  AIM_READMEURL         = $40000;
  {$EXTERNALSYM AIM_UPDATEINFOURL}
  AIM_UPDATEINFOURL     = $80000;

type
  PAppInfoData = ^TAppInfoData;
  {$EXTERNALSYM _AppInfoData}
  _AppInfoData = record
    cbSize: DWORD;
    dwMask: DWORD;
    pszDisplayName: PWideChar;
    pszVersion: PWideChar;
    pszPublisher: PWideChar;
    pszProductID: PWideChar;
    pszRegisteredOwner: PWideChar;
    pszRegisteredCompany: PWideChar;
    pszLanguage: PWideChar;
    pszSupportUrl: PWideChar;
    pszSupportTelephone: PWideChar;
    pszHelpLink: PWideChar;
    pszInstallLocation: PWideChar;
    pszInstallSource: PWideChar;
    pszInstallDate: PWideChar;
    pszContact: PWideChar;
    pszComments: PWideChar;
    pszImage: PWideChar;
    pszReadmeUrl: PWideChar;
    pszUpdateInfoUrl: PWideChar;
  end;
  {$EXTERNALSYM APPINFODATA}
  APPINFODATA = _AppInfoData;
  TAppInfoData = _AppInfoData;

  {$EXTERNALSYM _tagAppActionFlags}
  _tagAppActionFlags = DWORD;
  {$EXTERNALSYM APPACTIONFLAGS}
  APPACTIONFLAGS = _tagAppActionFlags;
  TAppActionFlags = _tagAppActionFlags;

const
  {$EXTERNALSYM APPACTION_INSTALL}
  APPACTION_INSTALL      = $1;
  {$EXTERNALSYM APPACTION_UNINSTALL}
  APPACTION_UNINSTALL    = $2;
  {$EXTERNALSYM APPACTION_MODIFY}
  APPACTION_MODIFY       = $4;
  {$EXTERNALSYM APPACTION_REPAIR}
  APPACTION_REPAIR       = $8;
  {$EXTERNALSYM APPACTION_UPGRADE}
  APPACTION_UPGRADE      = $10;
  {$EXTERNALSYM APPACTION_CANGETSIZE}
  APPACTION_CANGETSIZE   = $20;
  {$EXTERNALSYM APPACTION_MODIFYREMOVE}
  APPACTION_MODIFYREMOVE = $80;
  {$EXTERNALSYM APPACTION_ADDLATER}
  APPACTION_ADDLATER     = $100;
  {$EXTERNALSYM APPACTION_UNSCHEDULE}
  APPACTION_UNSCHEDULE   = $200;

type
  PSlowAppInfo = ^TSlowAppInfo;
  {$EXTERNALSYM _tagSlowAppInfo}
  _tagSlowAppInfo = record
    ullSize: Int64;
    ftLastUsed: TFileTime;
    iTimesUsed: Integer;
    pszImage: PWideChar;
  end;
  {$EXTERNALSYM SLOWAPPINFO}
  SLOWAPPINFO = _tagSlowAppInfo;
  TSlowAppInfo = _tagSlowAppInfo;

  {$EXTERNALSYM IShellApp}
  IShellApp = interface(IUnknown)
  ['{A3E14960-935F-11D1-B8B8-006008059382}']
    function GetAppInfo(var pai: TAppInfoData): HResult; stdcall;
    function GetPossibleActions(out pdwActions: DWORD): HResult; stdcall;
    function GetSlowAppInfo(var psaid: TSlowAppInfo): HResult; stdcall;
    function GetCachedSlowAppInfo(var psaid: TSloWAppInfo): HResult; stdcall;
    function IsInstalled: HResult; stdcall;
  end;

  {$EXTERNALSYM _tagPublishedAppInfoFlags}
  _tagPublishedAppInfoFlags = DWORD;
  {$EXTERNALSYM PUBAPPINFOFLAGS}
  PUBAPPINFOFLAGS = _tagPublishedAppInfoFlags;
  TPubAppInfoFlags = _tagPublishedAppInfoFlags;

const
  {$EXTERNALSYM PAI_SOURCE}
  PAI_SOURCE        = $1;
  {$EXTERNALSYM PAI_ASSIGNEDTIME}
  PAI_ASSIGNEDTIME  = $2;
  {$EXTERNALSYM PAI_PUBLISHEDTIME}
  PAI_PUBLISHEDTIME = $4;
  {$EXTERNALSYM PAI_SCHEDULEDTIME}
  PAI_SCHEDULEDTIME = $8;
  {$EXTERNALSYM PAI_EXPIRETIME}
  PAI_EXPIRETIME    = $10;

type
  PPubAppInfo = ^TPubAppInfo;
  {$EXTERNALSYM _PubAppInfo}
  _PubAppInfo = record
    cbSize: DWORD;
    dwMask: DWORD;
    pszSource: PWideChar;
    stAssigned: TSystemTime;
    stPublished: TSystemTime;
    stScheduled: TSystemTime;
    stExpire: TSystemTime;
  end;
  {$EXTERNALSYM PUBAPPINFO}
  PUBAPPINFO = _PubAppInfo;
  TPubAppInfo = _PubAppInfo;

  {$EXTERNALSYM IPublishedApp}
  IPublishedApp = interface(IShellApp)
  ['{1BC752E0-9046-11D1-B8B3-006008059382}']
    function Install(var pstInstall: TSystemTime): HResult; stdcall;
    function GetPublishedAppInfo(var ppai: TPubAppInfo): HResult; stdcall;
    function Unschedule: HResult; stdcall;
  end;

  {$EXTERNALSYM IEnumPublishedApps}
  IEnumPublishedApps = interface(IUnknown)
  ['{0B124F8C-91F0-11D1-B8B5-006008059382}']
    function Next(out pia: IPublishedApp): HResult; stdcall;
    function Reset: HResult; stdcall;
  end;

  {$EXTERNALSYM IAppPublisher}
  IAppPublisher = interface(IUnknown)
  ['{07250A10-9CF9-11D1-9076-006008059382}']
    function GetNumberOfCategories(var pdwCat: DWORD): HResult; stdcall;
    function GetCategories(
      out pAppCategoryList: TAppCategoryInfoList): HResult; stdcall;
    function GetNumberOfApps(var pdwApps: DWORD): HResult; stdcall;
    function EnumApps(const pAppCategoryId: TGUID;
      out ppepa: IEnumPublishedApps): HResult; stdcall;
  end;

  
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INCLUDEMODE}
const
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}
{$ENDIF JWA_INCLUDEMODE}

{$IFNDEF JWA_INTERFACESECTION}


{$IFNDEF DYNAMIC_LINK}
function InstallApplication; external Advapi32 name 'InstallApplication';
function UninstallApplication; external Advapi32 name 'UninstallApplication';
function CommandLineFromMsiDescriptor; external Advapi32 name 'CommandLineFromMsiDescriptor';
function GetManagedApplications; external Advapi32 name 'GetManagedApplications';
function GetLocalManagedApplications; external Advapi32 name 'GetLocalManagedApplications';
procedure GetLocalManagedApplicationData; external Advapi32 name 'GetLocalManagedApplicationData';
function GetManagedApplicationCategories; external Advapi32 name 'GetManagedApplicationCategories';

{$ELSE}

var
  _InstallApplication: Pointer;

function InstallApplication;
begin
  GetProcedureAddress(_InstallApplication, Advapi32, 'InstallApplication');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_InstallApplication]
  end;
end;

var
  _UninstallApplication: Pointer;

function UninstallApplication;
begin
  GetProcedureAddress(_UninstallApplication, Advapi32, 'UninstallApplication');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_UninstallApplication]
  end;
end;

var
  _CommandLineFromMsiDescriptor: Pointer;

function CommandLineFromMsiDescriptor;
begin
  GetProcedureAddress(_CommandLineFromMsiDescriptor, Advapi32, 'CommandLineFromMsiDescriptor');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CommandLineFromMsiDescriptor]
  end;
end;

var
  _GetManagedApplications: Pointer;

function GetManagedApplications;
begin
  GetProcedureAddress(_GetManagedApplications, Advapi32, 'GetManagedApplications');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetManagedApplications]
  end;
end;

var
  _GetLocalManagedApplications: Pointer;

function GetLocalManagedApplications;
begin
  GetProcedureAddress(_GetLocalManagedApplications, Advapi32, 'GetLocalManagedApplications');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetLocalManagedApplications]
  end;
end;

var
  _GetLocalManagedApplicationData: Pointer;

procedure GetLocalManagedApplicationData;
begin
  GetProcedureAddress(_GetLocalManagedApplicationData, Advapi32, 'GetLocalManagedApplicationData');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetLocalManagedApplicationData]
  end;
end;

var
  _GetManagedApplicationCategories: Pointer;

function GetManagedApplicationCategories;
begin
  GetProcedureAddress(_GetManagedApplicationCategories, Advapi32, 'GetManagedApplicationCategories');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_GetManagedApplicationCategories]
  end;
end;



{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
