{******************************************************************************}
{                                                                              }
{ Shell Dispatch Interface Unit for Object Pascal                     		   }
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
{ The original code is: shldisp.h, released 2005.                 			   }	
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
unit JwaShlDisp;
{$I ..\Includes\JediAPILib.inc}

interface

{$HPPEMIT '#include "shldisp.h"'}

uses
  JwaWinBase, JwaWinUser, JwaWinType, JwaActiveX;


{$HPPEMIT 'interface DECLSPEC_UUID("9BA05970-F6A8-11CF-A442-00A0C90A8F39") IFolderViewOC;'}
{$HPPEMIT 'interface DECLSPEC_UUID("62112AA2-EBE4-11CF-A5FB-0020AFE7292D") DShellFolderViewEvents;'}
{$HPPEMIT 'interface DECLSPEC_UUID("4A3DF050-23BD-11D2-939F-00A0C91EEDBA") DFConstraint;'}
{$HPPEMIT 'interface DECLSPEC_UUID("1D2EFD50-75CE-11D1-B75A-00A0C90564FE") ISearchCommandExt;'}
{$HPPEMIT 'interface DECLSPEC_UUID("FAC32C80-CBE4-11CE-8350-444553540000") FolderItem;'}
{$HPPEMIT 'interface DECLSPEC_UUID("744129E0-CBE5-11CE-8350-444553540000") FolderItems;'}
{$HPPEMIT 'interface DECLSPEC_UUID("08EC3E00-50B0-11CF-960C-0080C7F4EE85") FolderItemVerb;'}
{$HPPEMIT 'interface DECLSPEC_UUID("1F8352C0-50B0-11CF-960C-0080C7F4EE85") FolderItemVerbs;'}
{$HPPEMIT 'interface DECLSPEC_UUID("BBCBDE60-C3FF-11CE-8350-444553540000") Folder;'}
{$HPPEMIT 'interface DECLSPEC_UUID("F0D2D8EF-3890-11D2-BF8B-00C04FB93661") Folder2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("A7AE5F64-C4D7-4D7F-9307-4D24EE54B841") Folder3;'}
{$HPPEMIT 'interface DECLSPEC_UUID("EDC817AA-92B8-11D1-B075-00C04FC33AA5") FolderItem2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("C94F0AD0-F363-11D2-A327-00C04F8EEC7F") FolderItems2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("EAA7C309-BBEC-49D5-821D-64D966CB667F") FolderItems3;'}
{$HPPEMIT 'interface DECLSPEC_UUID("88A05C00-F000-11CE-8350-444553540000") IShellLinkDual;'}
{$HPPEMIT 'interface DECLSPEC_UUID("317EE249-F12E-11D2-B1E4-00C04F8EEB3E") IShellLinkDual2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("E7A1AF80-4D96-11CF-960C-0080C7F4EE85") IShellFolderViewDual;'}
{$HPPEMIT 'interface DECLSPEC_UUID("31C147B6-0ADE-4A3C-B514-DDF932EF6D17") IShellFolderViewDual2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("D8F015C0-C278-11CE-A49E-444553540000") IShellDispatch;'}
{$HPPEMIT 'interface DECLSPEC_UUID("A4C6892C-3BA9-11D2-9DEA-00C04FB16162") IShellDispatch2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("177160CA-BB5A-411C-841D-BD38FACDEAA0") IShellDispatch3;'}
{$HPPEMIT 'interface DECLSPEC_UUID("EFD84B2D-4BCF-4298-BE25-EB542A59FBDA") IShellDispatch4;'}
{$HPPEMIT 'interface DECLSPEC_UUID("60890160-69F0-11D1-B758-00A0C90564FE") DSearchCommandEvents;'}
{$HPPEMIT 'interface DECLSPEC_UUID("2D91EEA1-9932-11D2-BE86-00A0C9A83DA1") IFileSearchBand;'}

{$HPPEMIT 'typedef System::DelphiInterface<IFolderViewOC> _di_IFolderViewOC;'}
{$HPPEMIT 'typedef System::DelphiInterface<DShellFolderViewEvents> _di_DShellFolderViewEvents;'}
{$HPPEMIT 'typedef System::DelphiInterface<DFConstraint> _di_DFConstraint;'}
{$HPPEMIT 'typedef System::DelphiInterface<ISearchCommandExt> _di_ISearchCommandExt;'}
{$HPPEMIT 'typedef System::DelphiInterface<FolderItem> _di_FolderItem;'}
{$HPPEMIT 'typedef System::DelphiInterface<FolderItems> _di_FolderItems;'}
{$HPPEMIT 'typedef System::DelphiInterface<FolderItemVerb> _di_FolderItemVerb;'}
{$HPPEMIT 'typedef System::DelphiInterface<FolderItemVerbs> _di_FolderItemVerbs;'}
{$HPPEMIT 'typedef System::DelphiInterface<Folder> _di_Folder;'}
{$HPPEMIT 'typedef System::DelphiInterface<Folder2> _di_Folder2;'}
{$HPPEMIT 'typedef System::DelphiInterface<Folder3> _di_Folder3;'}
{$HPPEMIT 'typedef System::DelphiInterface<FolderItem2> _di_FolderItem2;'}
{$HPPEMIT 'typedef System::DelphiInterface<FolderItems2> _di_FolderItems2;'}
{$HPPEMIT 'typedef System::DelphiInterface<FolderItems3> _di_FolderItems3;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellLinkDual> _di_IShellLinkDual;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellLinkDual2> _di_IShellLinkDual2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellFolderViewDual> _di_IShellFolderViewDual;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellFolderViewDual2> _di_IShellFolderViewDual2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellDispatch> _di_IShellDispatch;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellDispatch2> _di_IShellDispatch2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellDispatch3> _di_IShellDispatch3;'}
{$HPPEMIT 'typedef System::DelphiInterface<IShellDispatch4> _di_IShellDispatch4;'}
{$HPPEMIT 'typedef System::DelphiInterface<DSearchCommandEvents> _di_DSearchCommandEvents;'}
{$HPPEMIT 'typedef System::DelphiInterface<IFileSearchBand> _di_IFileSearchBand;'}

{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  {$EXTERNALSYM IID_IFolderViewOC}
  IID_IFolderViewOC          = '{9BA05970-F6A8-11CF-A442-00A0C90A8F39}';
  {$EXTERNALSYM IID_DShellFolderViewEvents}
  IID_DShellFolderViewEvents = '{62112AA2-EBE4-11cf-A5FB-0020AFE7292D}';
  {$EXTERNALSYM IID_DFConstraint}
  IID_DFConstraint           = '{4A3DF050-23BD-11D2-939F-00A0C91EEDBA}';
  {$EXTERNALSYM IID_ISearchCommandExt}
  IID_ISearchCommandExt      = '{1D2EFD50-75CE-11d1-B75A-00A0C90564FE}';
  {$EXTERNALSYM IID_FolderItem}
  IID_FolderItem             = '{FAC32C80-CBE4-11CE-8350-444553540000}';
  {$EXTERNALSYM IID_FolderItems}
  IID_FolderItems            = '{744129E0-CBE5-11CE-8350-444553540000}';
  {$EXTERNALSYM IID_FolderItemVerb}
  IID_FolderItemVerb         = '{08EC3E00-50B0-11CF-960C-0080C7F4EE85}';
  {$EXTERNALSYM IID_FolderItemVerbs}
  IID_FolderItemVerbs        = '{1F8352C0-50B0-11CF-960C-0080C7F4EE85}';
  {$EXTERNALSYM IID_Folder}
  IID_Folder                 = '{BBCBDE60-C3FF-11CE-8350-444553540000}';
  {$EXTERNALSYM IID_Folder2}
  IID_Folder2                = '{F0D2D8EF-3890-11D2-BF8B-00C04FB93661}';
  {$EXTERNALSYM IID_Folder3}
  IID_Folder3                = '{A7AE5F64-C4D7-4d7f-9307-4D24EE54B841}';
  {$EXTERNALSYM IID_FolderItem2}
  IID_FolderItem2            = '{EDC817AA-92B8-11D1-B075-00C04FC33AA5}';
  {$EXTERNALSYM IID_FolderItems2}
  IID_FolderItems2           = '{C94F0AD0-F363-11d2-A327-00C04F8EEC7F}';
  {$EXTERNALSYM IID_FolderItems3}
  IID_FolderItems3           = '{EAA7C309-BBEC-49D5-821D-64D966CB667F}';
  {$EXTERNALSYM IID_IShellLinkDual}
  IID_IShellLinkDual         = '{88A05C00-F000-11CE-8350-444553540000}';
  {$EXTERNALSYM IID_IShellLinkDual2}
  IID_IShellLinkDual2        = '{317EE249-F12E-11d2-B1E4-00C04F8EEB3E}';
  {$EXTERNALSYM IID_IShellFolderViewDual}
  IID_IShellFolderViewDual   = '{E7A1AF80-4D96-11CF-960C-0080C7F4EE85}';
  {$EXTERNALSYM IID_IShellFolderViewDual2}
  IID_IShellFolderViewDual2  = '{31C147b6-0ADE-4A3C-B514-DDF932EF6D17}';
  {$EXTERNALSYM IID_IShellDispatch}
  IID_IShellDispatch         = '{D8F015C0-C278-11CE-A49E-444553540000}';
  {$EXTERNALSYM IID_IShellDispatch2}
  IID_IShellDispatch2        : TGUID = '{A4C6892C-3BA9-11d2-9DEA-00C04FB16162}';
  {$EXTERNALSYM IID_IShellDispatch3}
  IID_IShellDispatch3        = '{177160CA-BB5A-411C-841D-BD38FACDEAA0}';
  {$EXTERNALSYM IID_IShellDispatch4}
  IID_IShellDispatch4        : TGUID = '{EFD84B2D-4BCF-4298-BE25-EB542A59FBDA}';
  {$EXTERNALSYM IID_DSearchCommandEvents}
  IID_DSearchCommandEvents   = '{60890160-69F0-11D1-B758-00A0C90564FE}';
  {$EXTERNALSYM IID_IFileSearchBand}
  IID_IFileSearchBand        = '{2D91EEA1-9932-11d2-BE86-00A0C9A83DA1}';

type
  {$EXTERNALSYM SearchCommandExecuteErrors}
  SearchCommandExecuteErrors = DWORD;
  TSearchCommandExecuteErrors = DWORD;

const
  {$EXTERNALSYM SCEE_PATHNOTFOUND}
  SCEE_PATHNOTFOUND     = 1;
  {$EXTERNALSYM SCEE_MAXFILESFOUND}
  SCEE_MAXFILESFOUND    = SCEE_PATHNOTFOUND + 1;
  {$EXTERNALSYM SCEE_INDEXSEARCH}
  SCEE_INDEXSEARCH      = SCEE_MAXFILESFOUND + 1;
  {$EXTERNALSYM SCEE_CONSTRAINT}
  SCEE_CONSTRAINT       = SCEE_INDEXSEARCH + 1;
  {$EXTERNALSYM SCEE_SCOPEMISMATCH}
  SCEE_SCOPEMISMATCH    = SCEE_CONSTRAINT + 1;
  {$EXTERNALSYM SCEE_CASESENINDEX}
  SCEE_CASESENINDEX     = SCEE_SCOPEMISMATCH + 1;
  {$EXTERNALSYM SCEE_INDEXNOTCOMPLETE}
  SCEE_INDEXNOTCOMPLETE = SCEE_CASESENINDEX + 1;

type
  {$EXTERNALSYM OfflineFolderStatus}
  OfflineFolderStatus = DWORD;
  TOfflineFolderStatus = DWORD;

const
  {$EXTERNALSYM OFS_INACTIVE}
  OFS_INACTIVE   = -1;
  {$EXTERNALSYM OFS_ONLINE}
  OFS_ONLINE     = OFS_INACTIVE + 1;
  {$EXTERNALSYM OFS_OFFLINE}
  OFS_OFFLINE    = OFS_ONLINE + 1;
  {$EXTERNALSYM OFS_SERVERBACK}
  OFS_SERVERBACK = OFS_OFFLINE + 1;
  {$EXTERNALSYM OFS_DIRTYCACHE}
  OFS_DIRTYCACHE = OFS_SERVERBACK + 1;

type
  {$EXTERNALSYM ShellFolderViewOptions}
  ShellFolderViewOptions = DWORD;
  TShellFolderViewOptions = DWORD;

const
  {$EXTERNALSYM SFVVO_SHOWALLOBJECTS}
  SFVVO_SHOWALLOBJECTS       = $1;
  {$EXTERNALSYM SFVVO_SHOWEXTENSIONS}
  SFVVO_SHOWEXTENSIONS       = $2;
  {$EXTERNALSYM SFVVO_SHOWCOMPCOLOR}
  SFVVO_SHOWCOMPCOLOR        = $8;
  {$EXTERNALSYM SFVVO_SHOWSYSFILES}
  SFVVO_SHOWSYSFILES         = $20;
  {$EXTERNALSYM SFVVO_WIN95CLASSIC}
  SFVVO_WIN95CLASSIC         = $40;
  {$EXTERNALSYM SFVVO_DOUBLECLICKINWEBVIEW}
  SFVVO_DOUBLECLICKINWEBVIEW = $80;
  {$EXTERNALSYM SFVVO_DESKTOPHTML}
  SFVVO_DESKTOPHTML          = $200;

type
  {$EXTERNALSYM ShellSpecialFolderConstants}
  ShellSpecialFolderConstants = DWORD;
  TShellSpecialFolderConstants = DWORD;

const
  {$EXTERNALSYM ssfDESKTOP}
  ssfDESKTOP          = 0;
  {$EXTERNALSYM ssfPROGRAMS}
  ssfPROGRAMS         = $2;
  {$EXTERNALSYM ssfCONTROLS}
  ssfCONTROLS         = $3;
  {$EXTERNALSYM ssfPRINTERS}
  ssfPRINTERS         = $4;
  {$EXTERNALSYM ssfPERSONAL}
  ssfPERSONAL         = $5;
  {$EXTERNALSYM ssfFAVORITES}
  ssfFAVORITES        = $6;
  {$EXTERNALSYM ssfSTARTUP}
  ssfSTARTUP          = $7;
  {$EXTERNALSYM ssfRECENT}
  ssfRECENT           = $8;
  {$EXTERNALSYM ssfSENDTO}
  ssfSENDTO           = $9;
  {$EXTERNALSYM ssfBITBUCKET}
  ssfBITBUCKET        = $a;
  {$EXTERNALSYM ssfSTARTMENU}
  ssfSTARTMENU        = $b;
  {$EXTERNALSYM ssfDESKTOPDIRECTORY}
  ssfDESKTOPDIRECTORY = $10;
  {$EXTERNALSYM ssfDRIVES}
  ssfDRIVES           = $11;
  {$EXTERNALSYM ssfNETWORK}
  ssfNETWORK          = $12;
  {$EXTERNALSYM ssfNETHOOD}
  ssfNETHOOD          = $13;
  {$EXTERNALSYM ssfFONTS}
  ssfFONTS            = $14;
  {$EXTERNALSYM ssfTEMPLATES}
  ssfTEMPLATES        = $15;
  {$EXTERNALSYM ssfCOMMONSTARTMENU}
  ssfCOMMONSTARTMENU  = $16;
  {$EXTERNALSYM ssfCOMMONPROGRAMS}
  ssfCOMMONPROGRAMS   = $17;
  {$EXTERNALSYM ssfCOMMONSTARTUP}
  ssfCOMMONSTARTUP    = $18;
  {$EXTERNALSYM ssfCOMMONDESKTOPDIR}
  ssfCOMMONDESKTOPDIR = $19;
  {$EXTERNALSYM ssfAPPDATA}
  ssfAPPDATA          = $1a;
  {$EXTERNALSYM ssfPRINTHOOD}
  ssfPRINTHOOD        = $1b;
  {$EXTERNALSYM ssfLOCALAPPDATA}
  ssfLOCALAPPDATA     = $1c;
  {$EXTERNALSYM ssfALTSTARTUP}
  ssfALTSTARTUP       = $1d;
  {$EXTERNALSYM ssfCOMMONALTSTARTUP}
  ssfCOMMONALTSTARTUP = $1e;
  {$EXTERNALSYM ssfCOMMONFAVORITES}
  ssfCOMMONFAVORITES  = $1f;
  {$EXTERNALSYM ssfINTERNETCACHE}
  ssfINTERNETCACHE    = $20;
  {$EXTERNALSYM ssfCOOKIES}
  ssfCOOKIES          = $21;
  {$EXTERNALSYM ssfHISTORY}
  ssfHISTORY          = $22;
  {$EXTERNALSYM ssfCOMMONAPPDATA}
  ssfCOMMONAPPDATA    = $23;
  {$EXTERNALSYM ssfWINDOWS}
  ssfWINDOWS          = $24;
  {$EXTERNALSYM ssfSYSTEM}
  ssfSYSTEM           = $25;
  {$EXTERNALSYM ssfPROGRAMFILES}
  ssfPROGRAMFILES     = $26;
  {$EXTERNALSYM ssfMYPICTURES}
  ssfMYPICTURES       = $27;
  {$EXTERNALSYM ssfPROFILE}
  ssfPROFILE          = $28;
  {$EXTERNALSYM ssfSYSTEMx86}
  ssfSYSTEMx86        = $29;
  {$EXTERNALSYM ssfPROGRAMFILESx86}
  ssfPROGRAMFILESx86  = $30;

type
  {$EXTERNALSYM IFolderViewOC}
  IFolderViewOC = interface(IDispatch)
  ['{9BA05970-F6A8-11CF-A442-00A0C90A8F39}']
     procedure SetFolderView(pdisp: IDispatch); safecall;
  end;

  {$EXTERNALSYM DShellFolderViewEvents}
  DShellFolderViewEvents = interface(IDispatch)
  ['{62112AA2-EBE4-11cf-A5FB-0020AFE7292D}']
  end;

const
  {$EXTERNALSYM CLSID_ShellFolderViewOC}
  CLSID_ShellFolderViewOC = '{9BA05971-F6A8-11CF-A442-00A0C90A8F39}';

type
  {$EXTERNALSYM DFConstraint}
  DFConstraint = interface(IDispatch)
  ['{4A3DF050-23BD-11D2-939F-00A0C91EEDBA}']
    function get_Name: TBStr; safecall;
    function get_Value: Variant; safecall;
  end;

  {$EXTERNALSYM SEARCH_FOR_TYPE}
  SEARCH_FOR_TYPE = DWORD;
  TSearchForType = DWORD;

const
  {$EXTERNALSYM SCE_SEARCHFORFILES}
  SCE_SEARCHFORFILES     = 0;
  {$EXTERNALSYM SCE_SEARCHFORCOMPUTERS}
  SCE_SEARCHFORCOMPUTERS = 1;

type
  {$EXTERNALSYM ISearchCommandExt}
  ISearchCommandExt = interface(IDispatch)
  ['{1D2EFD50-75CE-11d1-B75A-00A0C90564FE}']
    procedure ClearResults; safecall;
    procedure NavigateToSearchResults; safecall;
    function get_ProgressText: TBStr; safecall;
    procedure SaveSearch; safecall;
    function GetErrorInfo(out pbs: TBStr): Integer; safecall;
    procedure SearchFor(iFor: Integer); safecall;
    function GetScopeInfo(bsScope: TBStr): Integer; safecall;
    procedure RestoreSavedSearch(var pvarFile: Variant); safecall;
    procedure Execute(var RecordsAffected, Parameters: Variant; Options: LongInt); safecall;
    procedure AddConstraint(Name: TBStr; Value: Variant); safecall;
    function GetNextConstraint(fReset: WordBool): DFConstraint; safecall;
  end;

  {$EXTERNALSYM FolderItemVerbs}
  FolderItemVerbs = interface;

  {$EXTERNALSYM FolderItem}
  FolderItem = interface(IDispatch)
  ['{FAC32C80-CBE4-11CE-8350-444553540000}']
    function get_Application: IDispatch; safecall;
    function get_Parent: IDispatch; safecall;
    function get_Name: TBStr; safecall;
    procedure put_Name(bs: TBStr); safecall;
    function get_Path: TBStr; safecall;
    function get_GetLink: IDispatch; safecall;
    function get_GetFolder: IDispatch; safecall;
    function get_IsLink: WordBool; safecall;
    function get_IsFolder: WordBool; safecall;
    function get_IsFileSystem: WordBool; safecall;
    function get_IsBrowsable: WordBool; safecall;
    function get_ModifyDate: TDateTime; safecall;
    procedure put_ModifyDate(dt: TDateTime); safecall;
    function get_Size: Longint; safecall;
    function get_Type: TBStr; safecall;
    function Verbs: FolderItemVerbs; safecall;
    procedure InvokeVerb(vVerb: Variant); safecall;
  end;

  {$EXTERNALSYM FolderItems}
  FolderItems = interface(IDispatch)
  ['{744129E0-CBE5-11CE-8350-444553540000}']
    function get_Count: Longint; safecall;
    function get_Application: IDispatch; safecall;
    function get_Parent: IDispatch; safecall;
    function Item(index: Variant): FolderItem; safecall;
    function _NewEnum: IUnknown; safecall;
  end;

  {$EXTERNALSYM FolderItemVerb}
  FolderItemVerb = interface(IDispatch)
  ['{08EC3E00-50B0-11CF-960C-0080C7F4EE85}']
    function get_Application: IDispatch; safecall;
    function get_Parent: IDispatch; safecall;
    function get_Name: TBStr; safecall;
    procedure DoIt; safecall;
  end;

  FolderItemVerbs = interface(IDispatch)
  ['{1F8352C0-50B0-11CF-960C-0080C7F4EE85}']
    function get_Count: Longint; safecall;
    function get_Application: IDispatch; safecall;
    function get_Parent: IDispatch; safecall;
    function Item(index: Variant): FolderItemVerb; safecall;
    function _NewEnum: IUnknown; safecall;
  end;

  {$EXTERNALSYM Folder}
  Folder = interface(IDispatch)
  ['{BBCBDE60-C3FF-11CE-8350-444553540000}']
    function get_Title: TBStr; safecall;
    function get_Application: IDispatch; safecall;
    function get_Parent: IDispatch; safecall;
    function get_ParentFolder: Folder; safecall;
    function Items: FolderItems; safecall;
    function ParseName(bName: TBStr): FolderItem; safecall;
    procedure NewFolder(bName: TBStr; vOptions: Variant); safecall;
    procedure MoveHere(vItem, vOptions: Variant); safecall;
    procedure CopyHere(vItem, vOptions: Variant); safecall;
    function GetDetailsOf(vItem: Variant; iColumn: Integer): TBStr; safecall;
  end;

  {$EXTERNALSYM Folder2}
  Folder2 = interface(Folder)
  ['{F0D2D8EF-3890-11D2-BF8B-00C04FB93661}']
    function get_Self: FolderItem; safecall;
    function get_OfflineStatus: Longint; safecall;
    procedure Synchronize; safecall;
    function get_HaveToShowWebViewBarricade: WordBool; safecall;
    procedure DismissedWebViewBarricade; safecall;
  end;

  {$EXTERNALSYM Folder3}
  Folder3 = interface(Folder2)
  ['{A7AE5F64-C4D7-4d7f-9307-4D24EE54B841}']
    function get_ShowWebViewBarricade: WordBool; safecall;
    procedure put_ShowWebViewBarricade(bShowWebViewBarricade: WordBool); safecall;
  end;

  {$EXTERNALSYM FolderItem2}
  FolderItem2 = interface(FolderItem)
  ['{EDC817AA-92B8-11D1-B075-00C04FC33AA5}']
    procedure InvokeVerbEx(vVerb, vArgs: Variant); safecall;
    function ExtendedProperty(bstrPropName: TBStr): Variant; safecall;
  end;

const
  {$EXTERNALSYM CLSID_ShellFolderItem}
  CLSID_ShellFolderItem: TGUID = '{2FE352EA-FD1F-11D2-B1F4-00C04F8EEB3E}';

type
  {$EXTERNALSYM FolderItems2}
  FolderItems2 = interface(FolderItems)
  ['{C94F0AD0-F363-11d2-A327-00C04F8EEC7F}']
    procedure InvokeVerbEx(vVerb, vArgs: Variant); safecall;
  end;

  {$EXTERNALSYM FolderItems3}
  FolderItems3 = interface(FolderItems2)
  ['{EAA7C309-BBEC-49D5-821D-64D966CB667F}']
    procedure Filter(grfFlags: LongInt; bstrFileSpec: TBStr); safecall;
    function get_Verbs: FolderItemVerbs; safecall;
  end;

  {$EXTERNALSYM IShellLinkDual}
  IShellLinkDual = interface(IDispatch)
  ['{88A05C00-F000-11CE-8350-444553540000}']
    function get_Path: TBStr; safecall;
    procedure put_Path(bs: TBStr); safecall;
    function get_Description: TBStr; safecall;
    procedure put_Description(bs: TBStr); safecall;
    function get_WorkingDirectory: TBStr; safecall;
    procedure put_WorkingDirectory(bs: TBStr); safecall;
    function get_Arguments: TBStr; safecall;
    procedure put_Arguments(bs: TBStr); safecall;
    function get_Hotkey: Integer; safecall;
    procedure put_Hotkey(iHK: Integer); safecall;
    function get_ShowCommand: Integer; safecall;
    procedure put_ShowCommand(iShowCommand: Integer); safecall;
    procedure Resolve(fFlags: Integer); safecall;
    function GetIconLocation(out pbs: TBStr): Integer; safecall;
    procedure SetIconLocation(bs: TBStr; iIcon: Integer); safecall;
    procedure Save(vWhere: Variant); safecall;
  end;

  {$EXTERNALSYM IShellLinkDual2}
  IShellLinkDual2 = interface(IShellLinkDual)
  ['{317EE249-F12E-11d2-B1E4-00C04F8EEB3E}']
    function get_Target: FolderItem; safecall;
  end;

const
  {$EXTERNALSYM CLSID_ShellLinkObject}
  CLSID_ShellLinkObject: TGUID = '{11219420-1768-11d1-95BE-00609797EA4F}';

type
  {$EXTERNALSYM IShellFolderViewDual}
  IShellFolderViewDual = interface(IDispatch)
  ['{E7A1AF80-4D96-11CF-960C-0080C7F4EE85}']
    function get_Application: IDispatch; safecall;
    function get_Parent: IDispatch; safecall;
    function get_Folder: Folder; safecall;
    function SelectedItems: FolderItems; safecall;
    function get_FocusedItem: FolderItem; safecall;
    procedure SelectItem(var pvfi: Variant; dwFlags: Integer); safecall;
    function PopupItemMenu(pfi: FolderItem; vx, vy: Variant): TBStr; safecall;
    function get_Script: IDispatch; safecall;
    function get_ViewOptions: Longint; safecall;
  end;

  {$EXTERNALSYM IShellFolderViewDual2}
  IShellFolderViewDual2 = interface(IShellFolderViewDual)
  ['{31C147B6-0ADE-4A3C-B514-DDF932EF6D17}']
    function get_CurrentViewMode: UINT; safecall;
    procedure put_CurrentViewMode(ViewMode: UINT); safecall;
    procedure SelectItemRelative(iRelative: Integer); safecall;
  end;

const
  {$EXTERNALSYM CLSID_ShellFolderView}
  CLSID_ShellFolderView: TGUID = '{62112AA1-EBE4-11cf-A5FB-0020AFE7292D}';

type
  {$EXTERNALSYM IShellDispatch}
  IShellDispatch = interface(IDispatch)
  ['{D8F015C0-C278-11CE-A49E-444553540000}']
    function get_Application: IDispatch; safecall;
    function get_Parent: IDispatch; safecall;
    function NameSpace(vDir: Variant): Folder; safecall;
    function BrowseForFolder(Hwnd: LongInt; Title: TBStr; Options: LongInt; RootFolder: Variant): Folder; safecall;
    function Windows: IDispatch; safecall;
    procedure Open(vDir: Variant); safecall;
    procedure Explore(vDir: Variant); safecall;
    procedure MinimizeAll; safecall;
    procedure UndoMinimizeALL; safecall;
    procedure FileRun; safecall;
    procedure CascadeWindows; safecall;
    procedure TileVertically; safecall;
    procedure TileHorizontally; safecall;
    procedure ShutdownWindows; safecall;
    procedure Suspend; safecall;
    procedure EjectPC; safecall;
    procedure SetTime; safecall;
    procedure TrayProperties; safecall;
    procedure Help; safecall;
    procedure FindFiles; safecall;
    procedure FindComputer; safecall;
    procedure RefreshMenu; safecall;
    procedure ControlPanelItem(szDir: TBStr); safecall;
  end;

  {$EXTERNALSYM IShellDispatch2}
  IShellDispatch2 = interface(IShellDispatch)
  ['{A4C6892C-3BA9-11d2-9DEA-00C04FB16162}']
    function IsRestricted(Group, Restriction: TBStr): Longint; safecall;
    procedure ShellExecute(AFile: TBStr; vArgs, vDir, vOperation, vShow: Variant); safecall;
    procedure FindPrinter(name, location, model: TBStr); safecall;
    function GetSystemInformation(name: TBStr): Variant; safecall;
    function ServiceStart(ServiceName: TBStr; Persistent: Variant): Variant; safecall;
    function ServiceStop(ServiceName: TBStr; Persistent: Variant): Variant; safecall;
    function IsServiceRunning(ServiceName: TBStr): Variant; safecall;
    function CanStartStopService(ServiceName: TBStr): Variant; safecall;
    function ShowBrowserBar(bstrClsid: TBStr; bShow: Variant): Variant; safecall;
  end;

  {$EXTERNALSYM IShellDispatch3}
  IShellDispatch3 = interface(IShellDispatch2)
  ['{177160CA-BB5A-411C-841D-BD38FACDEAA0}']
    procedure AddToRecent(varFile: Variant; bstrCategory: TBStr); safecall;
  end;

  {$EXTERNALSYM IShellDispatch4}
  IShellDispatch4 = interface(IShellDispatch3)
  ['{EFD84B2D-4BCF-4298-BE25-EB542A59FBDA}']
    procedure WindowsSecurity; safecall;
    procedure ToggleDesktop; safecall;
    function ExplorerPolicy(bstrPolicyName: TBStr): Variant; safecall;
    function GetSetting(lSetting: LongInt): WordBool; safecall;
  end;

const
  {$EXTERNALSYM CLSID_Shell}
  CLSID_Shell: TGUID                 = '{13709620-C279-11CE-A49E-444553540000}';
  {$EXTERNALSYM CLSID_ShellDispatchInproc}
  CLSID_ShellDispatchInproc: TGUID   = '{0A89A860-D7B1-11CE-8350-444553540000}';
  {$EXTERNALSYM CLSID_WebViewFolderContents}
  CLSID_WebViewFolderContents: TGUID = '{1820FED0-473E-11D0-A96C-00C04FD705A2}';

type
  {$EXTERNALSYM DSearchCommandEvents}
  DSearchCommandEvents = interface(IDispatch)
  ['{60890160-69F0-11D1-B758-00A0C90564FE}']
  end;

const
  {$EXTERNALSYM CLSID_SearchCommand}
  CLSID_SearchCommand: TGUID = '{B005E690-678D-11d1-B758-00A0C90564FE}';

type
  {$EXTERNALSYM IFileSearchBand}
  IFileSearchBand = interface(IDispatch)
  ['{2D91EEA1-9932-11d2-BE86-00A0C9A83DA1}']
    procedure SetFocus; safecall;
    procedure SetSearchParameters(pbstrSearchID: PBSTR; bNavToResults: WordBool; pvarScope, pvarQueryFile: PVariant); safecall;
    function get_SearchID: TBStr; safecall;
    function get_Scope: Variant; safecall;
    function get_QueryFile: Variant; safecall;
  end;

const
  {$EXTERNALSYM CLSID_FileSearchBand}
  CLSID_FileSearchBand: TGUID = '{C4EE31F3-4768-11D2-BE5C-00A0C9A83DA1}';

type
  {$EXTERNALSYM IWebWizardHost}
  IWebWizardHost = interface(IDispatch)
  ['{18BCC359-4990-4BFB-B951-3C83702BE5F9}']
    procedure FinalBack; safecall;
    procedure FinalNext; safecall;
    procedure Cancel; safecall;
    procedure put_Caption(bstrCaption: TBStr); safecall;
    function get_Caption: TBStr; safecall;
    procedure put_Property(bstrPropertyName: TBStr; var pvProperty: Variant); safecall;
    function get_Property(bstrPropertyName: TBStr): Variant; safecall;
    procedure SetWizardButtons(vfEnableBack, vfEnableNext, vfLastPage: WordBool); safecall;
    procedure SetHeaderText(bstrHeaderTitle, bstrHeaderSubtitle: TBStr); safecall;
  end;

  {$EXTERNALSYM INewWDEvents}
  INewWDEvents = interface(IWebWizardHost)
  ['{0751C551-7568-41C9-8E5B-E22E38919236}']
    function PassportAuthenticate(bstrSignInUrl: TBStr): WordBool; safecall;
  end;

  {$EXTERNALSYM IPassportClientServices}
  IPassportClientServices = interface(IDispatch)
  ['{B30F7305-5967-45D1-B7BC-D6EB7163D770}']
    function MemberExists(bstrUser: TBStr; bstrPassword: TBStr): WordBool; safecall;
  end;

const
  {$EXTERNALSYM CLSID_PassportClientServices}
  CLSID_PassportClientServices: TGUID = '{2D2307C8-7DB4-40D6-9100-D52AF4F97A5B}';


//-------------------------------------------------------------------------
//
// IAutoComplete interface
//
//
// [Member functions]
//
// IAutoComplete::Init(hwndEdit, punkACL, pwszRegKeyPath, pwszQuickComplete)
//   This function initializes an AutoComplete object, telling it
//   what control to subclass, and what list of strings to process.
//
// IAutoComplete::Enable(fEnable)
//   This function enables or disables the AutoComplete functionality.
//
//-------------------------------------------------------------------------

type
  {$EXTERNALSYM IAutoComplete}
  IAutoComplete = interface(IUnknown)
  ['{00BB2762-6A77-11D0-A535-00C04FD7D062}']
    function Init(hwndEdit: HWND; punkACL: IUnknown; pwszRegKeyPath, pwszQuickComplete: POleStr): HResult; stdcall;
    function Enable(fEnable: BOOL): HResult; stdcall;
  end;

  {$EXTERNALSYM _tagAUTOCOMPLETEOPTIONS}
  _tagAUTOCOMPLETEOPTIONS = DWORD;
  {$EXTERNALSYM AUTOCOMPLETEOPTIONS}
  AUTOCOMPLETEOPTIONS = _tagAUTOCOMPLETEOPTIONS;
  TAutoCompleteOptions = _tagAUTOCOMPLETEOPTIONS;

const
  {$EXTERNALSYM ACO_NONE}
  ACO_NONE               = 0;
  {$EXTERNALSYM ACO_AUTOSUGGEST}
  ACO_AUTOSUGGEST        = $1;
  {$EXTERNALSYM ACO_AUTOAPPEND}
  ACO_AUTOAPPEND         = $2;
  {$EXTERNALSYM ACO_SEARCH}
  ACO_SEARCH             = $4;
  {$EXTERNALSYM ACO_FILTERPREFIXES}
  ACO_FILTERPREFIXES     = $8;
  {$EXTERNALSYM ACO_USETAB}
  ACO_USETAB             = $10;
  {$EXTERNALSYM ACO_UPDOWNKEYDROPSLIST}
  ACO_UPDOWNKEYDROPSLIST = $20;
  {$EXTERNALSYM ACO_RTLREADING}
  ACO_RTLREADING         = $40;

type
  {$EXTERNALSYM IAutoComplete2}
  IAutoComplete2 = interface(IAutoComplete)
  ['{EAC04BC0-3791-11d2-BB95-0060977B464C}']
    function SetOptions(dwFlag: DWORD): HResult; stdcall;
    function GetOptions(out pdwFlag: DWORD): HResult; stdcall;
  end;

// INTERFACE: IEnumACString
//
// This interface was implemented to return autocomplete strings
// into the caller's buffer (to reduce the number of memory allocations).
// A sort index is also returned to control the order of items displayed.
// by autocomplete.  The sort index should be set to zero if unused.
//
// The NextItem method increments the current index by one (similar to Next
// when one item is requested).
//


  {$EXTERNALSYM _tagACENUMOPTION}
  _tagACENUMOPTION = DWORD;
  {$EXTERNALSYM ACENUMOPTION}
  ACENUMOPTION = _tagACENUMOPTION;
  TACEnumOption = _tagACENUMOPTION;

const
  {$EXTERNALSYM ACEO_NONE}
  ACEO_NONE            = 0;
  {$EXTERNALSYM ACEO_MOSTRECENTFIRST}
  ACEO_MOSTRECENTFIRST = $1;
  {$EXTERNALSYM ACEO_FIRSTUNUSED}
  ACEO_FIRSTUNUSED     = $10000;

type
  {$EXTERNALSYM IEnumACString}
  IEnumACString = interface(IEnumString)
  ['{8E74C210-CF9D-4eaf-A403-7356428F0A5A}']
    function NextItem(pszUrl: POleStr; cchMax: ULONG; out pulSortIndex: ULONG): HResult; stdcall;
    function SetEnumOptions(dwOptions: DWORD): HResult; stdcall;
    function GetEnumOptions(out pdwOptions: DWORD): HResult; stdcall;
  end;

// INTERFACE: IAsyncOperation
//
// This interface was implemented to turn some previously synchronous
// interfaces into async.  The following example is for
// doing the IDataObject::Drop() operation asynchronously.
//
// Sometimes the rendering of the IDataObject data (IDataObject::GetData() or
// STGMEDIUM.pStream->Read()) can be time intensive.  The IDropTarget
// may want to do this on another thread.
//
// Implimentation Check list:
// DoDragDrop Caller:
//    If this code can support asynch operations, then it needs to
//    QueryInterface() the IDataObject for IAsyncOperation.
//    IAsyncOperation::SetAsyncMode(VARIANT_TRUE).
//    After calling DoDragDrop(), call InOperation().  If any call fails
//    or InOperation() return FALSE, use the pdwEffect returned by DoDragDrop()
//    and the operation completed synchrously.
//
// OleSetClipboard Caller:
//    If this code can support asynch operations, then it needs to
//    QueryInterface() the IDataObject for IAsyncOperation.  Then call
//    IAsyncOperation::SetAsyncMode(VARIANT_TRUE).
//    If any of that fails, the final dwEffect should be passed to the IDataObject via
//    CFSTR_PERFORMEDDROPEFFECT.
//
// IDataObect Object:
//    IAsyncOperation::GetAsyncMode() should return whatever was last passed in
//          fDoOpAsync to ::SetAsyncMode() or VARIANT_FALSE if ::SetAsyncMode()
//          was never called.
//    IAsyncOperation::SetAsyncMode() should AddRef and store paocb.
//    IAsyncOperation::StartOperation() should store the fact that this was called and
//          cause InOperation() to return VARIANT_TRUE.  pbcReserved is not used and needs
//          to be NULL.
//    IAsyncOperation::InOperation() should return VARIANT_TRUE only if ::StartOperation()
//          was called.
//    IAsyncOperation::EndOperation() needs to call paocbpaocb->EndOperation() with the same
//          parameters.  Then release paocb.
//    IDataObject::SetData(CFSTR_PERFORMEDDROPEFFECT) When this happens, call
//          EndOperation(<into VAR>S_OK, NULL, <into VAR>dwEffect) and pass the dwEffect from the hglobal.
//
// IDropTarget Object:
//    IDropTarget::Drop() If asynch operations aren't supported, nothing is required.
//          The asynch operation can only happen if GetAsyncMode() returns VARIANT_TRUE.
//          Before starting the asynch operation, StartOperation(NULL) needs to be called before
//          returning from IDropTarget::Drop().

  {$EXTERNALSYM IAsyncOperation}
  IAsyncOperation = interface(IUnknown)
  ['{3D8B0590-F691-11d2-8EA9-006097DF5BD4}']
    function SetAsyncMode(fDoOpAsync: BOOL): HResult; stdcall;
    function GetAsyncMode(out pfIsOpAsync: BOOL): HResult; stdcall;
    function StartOperation(pbcReserved: IBindCtx): HResult; stdcall;
    function InOperation(out pfInAsyncOp: BOOL): HResult; stdcall;
    function EndOperation(hResult: HResult; pbcReserved: IBindCtx; dwEffects: DWORD): HResult; stdcall;
  end;
  
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
