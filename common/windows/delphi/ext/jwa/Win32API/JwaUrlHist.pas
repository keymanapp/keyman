{******************************************************************************}
{                                                                              }
{ URL History Interface Unit for Object Pascal                      }
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
{ The original code is: UrlHist.h, released 2005.                 			   }
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
unit JwaUrlHist;

interface

uses
  //some types are missing in JwaActiveX
  ActiveX,
  JwaWinType
  ;

{$IFDEF DELPHI6_UP}
{$ALIGN 8}
{$ELSE}
{$A+}
//Warning: Record alignment 4
{$ENDIF DELPHI6_UP}

{$HPPEMIT '#include "UrlHist.h"'}

{$HPPEMIT 'interface DECLSPEC_UUID("3C374A42-BAE4-11CF-BF7D-00AA006946EE") IEnumSTATURL;'}
{$HPPEMIT 'interface DECLSPEC_UUID("3C374A41-BAE4-11CF-BF7D-00AA006946EE") IUrlHistoryStg;'}
{$HPPEMIT 'interface DECLSPEC_UUID("AFA0DC11-C313-11D0-831A-00C04FD5AE38") IUrlHistoryStg2;'}
{$HPPEMIT 'interface DECLSPEC_UUID("BC40BEC1-C493-11D0-831B-00C04FD5AE38") IUrlHistoryNotify;'}

{$HPPEMIT 'typedef System::DelphiInterface<IEnumSTATURL> _di_IEnumSTATURL;'}
{$HPPEMIT 'typedef System::DelphiInterface<IUrlHistoryStg> _di_IUrlHistoryStg;'}
{$HPPEMIT 'typedef System::DelphiInterface<IUrlHistoryStg2> _di_IUrlHistoryStg2;'}
{$HPPEMIT 'typedef System::DelphiInterface<IUrlHistoryNotify> _di_IUrlHistoryNotify;'}

{$ENDIF JWA_OMIT_SECTIONS}
{$IFNDEF JWA_IMPLEMENTATIONSECTION}

//=--------------------------------------------------------------------------=
// UrlHist.h
//=--------------------------------------------------------------------------=
// (C) Copyright 1995-1998 Microsoft Corporation.  All Rights Reserved.
//
// THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF
// ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO
// THE IMPLIED WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A
// PARTICULAR PURPOSE.
//=--------------------------------------------------------------------------=

//---------------------------------------------------------------------------=
// Url History Interfaces.

const
  {$EXTERNALSYM IID_IEnumSTATURL}
  IID_IEnumSTATURL: TIID      = '{3C374A42-BAE4-11CF-BF7D-00AA006946EE}';
  {$EXTERNALSYM IID_IUrlHistoryStg}
  IID_IUrlHistoryStg: TIID    = '{3C374A41-BAE4-11CF-BF7D-00AA006946EE}';
  {$EXTERNALSYM IID_IUrlHistoryStg2}
  IID_IUrlHistoryStg2: TIID   = '{AFA0DC11-C313-11D0-831A-00C04FD5AE38}';
  {$EXTERNALSYM IID_IUrlHistoryNotify}
  IID_IUrlHistoryNotify: TIID = '{BC40BEC1-C493-11D0-831B-00C04FD5AE38}';

const
  {$EXTERNALSYM STATURL_QUERYFLAG_ISCACHED}
  STATURL_QUERYFLAG_ISCACHED  = $00010000;
  {$EXTERNALSYM STATURL_QUERYFLAG_NOURL}
  STATURL_QUERYFLAG_NOURL     = $00020000;
  {$EXTERNALSYM STATURL_QUERYFLAG_NOTITLE}
  STATURL_QUERYFLAG_NOTITLE   = $00040000;
  {$EXTERNALSYM STATURL_QUERYFLAG_TOPLEVEL}
  STATURL_QUERYFLAG_TOPLEVEL  = $00080000;
  {$EXTERNALSYM STATURLFLAG_ISCACHED}
  STATURLFLAG_ISCACHED        = $00000001;
  {$EXTERNALSYM STATURLFLAG_ISTOPLEVEL}
  STATURLFLAG_ISTOPLEVEL      = $00000002;

type
  {$EXTERNALSYM _ADDURL_FLAG}
  _ADDURL_FLAG = DWORD;
  {$EXTERNALSYM ADDURL_FLAG}
  ADDURL_FLAG = _ADDURL_FLAG;
  TAddUrlFlag = _ADDURL_FLAG;

const
  {$EXTERNALSYM ADDURL_FIRST}
  ADDURL_FIRST                = 0;
  {$EXTERNALSYM ADDURL_ADDTOHISTORYANDCACHE}
  ADDURL_ADDTOHISTORYANDCACHE = 0;
  {$EXTERNALSYM ADDURL_ADDTOCACHE}
  ADDURL_ADDTOCACHE           = 1;
  {$EXTERNALSYM ADDURL_Max}
  ADDURL_Max                  = 2147483647;

////////////////////////////////////////////////////////////////////////////
//  Interface Definitions

type
  PStatUrl = ^TStatUrl;
  {$EXTERNALSYM _STATURL}
  _STATURL = record
    cbSize: DWORD;
    pwcsUrl: PWideChar;
    pwcsTitle: PWideChar;
    ftLastVisited: TFileTime;
    ftLastUpdated: TFileTime;
    ftExpires: TFileTime;
    dwFlags: DWORD;
  end;
  {$EXTERNALSYM STATURL}
  STATURL = _STATURL;
  TStatUrl = _STATURL;

  {$EXTERNALSYM IEnumSTATURL}
  IEnumSTATURL = interface(IUnknown)
  ['{3C374A42-BAE4-11CF-BF7D-00AA006946EE}']
    function Next(celt: ULONG; var rgelt: TStatUrl;
      pceltFetched: ULONG): HResult; stdcall;
    function Skip(celt: ULONG): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumSTATURL): HResult; stdcall;
    function SetFilter(poszFilter: POleStr; dwFlags: DWORD): HResult; stdcall;
  end;

  {$EXTERNALSYM IUrlHistoryStg}
  IUrlHistoryStg = interface(IUnknown)
  ['{3C374A41-BAE4-11CF-BF7D-00AA006946EE}']
    function AddUrl(pocsUrl, pocsTitle: POleStr;
      dwFlags: DWORD): HResult; stdcall;
    function DeleteUrl(pocsUrl: POleStr; dwFlags: DWORD): HResult; stdcall;
    function QueryUrl(pocsUrl: POleStr; dwFlags: DWORD;
      var lpSTATURL: TStatUrl): HResult; stdcall;
    function BindToObject(pocsUrl: POleStr; const riid: TIID;
      out ppvOut: Pointer): HResult; stdcall;
    function EnumUrls(out ppEnum: IEnumSTATURL): HResult; stdcall;
  end;

  {$EXTERNALSYM IUrlHistoryStg2}
  IUrlHistoryStg2 = interface(IUrlHistoryStg)
  ['{AFA0DC11-C313-11d0-831A-00C04FD5AE38}']
    function AddUrlAndNotify(pocsUrl, pocsTitle: POleStr; dwFlags: DWORD;
      fWriteHistory: BOOL; poctNotify: IOleCommandTarget;
      punkISFolder: IUnknown): HResult; stdcall;
    function ClearHistory: HResult; stdcall;
  end;

  {$EXTERNALSYM IUrlHistoryNotify}
  IUrlHistoryNotify = interface(IOleCommandTarget)
  ['{BC40BEC1-C493-11d0-831B-00C04FD5AE38}']
  end;
{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
