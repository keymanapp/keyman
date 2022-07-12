{******************************************************************************}
{                                                                              }
{ Background Intelligent Transfer API interface Unit for Object Pascal         }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: bits3_0.h, The initial developer of the                }
{ Pascal code is TUO (http://www.TheUnknownOnes.net).                          }
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

{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaBits3_0;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "bits3_0.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}


interface

uses
  JwaWinBase,
  JwaWinType,
  JwaBits,
  JwaBits1_5,
  JwaBits2_0,
  JwaBits2_5;
  
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}

const
  IID_IBitsPeerCacheRecord : TGUID = '{659cdeaf-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBitsPeerCacheRecord}

type
  IBitsPeerCacheRecord = interface(IUnknown)
  ['{659cdeaf-489e-11d9-a9cd-000d56965251}']
    function GetId(out ID : GUID) : HRESULT; stdcall;
    function GetOriginUrl(out OriginUrl : LPWSTR) : HRESULT; stdcall;
    function GetFileSize(out FileSize : UINT64) : HRESULT; stdcall;
    function GetFileModificationTime(out ModificationTime : FILETIME) : HRESULT; stdcall;
    function GetLastAccessTime(out AccessTime : FILETIME) : HRESULT; stdcall;
    function IsFileValidated() : HRESULT; stdcall;
    function GetFileRanges(out RangeCount : DWORD; out Ranges : TBgFileRanges) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBitsPeerCacheRecord}

const
  IID_IEnumBitsPeerCacheRecords : TGUID = '{659cdea4-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IEnumBitsPeerCacheRecords}

type
  IEnumBitsPeerCacheRecords = interface(IUnknown)
  ['{659cdea4-489e-11d9-a9cd-000d56965251}']
    function Next(celt: ULONG; out rgelt: IBitsPeerCacheRecord;  pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumBitsPeerCacheRecords): HRESULT; stdcall;
    function GetCount(out puCount: ULONG): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumBitsPeerCacheRecords}

const
  IID_IBitsPeer : TGUID = '{659cdea2-489e-11d9-a9cd-000d56965251}';

type
  IBitsPeer = interface(IUnknown)
  ['{659cdea2-489e-11d9-a9cd-000d56965251}']
    function GetPeerName(out PeerName : LPWSTR) : HRESULT; stdcall;
    function IsAuthenticated(out Authenticated : BOOL) : HRESULT; stdcall;
    function IsAvailable(out Available : BOOL) : HRESULT; stdcall; 
  end;
  {$EXTERNALSYM IBitsPeer}

const
  IID_IEnumBitsPeers : TGUID = '{659cdea5-489e-11d9-a9cd-000d56965251}';

type
  IEnumBitsPeers = interface(IUnknown)
  ['{659cdea5-489e-11d9-a9cd-000d56965251}']
    function Next(celt: ULONG; out rgelt: IBitsPeer;  pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumBitsPeers): HRESULT; stdcall;
    function GetCount(out puCount: ULONG): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumBitsPeers}

const
  BG_ENABLE_PEERCACHING_CLIENT = $0001;
  {$EXTERNALSYM BG_ENABLE_PEERCACHING_CLIENT}
  BG_ENABLE_PEERCACHING_SERVER = $0002;
  {$EXTERNALSYM BG_ENABLE_PEERCACHING_SERVER}

  IID_IBitsPeerCacheAdministration : TGUID = '{659cdead-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBitsPeerCacheAdministration}

type
  IBitsPeerCacheAdministration = interface(IUnknown)
  ['{659cdead-489e-11d9-a9cd-000d56965251}']
    function GetMaximumCacheSize(out Bytes : DWORD) : HRESULT; stdcall;
    function SetMaximumCacheSize(Bytes : DWORD) : HRESULT; stdcall;
    function GetMaximumContentAge(out Seconds : ULONG) : HRESULT; stdcall;
    function SetMaximumContentAge(Seconds : ULONG) : HRESULT; stdcall;
    function GetConfigurationFlags(out Flags : DWORD) : HRESULT; stdcall;
    function SetConfigurationFlags(Flags : DWORD) : HRESULT; stdcall;
    function EnumRecords(out Enum : IEnumBitsPeerCacheRecords) : HRESULT; stdcall;
    function GetRecord(var Id : TGUID; out _Record : IBitsPeerCacheRecord) : HRESULT; stdcall;
    function ClearRecords() : HRESULT; stdcall;
    function DeleteRecord(var Id : TGUID) : HRESULT; stdcall;
    function DeleteUrl(Url : LPCWSTR) : HRESULT; stdcall;
    function EnumPeers(out Enum : IEnumBitsPeers) : HRESULT; stdcall;
    function ClearPeers() : HRESULT; stdcall;
    function DiscoverPeers : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBitsPeerCacheAdministration}

const
  BG_JOB_ENABLE_PEERCACHING_CLIENT = $0001;
  {$EXTERNALSYM BG_JOB_ENABLE_PEERCACHING_CLIENT}
  BG_JOB_ENABLE_PEERCACHING_SERVER = $0002;
  {$EXTERNALSYM BG_JOB_ENABLE_PEERCACHING_SERVER}

  IID_IBackgroundCopyJob4 : TGUID = '{659cdeae-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBackgroundCopyJob4}

type
  IBackgroundCopyJob4 = interface(IBackgroundCopyJob3)
  ['{659cdeae-489e-11d9-a9cd-000d56965251}']
    function SetPeerCachingFlags(Flags : DWORD) : HRESULT; stdcall;
    function GetPeerCachingFlags(out Flags : DWORD) : HRESULT; stdcall;
    function GetOwnerIntegrityLevel(out Level : ULONG) : HRESULT; stdcall;
    function GetOwnerElevationState(out Elevated : BOOL) : HRESULT; stdcall;
    function SetMaximumDownloadTime(Timeout : ULONG) : HRESULT; stdcall;
    function GetMaximumDownloadTime(out Timeout : ULONG) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBackgroundCopyJob4}

const
  IID_IBackgroundCopyFile3 : TGUID = '{659cdeaa-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBackgroundCopyFile3}

type
  IBackgroundCopyFile3 = interface(IBackgroundCopyFile2)
  ['{659cdeaa-489e-11d9-a9cd-000d56965251}']
    function GetTemporaryName(out Filename : LPWSTR) : HRESULT; stdcall;
    function SetValidationState(State : BOOL) : HRESULT; stdcall;
    function GetValidationState(out State : BOOL) : HRESULT; stdcall;
    function IsDownloadedFromPeer(out FromPeer : BOOL) : HRESULT; stdcall;
  end;  
  {$EXTERNALSYM IBackgroundCopyFile3}

const
  IID_IBackgroundCopyCallback2 : TGUID = '{659cdeac-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM IID_IBackgroundCopyCallback2}

type
  IBackgroundCopyCallback2 = interface(IBackgroundCopyCallback)
  ['{659cdeac-489e-11d9-a9cd-000d56965251}']
    function FileTransferred(Job : IBackgroundCopyJob; _File : IBackgroundCopyFile) : HRESULT; stdcall;  
  end;

//---------------------------------------------------------------------------

const
  LIBID_BackgroundCopyManager3_0: GUID = '{659cdea7-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager3_0}
  CLSID_BackgroundCopyManager3_0: GUID = '{659cdea7-489e-11d9-a9cd-000d56965251}';
  {$EXTERNALSYM CLSID_BackgroundCopyManager3_0}
  
{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}

implementation
//uses ...
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_INTERFACESECTION}
//your implementation here
{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
