{******************************************************************************}
{                                                                              }
{ Background Intelligent Transfer API interface Unit for Object Pascal         }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: bits2_0.h, The initial developer of the                }
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
unit JwaBits2_0;

{$WEAKPACKAGEUNIT}
{$ENDIF JWA_OMIT_SECTIONS}

{$HPPEMIT ''}
{$HPPEMIT '#include "bits2_0.h"'}
{$HPPEMIT ''}

{$IFNDEF JWA_OMIT_SECTIONS}
{$I ..\Includes\JediAPILib.inc}

interface

uses
  JwaWinBase,
  JwaWinType,
  JwaBits,
  JwaBits1_5;
  
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_IMPLEMENTATIONSECTION}

type
  _BG_FILE_RANGE = record
    InitialOffset : UINT64;
    Length : UINT64;
  end;
  {$EXTERNALSYM _BG_FILE_RANGE}
  BG_FILE_RANGE = _BG_FILE_RANGE;
  {$EXTERNALSYM BG_FILE_RANGE}
  TBgFileRange = BG_FILE_RANGE;
  PBgFileRange = ^BG_FILE_RANGE;
  TBgFileRanges = array of TBgFileRange;

const
  BG_COPY_FILE_OWNER = 1;
  {$EXTERNALSYM BG_COPY_FILE_OWNER}
  BG_COPY_FILE_GROUP = 2;
  {$EXTERNALSYM BG_COPY_FILE_GROUP}
  BG_COPY_FILE_DACL = 4;
  {$EXTERNALSYM BG_COPY_FILE_DACL}
  BG_COPY_FILE_SACL = 8;
  {$EXTERNALSYM BG_COPY_FILE_SACL}
  BG_COPY_FILE_ALL = 15;
  {$EXTERNALSYM BG_COPY_FILE_ALL}

  IID_IBackgroundCopyJob3 : TGUID = '{443c8934-90ff-48ed-bcde-26f5c7450042}';
  {$EXTERNALSYM IID_IBackgroundCopyJob3}

type
  IBackgroundCopyJob3 = interface(IBackgroundCopyJob2)
  ['{443c8934-90ff-48ed-bcde-26f5c7450042}']
    function ReplaceRemotePrefix(OldPrefix : LPCWSTR; NewPrefix : LPCWSTR) : HRESULT; stdcall;
    function AddFileWithRanges(RemoteUrl : LPCWSTR; LocalName : LPCWSTR; RangeCount : DWORD; Ranges : TBgFileRanges) : HRESULT; stdcall;
    function SetFileACLFlags(Flags : DWORD) : HRESULT; stdcall;
    function GetFileACLFlags(out Flags : DWORD) : HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBackgroundCopyJob3}

const
  IID_IBackgroundCopyFile2 : TGUID = '{83e81b93-0873-474d-8a8c-f2018b1a939c}';
  {$EXTERNALSYM IID_IBackgroundCopyFile2}

type
  IBackgroundCopyFile2 = interface(IBackgroundCopyFile)
  ['{83e81b93-0873-474d-8a8c-f2018b1a939c}']
    function GetFileRanges(var RangeCount : DWORD; var Ranges : TBgFileRanges) : HRESULT; stdcall;
    function SetRemoteName(RemoteName : LPCWSTR) : HRESULT; stdcall;
  end;

//---------------------------------------------------------------------------

const
  LIBID_BackgroundCopyManager2_0: GUID = '{6d18ad12-bde3-4393-b311-099c346e6df9}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager2_0}
  CLSID_BackgroundCopyManager2_0: GUID = '{6d18ad12-bde3-4393-b311-099c346e6df9}';
  {$EXTERNALSYM LIBID_BackgroundCopyManager2_0}
  
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
