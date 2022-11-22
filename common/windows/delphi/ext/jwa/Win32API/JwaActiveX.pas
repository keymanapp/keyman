{******************************************************************************}
{                                                                              }
{ Windows API interface Unit for Object Pascal                                 }
{ Master file for Windows applications                                         }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) Microsoft Corporation.       }
{  All Rights Reserved.                                                        }
{                                                                              }
{ The original Pascal code is: JwaActiveX.pas, released September 2005.        }
{ The initial developer of the Pascal code is                                  }
{ Robert Marquardt (robert_marquardt att gmx dott de).                         }
{                                                                              }
{ Portions created by Robert Marquardt are Copyright (C) 2005                  }
{ Robert Marquardt. All Rights Reserved.                                       }
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

// $Id: JwaActiveX.pas,v 1.5 2007/10/18 16:32:37 dezipaitor Exp $
{$IFNDEF JWA_OMIT_SECTIONS}
unit JwaActiveX;

{$WEAKPACKAGEUNIT}

{$I ..\Includes\JediAPILib.inc}

interface

uses
{$IFDEF DELPHI5}
  ActiveX,
{$ENDIF}
  JwaWinType;
{$ENDIF JWA_OMIT_SECTIONS}

{$IFNDEF JWA_IMPLEMENTATIONSECTION}


const
  { IConnectionPoint status codes }

  CONNECT_E_FIRST = HRESULT($80040200);
  {$EXTERNALSYM CONNECT_E_FIRST}
  CONNECT_E_LAST  = HRESULT($8004020F);
  {$EXTERNALSYM CONNECT_E_LAST}
  CONNECT_S_FIRST = $00040200;
  {$EXTERNALSYM CONNECT_S_FIRST}
  CONNECT_S_LAST  = $0004020F;
  {$EXTERNALSYM CONNECT_S_LAST}

  CONNECT_E_NOCONNECTION  = CONNECT_E_FIRST + 0;
  {$EXTERNALSYM CONNECT_E_NOCONNECTION}
  CONNECT_E_ADVISELIMIT   = CONNECT_E_FIRST + 1;
  {$EXTERNALSYM CONNECT_E_ADVISELIMIT}
  CONNECT_E_CANNOTCONNECT = CONNECT_E_FIRST + 2;
  {$EXTERNALSYM CONNECT_E_CANNOTCONNECT}
  CONNECT_E_OVERRIDDEN    = CONNECT_E_FIRST + 3;
  {$EXTERNALSYM CONNECT_E_OVERRIDDEN}

type
  IStream = interface;
  IMoniker = interface;
  IEnumMoniker = interface;
  IConnectionPointContainer = interface;
  IEnumSTATPROPSTG = interface;
  IEnumSTATPROPSETSTG = interface;

  TOleChar = WideChar;
  POleStr = PWideChar;
  PPOleStr = ^POleStr;

  PIID = PGUID;
  TIID = TGUID;

  PCLSID = PGUID;
  TCLSID = TGUID;

  Largeint = Int64;
  {$EXTERNALSYM Largeint}

  TClipFormat = Word;
  {$EXTERNALSYM TClipFormat}
  PClipFormat = ^TClipFormat;
  {$EXTERNALSYM PClipFormat}

  POleBool = ^TOleBool;
  TOleBool = WordBool;
  {$EXTERNALSYM TOleBool}

  POleDate = ^TOleDate;
  TOleDate = Double;
  {$EXTERNALSYM TOleDate}

  PBStr = ^TBStr;
  TBStr = POleStr;

  PSCODE = ^SCODE;
  {$EXTERNALSYM PSCODE}
  SCODE = Integer;
  {$EXTERNALSYM SCODE}

  PPropID = ^PROPID;
  PROPID = ULONG;
  {$EXTERNALSYM PROPID}
  TPropID = PROPID;

  PFmtID = ^FMTID;
  FMTID = TGUID;
  {$EXTERNALSYM FMTID}
  TFmtID = FMTID;

  PSafeArrayBound = ^SAFEARRAYBOUND;
  SAFEARRAYBOUND = record
    cElements: Longint;
    lLbound: Longint;
  end;
  TSafeArrayBound = SAFEARRAYBOUND;
  {$EXTERNALSYM SAFEARRAYBOUND}

  PSafeArray = ^SAFEARRAY;
  SAFEARRAY = record
    cDims: Word;
    fFeatures: Word;
    cbElements: Longint;
    cLocks: Longint;
    pvData: Pointer;
    rgsabound: array [0..0] of TSafeArrayBound;
  end;
  TSafeArray = SAFEARRAY;
  {$EXTERNALSYM SAFEARRAY}

  {$IFNDEF JWA_INCLUDEMODE}
  (* tagBLOB related types cause problem due to two definitions of BLOB in wtypes.h/nspapi.h *)
  (* Investigating something that will always work even when a BLOB is not a tagBLOB         *)
  {$EXTERNALSYM PBlob}
  PBlob = ^TBlob;
  {$EXTERNALSYM tagBLOB}
  tagBLOB = record
    cbSize: Longint;
    pBlobData: Pointer;
  end;
  (* tagBLOB related types cause problem due to two definitions of BLOB in wtypes.h/nspapi.h *)
  (* Investigating something that will always work even when a BLOB is not a tagBLOB         *)
  {$EXTERNALSYM TBlob}
  TBlob = tagBLOB;
  {$EXTERNALSYM BLOB}
  BLOB = TBlob;
  {$ENDIF JWA_INCLUDEMODE}

  PClipData = ^CLIPDATA;
  CLIPDATA = record
    cbSize: Longint;
    ulClipFmt: Longint;
    pClipData: Pointer;
  end;
  TClipData = CLIPDATA;
  {$EXTERNALSYM CLIPDATA}

  PDVTargetDevice = ^TDVTargetDevice;
  DVTARGETDEVICE = record
    tdSize: Longint;
    tdDriverNameOffset: Word;
    tdDeviceNameOffset: Word;
    tdPortNameOffset: Word;
    tdExtDevmodeOffset: Word;
    tdData: record end;
  end;
  {$EXTERNALSYM DVTARGETDEVICE}
  TDVTargetDevice = DVTARGETDEVICE;

  PFormatEtc = ^FORMATETC;
  FORMATETC = record
    cfFormat: TClipFormat;
    ptd: PDVTargetDevice;
    dwAspect: Longint;
    lindex: Longint;
    tymed: Longint;
  end;
  TFormatEtc = FORMATETC;
  {$EXTERNALSYM FORMATETC}

  PStgMedium = ^STGMEDIUM;
  STGMEDIUM = record
    tymed: Longint;
    case Integer of
      0: (hBitmap: THandle; // HBITMAP
          unkForRelease: Pointer); // IUnknown
      1: (hMetaFilePict: THandle);
      2: (hEnhMetaFile: THandle);
      3: (hGlobal: HGLOBAL);
      4: (lpszFileName: POleStr);
      5: (stm: Pointer); // IStream
      6: (stg: Pointer); // IStorage
  end;
  TStgMedium = STGMEDIUM;
  {$EXTERNALSYM STGMEDIUM}

  IEnumVariant = interface(IUnknown)
    ['{00020404-0000-0000-C000-000000000046}']
    function Next(celt: Longword; var rgvar: OleVariant;
      out pceltFetched: Longword): HRESULT; stdcall;
    function Skip(celt: Longword): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out Enum: IEnumVariant): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumVariant}

  IPersist = interface(IUnknown)
    ['{0000010C-0000-0000-C000-000000000046}']
    function GetClassID(out classID: TCLSID): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IPersist}

  IEnumFORMATETC = interface(IUnknown)
    ['{00000103-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumFORMATETC}

  IPersistStream = interface(IPersist)
    ['{00000109-0000-0000-C000-000000000046}']
    function IsDirty: HRESULT; stdcall;
    function Load(const stm: IStream): HRESULT; stdcall;
    function Save(const stm: IStream; fClearDirty: BOOL): HRESULT; stdcall;
    function GetSizeMax(out cbSize: Largeint): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IPersistStream}

  PStatStg = ^TStatStg;
  STATSTG = record
    pwcsName: POleStr;
    dwType: Longint;
    cbSize: Largeint;
    mtime: TFileTime;
    ctime: TFileTime;
    atime: TFileTime;
    grfMode: Longint;
    grfLocksSupported: Longint;
    clsid: TCLSID;
    grfStateBits: Longint;
    reserved: Longint;
  end;
  TStatStg = STATSTG;
  {$EXTERNALSYM STATSTG}

  {$EXTERNALSYM ISequentialStream}
  ISequentialStream = interface(IUnknown)
    ['{0c733a30-2a1c-11ce-ade5-00aa0044773d}']
    function Read(pv: Pointer; cb: Longint; pcbRead: PLongint): HRESULT; stdcall;
    function Write(pv: Pointer; cb: Longint; pcbWritten: PLongint): HRESULT; stdcall;
  end;

  IStream = interface(ISequentialStream)
    ['{0000000C-0000-0000-C000-000000000046}']
    function Seek(dlibMove: Largeint; dwOrigin: Longint;
      out libNewPosition: Largeint): HRESULT; stdcall;
    function SetSize(libNewSize: Largeint): HRESULT; stdcall;
    function CopyTo(stm: IStream; cb: Largeint; out cbRead: Largeint;
      out cbWritten: Largeint): HRESULT; stdcall;
    function Commit(grfCommitFlags: Longint): HRESULT; stdcall;
    function Revert: HRESULT; stdcall;
    function LockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HRESULT; stdcall;
    function UnlockRegion(libOffset: Largeint; cb: Largeint;
      dwLockType: Longint): HRESULT; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HRESULT; stdcall;
    function Clone(out stm: IStream): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IStream}

  IEnumString = interface(IUnknown)
    ['{00000101-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out enm: IEnumString): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumString}

  IRunningObjectTable = interface(IUnknown)
    ['{00000010-0000-0000-C000-000000000046}']
    function Register(grfFlags: Longint; const unkObject: IUnknown;
      const mkObjectName: IMoniker; out dwRegister: Longint): HRESULT; stdcall;
    function Revoke(dwRegister: Longint): HRESULT; stdcall;
    function IsRunning(const mkObjectName: IMoniker): HRESULT; stdcall;
    function GetObject(const mkObjectName: IMoniker;
      out unkObject: IUnknown): HRESULT; stdcall;
    function NoteChangeTime(dwRegister: Longint; const filetime: TFileTime): HRESULT; stdcall;
    function GetTimeOfLastChange(const mkObjectName: IMoniker;
      out filetime: TFileTime): HRESULT; stdcall;
    function EnumRunning(out enumMoniker: IEnumMoniker): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IRunningObjectTable}

  PBindOpts = ^TBindOpts;
  BIND_OPTS = record
    cbStruct: Longint;
    grfFlags: Longint;
    grfMode: Longint;
    dwTickCountDeadline: Longint;
  end;
  TBindOpts = BIND_OPTS;
  {$EXTERNALSYM BIND_OPTS}

  //>= 2000
  BIND_OPTS2 = record
    cbStruct:            DWORD;
    grfFlags:            DWORD;
    grfMode:             DWORD;
    dwTickCountDeadline: DWORD;
    dwTrackFlags:        DWORD;
    dwClassContext:      DWORD;
    locale:              LCID;
    pServerInfo:         Pointer;//PCOSERVERINFO;
  end;
  TBindOpts2 = BIND_OPTS2;
  PBindOpts2 = ^TBindOpts2;
  {$EXTERNALSYM BIND_OPTS2}

  //>= VISTA
  BIND_OPTS3 = record
    cbStruct:            DWORD;
    grfFlags:            DWORD;
    grfMode:             DWORD;
    dwTickCountDeadline: DWORD;
    dwTrackFlags:        DWORD;
    dwClassContext:      DWORD;
    locale:              LCID;
    pServerInfo:         Pointer;//PCOSERVERINFO;
    hwnd:                HWND;
  end;
  TBindOpts3 = BIND_OPTS3;
  PBindOpts3 = ^TBindOpts3;
  {$EXTERNALSYM BIND_OPTS3}


  IBindCtx = interface(IUnknown)
    ['{0000000E-0000-0000-C000-000000000046}']
    function RegisterObjectBound(const unk: IUnknown): HRESULT; stdcall;
    function RevokeObjectBound(const unk: IUnknown): HRESULT; stdcall;
    function ReleaseBoundObjects: HRESULT; stdcall;
    function SetBindOptions(const bindopts: TBindOpts): HRESULT; stdcall;
    function GetBindOptions(var bindopts: TBindOpts): HRESULT; stdcall;
    function GetRunningObjectTable(out rot: IRunningObjectTable): HRESULT; stdcall;
    function RegisterObjectParam(pszKey: POleStr; const unk: IUnknown): HRESULT; stdcall;
    function GetObjectParam(pszKey: POleStr; out unk: IUnknown): HRESULT; stdcall;
    function EnumObjectParam(out Enum: IEnumString): HRESULT; stdcall;
    function RevokeObjectParam(pszKey: POleStr): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IBindCtx}

  PIMoniker = ^IMoniker;
  {$EXTERNALSYM PIMoniker}
  IMoniker = interface(IPersistStream)
    ['{0000000F-0000-0000-C000-000000000046}']
    function BindToObject(const bc: IBindCtx; const mkToLeft: IMoniker;
      const iidResult: TIID; out vResult): HRESULT; stdcall;
    function BindToStorage(const bc: IBindCtx; const mkToLeft: IMoniker;
      const iid: TIID; out vObj): HRESULT; stdcall;
    function Reduce(const bc: IBindCtx; dwReduceHowFar: Longint;
      mkToLeft: PIMoniker; out mkReduced: IMoniker): HRESULT; stdcall;
    function ComposeWith(const mkRight: IMoniker; fOnlyIfNotGeneric: BOOL;
      out mkComposite: IMoniker): HRESULT; stdcall;
    function Enum(fForward: BOOL; out enumMoniker: IEnumMoniker): HRESULT; stdcall;
    function IsEqual(const mkOtherMoniker: IMoniker): HRESULT; stdcall;
    function Hash(out dwHash: Longint): HRESULT; stdcall;
    function IsRunning(const bc: IBindCtx; const mkToLeft: IMoniker;
      const mkNewlyRunning: IMoniker): HRESULT; stdcall;
    function GetTimeOfLastChange(const bc: IBindCtx; const mkToLeft: IMoniker;
      out filetime: TFileTime): HRESULT; stdcall;
    function Inverse(out mk: IMoniker): HRESULT; stdcall;
    function CommonPrefixWith(const mkOther: IMoniker;
      out mkPrefix: IMoniker): HRESULT; stdcall;
    function RelativePathTo(const mkOther: IMoniker;
      out mkRelPath: IMoniker): HRESULT; stdcall;
    function GetDisplayName(const bc: IBindCtx; const mkToLeft: IMoniker;
      out pszDisplayName: POleStr): HRESULT; stdcall;
    function ParseDisplayName(const bc: IBindCtx; const mkToLeft: IMoniker;
      pszDisplayName: POleStr; out chEaten: Longint;
      out mkOut: IMoniker): HRESULT; stdcall;
    function IsSystemMoniker(out dwMksys: Longint): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IMoniker}

  {$EXTERNALSYM IEnumMoniker}
  IEnumMoniker = interface(IUnknown)
    ['{00000102-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out enm: IEnumMoniker): HRESULT; stdcall;
  end;

  IAdviseSink = interface(IUnknown)
    ['{0000010F-0000-0000-C000-000000000046}']
    procedure OnDataChange(const formatetc: TFormatEtc; const stgmed: TStgMedium); stdcall;
    procedure OnViewChange(dwAspect: Longint; lindex: Longint); stdcall;
    procedure OnRename(const mk: IMoniker); stdcall;
    procedure OnSave; stdcall;
    procedure OnClose; stdcall;
  end;
  {$EXTERNALSYM IAdviseSink}

  PStatData = ^TStatData;
  STATDATA = record
    formatetc: TFormatEtc;
    advf: Longint;
    advSink: IAdviseSink;
    dwConnection: Longint;
  end;
  TStatData = STATDATA;
  {$EXTERNALSYM STATDATA}

  IEnumSTATDATA = interface(IUnknown)
    ['{00000105-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out Enum: IEnumSTATDATA): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumSTATDATA}

  IDataObject = interface(IUnknown)
    ['{0000010E-0000-0000-C000-000000000046}']
    function GetData(const formatetcIn: TFormatEtc; out medium: TStgMedium): HRESULT; stdcall;
    function GetDataHere(const formatetc: TFormatEtc; out medium: TStgMedium): HRESULT; stdcall;
    function QueryGetData(const formatetc: TFormatEtc): HRESULT; stdcall;
    function GetCanonicalFormatEtc(const formatetc: TFormatEtc; out formatetcOut: TFormatEtc): HRESULT; stdcall;
    function SetData(const formatetc: TFormatEtc; var medium: TStgMedium; fRelease: BOOL): HRESULT; stdcall;
    function EnumFormatEtc(dwDirection: DWORD; out enumFormatEtc: IEnumFORMATETC): HRESULT; stdcall;
    function DAdvise(const formatetc: TFormatEtc; advf: DWORD;
      const advSink: IAdviseSink; out dwConnection: DWORD): HRESULT; stdcall;
    function DUnadvise(dwConnection: DWORD): HRESULT; stdcall;
    function EnumDAdvise(out enumAdvise: IEnumSTATDATA): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IDataObject}

  IEnumConnectionPoints = interface
    ['{B196B285-BAB4-101A-B69C-00AA00341D07}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out Enum: IEnumConnectionPoints): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumConnectionPoints}

  IEnumConnections = interface
    ['{B196B287-BAB4-101A-B69C-00AA00341D07}']
    function Next(celt: Longint; out elt; pceltFetched: PLongint): HRESULT; stdcall;
    function Skip(celt: Longint): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out Enum: IEnumConnections): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumConnections}

  IConnectionPoint = interface
    ['{B196B286-BAB4-101A-B69C-00AA00341D07}']
    function GetConnectionInterface(out iid: TIID): HRESULT; stdcall;
    function GetConnectionPointContainer(out cpc: IConnectionPointContainer): HRESULT; stdcall;
    function Advise(const unkSink: IUnknown; out dwCookie: Longint): HRESULT; stdcall;
    function Unadvise(dwCookie: Longint): HRESULT; stdcall;
    function EnumConnections(out Enum: IEnumConnections): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IConnectionPoint}

  IConnectionPointContainer = interface
    ['{B196B284-BAB4-101A-B69C-00AA00341D07}']
    function EnumConnectionPoints(out Enum: IEnumConnectionPoints): HRESULT; stdcall;
    function FindConnectionPoint(const iid: TIID; out cp: IConnectionPoint): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IConnectionPointContainer}

  PPropVariant = ^TPropVariant;

  CAUB = packed record
    cElems: ULONG;
    pElems: PByte;
  end;
  {$EXTERNALSYM CAUB}
  PCAUB = ^TCAUB;
  TCAUB = CAUB;

  CAI = packed record
    cElems: ULONG;
    pElems: PShortInt;
  end;
  {$EXTERNALSYM CAI}
  PCAI = ^TCAI;
  TCAI = CAI;

  CAUI = packed record
    cElems: ULONG;
    pElems: PWord;
  end;
  {$EXTERNALSYM CAUI}
  PCAUI = ^TCAUI;
  TCAUI = CAUI;

  CAL = packed record
    cElems: ULONG;
    pElems: PLongint;
  end;
  {$EXTERNALSYM CAL}
  PCAL = ^TCAL;
  TCAL = CAL;

  CAUL = packed record
    cElems: ULONG;
    pElems: PULONG;
  end;
  {$EXTERNALSYM CAUL}
  PCAUL = ^TCAUL;
  TCAUL = CAUL;

  CAFLT = packed record
    cElems: ULONG;
    pElems: PSingle;
  end;
  {$EXTERNALSYM CAFLT}
  PCAFLT = ^TCAFLT;
  TCAFLT = CAFLT;

  CADBL = packed record
    cElems: ULONG;
    pElems: PDouble;
  end;
  {$EXTERNALSYM CADBL}
  PCADBL = ^TCADBL;
  TCADBL = CADBL;

  CACY = packed record
    cElems: ULONG;
    pElems: PCurrency;
  end;
  {$EXTERNALSYM CACY}
  PCACY = ^TCACY;
  TCACY = CACY;

  CADATE = packed record
    cElems: ULONG;
    pElems: POleDate;
  end;
  {$EXTERNALSYM CADATE}
  PCADATE = ^TCADATE;
  TCADATE = CADATE;

  CABSTR = packed record
    cElems: ULONG;
    pElems: PBSTR;
  end;
  {$EXTERNALSYM CABSTR}
  PCABSTR = ^TCABSTR;
  TCABSTR = CABSTR;

  CABOOL = packed record
    cElems: ULONG;
    pElems: POleBool;
  end;
  {$EXTERNALSYM CABOOL}
  PCABOOL = ^TCABOOL;
  TCABOOL = CABOOL;

  CASCODE = packed record
    cElems: ULONG;
    pElems: PSCODE;
  end;
  {$EXTERNALSYM CASCODE}
  PCASCODE = ^TCASCODE;
  TCASCODE = CASCODE;

  CAPROPVARIANT = packed record
    cElems: ULONG;
    pElems: PPropVariant;
  end;
  {$EXTERNALSYM CAPROPVARIANT}
  PCAPROPVARIANT = ^TCAPROPVARIANT;
  TCAPROPVARIANT = CAPROPVARIANT;

  CAH = packed record
    cElems: ULONG;
    pElems: PLargeInteger;
  end;
  {$EXTERNALSYM CAH}
  PCAH = ^TCAH;
  TCAH = CAH;

  CAUH = packed record
    cElems: ULONG;
    pElems: PULargeInteger;
  end;
  {$EXTERNALSYM CAUH}
  PCAUH = ^TCAUH;
  TCAUH = CAUH;

  CALPSTR = packed record
    cElems: ULONG;
    pElems: PLPSTR;
  end;
  {$EXTERNALSYM CALPSTR}
  PCALPSTR = ^TCALPSTR;
  TCALPSTR = CALPSTR;

  CALPWSTR = packed record
    cElems: ULONG;
    pElems: PLPWSTR;
  end;
  {$EXTERNALSYM CALPWSTR}
  PCALPWSTR = ^TCALPWSTR;
  TCALPWSTR = CALPWSTR;

  CAFILETIME = packed record
    cElems: ULONG;
    pElems: PFileTime;
  end;
  {$EXTERNALSYM CAFILETIME}
  PCAFILETIME = ^TCAFILETIME;
  TCAFILETIME = CAFILETIME;

  CACLIPDATA = packed record
    cElems: ULONG;
    pElems: PClipData;
  end;
  {$EXTERNALSYM CACLIPDATA}
  PCACLIPDATA = ^TCACLIPDATA;
  TCACLIPDATA = CACLIPDATA;

  CACLSID = packed record
    cElems: ULONG;
    pElems: PCLSID;
  end;
  {$EXTERNALSYM CACLSID}
  PCACLSID = ^TCACLSID;
  TCACLSID = CACLSID;

  PROPVARIANT = packed record
    vt: TVarType;
    wReserved1: Word;
    wReserved2: Word;
    wReserved3: Word;
    case Integer of
      0: (bVal: Byte);
      1: (iVal: Smallint);
      2: (uiVal: Word);
      3: (boolVal: TOleBool);
      4: (bool: TOleBool);
      5: (lVal: Longint);
      6: (ulVal: Cardinal);
      7: (fltVal: Single);
      8: (scode: SCODE);
      9: (hVal: LARGE_INTEGER);
      10: (uhVal: ULARGE_INTEGER);
      11: (dblVal: Double);
      12: (cyVal: Currency);
      13: (date: TOleDate);
      14: (filetime: TFileTime);
      15: (puuid: PGUID);
      16: (blob: TBlob);
      17: (pclipdata: PClipData);
      18: (pStream: Pointer);  // IStream
      19: (pStorage: Pointer);  // IStorage
      20: (bstrVal: TBStr);
      21: (pszVal: PAnsiChar);
      22: (pwszVal: PWideChar);
      23: (caub: TCAUB);
      24: (cai: TCAI);
      25: (caui: TCAUI);
      26: (cabool: TCABOOL);
      27: (cal: TCAL);
      28: (caul: TCAUL);
      29: (caflt: TCAFLT);
      30: (cascode: TCASCODE);
      31: (cah: TCAH);
      32: (cauh: TCAUH);
      33: (cadbl: TCADBL);
      34: (cacy: TCACY);
      35: (cadate: TCADATE);
      36: (cafiletime: TCAFILETIME);
      37: (cauuid: TCACLSID);
      38: (caclipdata: TCACLIPDATA);
      39: (cabstr: TCABSTR);
      40: (calpstr: TCALPSTR);
      41: (calpwstr: TCALPWSTR );
      42: (capropvar: TCAPROPVARIANT);
  end;
  {$EXTERNALSYM PROPVARIANT}
  TPropVariant = PROPVARIANT;

  PROPSPEC = packed record
    ulKind: ULONG;
    case Integer of
      0: (propid: TPropID);
      1: (lpwstr: POleStr);
  end;
  {$EXTERNALSYM PROPSPEC}
  PPropSpec = ^TPropSpec;
  TPropSpec = PROPSPEC;

  STATPROPSTG = record
    lpwstrName: POleStr;
    propid: TPropID;
    vt: TVarType;
  end;
  {$EXTERNALSYM STATPROPSTG}
  PStatPropStg = ^TStatPropStg;
  TStatPropStg = STATPROPSTG;

  STATPROPSETSTG = packed record
    fmtid: TFmtID;
    clsid: TClsID;
    grfFlags: DWORD;
    mtime: TFileTime;
    ctime: TFileTime;
    atime: TFileTime;
    dwOSVersion: DWORD;
  end;
  {$EXTERNALSYM STATPROPSETSTG}
  PStatPropSetStg = ^TStatPropSetStg;
  TStatPropSetStg = STATPROPSETSTG;

  IPropertyStorage = interface(IUnknown)
    ['{00000138-0000-0000-C000-000000000046}']
    function ReadMultiple(cpspec: ULONG; rgpspec : PPropSpec; rgpropvar: PPropVariant): HRESULT; stdcall;
    function WriteMultiple(cpspec: ULONG; rgpspec : PPropSpec; rgpropvar: PPropVariant;
      propidNameFirst: TPropID): HRESULT; stdcall;
    function DeleteMultiple(cpspec: ULONG; rgpspec: PPropSpec): HRESULT; stdcall;
    function ReadPropertyNames(cpropid: ULONG; rgpropid: PPropID;
      rglpwstrName: PPOleStr): HRESULT; stdcall;
    function WritePropertyNames(cpropid: ULONG; rgpropid: PPropID;
      rglpwstrName: PPOleStr): HRESULT; stdcall;
    function DeletePropertyNames(cpropid: ULONG; rgpropid: PPropID): HRESULT; stdcall;
    function Commit(grfCommitFlags: DWORD): HRESULT; stdcall;
    function Revert: HRESULT; stdcall;
    function Enum(out ppenum: IEnumSTATPROPSTG): HRESULT; stdcall;
    function SetTimes(const pctime, patime, pmtime: TFileTime): HRESULT; stdcall;
    function SetClass(const clsid: TCLSID): HRESULT; stdcall;
    function Stat(pstatpsstg: PStatPropSetStg): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IPropertyStorage}

  IPropertySetStorage = interface(IUnknown)
    ['{0000013A-0000-0000-C000-000000000046}']
    function Create(const rfmtid: TFmtID; const pclsid: TCLSID; grfFlags,
      grfMode: DWORD; out ppprstg: IPropertyStorage): HRESULT; stdcall;
    function Open(const rfmtid: TFmtID; grfMode: DWORD;
      out ppprstg: IPropertyStorage): HRESULT; stdcall;
    function Delete(const rfmtid: TFmtID): HRESULT; stdcall;
    function Enum(out ppenum: IEnumSTATPROPSETSTG): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IPropertySetStorage}

  IEnumSTATPROPSTG = interface(IUnknown)
    ['{00000139-0000-0000-C000-000000000046}']
    function Next(celt: ULONG; out rgelt; pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumSTATPROPSTG): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumSTATPROPSTG}

  IEnumSTATPROPSETSTG = interface(IUnknown)
    ['{0000013B-0000-0000-C000-000000000046}']
    function Next(celt: ULONG; out rgelt; pceltFetched: PULONG): HRESULT; stdcall;
    function Skip(celt: ULONG): HRESULT; stdcall;
    function Reset: HRESULT; stdcall;
    function Clone(out ppenum: IEnumSTATPROPSETSTG): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IEnumSTATPROPSETSTG}

  IEnumStatStg = interface(IUnknown)
    ['{0000000D-0000-0000-C000-000000000046}']
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumStatStg): HResult; stdcall;
  end;
  {$EXTERNALSYM IEnumStatStg}

  TSNB = ^POleStr;

  IStorage = interface(IUnknown)
    ['{0000000B-0000-0000-C000-000000000046}']
    function CreateStream(pwcsName: POleStr; grfMode: Longint; reserved1: Longint;
      reserved2: Longint; out stm: IStream): HRESULT; stdcall;
    function OpenStream(pwcsName: POleStr; reserved1: Pointer; grfMode: Longint;
      reserved2: Longint; out stm: IStream): HRESULT; stdcall;
    function CreateStorage(pwcsName: POleStr; grfMode: Longint;
      dwStgFmt: Longint; reserved2: Longint; out stg: IStorage): HRESULT; stdcall;
    function OpenStorage(pwcsName: POleStr; const stgPriority: IStorage;
      grfMode: Longint; snbExclude: TSNB; reserved: Longint; out stg: IStorage): HRESULT; stdcall;
    function CopyTo(ciidExclude: Longint; rgiidExclude: PIID;
      snbExclude: TSNB; const stgDest: IStorage): HRESULT; stdcall;
    function MoveElementTo(pwcsName: POleStr; const stgDest: IStorage;
      pwcsNewName: POleStr; grfFlags: Longint): HRESULT; stdcall;
    function Commit(grfCommitFlags: Longint): HRESULT; stdcall;
    function Revert: HRESULT; stdcall;
    function EnumElements(reserved1: Longint; reserved2: Pointer; reserved3: Longint;
      out enm: IEnumStatStg): HRESULT; stdcall;
    function DestroyElement(pwcsName: POleStr): HRESULT; stdcall;
    function RenameElement(pwcsOldName: POleStr; pwcsNewName: POleStr): HRESULT; stdcall;
    function SetElementTimes(pwcsName: POleStr; const ctime: TFileTime;
      const atime: TFileTime; const mtime: TFileTime): HRESULT; stdcall;
    function SetClass(const clsid: TCLSID): HRESULT; stdcall;
    function SetStateBits(grfStateBits: Longint; grfMask: Longint): HRESULT; stdcall;
    function Stat(out statstg: TStatStg; grfStatFlag: Longint): HRESULT; stdcall;
  end;
  {$EXTERNALSYM IStorage}


function CoGetObject(pszName: PWideChar; pBindOptions: PBindOpts;
    const iid: TIID; out ppv): HResult; stdcall;
{$EXTERNALSYM CoGetObject}


{$ENDIF JWA_IMPLEMENTATIONSECTION}



{$IFNDEF JWA_OMIT_SECTIONS}
implementation
uses JwaWinDLLNames;
{$ENDIF JWA_OMIT_SECTIONS}


{$IFNDEF JWA_INTERFACESECTION}
//your implementation here

{$IFDEF DYNAMIC_LINK}
var
  _CoGetObject: Pointer;

function CoGetObject(pszName: PWideChar; pBindOptions: PBindOpts;
     const iid: TIID; out ppv): HResult;
begin
  GetProcedureAddress(_CoGetObject, 'ole32.dll', 'CoGetObject');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_CoGetObject]
  end;
end;
{$ELSE}
  function CoGetObject(pszName: PWideChar; pBindOptions: PBindOpts;
     const iid: TIID; out ppv): HResult; stdcall; external 'ole32.dll' name 'CoGetObject';
{$ENDIF DYNAMIC_LINK}


{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_OMIT_SECTIONS}
end.
{$ENDIF JWA_OMIT_SECTIONS}
