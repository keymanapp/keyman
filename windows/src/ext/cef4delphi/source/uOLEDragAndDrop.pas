// ************************************************************************
// ***************************** CEF4Delphi *******************************
// ************************************************************************
//
// CEF4Delphi is based on DCEF3 which uses CEF3 to embed a chromium-based
// browser in Delphi applications.
//
// The original license of DCEF3 still applies to CEF4Delphi.
//
// For more information about CEF4Delphi visit :
//         https://www.briskbard.com/index.php?lang=en&pageid=cef
//
//        Copyright � 2018 Salvador Diaz Fau. All rights reserved.
//
// ************************************************************************
// ************ vvvv Original license and comments below vvvv *************
// ************************************************************************
(*
 *                       Delphi Chromium Embedded 3
 *
 * Usage allowed under the restrictions of the Lesser GNU General Public License
 * or alternatively the restrictions of the Mozilla Public License 1.1
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * Unit owner : Henri Gourvest <hgourvest@gmail.com>
 * Web site   : http://www.progdigy.com
 * Repository : http://code.google.com/p/delphichromiumembedded/
 * Group      : http://groups.google.com/group/delphichromiumembedded
 *
 * Embarcadero Technologies, Inc is not permitted to use or redistribute
 * this source code without explicit permission.
 *
 *)

unit uOLEDragAndDrop;

{$IFDEF FPC}
  {$MODE OBJFPC}{$H+}
{$ENDIF}

{$I cef.inc}

interface

uses
  {$IFDEF DELPHI16_UP}
    {$IFDEF MSWINDOWS}WinApi.Windows, WinApi.ShlObj, WinApi.ActiveX,{$ENDIF} System.Classes, System.Math;
  {$ELSE}
    {$IFDEF MSWINDOWS}Windows, ShlObj, ActiveX,{$ENDIF} Classes, Math;
  {$ENDIF}

const
  CUSTOM_ARRAY_LENGTH = 25;

type
  TOLEMediumArray = array [0..pred(CUSTOM_ARRAY_LENGTH)] of TStgMedium;
  TOLEFormatArray = array [0..pred(CUSTOM_ARRAY_LENGTH)] of TFormatEtc;

  TOLEDragAndDropMgr = class
    protected
      function  GetStorageForBytes(var aMedium : TStgMedium; const aData : pointer; aLength : NativeUInt) : boolean;
      function  GetStorageForString(var aMedium : TStgMedium; const aData : WideString) : boolean; overload;
      function  GetStorageForString(var aMedium : TStgMedium; const aData : AnsiString) : boolean; overload;
      function  GetStorageForFileDescriptor(var aMedium : TStgMedium; const aFileName : string) : boolean;

    public
      function  DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; virtual;
      function  DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; virtual;
      function  DragLeave: HRESULT; virtual;
      function  Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; virtual;
  end;

  TOLEEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
    protected
      FIndex       : integer;
      FNumFormats  : integer;
      FFormatArray : TOLEFormatArray;

      procedure CopyFromFormatArray(const aSrcFormatArray : TOLEFormatArray);
      procedure CopyFormatEtc(var aDstFormatEtc : TFormatEtc; const aSrcFormatEtc : TFormatEtc);

    public
      constructor Create; overload;
      constructor Create(const aFormatArray : TOLEFormatArray; aNumFormats : integer; aIndex : integer = 0); overload;
      destructor  Destroy; override;

      // IEnumFormatEtc
      function Next(Celt: LongInt; out Elt; pCeltFetched: pLongInt): HRESULT; stdcall;
      function Skip(Celt: Longint): HRESULT; stdcall;
      function Reset: HRESULT; stdcall;
      function Clone(out Enum: IEnumFormatEtc): HRESULT; stdcall;
  end;

  TOLEDropSource = class(TInterfacedObject, IDropSource)
    public
      // IDropSource
      function QueryContinueDrag(fEscapePressed: bool; grfKeyState: LongInt): HRESULT; stdcall;
      function GiveFeedback(dwEffect: LongInt): HRESULT; stdcall;
  end;

  TOLEDataObject = class(TInterfacedObject, IDataObject)
    protected
      FNumFormats  : integer;
      FFormatArray : TOLEFormatArray;
      FMediumArray : TOLEMediumArray;
      FAsync       : boolean;
      FInOperation : boolean;

      function LookupFormatEtc(const aFormatEtc : TFormatEtc) : integer;
      function DupGlobalMem(hMem : HGLOBAL) : HGLOBAL;

    public
      constructor Create(const aFormatArray : TOLEFormatArray; const aMediumArray : TOLEMediumArray; aNumFormats : integer); reintroduce;

      // IDataObject
      function GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium):HRESULT; stdcall;
      function GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium):HRESULT; stdcall;
      function QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
      function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT; stdcall;
      function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: Bool): HRESULT; stdcall;
      function EnumFormatEtc(dwDirection: LongInt; out aEnumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
      function dAdvise(const FormatEtc: TFormatEtc; advf: LongInt; const advsink: IAdviseSink; out dwConnection: LongInt): HRESULT; stdcall;
      function dUnadvise(dwConnection: LongInt): HRESULT; stdcall;
      function EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;
  end;

  TOLEDropTarget = class(TInterfacedObject, IDropTarget)
    protected
      FManager : TOLEDragAndDropMgr;

    public
      constructor Create(const aManager : TOLEDragAndDropMgr); reintroduce;

      // IDropTarget
      function DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
      function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
      function DragLeave: HRESULT; stdcall;
      function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
  end;

implementation

// *****************************************************
// *************** TOLEDragAndDropMgr ******************
// *****************************************************

function TOLEDragAndDropMgr.GetStorageForBytes(var aMedium : TStgMedium; const aData : pointer; aLength : NativeUInt) : boolean;
var
  TempHandle  : HGLOBAL;
  TempPointer : pointer;
begin
  Result := False;

  if (aData <> nil) then
    begin
      TempHandle := GlobalAlloc(GHND, aLength);

      if (TempHandle <> 0) then
        begin
          TempPointer := GlobalLock(TempHandle);

          if (TempPointer <> nil) then
            begin
              Move(aData^, TempPointer^, aLength);

              aMedium.hGlobal       := TempHandle;
              aMedium.tymed         := TYMED_HGLOBAL;
              aMedium.unkForRelease := nil;

              GlobalUnlock(TempHandle);

              Result := True;
            end
           else
            GlobalFree(TempHandle);
        end;
    end;
end;

function TOLEDragAndDropMgr.GetStorageForString(var aMedium : TStgMedium; const aData : WideString) : boolean;
var
  TempPointer : pointer;
begin
  Result := False;

  if (length(aData) > 0) then
    begin
      TempPointer := @aData[1];
      Result      := GetStorageForBytes(aMedium, TempPointer, length(aData) * SizeOf(WideChar));
    end;
end;

function TOLEDragAndDropMgr.GetStorageForString(var aMedium : TStgMedium; const aData : AnsiString) : boolean;
var
  TempPointer : pointer;
begin
  Result := False;

  if (length(aData) > 0) then
    begin
      TempPointer := @aData[1];
      Result      := GetStorageForBytes(aMedium, TempPointer, length(aData) * SizeOf(AnsiChar));
    end;
end;

function TOLEDragAndDropMgr.GetStorageForFileDescriptor(var aMedium : TStgMedium; const aFileName : string) : boolean;
var
  TempHandle     : HGLOBAL;
  TempDescriptor : TFileGroupDescriptor;
  TempPointer    : pointer;
  i, j           : integer;
  TempString     : string;
begin
  Result := False;

  if (length(aFileName) > 0) then
    begin
      TempHandle := GlobalAlloc(GHND, sizeof(TFileGroupDescriptor));

      if (TempHandle <> 0) then
        begin
          TempPointer := GlobalLock(TempHandle);

          if (TempPointer <> nil) then
            begin
              TempDescriptor                := TFileGroupDescriptor(TempPointer^);
              TempDescriptor.cItems         := 1;
              TempDescriptor.fgd[0].dwFlags := FD_LINKUI;

              TempString := aFileName + #0;

              i := 1;
              j := length(TempString);

              while (i <= j) do
                begin
                  TempDescriptor.fgd[0].cFileName[pred(i)] := TempString[i];
                  inc(i);
                end;

              aMedium.tymed         := TYMED_HGLOBAL;
              aMedium.hGlobal       := TempHandle;
              aMedium.unkForRelease := nil;

              GlobalUnlock(TempHandle);

              Result := True;
            end
           else
            GlobalFree(TempHandle);
        end;
    end;
end;

function TOLEDragAndDropMgr.DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
begin
  Result   := S_OK;
  dwEffect := DROPEFFECT_NONE;
end;

function TOLEDragAndDropMgr.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
begin
  Result   := S_OK;
  dwEffect := DROPEFFECT_NONE;
end;

function TOLEDragAndDropMgr.DragLeave: HRESULT;
begin
  Result   := S_OK;
end;

function TOLEDragAndDropMgr.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT;
begin
  Result   := S_OK;
  dwEffect := DROPEFFECT_NONE;
end;


// *****************************************************
// **************** TOLEEnumFormatEtc ******************
// *****************************************************

constructor TOLEEnumFormatEtc.Create;
begin
  inherited Create;

  FIndex      := 0;
  FNumFormats := 0;
end;

constructor TOLEEnumFormatEtc.Create(const aFormatArray : TOLEFormatArray; aNumFormats, aIndex : integer);
begin
  inherited Create;

  FIndex      := aIndex;
  FNumFormats := min(aNumFormats, CUSTOM_ARRAY_LENGTH);

  CopyFromFormatArray(aFormatArray);
end;

destructor TOLEEnumFormatEtc.Destroy;
var
  i : integer;
begin
  i := 0;

  while (i < FNumFormats) do
    begin
      if (FFormatArray[i].ptd <> nil) then CoTaskMemFree(FFormatArray[i].ptd);
      inc(i);
    end;

  inherited Destroy;
end;

procedure TOLEEnumFormatEtc.CopyFromFormatArray(const aSrcFormatArray : TOLEFormatArray);
var
  i : integer;
begin
  i := 0;

  while (i < FNumFormats) do
    begin
      CopyFormatEtc(FFormatArray[i], aSrcFormatArray[i]);
      inc(i);
    end;
end;

procedure TOLEEnumFormatEtc.CopyFormatEtc(var aDstFormatEtc : TFormatEtc; const aSrcFormatEtc : TFormatEtc);
begin
  aDstFormatEtc.cfFormat := aSrcFormatEtc.cfFormat;
  aDstFormatEtc.dwAspect := aSrcFormatEtc.dwAspect;
  aDstFormatEtc.lindex   := aSrcFormatEtc.lindex;
  aDstFormatEtc.tymed    := aSrcFormatEtc.tymed;

  if (aSrcFormatEtc.ptd = nil) then
    aDstFormatEtc.ptd := nil
   else
    begin
      aDstFormatEtc.ptd := CoTaskMemAlloc(SizeOf(TDVTargetDevice));

      aDstFormatEtc.ptd.tdSize             := aSrcFormatEtc.ptd.tdSize;
      aDstFormatEtc.ptd.tdDriverNameOffset := aSrcFormatEtc.ptd.tdDriverNameOffset;
      aDstFormatEtc.ptd.tdDeviceNameOffset := aSrcFormatEtc.ptd.tdDeviceNameOffset;
      aDstFormatEtc.ptd.tdPortNameOffset   := aSrcFormatEtc.ptd.tdPortNameOffset;
      aDstFormatEtc.ptd.tdExtDevmodeOffset := aSrcFormatEtc.ptd.tdExtDevmodeOffset;
      aDstFormatEtc.ptd.tdData             := aSrcFormatEtc.ptd.tdData;
    end;
end;

function TOLEEnumFormatEtc.Next(Celt: LongInt; out Elt; pCeltFetched: pLongInt): HRESULT;
var
  i : integer;
  TempArray : TOLEFormatArray absolute Elt;
begin
  i := 0;

  while (i < Celt) and (FIndex < FNumFormats) do
    begin
      CopyFormatEtc(TempArray[i], FFormatArray[FIndex]);
      inc(i);
      inc(FIndex);
    end;

  if (pCeltFetched <> nil) then pCeltFetched^ := i;

  if (i = Celt) then
    Result := S_OK
   else
    Result := S_FALSE;
end;

function TOLEEnumFormatEtc.Skip(Celt: Longint): HRESULT;
begin
  FIndex := FIndex + Celt;

  if (FIndex <= FNumFormats) then
    Result := S_OK
   else
    Result := S_FALSE;
end;

function TOLEEnumFormatEtc.Reset: HRESULT;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TOLEEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HRESULT;
begin
  Enum   := TOLEEnumFormatEtc.Create(FFormatArray, FNumFormats, FIndex);
  Result := S_OK;
end;


// *****************************************************
// ****************** TOLEDropTarget *******************
// *****************************************************

constructor TOLEDropTarget.Create(const aManager : TOLEDragAndDropMgr);
begin
  inherited Create;

  FManager := aManager;
end;

function TOLEDropTarget.DragEnter(const DataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
begin
  Result := FManager.DragEnter(DataObj, grfKeyState, pt, dwEffect);
end;

function TOLEDropTarget.DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
begin
  Result := FManager.DragOver(grfKeyState, pt, dwEffect);
end;

function TOLEDropTarget.DragLeave: HRESULT; stdcall;
begin
  Result := FManager.DragLeave;
end;

function TOLEDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HRESULT; stdcall;
begin
  Result := FManager.Drop(dataObj, grfKeyState, pt, dwEffect);
end;


// *****************************************************
// ****************** TOLEDropSource *******************
// *****************************************************

function TOLEDropSource.QueryContinueDrag(fEscapePressed: bool; grfKeyState: LongInt): HRESULT; stdcall;
begin
  if fEscapePressed then
    Result := DRAGDROP_S_CANCEL
   else
    if ((grfKeyState and MK_LBUTTON) = 0) then
      Result := DRAGDROP_S_DROP
     else
      Result := S_OK;
end;

function TOLEDropSource.GiveFeedback(dwEffect: LongInt): HRESULT; stdcall;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;


// *****************************************************
// ****************** TOLEDataObject *******************
// *****************************************************

constructor TOLEDataObject.Create(const aFormatArray : TOLEFormatArray;
                                  const aMediumArray : TOLEMediumArray;
                                        aNumFormats  : integer);
var
  i : integer;
begin
  inherited Create;

  FAsync       := False;
  FInOperation := False;
  FNumFormats  := min(aNumFormats, CUSTOM_ARRAY_LENGTH);

  i := 0;

  while (i < FNumFormats) do
    begin
      FFormatArray[i].cfFormat := aFormatArray[i].cfFormat;
      FFormatArray[i].ptd      := aFormatArray[i].ptd;
      FFormatArray[i].dwAspect := aFormatArray[i].dwAspect;
      FFormatArray[i].lindex   := aFormatArray[i].lindex;
      FFormatArray[i].tymed    := aFormatArray[i].tymed;

      FMediumArray[i]          := aMediumArray[i];

      inc(i);
    end;
end;

function TOLEDataObject.LookupFormatEtc(const aFormatEtc : TFormatEtc) : integer;
var
  i : integer;
begin
  Result := -1;
  i      := 0;

  while (i < FNumFormats) do
    if ((FFormatArray[i].tymed and aFormatEtc.tymed) <> 0) and
       (FFormatArray[i].cfFormat = aFormatEtc.cfFormat)    and
       (FFormatArray[i].dwAspect = aFormatEtc.dwAspect)    then
      begin
        Result := i;
        exit;
      end
     else
      inc(i);
end;

function TOLEDataObject.DupGlobalMem(hMem : HGLOBAL) : HGLOBAL;
var
  TempLen    : cardinal;
  TempHandle : HGLOBAL;
  TempSrc    : Pointer;
  TempDst    : Pointer;
begin
  Result  := 0;
  TempSrc := GlobalLock(hMem);

  if (TempSrc <> nil) then
    begin
      TempLen    := GlobalSize(hMem);
      TempHandle := GlobalAlloc(GHND, TempLen);

      if (TempHandle <> 0) then
        begin
          TempDst := GlobalLock(TempHandle);

          if (TempDst <> nil) then
            begin
              Move(TempSrc^, TempDst^, TempLen);
              Result := TempHandle;
              GlobalUnlock(TempHandle);
            end
           else
            GlobalFree(TempHandle);
        end;

      GlobalUnlock(hMem);
    end;
end;

function TOLEDataObject.GetData(const FormatEtcIn: TFormatEtc; out Medium: TStgMedium):HRESULT; stdcall;
var
  i : integer;
begin
  i := LookupFormatEtc(FormatEtcIn);

  if (i < 0) or ((FFormatArray[i].tymed and TYMED_HGLOBAL) = 0) then
    begin
      Medium.tymed         := TYMED_NULL;
      Medium.unkForRelease := nil;
      Medium.hGlobal       := 0;
      Result               := DV_E_FORMATETC;
    end
   else
    begin
      Medium.tymed         := FFormatArray[i].tymed;
      Medium.unkForRelease := nil;
      Medium.hGlobal       := DupGlobalMem(FMediumArray[i].hGlobal);
      Result               := S_OK;
    end;
end;

function TOLEDataObject.GetDataHere(const FormatEtc: TFormatEtc; out Medium: TStgMedium):HRESULT; stdcall;
begin
  Result := DV_E_FORMATETC;
end;

function TOLEDataObject.QueryGetData(const FormatEtc: TFormatEtc): HRESULT; stdcall;
begin
  if (LookupFormatEtc(FormatEtc) < 0) then
    Result := DV_E_FORMATETC
   else
    Result := S_OK;
end;

function TOLEDataObject.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc; out FormatEtcout: TFormatEtc): HRESULT; stdcall;
begin
  FormatEtcout.cfFormat := 0;
  FormatEtcout.dwAspect := DVASPECT_CONTENT;
  FormatEtcout.lindex   := -1;
  FormatEtcout.tymed    := TYMED_NULL;
  FormatEtcout.ptd      := nil;

  Result := E_NOTIMPL;
end;

function TOLEDataObject.SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium; fRelease: Bool): HRESULT; stdcall;
begin
  Result := E_NOTIMPL;
end;

function TOLEDataObject.EnumFormatEtc(dwDirection: LongInt; out aEnumFormatEtc: IEnumFormatEtc): HRESULT; stdcall;
begin
  if (dwDirection = DATADIR_GET) then
    begin
      aEnumFormatEtc := TOLEEnumFormatEtc.Create(FFormatArray, FNumFormats);
      Result         := S_OK;
    end
   else
    begin
      aEnumFormatEtc := nil;

      if (dwDirection = DATADIR_SET) then
        Result := E_NOTIMPL
       else
        Result := E_INVALIDARG;
    end;
end;

function TOLEDataObject.dAdvise(const FormatEtc: TFormatEtc;
                                      advf: LongInt;
                                const advsink: IAdviseSink;
                                out   dwConnection: LongInt): HRESULT; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TOLEDataObject.dUnadvise(dwConnection: LongInt): HRESULT; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

function TOLEDataObject.EnumdAdvise(out EnumAdvise: IEnumStatData): HRESULT; stdcall;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;

end.
