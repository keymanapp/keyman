//**************************************************************
//                                                             *
//                        MenuContext                          *                                                      *
//                       Freeware unit                         *
//         Eran Bodankin - (bsalsa) - bsalsa@gmail.com        *
//                                                             *
//                  based on Code ideas by                     *
//            Brad Stowers - bstowers@pobox.com                *
//                                                             *
//  Updated versions:                                          *
//               http://www.bsalsa.com                         *
//**************************************************************

{*******************************************************************************}
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@gmail.com) any code change in the unit
   for the benefit of the other users.
{*******************************************************************************}
//$Id: MenuContext.pas,v 1.2 2006/12/05 11:56:31 bsalsa Exp $

unit MenuContext;

interface

uses
  Windows, Classes, ShlObj, ActiveX;

type
  PPIDLArray = ^TPIDLArray;
  TPIDLArray = array[0..0] of PItemIDList;
  TInterfaceCommand = (icContextMenu, icProperties, icDefaultAction, icVerb);

function DisplayContextMenu(const Filename: string; Parent: Hwnd; Pos: TPoint): boolean; overload;
function DisplayContextMenu(const Directory: string; Items: TStringList; Parent: Hwnd;
  Pos: TPoint): boolean; overload;
function DisplayContextMenu(AParent: IShellFolder; var APIDL: PItemIDList; Attr: ULONG;
  Parent: Hwnd; Pos: TPoint; PidlCount: integer): boolean; overload;
function DisplayPropertiesDialog(const Filename: string; Parent: Hwnd): boolean; overload;
function DisplayPropertiesDialog(const Directory: string; Items: TStringList;
  Parent: Hwnd): boolean; overload;
function DisplayPropertiesDialog(AParent: IShellFolder; var APIDL: PItemIDList;
  Attr: ULONG; Parent: Hwnd; PidlCount: integer): boolean; overload;
function PerformDefaultAction(const Filename: string; Parent: Hwnd): boolean; overload;
function PerformDefaultAction(const Directory: string; Items: TStringList;
  Parent: Hwnd): boolean; overload;
function PerformDefaultAction(AParent: IShellFolder; var APIDL: PItemIDList;
  Attr: ULONG; Parent: Hwnd; PidlCount: integer): boolean; overload;
function PerformVerb(const Verb, Filename: string; Parent: Hwnd): boolean; overload;
function PerformVerb(const Verb, Directory: string; Items: TStringList;
  Parent: Hwnd): boolean; overload;
function PerformVerb(const Verb: string; AParent: IShellFolder; var APIDL: PItemIDList;
  Attr: ULONG; Parent: Hwnd; PidlCount: integer): boolean; overload;
function GetPIDLAndShellFolder(Path: string; out Folder: IShellFolder;
  var PIDL: PItemIDList; ShellMalloc: IMalloc; Parent: Hwnd): boolean;

implementation

uses
  SysUtils, Forms, Controls;

var
  IsCM3: Boolean;

function MenuCallbackProc(Wnd: HWND; Msg: UINT; wParam: WPARAM; lParam: LPARAM): LResult; stdcall; export;
const
  WM_CREATE = $0001;
  WM_DRAWITEM = $002B;
  WM_MEASUREITEM = $002C;
  WM_INITMENUPOPUP = $0117;
var
  CM2: IContextMenu2;
  CM3: IContextMenu3;
begin
  case Msg of
    WM_CREATE:
      begin
        if IsCM3 then
        begin
          CM3 := IContextMenu3(PCreateStruct(lParam).lpCreateParams);
          SetWindowLong(Wnd, GWL_USERDATA, LongInt(CM3));
        end
        else
        begin
          CM2 := IContextMenu2(PCreateStruct(lParam).lpCreateParams);
          SetWindowLong(Wnd, GWL_USERDATA, LongInt(CM2));
        end;
        Result := DefWindowProc(Wnd, Msg, wParam, lParam);
      end;
    WM_DRAWITEM,
      WM_MEASUREITEM,
      WM_INITMENUPOPUP:
      begin
        if IsCM3 then
        begin
          CM3 := IContextMenu3(GetWindowLong(Wnd, GWL_USERDATA));
          Assert(CM3 <> nil, 'nil Context Menu!');
          CM3.HandleMenuMsg2(Msg, wParam, lParam, Result);
        end
        else
        begin
          CM2 := IContextMenu2(GetWindowLong(Wnd, GWL_USERDATA));
          Assert(CM2 <> nil, 'Nil Context Menu!');
          CM2.HandleMenuMsg(Msg, wParam, lParam);
        end;
        if Msg = WM_INITMENUPOPUP then
          Result := 0
        else
          Result := 1;
      end;
  else
    Result := DefWindowProc(Wnd, Msg, wParam, lParam);
  end;
end;

function HasWildcards(const s: string): boolean;
begin
  Result := (StrScan(PChar(s), '*') <> nil) or (StrScan(PChar(s), '?') <> nil);
end;

function InvokeInterfaceElement(Filename: string; AFolder: IShellFolder;
  var APIDL: PItemIDList; AnAttr: ULONG; Cmd: TInterfaceCommand;
  const Verb: string; Parent: HWND; Pos: TPoint; PidlCount: integer): boolean;

  function HandleContextMenu(const CtxMenu: IContextMenu; Attr: ULONG; HaveCM2: boolean): boolean;
  var
    Popup: HMenu;
    ICI: TCMInvokeCommandInfo;
    MenuCmd: Cardinal;
    CallbackWnd: HWnd;
    Temp: HWnd;
    AWndClass: TWndClass;
  begin
    Result := False;
    CallbackWnd := 0;
    FillChar(ICI, SizeOf(TCMInvokeCommandInfo), #0);
    with ICI do
    begin
      cbSize := SizeOf(TCMInvokeCommandInfo);
      hWnd := Parent;
      nShow := SW_SHOWNORMAL;
    end;
    case Cmd of
      icContextMenu:
        begin
          Popup := CreatePopupMenu;
          try
            if Succeeded(CtxMenu.QueryContextMenu(Popup, 0, 1, $7FFF, CMF_EXPLORE)) then
            begin
              if HaveCM2 then
              begin
                FillChar(AWndClass, SizeOf(AWndClass), #0);
                with AWndClass do
                begin
                  lpszClassName := 'ItemPropMenuCallbackHelper';
                  Style := CS_PARENTDC;
                  lpfnWndProc := @MenuCallbackProc;
                  hInstance := HInstance;
                end;
                Windows.RegisterClass(AWndClass);
                CallbackWnd := CreateWindow('ItemPropMenuCallbackHelper',
                  'ItemPropCallbackProcessor', WS_POPUPWINDOW, 0, 0, 0, 0, 0,
                  0, HInstance, Pointer(CtxMenu));
              end;
              Result := TRUE;
              if CallbackWnd = 0 then
                Temp := Parent
              else
                Temp := CallbackWnd;
              MenuCmd := Cardinal(TrackPopupMenuEx(Popup, TPM_LEFTALIGN or
                TPM_RETURNCMD or TPM_RIGHTBUTTON, Pos.x, Pos.y, Temp, nil));
              if MenuCmd <> 0 then
              begin
                ICI.lpVerb := PAnsiChar(MakeIntResource(MenuCmd - 1));
                Result := Succeeded(CtxMenu.InvokeCommand(ICI));
              end;
            end;
          finally
            DestroyMenu(Popup);
            if CallbackWnd <> 0 then
              DestroyWindow(CallbackWnd);
          end;
        end;
      icVerb:
        begin
          ICI.lpVerb := PAnsiChar(AnsiString(Verb));
          Result := Succeeded(CtxMenu.InvokeCommand(ICI));
        end;
      icProperties:
        begin
          if (Attr and SFGAO_HASPROPSHEET) <> 0 then
          begin
            ICI.lpVerb := 'properties';
            Result := Succeeded(CtxMenu.InvokeCommand(ICI));
          end;
        end;
      icDefaultAction:
        begin
          Popup := CreatePopupMenu;
          if Succeeded(CtxMenu.QueryContextMenu(Popup, 0, 1, $7FFF,
            CMF_DEFAULTONLY)) then
          begin
            MenuCmd := GetMenuDefaultItem(Popup, 0, 0);
            if MenuCmd <> $FFFFFFFF then
            begin
              ICI.lpVerb := PAnsiChar(MakeIntResource(MenuCmd - 1));
              Result := Succeeded(CtxMenu.InvokeCommand(ICI));
            end;
          end;
        end;
    end;
  end;

  function HandleFromPIDLs(Parent: HWND; SubFolder: IShellFolder; var ItemID: PItemIDList;
    Attr: ULONG; PidlCount: integer): boolean;
  const
    IID_IContextMenu3: TGUID = (D1: $BCFCE0A0; D2: $EC17; D3: $11D0; D4: ($8D, $10, $00, $A0, $C9, $0F, $27, $19));
  var
    HaveCM2: boolean;
    ContextMenu: IContextMenu;
    ContextMenu2: IContextMenu2;
    ContextMenu3: IContextMenu3;
  begin
    Result := False;
    HaveCM2 := False;
    if Succeeded(SubFolder.GetUIObjectOf(Parent, PidlCount, ItemID,
      IID_IContextMenu, nil, pointer(ContextMenu))) then
    begin
      if Succeeded(ContextMenu.QueryInterface(IID_IContextMenu2,
        pointer(ContextMenu2))) then
      begin
        ContextMenu := ContextMenu2;
        HaveCM2 := TRUE;
        if Succeeded(ContextMenu.QueryInterface(IID_IContextMenu3,
          pointer(ContextMenu3))) then
        begin
          ContextMenu := ContextMenu3;
          IsCM3 := TRUE;
        end;
      end;
      try
        Result := HandleContextMenu(ContextMenu, Attr, HaveCM2);
      finally
      end;
    end;
  end;
var
  ShellMalloc: IMalloc;
  SubFolder: IShellFolder;
  ShellFolder: IShellFolder;
  FolderID: pItemIDList;
  ItemID: pItemIDList;
  Eaten, Attr: ULONG;
  oleWild: widestring;
  oleAll: widestring;
  oleSubDir: widestring;
  oleFilename: widestring;
  OldCursor: TCursor;
  JustName: string;
  EnumList: IEnumIDList;
  CompID: pItemIDList;
  CompFolder: IShellFolder;
  Fetched: ULONG;
  SR: TSearchRec;
  WildFiles: TStringList;
  WildPIDLs: PPIDLArray;
  Count: Integer;
  x: Integer;
begin
  IsCM3 := False;
  Result := False;
  OldCursor := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    if (APIDL <> nil) then
    begin
      Result := HandleFromPIDLs(Parent, AFolder, APIDL, AnAttr, PidlCount);
    end
    else
    begin
      SHGetMalloc(ShellMalloc);
      try
        JustName := ExtractFileName(FileName);
        oleSubDir := ExtractFilePath(Filename);
        oleFilename := JustName;
        if Succeeded(SHGetDesktopFolder(ShellFolder)) and
          Succeeded(ShellFolder.ParseDisplayName(Parent, nil,
          PWideChar(oleSubDir), Eaten, FolderID, Attr)) then
          if Succeeded(ShellFolder.BindToObject(FolderID, nil,
            IID_IShellFolder, pointer(SubFolder))) then
            if HasWildcards(JustName) then
            begin
              WildFiles := TStringList.Create;
              try
                if FindFirst(Filename, faAnyFile, SR) = 0 then
                begin
                  WildFiles.Add(SR.Name);
                  while FindNext(SR) = 0 do
                    WildFiles.Add(SR.Name);
                  FindClose(SR);
                end;
                if WildFiles.Count > 0 then
                begin
                  Count := 0;
                  GetMem(WildPIDLs, SizeOf(PItemIDList) * WildFiles.Count);
                  try
                    for x := 0 to WildFiles.Count - 1 do
                    begin
                      oleWild := StringToOLEStr(WildFiles[x]);
                      if Succeeded(SubFolder.ParseDisplayName(Parent,
                        nil, PWideChar(oleWild), Eaten,
                        WildPIDLs^[Count], Attr)) then
                        Inc(Count);
                    end;
                    Result := HandleFromPIDLS(Parent, SubFolder,
                      WildPIDLs^[0], Attr, Count);
                  finally
                    for x := 0 to Count - 1 do
                      ShellMalloc.Free(WildPIDLs^[x]);
                    FreeMem(WildPIDLs);
                  end;
                end;
              finally
                WildFiles.Free;
              end;
            end
            else
              if Succeeded(SubFolder.ParseDisplayName(Parent, nil,
                PWideChar(oleFilename), Eaten, ItemID, Attr)) then
              begin
                try
                  Result := HandleFromPIDLS(Parent, SubFolder, ItemID, Attr, 1);
                finally
                  ShellMalloc.Free(ItemID);
                end;
              end
              else
              begin
                oleAll := Filename;
                if (Succeeded(ShellFolder.EnumObjects(Parent, SHCONTF_FOLDERS, EnumList))) and
                  (EnumList.Next(1, CompID, Fetched) = S_OK) then
                begin
                  if Succeeded(ShellFolder.BindToObject(CompID, nil,
                    IID_IShellFolder, pointer(CompFolder))) and
                    Succeeded(CompFolder.ParseDisplayName(Parent, nil,
                    PWideChar(oleAll), Eaten, ItemID, Attr)) then
                  try
                    Result := HandleFromPIDLS(Parent, CompFolder, ItemID,
                      Attr, 1);
                  finally
                    ShellMalloc.Free(ItemID);
                  end;
                end;

              end;
      finally
        ShellMalloc.Free(FolderID);
        ShellMalloc._Release;
      end;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

function InvokeListInterfaceElement(const Directory: string; Items: TStringList;
  Parent: HWND; Pos: TPoint; Cmd: TInterfaceCommand; const Verb: string): boolean;
var
  ShellMalloc: IMalloc;
  SubFolder, ShellFolder: IShellFolder;
  FolderID: PItemIDList;
  Eaten, Attr: ULONG;
  oleSubDir, oleFilename: widestring;
  ItemPIDLs: PPIDLArray;
  Count, x: Integer;
begin
  Result := False;
  if (Items = nil) or (Items.Count < 1) then Exit;
  Count := 0;
  SHGetMalloc(ShellMalloc);
  if Succeeded(SHGetDesktopFolder(ShellFolder)) then
    oleSubDir := Directory;
  if Succeeded(ShellFolder.ParseDisplayName(Parent, nil,
    PWideChar(oleSubDir), Eaten, FolderID, Attr)) and
    Succeeded(ShellFolder.BindToObject(FolderID, nil, IID_IShellFolder,
    pointer(SubFolder))) then
    Count := 0;
  GetMem(ItemPIDLs, SizeOf(PItemIDList) * Items.Count);
  try
    for x := 0 to Items.Count - 1 do
    begin
      oleFilename := Items[x];
      if Succeeded(SubFolder.ParseDisplayName(Parent, nil, PWideChar(oleFilename), Eaten, ItemPIDLs^[Count], Attr)) then
        Inc(Count);
    end;
    Result := InvokeInterfaceElement('', SubFolder, ItemPIDLs^[0], Attr,
      Cmd, Verb, Parent, Pos, Count);
  finally
    for x := 0 to Count - 1 do
      ShellMalloc.Free(ItemPIDLs^[x]);
    FreeMem(ItemPIDLs);
  end;
  ShellMalloc.Free(FolderID);
  ShellMalloc._Release;
end;

function NextPIDL(PIDL: PItemIDList): PItemIDList;
begin
  if PIDL.mkid.cb > 0 then
    Result := PItemIDList(Longint(PIDL) + PIDL.mkid.cb)
  else
    Result := nil;
end;

function CopyPidl(APidl: PItemIDList; ShellMalloc: IMalloc): PItemIDList;
var
  CB: UINT;
begin
  Result := nil;
  CB := APidl.mkid.cb + SizeOf(APidl.mkid.cb);
  if NextPidl(APidl)^.mkid.cb <> 0 then
    Exit;
  Result := ShellMalloc.Alloc(CB);
  if Result <> nil then
  begin
    FillChar(Result^, CB, #0);
    Move(APidl^, Result^, APidl.mkid.cb);
  end;
end;

function GetPIDLAndShellFolder(Path: string; out Folder: IShellFolder;
  var PIDL: PItemIDList; ShellMalloc: IMalloc; Parent: HWND): boolean;
var
  DesktopFolder: IShellFolder;
  WidePath: WideString;
  Eaten: ULONG;
  pidlNext, pidlLast, pidlFull: PItemIDList;
  Attrs: ULONG;
  CurFolder, NextFolder: IShellFolder;
  SaveCB: UINT;
begin
  Result := False;
  pidlFull := nil;
  if Succeeded(SHGetDesktopFolder(DesktopFolder)) then
  begin
    WidePath := Path;
    if Succeeded(DesktopFolder.ParseDisplayName(Parent, nil, PWideChar(WidePath), Eaten,
      pidlFull, Attrs)) then
    begin
      if Succeeded(DesktopFolder.QueryInterface(IID_IShellFolder, CurFolder)) then
      begin
        pidlNext := NextPidl(pidlFull);
        pidlLast := pidlFull;
        while pidlNext^.mkid.cb <> 0 do
        begin
          Result := TRUE;
          SaveCB := pidlNext^.mkid.cb;
          pidlNext^.mkid.cb := 0;
          if not Succeeded(CurFolder.BindToObject(pidlLast, nil, IID_IShellFolder,
            pointer(NextFolder))) then
          begin
            Result := False;
            Break;
          end;
          pidlNext^.mkid.cb := SaveCB;
          CurFolder := NextFolder;
          pidlLast := pidlNext;
          pidlNext := NextPidl(pidlNext);
        end;
        PIDL := CopyPidl(pidlLast, ShellMalloc);
        Folder := CurFolder;
      end;
    end;
  end;
  if pidlFull <> nil then
    ShellMalloc.Free(pidlFull);
end;

function DisplayContextMenu(const Filename: string; Parent: Hwnd; Pos: TPoint): boolean;
var
  Dummy: PItemIDList;
begin
  Dummy := nil;
  Result := InvokeInterfaceElement(Filename, nil, Dummy, 0, icContextMenu, '', HWND(Parent), Pos, 1);
end;

function DisplayContextMenu(const Directory: string; Items: TStringList;
  Parent: Hwnd; Pos: TPoint): boolean; overload;
begin
  Result := InvokeListInterfaceElement(Directory, Items, HWND(Parent), Pos, icContextMenu, '');
end;

function DisplayContextMenu(AParent: IShellFolder; var APIDL: PItemIDList;
  Attr: ULONG; Parent: Hwnd; Pos: TPoint; PidlCount: integer): boolean;
begin
  Result := InvokeInterfaceElement('', AParent, APIDL, Attr, icContextMenu, '',
    HWND(Parent), Pos, PidlCount);
end;

function DisplayPropertiesDialog(const Filename: string; Parent: Hwnd): boolean;
var
  Dummy: PItemIDList;
begin
  Dummy := nil;
  Result := InvokeInterfaceElement(Filename, nil, Dummy, 0, icProperties, '',
    HWND(Parent), Point(0, 0), 1);
end;

function DisplayPropertiesDialog(const Directory: string; Items: TStringList;
  Parent: Hwnd): boolean; overload;
begin
  Result := InvokeListInterfaceElement(Directory, Items, HWND(Parent), Point(0, 0), icProperties, '');
end;

function DisplayPropertiesDialog(AParent: IShellFolder; var APIDL: PItemIDList;
  Attr: ULONG; Parent: Hwnd; PidlCount: integer): boolean;
begin
  Result := InvokeInterfaceElement('', AParent, APIDL, Attr, icProperties, '',
    HWND(Parent), Point(0, 0), PidlCount);
end;

function PerformDefaultAction(const Filename: string; Parent: Hwnd): boolean;
var
  Dummy: PItemIDList;
begin
  Dummy := nil;
  Result := InvokeInterfaceElement(Filename, nil, Dummy, 0, icDefaultAction, '',
    HWND(Parent), Point(0, 0), 1);
end;

function PerformDefaultAction(const Directory: string; Items: TStringList;
  Parent: Hwnd): boolean; overload;
begin
  Result := InvokeListInterfaceElement(Directory, Items, HWND(Parent),
    Point(0, 0), icDefaultAction, '');
end;

function PerformDefaultAction(AParent: IShellFolder; var APIDL: PItemIDList;
  Attr: ULONG; Parent: Hwnd; PidlCount: integer): boolean;
begin
  Result := InvokeInterfaceElement('', AParent, APIDL, Attr, icDefaultAction,
    '', HWND(Parent), Point(0, 0), PidlCount);
end;

function PerformVerb(const Verb, Filename: string; Parent: Hwnd): boolean;
var
  Dummy: PItemIDList;
begin
  Dummy := nil;
  Result := InvokeInterfaceElement(Filename, nil, Dummy, 0, icVerb, Verb,
    HWND(Parent), Point(0, 0), 1);
end;

function PerformVerb(const Verb, Directory: string; Items: TStringList; Parent: Hwnd): boolean;
begin
  Result := InvokeListInterfaceElement(Directory, Items, HWND(Parent),
    Point(0, 0), icVerb, Verb);
end;

function PerformVerb(const Verb: string; AParent: IShellFolder; var APIDL: PItemIDList;
  Attr: ULONG; Parent: Hwnd; PidlCount: integer): boolean;
begin
  Result := InvokeInterfaceElement('', AParent, APIDL, Attr, icVerb, Verb,
    HWND(Parent), Point(0, 0), PidlCount);
end;

initialization
  OLEInitialize(nil);
finalization
  OLEUninitialize;
end.
