//**************************************************************
//                                                             *
//                      THistoryListView                       *                                                      *
//                     Freeware Component                      *
//                     For Delphi 5 to XE                      *
//                            by                               *
//                     Per Lindsø Larsen                       *
//                     and Eran Bodankin                       *
//                                                             *
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
4. Please consider donation in our web site!
{*******************************************************************************}
//$Id: HistoryListView.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $

unit HistoryListView;

interface

{$I EWB.inc}

uses
  Windows, SysUtils, Classes, Forms, Registry, ComCtrls, ShlObj,
  SHDocVw_EWB, EmbeddedWB;

type

  PItem = ^TItem;
  TItem = record
    FullID, ID: PItemIDList;
    Empty: Boolean;
    DisplayName: string;
    ImageIndex: Integer;
  end;

  TOnUrlSelectedEvent = procedure(Sender: TObject; Url: string) of object;
  TCustomHistoryListView = class(TCustomListView)
  private
    { Private declarations }
    fEmbeddedWB: TEmbeddedWB;
    fPIDL: PItemIDList;
    List: TList;
    Desktop: IShellFolder;
    Level: Integer;
    fOnUrlSelected: TOnUrlSelectedEvent;
    procedure ClearIDList;
    function ShellItem(Index: Integer): PItem;
  protected
    { Protected declarations }
    procedure SetPath(ID: PItemIDList);
    function CustomDrawSubItem(Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; Stage: TCustomDrawStage): Boolean; override;
    function OwnerDataFetch(Item: TListItem; Request: TItemRequest): Boolean;
      override;
    function OwnerDataFind(Find: TItemFind; const FindString: string;
      const FindPosition: TPoint; FindData: Pointer; StartIndex: Integer;
      Direction: TSearchDirection; Wrap: Boolean): Integer; override;
    function OwnerDataHint(StartIndex, EndIndex: Integer): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DblClick; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure LevelUp;
    { Public declarations }
  published
    { Published declarations }
    property EmbeddedWB: TEmbeddedWB read fEmbeddedWB write fEmbeddedWB;
    property OnURLSelected: TOnUrlSelectedEvent read fOnUrlSelected write
      fOnUrlSelected;
  end;

  THistoryListView = class(TCustomHistoryListView)
  published
    property Align;
    property AllocBy;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Ctl3D;
    property Enabled;
    property Font;
    property FlatScrollBars;
    property HideSelection;
    property HotTrack;
    property HotTrackStyles;
    property HoverTime;
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowColumnHeaders;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnColumnClick;
    property OnColumnDragged;
    property OnColumnRightClick;
    property OnCompare;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnCustomDrawSubItem;
    property OnData;
    property OnDataFind;
    property OnDataHint;
    property OnDataStateChange;
    property OnDblClick;
    property OnDeletion;
    property OnDrawItem;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetImageIndex;
    property OnGetSubItemImage;
    property OnDragDrop;
    property OnDragOver;
    property OnInfoTip;
    property OnInsert;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnSelectItem;
    property OnStartDock;
    property OnStartDrag;
  end;

var
  Folder: IShellFolder;

implementation

uses ShellApi, ActiveX, ComObj, CommCtrl, EwbTools;

function ListSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := SmallInt(Folder.CompareIDs(0, PItem(Item1).ID, PItem(Item2).ID));
end;

procedure TCustomHistoryListView.SetPath(ID: PItemIDList);
var
  PID: PItemIDList;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  ShellItem: PItem;
  NewShellFolder: IShellFolder;
begin
  OLECheck(Desktop.BindToObject(ID, nil, IID_IShellFolder,
    Pointer(NewShellFolder)));
  Items.BeginUpdate;
  try
    OleCheck(NewShellFolder.EnumObjects(Application.Handle,
      SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN,
      EnumList));
    Folder := NewShellFolder;
    ClearIDList;
    while EnumList.Next(1, PID, NumIDs) = S_OK do
    begin
      ShellItem := New(PItem);
      ShellItem.ID := PID;
      ShellItem.DisplayName := GetDisplayName(Folder, PID);
      ShellItem.Empty := True;
      List.Add(ShellItem);
    end;
    List.Sort(ListSortFunc);
    Items.Count := List.Count;
    Repaint;
    fPIDL := ID;
    if Items.Count > 0 then
    begin
      Selected := Items[0];
      Selected.Focused := True;
      Selected.MakeVisible(False);
    end;
  finally
    Items.EndUpdate;
  end;
end;

function TCustomHistoryListView.ShellItem(Index: Integer): PItem;
begin
  Result := PItem(List[Index]);
end;

procedure TCustomHistoryListView.ClearIDList;
var
  I: Integer;
begin
  for I := 0 to List.Count - 1 do
  begin
    DisposePIDL(ShellItem(I).ID);
    Dispose(ShellItem(I));
  end;
  List.Clear;
end;

{ TCustomHistoryListView }

function TCustomHistoryListView.CustomDrawSubItem(Item: TListItem; SubItem:
  Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
begin
  if (Stage = cdPrePaint) and (SubItem <> 0) then
    Canvas.Font.Color := GetSysColor(COLOR_WINDOWTEXT);
  Result := inherited CustomDrawSubItem(Item, SubItem, State, Stage);
end;

procedure TCustomHistoryListView.DblClick;
var
  X: Olevariant;
  Url: string;
  RootPIDL, ID: PItemIDList;
begin
  inherited;
  if Selected <> nil then
  begin
    ID := ShellItem(Selected.Index).ID;
    if not IsFolder(Folder, ID) then
    begin
      Url := ExtractUrl(Folder, ID);
      if Assigned(fOnUrlSelected) then
        fOnUrlSelected(self, Url)
      else if Assigned(fEmbeddedWB) then
        fEmbeddedWB.Navigate(Url, X, X, X, X);
    end
    else
    begin
      RootPIDL := ConcatPIDLs(fPIDL, ID);
      SetPath(RootPIDL);
      Inc(Level);
    end;
  end;
end;

destructor TCustomHistoryListView.Destroy;
begin
  if List <> nil then
  begin
    ClearIDList;
    List.Free;
  end;
  inherited;
end;

procedure TCustomHistoryListView.LevelUp;
var
  Temp: PItemIDList;
begin
  Temp := CopyPIDL(fPIDL);
  if Assigned(Temp) then
    StripLastID(Temp);
  if (Temp.mkid.cb <> 0) and (Level > 0) then
  begin
    Dec(Level);
    SetPath(Temp)
  end
  else
    Beep;
end;

procedure TCustomHistoryListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_RETURN:
      DblClick;
    VK_BACK:
      LevelUp;
  end;
end;

procedure TCustomHistoryListView.Loaded;
var
  FileInfo: TSHFileInfo;
  ImageListHandle: THandle;
  szPath: array[0..MAX_PATH] of char;
  NewPIDL: PItemIDList;
begin
  inherited;
  OLECheck(SHGetSpecialFolderLocation(Application.Handle, CSIDL_HISTORY,
    NewPIDL));
  ShGetPathFromIDList(NewPIDL, szPath);
  OLECheck(SHGetDesktopFolder(Desktop));
  Folder := Desktop;
  if List = nil then
  begin
    List := TList.Create;
    List.Clear;
  end;
  ImageListHandle := SHGetFileInfo(szPath, 0, FileInfo, SizeOf(FileInfo),
    SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListHandle);
  SetPath(NewPIDL);
  Level := 0;
  //  Column[0].Width := width - 4;
end;

constructor TCustomHistoryListView.Create(AOwner: TComponent);
var
  HistoryColumn: TListColumn;
begin
  inherited;
  ReadOnly := True;
  Height := 300;
  Width := 200;
  ShowHint := True;
  ColumnClick := False;
  OwnerData := True;
  ViewStyle := vsReport;
  HistoryColumn := Columns.Add;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('CLSID\{FF393560-C2A7-11CF-BFF4-444553540000}', FALSE);
    HistoryColumn.Caption := ReadString('');
    Closekey;
    Free;
  end;
  HistoryColumn.Width := width - 4;
end;

function TCustomHistoryListView.OwnerDataFetch(Item: TListItem;
  Request: TItemRequest): Boolean;
begin
  inherited OwnerDataFetch(Item, Request);
  Result := True;
  if (Item.Index <= List.Count) then
    with ShellItem(Item.Index)^ do
    begin
      Item.Caption := DisplayName;
      Item.ImageIndex := ImageIndex;
    end;
end;

function TCustomHistoryListView.OwnerDataFind(Find: TItemFind;
  const FindString: string; const FindPosition: TPoint; FindData: Pointer;
  StartIndex: Integer; Direction: TSearchDirection;
  Wrap: Boolean): Integer;
var
  I: Integer;
  Found: Boolean;
begin
  Result := inherited OwnerDataFind(Find, FindString, FindPosition, FindData,
    StartIndex, Direction, Wrap);
  I := StartIndex;
  if (Find = ifExactString) or (Find = ifPartialString) then
  begin
    repeat
      if (I = List.Count - 1) then
        if Wrap then
          I := 0
        else
          Exit;
      Found := Pos(UpperCase(FindString), UpperCase(ShellItem(I)^.DisplayName))
        = 1;
      Inc(I);
    until Found or (I = StartIndex);
    if Found then
      Result := I - 1;
  end;
end;

function TCustomHistoryListView.OwnerDataHint(StartIndex,
  EndIndex: Integer): Boolean;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
  I: Integer;
begin
  Result := inherited OwnerDataHint(StartIndex, EndIndex);
  if (StartIndex > List.Count) or (EndIndex > List.Count) then
    Exit;
  for I := StartIndex to EndIndex do
  begin
    if ShellItem(I)^.Empty then
      with ShellItem(I)^ do
      begin
        FullID := ConcatPIDLs(fPIDL, ID);
        FillChar(FileInfo, SizeOf(FileInfo), #0);
        Flags := SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or
          SHGFI_SMALLICON;
        SHGetFileInfo(PChar(FullID), 0, FileInfo, SizeOf(FileInfo), Flags);
        ImageIndex := FileInfo.iIcon;
        Empty := False;
      end;
  end;
  Result := True;
end;

end.
