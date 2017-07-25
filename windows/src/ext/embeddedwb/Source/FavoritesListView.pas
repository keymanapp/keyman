//******************************************************************
//                                                                 *
//                     TFavoritesListView                          *                                                      *
//                     Freeware Component                          *
//                     For Delphi 5 to XE                          *
//                            by                                   *
//                     Per Lindsø Larsen                           *
//                     and Eran Bodankin                           *
//                                                                 *
//  Updated versions:                                              *
//               http://www.bsalsa.com                             *
//******************************************************************

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

unit FavoritesListView;

interface

{$I EWB.inc}

uses
  Windows, SysUtils, Classes, Controls, Forms, Registry, ComCtrls, ShlObj,
  Messages, MenuContext, ShellApi, ActiveX, ComObj, CommCtrl, EmbeddedWB;

type
  IShellCommandVerb = interface
    ['{7D2A7245-2376-4D33-8008-A130935A2E8B}']
    procedure ExecuteCommand(Verb: string; var Handled: boolean);
    procedure CommandCompleted(Verb: string; Succeeded: boolean);
  end;

type
  PItem = ^TItem;
  TItem = record
    FullID, ID: PItemIDList;
    Empty: Boolean;
    DisplayName: string;
    ImageIndex: Integer;
  end;

  TResolveUrl = (IntShCut, IniFile);
  TOnUrlSelectedEvent = procedure(Sender: TObject; Url: string) of object;
  TPopupMenuMode = (pmm_System, pmm_PopupMenu);
  TCustomFavoritesListView = class(TCustomListView, IShellCommandVerb)
  private
    { Private declarations }
    FEmbeddedWB: TEmbeddedWB;
    FPIDL: PItemIDList;
    Desktop: IShellFolder;
    List: TList;
    Level: Integer;
    FResolveUrl: TResolveUrl;
    FChannels: Boolean;
    FavoritesPIDL: PItemIDList;
    FOnUrlSelected: TOnUrlSelectedEvent;
    FSavePath: string;
    FPopupMenuMode: TPopupMenuMode;
    procedure EditText;
    procedure ClearIDList;

  protected
    { Protected declarations }
    procedure SetPath(ID: PItemIDList);
    function ShellItem(Index: Integer): PItem;
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
    procedure PopupSystemContextMenu(Item: TListItem; Point: TPoint);
    procedure WMContextMenu(var Message: TMessage); message WM_Contextmenu;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure LevelUp;
    procedure RefreshList;
    //IShellCommandVerb
    procedure CommandCompleted(Verb: string; Succeeded: Boolean);
    procedure ExecuteCommand(Verb: string; var Handled: Boolean);
    //  property Folders[Index: Integer]: TShellFolder read GetFolder; default;
     { Public declarations }
  published
    { Published declarations }
    property ResolveUrl: TResolveUrl read FResolveUrl write FResolveUrl;
    property Channels: Boolean read FChannels write FChannels;
    property OnURLSelected: TOnURLSelectedEvent read FOnURLSelected write
      FOnURLSelected;
    property EmbeddedWB: TEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property PopupMenuMode: TPopupMenuMode read FPopupMenuMode write
      FPopupMenuMode;
  end;

  TFavoritesListView = class(TCustomFavoritesListView)
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
    property Items;
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
  ChannelShortcut, InternetShortcut: string;

implementation

uses
  EwbTools;

procedure TCustomFavoritesListView.RefreshList;
begin
  SetPath(FPIDL);
end;

function ListSortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := SmallInt(Folder.CompareIDs(0, PItem(Item1).ID, PItem(Item2).ID));
end;

procedure TCustomFavoritesListView.SetPath(ID: PItemIDList);
var
  PID: PItemIDList;
  EnumList: IEnumIDList;
  NumIDs: LongWord;
  Item: PItem;
  NewShellFolder: IShellFolder;
begin
  OLECheck(Desktop.BindToObject(ID, nil, IID_IShellFolder,
    Pointer(NewShellFolder)));
  Items.BeginUpdate;
  try
    OleCheck(NewShellFolder.EnumObjects(Application.Handle,
      SHCONTF_FOLDERS or SHCONTF_NONFOLDERS, EnumList));
    Folder := NewShellFolder;
    ClearIDList;
    while EnumList.Next(1, PID, NumIDs) = S_OK do
    begin
      if not Channels and IsChannel(ChannelShortcut, Folder, PID) then
        Continue;
      Item := New(PItem);
      Item.ID := PID;
      Item.DisplayName := GetDisplayName(Folder, PID);
      Item.Empty := True;
      List.Add(Item);
    end;
    List.Sort(ListSortFunc);
    Items.Count := List.Count;
    Repaint;
    FPIDL := ID;
    if Items.Count > 0 then
    begin
      Selected := Items[0];
      Selected.Focused := True;
      Selected.MakeVisible(False);
      if (Pos('Links', Caption) > 0) or (pos('Imported', Text) > 0) then
        Selected.ImageIndex := -1
    end;
  finally
    Items.EndUpdate;
  end;
end;

function TCustomFavoritesListView.ShellItem(Index: Integer): PItem;
begin
  Result := PItem(List[Index]);
end;

procedure TCustomFavoritesListView.ClearIDList;
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

{ TCustomFavoritesListView }

function TCustomFavoritesListView.CustomDrawSubItem(Item: TListItem; SubItem:
  Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
begin
  if (Stage = cdPrePaint) and (SubItem <> 0) then
    Canvas.Font.Color := GetSysColor(COLOR_WINDOWTEXT);
  Result := inherited CustomDrawSubItem(Item, SubItem, State, Stage);
end;

procedure TCustomFavoritesListView.DblClick;
var
  FileInfo: TSHFileInfo;
  x: Olevariant;
  url: string;
  RootPIDL, ID: PItemIDList;
begin
  inherited;
  if Selected <> nil then
  begin
    ID := ShellItem(Selected.Index).ID;
    if not IsFolderEx(ChannelShortcut, Folder, ID) then
    begin
      SHGetFileInfo(PChar(ID), 0, FileInfo, SizeOf(TSHFileInfo),
        SHGFI_PIDL or SHGFI_TYPENAME or SHGFI_ATTRIBUTES);
      if FileInfo.szTypeName = ChannelShortcut then
        ResolveChannel(Folder, ID, Url)
      else if FileInfo.szTypeName = InternetShortcut then
      begin
        if FResolveUrl = IntshCut then
          Url := ResolveUrlIntShCut(GetFileName(Folder, ID))
        else
          Url := ResolveUrlIni(GetFileName(Folder, ID));
      end
      else
        Url := Resolvelink(GetFileName(Folder, ID));
      if Assigned(FOnUrlSelected) then
        FOnUrlSelected(self, Url)
      else if Assigned(EmbeddedWB) then
        EmbeddedWB.Navigate(Url, X, X, X, X);
    end
    else
    begin
      RootPIDL := ConcatPIDLs(FPIDL, ID);
      SetPath(RootPIDL);
      Inc(Level);
    end;
  end;
end;

procedure TCustomFavoritesListView.LevelUp;
var
  Temp: PItemIDList;
begin
  Temp := CopyPIDL(FPIDL);
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

procedure TCustomFavoritesListView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  case Key of
    VK_RETURN: DblClick;
    VK_BACK, VK_HOME, VK_ESCAPE: LevelUp;
  end;
end;

procedure TCustomFavoritesListView.Loaded;
var
  FileInfo: TSHFileInfo;
  ImageListHandle: THandle;
begin
  inherited;
  OLECheck(SHGetSpecialFolderLocation(Application.Handle, CSIDL_Favorites,
    FavoritesPIDL));
  OLECheck(SHGetDesktopFolder(Desktop));
  Folder := Desktop;
  if list = nil then
    List := TList.Create;
  ImageListHandle := SHGetFileInfo(PChar(FavoritesPidl), 0, FileInfo,
    SizeOf(FileInfo),
    SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  SendMessage(Handle, LVM_SETIMAGELIST, LVSIL_SMALL, ImageListHandle);
  SetPath(FavoritesPIDL);
  Level := 0;
end;

constructor TCustomFavoritesListView.Create(AOwner: TComponent);
var
  FavoritesColumn: TListColumn;
begin
  inherited;
  ReadOnly := True;
  Height := 300;
  Width := 200;
  ShowHint := True;
  ColumnClick := False;
  OwnerData := True;
  ViewStyle := vsReport;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKey('ChannelShortcut', FALSE) then
      ChannelShortCut := ReadString('')
    else
      ChannelShortcut := 'Channel Shortcut';
    Closekey;
    if OpenKey('InternetShortcut', FALSE) then
      InternetShortCut := ReadString('')
    else
      InternetShortcut := 'Internet Shortcut';
    Closekey;
    Free;
  end;
  FavoritesColumn := Columns.Add;
  SHGetDesktopFolder(Desktop);
  SHGetSpecialFolderLocation(Application.Handle, CSIDL_FAVORITES,
    FavoritesPIDL);
  FavoritesColumn.Caption := ExtractfileName(GetFileName(Desktop,
    FavoritesPidl));
  FavoritesColumn.Width := Width - 4;
end;

destructor TCustomFavoritesListView.Destroy;
begin
  if List <> nil then
  begin
    ClearIDList;
    List.Free;
  end;
  inherited;
end;

function TCustomFavoritesListView.OwnerDataFetch(Item: TListItem;
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

function TCustomFavoritesListView.OwnerDataFind(Find: TItemFind;
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
      begin
        if Wrap then
          I := 0
        else
          Exit;
      end;
      Found := Pos(UpperCase(FindString), UpperCase(ShellItem(I)^.DisplayName))
        = 1;
      Inc(I);
    until Found or (I = StartIndex);
    if Found then
      Result := I - 1;
  end;
end;

function CheckShortcut(Shortcut, Flag: Integer): Boolean;
begin
  Result := Shortcut and Flag <> 0;
end;

function GetImageIndex(PIDL: PItemIDList; Large, Open: Boolean): Integer;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
begin
  Flags := SHGFI_PIDL or SHGFI_SYSICONINDEX;
  if Open then
    Flags := Flags or SHGFI_OPENICON;
  if Large then
    Flags := Flags or SHGFI_LARGEICON
  else
    Flags := Flags or SHGFI_SMALLICON;
  SHGetFileInfo(PChar(PIDL), 0, FileInfo, SizeOf(FileInfo), Flags);
  Result := FileInfo.iIcon;
end;

function TCustomFavoritesListView.OwnerDataHint(StartIndex, EndIndex: Integer):
  Boolean;
var
  FileInfo: TSHFileInfo;
  Flags: Integer;
  I, J: Integer;
  sfi: TShFileInfo;
begin
  Result := inherited OwnerDataHint(StartIndex, EndIndex);
  if (StartIndex > List.Count) or (EndIndex > List.Count) then
    Exit;

  for I := StartIndex to EndIndex do
  begin
    if ShellItem(I)^.Empty then
      with ShellItem(I)^ do
      begin
        FullID := ConcatPIDLs(FPIDL, ID);
        FillChar(FileInfo, SizeOf(FileInfo), #0);
        Flags := SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_ICON or
          SHGFI_SMALLICON;
        SHGetFileInfo(PChar(FullID), 0, FileInfo, SizeOf(FileInfo), Flags);
        J := GetImageIndex(FullID, False, False);
        ImageIndex := J;
        if (J = 3) and (CheckShortcut(SFGAO_LINK, SFGAO_DISPLAYATTRMASK))
          then
        begin
          ShGetFileInfo('*.htm', FILE_ATTRIBUTE_NORMAL, sfi, sizeOf(sfi),
            SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or
            SHGFI_SMALLICON);
          J := sfi.iIcon;
          DestroyIcon(sfi.iIcon);
        end;
        ImageIndex := j;
        Empty := False;
      end;
  end;
  Result := True;
end;

procedure TCustomFavoritesListView.EditText;
begin
  if Selected <> nil then
    ListView_EditLabel(Handle, Selected.Index);
end;

{ IShellCommandVerb}

procedure TCustomFavoritesListView.CommandCompleted(Verb: string;
  Succeeded: Boolean);
const
  SCmdVerbOpen = 'open';
  SCmdVerbDelete = 'delete';
  SCmdVerbPaste = 'paste';
begin
  if Succeeded then
  begin
    if SameText(Verb, SCmdVerbDelete) or SameText(Verb, SCmdVerbPaste) then
      Refresh
    else if SameText(Verb, SCmdVerbOpen) then
      SetCurrentDirectory(PChar(FSavePath));
  end;
end;

procedure TCustomFavoritesListView.ExecuteCommand(Verb: string;
  var Handled: Boolean);
const
  SCmdVerbOpen = 'open';
  SCmdVerbRename = 'rename';
var
  szPath: array[0..MAX_PATH] of char;
begin
  if SameText(Verb, SCmdVerbRename) then
  begin
    EditText;
    Handled := True;
  end
  else if SameText(Verb, SCmdVerbOpen) then
  begin
    GetCurrentDirectory(MAX_PATH, szPath);
    FSavePath := StrPas(szPath);
    StrPCopy(szPath, ExtractFilePath(ShellItem(Selected.Index).DisplayName));
      // StrPCopy(szPath, ExtractFilePath(Folders[Selected.Index].PathName));
    SetCurrentDirectory(szPath);
  end;
end;

procedure TCustomFavoritesListView.PopupSystemContextMenu(Item: TListItem;
  Point: TPoint);
var
  ISF: IShellFolder;
  Pidl: PItemIdList;
begin
  if Assigned(Item) then
  begin
    ISF := Folder;
    if Assigned(ISF) then
    begin
      Pidl := ShellItem(Selected.Index).ID;
      try
        MenuContext.DisplayContextMenu(ISF, Pidl, 0, Application.Handle,
          Point, 1);
      except
      end;
    end;
  end;
end;

procedure TCustomFavoritesListView.WMContextMenu(var Message: TMessage);
var
  P: TPoint;
  R: TRect;
  NewSel, SelItem: TListItem;
begin
  NewSel := nil;
  if Message.lparam = -1 then
  begin
    SelItem := TListItem(Selected);
    if not Assigned(SelItem) then
      Exit;
    R := SelItem.DisplayRect(drSelectBounds);
    P.X := R.Left + ((R.Right - R.Left) div 2);
    P.Y := R.Top - ((R.Top - R.Bottom) div 2);
  end
  else
  begin
    P.X := LOWORD(Message.lParam);
    P.Y := HIWORD(Message.lParam);
    P := ScreenToClient(P);
    SelItem := Selected //TListItem(GetPosition(P.X,P.Y));
  end;
  P := ClientToScreen(P);
  if (Selected <> SelItem) then
    NewSel := TListItem(Selected);
  Selected := SelItem;
  if PopupMenuMode = pmm_PopupMenu then
  begin
    if Assigned(PopupMenu) then
    begin
      PopupMenu.PopupComponent := Self;
      PopupMenu.Popup(P.X, P.Y)
    end;
  end
  else
  begin
    PopupSystemContextMenu(SelItem, P);
  end;
  if Assigned(NewSel) then
    Selected := NewSel;
end;

end.
