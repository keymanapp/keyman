//**************************************************************
//                                                             *
//                        THistoryMenu                         *                                                      *
//                     For Delphi 5 to XE                      *
//                     Freeware Component                      *
//                            by                               *
//                     Per Lindsø Larsen                       *
//                   per.lindsoe@larsen.dk                     *
//                                                             *
//  Contributions:                                             *
//  Eran Bodankin (bsalsa) bsalsa@gmail.com                    *
//         -  D2005 update & added new functions               *
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
//$Id: HistoryMenu.pas,v 1.2 2006/11/15 21:01:42 sergev Exp $

unit HistoryMenu;

interface

{$I EWB.inc}

uses
  Windows, Classes, Controls, Forms, Registry, Menus, SHellApi, shlobj,
  imglist, ActiveX, EmbeddedWB, Dialogs;

type
  PItem = ^TItem;
  TItem = record
    ID, FullID: PItemIDList;
    Folder: Boolean;
    Created: Boolean;
  end;
  TOnUrlSelectedEvent = procedure(Sender: TObject; Url: string) of object;
  THistoryMenu = class(TComponent)
  private
    { Private declarations }
    FCaption: string;
    FMenuPosition: Integer;
    FMainMenu: TMainmenu;
    FOnUrlSelected: TOnUrlSelectedEvent;
    FEmbeddedWB: TEmbeddedWB;
    Desktop: IShellFolder;
    HistoryPidl: PItemIDList;
    HistMenu: TMenuItem;
    List: TList;
    Images: TImageList;
    Item: PItem;
  protected
    procedure AddMenu(Menu: TMenuItem; MenuTag: Integer);
    procedure AddDummy(menu: TMenuItem);
    procedure MenuClick(Sender: TObject);
    procedure AddEmpty(menu: TMenuItem);
    procedure DestroyList;
    procedure BuildMenu;
    { Protected declarations }

  public
    { Public declarations }
    procedure CreateMenu; overload;
    procedure CreateMenu(mi: TMenuItem); overload;
    procedure RebuildMenu;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property EmbeddedWB: TEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property MainMenu: TMainMenu read FMainMenu write FMainMenu;
    property MenuPosition: Integer read FMenuPosition write FMenuPosition;
    property Caption: string read FCaption write FCaption;
    property OnURLSelected: TOnURLSelectedEvent read FOnURLSelected write FOnURLSelected;
  end;

implementation

uses
  EwbTools;

var
  Counter: Integer;
  Folder: IShellFolder;

function SortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := SmallInt(Folder.CompareIDs(0, PItem(Item1).ID, PItem(Item2).ID));
end;

procedure THistoryMenu.AddDummy(menu: TMenuItem);
var
  Dummy: TMenuItem;
begin
  Dummy := TMenuItem.Create(self);
  Dummy.Visible := False;
  Menu.add(Dummy);
end;

procedure THistoryMenu.AddEmpty(menu: TMenuItem);
var
  Empty: TMenuItem;
begin
  Empty := TMenuItem.Create(self);
  Empty.Caption := ' (Empty) ';
  Empty.Enabled := False;
  Menu.add(Empty);
end;

procedure THistoryMenu.AddMenu(Menu: TMenuItem; MenuTag: Integer);
var
  MenuItem: TMenuItem;
  EnumList: IEnumIDList;
  FullID, ID: PItemIDList;
  NumIDs: LongWord;
  TempList: TList;
  I: Integer;
begin
  TempList := TList.Create;
  FullID := CopyPidl(PItem(List[menuTag])^.FullID);
  Desktop.BindToObject(FullID, nil, IID_IShellFolder, Pointer(Folder));
  Folder.EnumObjects(Application.Handle, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, EnumList);
  while EnumList.Next(1, ID, NumIDs) = S_OK do
  begin
    Item := New(PItem);
    Item.ID := CopyPidl(ID);
    Item.FullID := ConcatPIDLs(FullID, ID);
    Item.Folder := IsFolder(Folder, ID);
    Item.Created := False;
    TempList.Add(Item);
  end;
  if TempList.Count = 0 then
  begin
    AddEmpty(Menu);
    exit;
  end;
  TempList.Sort(SortFunc);
  for I := 0 to TempList.Count - 1 do
  begin
    List.Add(PItem(Templist[I]));
    MenuItem := TMenuItem.Create(Menu);
    MenuItem.SubmenuImages := Images;
    MenuItem.OnClick := MenuClick;
    MenuItem.Tag := Counter;
    MenuItem.Caption := GetDisplayName(Folder, PItem(TempList[I])^.ID);
    MenuItem.ImageIndex := GetImageIndex(PItem(TempList[I])^.FullID);
    if not PItem(TempList[I])^.Folder then
      Menuitem.Hint := ExtractUrl(Folder, PItem(TempList[I])^.ID);
    Menu.Add(MenuItem);
    Inc(Counter);
    if Item.Folder then
      AddDummy(MenuItem);
  end;
  TempList.Free;
end;

procedure THistoryMenu.MenuClick(Sender: TObject);
var
  X: OleVariant;
  Url: string;
begin
  if not PItem(list[(Sender as TMenuItem).Tag])^.Folder then
  begin
    url := ((Sender as TMenuItem).Hint);
    if Assigned(FOnUrlSelected) then
      FOnUrlSelected(Sender, (Sender as TMenuItem).Hint);
    rebuildmenu;
    if Assigned(FEmbeddedWB) then
      FEmbeddedWB.Navigate(Url, X, X, X, X);

  end
  else
    if
      not PItem(list[(Sender as TMenuItem).Tag]).Created then
    begin
      AddMenu(Sender as TMenuItem, (Sender as TMenuItem).Tag);
      PItem(list[(Sender as TMenuItem).Tag]).Created := TRUE;
    end;
end;

procedure THistoryMenu.BuildMenu;
var
  DateFolder: IShellFolder;
  DateEnumList: IEnumIDList;
  DateMenuItem: TMenuItem;
  DateId: PItemIDList;
  NumIDs: UINT;
begin
  List := TList.Create;
  Counter := 0;
  Desktop.BindToObject(HistoryPidl, nil, IID_IShellFolder, Pointer(DateFolder));
  DateFolder.EnumObjects(Application.Handle,
    SHCONTF_FOLDERS or SHCONTF_NONFOLDERS or SHCONTF_INCLUDEHIDDEN, DateEnumList);
  while DateEnumList.Next(1, DateID, NumIDs) = S_OK do
  begin
    DateMenuItem := TMenuItem.Create(Application);
    DateMenuItem.SubmenuImages := Images;
    DateMenuItem.OnClick := MenuClick;
    DateMenuItem.Tag := Counter;
    Inc(Counter);
    DateMenuItem.Caption := GetDisplayName(DateFolder, DateID);
    Item := New(PItem);
    Item.Id := CopyPidl(DateID);
    Item.FullID := ConcatPIDLs(HistoryPidl, DateID);
    DateMenuItem.ImageIndex := GetImageIndex(Item.FullID);
    Item.Folder := IsFolder(DateFolder, DateID);
    Item.Created := False;
    List.Add(Item);
    HistMenu.Add(DateMenuItem);
    if Item.Folder then
      AddDummy(DateMenuItem);
  end;
end;

procedure THistoryMenu.RebuildMenu;
begin
  DestroyList;
  Histmenu.Clear;
  BuildMenu;
end;

procedure THistoryMenu.CreateMenu;
var
  FileInfo: TSHFileInfo;
begin
  SHGetDesktopFolder(Desktop);
  SHGetSpecialFolderLocation(Application.Handle, CSIDL_HISTORY, HistoryPIDL);
  Images := TImagelist.Create(self);
  with images do
  begin
    ShareImages := True;
    DrawingStyle := dsTransparent;
    Handle := SHGetFileInfo(Pchar(HistoryPidl), 0, FileInfo, SizeOf(FileInfo),
      SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  end;
  HistMenu := TMenuitem.Create(self);
  with HistMenu do
  begin
    SubmenuImages := Images;
    Caption := FCaption;
  end;
  if Assigned(FMainMenu) then
  begin
    if FMenuPosition > FMainMenu.Items.Count + 1 then
      FMenuPosition := FMainMenu.Items.Count + 1
    else
      if FMenuPosition <= 0 then
        FMenuPosition := 1;
    FMainMenu.Items.Insert(FMenuPosition - 1, HistMenu);
  end;
  BuildMenu;
end;

procedure THistoryMenu.CreateMenu(mi: TMenuItem);
var
  FileInfo: TSHFileInfo;
begin
  SHGetDesktopFolder(Desktop);
  SHGetSpecialFolderLocation(Application.Handle, CSIDL_HISTORY, HistoryPIDL);
  Images := TImagelist.Create(self);
  with images do
  begin
    ShareImages := True;
    DrawingStyle := dsTransparent;
    Handle := SHGetFileInfo(Pchar(HistoryPidl), 0, FileInfo, SizeOf(FileInfo),
      SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  end;
  if mi = nil then
  begin
    HistMenu := TMenuitem.Create(self);
    HistMenu.Caption := FCaption;
  end
  else
    HistMenu := mi;
  HistMenu.SubmenuImages := Images;
  if Assigned(FMainMenu) then
  begin
    if FMenuPosition > FMainMenu.Items.Count + 1 then
      FMenuPosition := FMainMenu.Items.Count + 1
    else
      if FMenuPosition <= 0 then
        FMenuPosition := 1;
    FMainMenu.Items.Insert(FMenuPosition - 1, HistMenu);
  end;
  BuildMenu;
end;

constructor THistoryMenu.Create;
begin
  FMenuPosition := 1;
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    OpenKey('CLSID\{FF393560-C2A7-11CF-BFF4-444553540000}', FALSE);
    FCaption := ReadString('');
    Closekey;
    Free;
  end;
  inherited;
end;

procedure THistoryMenu.DestroyList;
var
  I: Integer;
begin
  if Assigned(list) then
  begin
    for I := 0 to List.Count - 1 do
    begin
      DisposePIDL(PItem(List[I]).ID);
      DisposePIDL(PItem(List[i]).FULLID);
      Dispose(PItem(List[i]));
    end;
    List.Free;
  end;
end;

destructor THistoryMenu.Destroy;
begin
  DestroyList;
  inherited;
end;

end.
