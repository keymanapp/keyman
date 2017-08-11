//******************************************************************
//                                                                 *
//                       TFavoritesPopup                           *                                                      *
//                     Freeware Component                          *
//                            by                                   *
//                     Eran Bodankin (bsalsa)                      *
//                       bsalsa@gmail.com                         *
//                  Based on TFavoritesMenu code                   *
//                     by  Per Lindsø Larsen                       *
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
//$Id: FavoritesPopup.pas,v 1.2 2006/12/05 11:56:31 bsalsa Exp $

unit FavoritesPopup;

interface

{$I EWB.inc}
uses
  Dialogs, Classes, Controls, Forms, Shlobj, Menus, EmbeddedWB;

type
  PItem = ^TItem;
  TItem = record
    ID: PItemIDList;
    FullID: PItemIDList;
    Folder: Boolean;
    Created: Boolean;
  end;

type
  TErrorEvent = procedure(const ErrorCode: integer; ErrMessage: string) of object;
  TFavOptions = (foAddFavorites, foOrganizeFavorites, foImportFavorites,
    foExportFavorites, foImportExportWizard);
  TOptions = set of TFavOptions;
  TOnAddFavoritesEvent = procedure(const EmbeddedWB: TEmbeddedWB; Title, URL: WideString; Success: integer) of object;
  TOnUrlSelectedEvent = procedure(Sender: TObject; Url: string) of object;
  TResolveUrl = (IntShCut, IniFile);
  TShellFolderProperty = (fpCut, fpIsLink, fpReadOnly, fpShared, fpFileSystem,
    fpFileSystemAncestor, fpRemovable, fpValidate);
  TShellFolderProperties = set of TShellFolderProperty;
  TLocalization = class(TPersistent)
  private
    FAddFavorites: string;
    FOrganizeFavorites: string;
    FImportFavorites: string;
    FExportFavorites: string;
    FImportExportWizard: string;
  published
    property AddFavorites: string read FAddFavorites write FAddFavorites;
    property OrganizeFavorites: string read FOrganizeFavorites write FOrganizeFavorites;
    property ImportFavorites: string read FImportFavorites write FImportFavorites;
    property ExportFavorites: string read FExportFavorites write FExportFavorites;
    property ImportExportWizard: string read FImportExportWizard write FImportExportWizard;
  end;

  TFavoritesPopup = class(TPopupMenu)
  private
    { Private declarations }
    Counter: Integer;
    Desktop: IShellFolder;
    FavoritesPopup: TMenuItem;
    FavoritesPidl: PItemIDList;
    FAbout: string;
    FCaption: string;
    FChannels: Boolean;
    FEmbeddedWB: TEmbeddedWB;
    FEnabled: Boolean;
    FOnError: TErrorEvent;
    FLocalization: TLocalization;
    FMaxWidth: Integer;
    FOnAddFavorites: TOnAddFavoritesEvent;
    FOnUrlSelected: TOnUrlSelectedEvent;
    FOptions: TOptions;
    FResolveUrl: TResolveUrl;
    Images: TImageList;
    Item: PItem;
    List: TList;
    procedure AddFavorite(Sender: TObject);
    procedure FavoritesExport(Sender: TObject);
    procedure FavoritesImport(Sender: TObject);
    procedure OrganizeFavorite(Sender: TObject);
    procedure ImportExportWizard(Sender: TObject);
    procedure SetAbout(Value: string);
  protected
    { Protected declarations }
    procedure AddDummy(menu: TMenuItem);
    procedure AddEmpty(menu: TMenuItem);
    procedure AddMenu(Menu: TMenuItem; FullID: PItemIDList);
    procedure BuildOptionsMenu;
    procedure DestroyList;
    procedure MenuClick(Sender: TObject);
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateMenu(mi: TMenuItem); overload;
    procedure ReBuildMenu;
  published
    { Published declarations }
    property About: string read fAbout write SetAbout;
    property Channels: Boolean read FChannels write FChannels default False;
    property EmbeddedWB: TEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Localization: TLocalization read FLocalization write FLocalization;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 50;
    property OnAddFavorites: TOnAddFavoritesEvent read FOnAddFavorites write FOnAddFavorites;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnURLSelected: TOnURLSelectedEvent read FOnURLSelected write FOnURLSelected;
    property Options: TOptions read FOptions write FOptions default [foAddFavorites, foOrganizeFavorites];
    property ResolveUrl: TResolveUrl read FResolveUrl write FResolveUrl default IntShCut;
  end;

implementation

uses
  ImportFavorites, ExportFavorites, Imglist, Windows, ShellApi, SysUtils, SHDocVw_EWB, ActiveX, Registry, EwbTools;

var
  ChannelShortcut, InternetShortcut: string;
  Folder: IShellFolder;
  Procedur: procedure(Handle: THandle; Path: PAnsiChar); stdcall;

function SortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := SmallInt(Folder.CompareIDs(0, PItem(Item1).ID, PItem(Item2).ID));
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

function CheckShortcut(Shortcut, Flag: Integer): Boolean;
begin
  Result := Shortcut and Flag <> 0;
end;

constructor TFavoritesPopup.Create;
begin
  FAbout := 'TFavoritesPopup - from http://www.bsalsa.com/';
  FEnabled := True;
  FLocalization := TLocalization.Create;
  FLocalization.FAddFavorites := 'Add to Favorites';
  FLocalization.FOrganizeFavorites := 'Organize Favorites';
  FLocalization.FImportFavorites := 'Import Favorites';
  FLocalization.FExportFavorites := 'Export Favorites';
  FLocalization.FImportExportWizard := 'Import Export Favorites Wizard';
  SHGetDesktopFolder(Desktop);
  SHGetSpecialFolderLocation(Application.Handle, CSIDL_FAVORITES, FavoritesPIDL);
  FCaption := ExtractFileName(GetFileName(Desktop, FavoritesPidl));
  with TRegistry.Create do
  begin
    RootKey := HKEY_CLASSES_ROOT;
    if OpenKey('ChannelShortcut', FALSE)
      then
      ChannelShortCut := ReadString('')
    else
      ChannelShortcut := 'Channel Shortcut';
    Closekey;
    if OpenKey('InternetShortcut', FALSE)
      then
      InternetShortCut := ReadString('')
    else
      InternetShortcut := 'Internet Shortcut';
    Closekey;
    Free;
  end;
  FMaxWidth := 50;
  FOptions := [foAddFavorites, foOrganizeFavorites];
  inherited;
end;

procedure TFavoritesPopup.Loaded;
begin
  CreateMenu(Self.Items);
  inherited;
end;

procedure TFavoritesPopup.DestroyList;
var
  I: Integer;
begin
  if list <> nil then
  begin
    for I := 0 to List.Count - 1 do
    begin
      DisposePIDL(PItem(List[I]).ID);
      DisposePIDL(PItem(List[i]).FULLID);
      Dispose(PItem(List[i]));
    end;
    Counter := 0;
    List.Free;
  end;
end;

destructor TFavoritesPopup.Destroy;
begin
  FLocalization.Free;
  DestroyList;
  inherited;
end;

procedure TFavoritesPopup.SetAbout(Value: string);
begin
  Exit;
end;

procedure TFavoritesPopup.FavoritesExport(Sender: TObject);
var
  ExportFav: TExportFavorite;
begin
  if FEnabled and Assigned(FEmbeddedWB) then
  begin
    if not FEmbeddedWB.DocumentLoaded then
    begin
      FEmbeddedWB.AssignEmptyDocument;
      FEmbeddedWB.Wait;
    end;
    ExportFAv := TExportFavorite.Create(Self);
    with ExportFav do
    begin
      ExportFavorites;
      Free;
    end;
  end;
end;

procedure TFavoritesPopup.FavoritesImport(Sender: TObject);
var
  ImportFav: TImportFavorite;
begin
  if FEnabled and Assigned(FEmbeddedWB) then
  begin
    if not FEmbeddedWB.DocumentLoaded then
    begin
      FEmbeddedWB.AssignEmptyDocument;
      FEmbeddedWB.Wait;
    end;
    ImportFav := TImportFavorite.Create(Self);
    with ImportFav do
    begin
      ImportFavorites;
      Free;
    end;
  end;
end;

procedure TFavoritesPopup.ImportExportWizard(Sender: TObject);
begin
  if FEnabled and Assigned(FEmbeddedWB) then
  begin
    if not FEmbeddedWB.DocumentLoaded then
    begin
      FEmbeddedWB.AssignEmptyDocument;
      FEmbeddedWB.Wait;
    end;
    FEmbeddedWB.ShowImportExportFavoritesAndCookies;
  end;
end;

procedure TFavoritesPopup.OrganizeFavorite(Sender: Tobject);
var
  SpecialPath: array[0..MAX_PATH] of Char;
  H: HWnd;
  bGet: Boolean;
begin
  if FEnabled then
  begin
    H := LoadLibrary('shdocvw.dll');
    if H <> 0 then
    begin
      Procedur := GetProcAddress(H, 'DoOrganizeFavDlg');
      if Assigned(Procedur) then
      begin
        bGet := SHGetPathFromIDList(FavoritesPidl, SpecialPath);
        if (not bGet) and (Assigned(FOnError)) then
          FOnError(E_FAIL, 'Failed while getting path for the favorites!');
        Procedur(Application.Handle, PAnsiChar(AnsiString(SpecialPath)));
      end;
    end
    else
      if Assigned(FOnError) then
        FOnError(E_FAIL, 'Failed while loading library!');
    FreeLibrary(H);
    RebuildMenu;
  end;
end;

procedure TFavoritesPopup.AddFavorite(Sender: TObject);
const
  CLSID_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
var
  ShellUIHelper: ISHellUIHelper;
  Url, Title: Olevariant;
  Success: integer;
begin
  if FEnabled then
  begin
    if Assigned(FEmbeddedWB) then
    begin
      Title := FEmbeddedWB.LocationName;
      Url := FEmbeddedWB.LocationUrl;
      if Url <> '' then
      begin
        Success := CoCreateInstance(CLSID_SHELLUIHELPER, nil, CLSCTX_INPROC_SERVER,
          IID_IShellUIHelper, ShellUIHelper);
        if Assigned(FOnAddFavorites) then
          FOnAddFavorites(FEmbeddedWB, Title, URL, Success);
        if (Success <> S_OK) and (Assigned(FOnError)) then
          FOnError(Success, 'Failed  while adding to favorites!');
        ShellUIHelper.AddFavorite(Url, Title);
        RebuildMenu;
      end
      else
        if (Assigned(FOnError)) then
          FOnError(E_FAIL, 'Failed - Empty URL string!');
    end
    else
      if (Assigned(FOnError)) then
        FOnError(E_FAIL, 'Please assign a TEmbeddedWB');
  end;
end;

procedure TFavoritesPopup.AddDummy(menu: TMenuItem);
var
  Dummy: TMenuItem;
begin
  Dummy := TMenuItem.Create(self);
  Dummy.Visible := False;
  Menu.Add(Dummy);
end;

procedure TFavoritesPopup.AddEmpty(menu: TMenuItem);
var
  Empty: TMenuItem;
begin
  Empty := TMenuItem.Create(self);
  Empty.Caption := ' (Empty) ';
  Empty.Enabled := False;
  Menu.add(Empty);
end;

procedure TFavoritesPopup.AddMenu(Menu: TMenuItem; FullID: PItemIDList);
var
  MenuItem: TMenuItem;
  EnumList: IEnumIDList;
  ID: PItemIDList;
  NumIDs: LongWord;
  TempList: TList;
  I, j: Integer;
  Success: integer;
  sfi: TShFileInfo;
begin
  if FEnabled then
  begin
    TempList := TList.Create;
    Success := Desktop.BindToObject(FullID, nil, IID_IShellFolder, Pointer(Folder));
    if (Success <> S_OK) and (Assigned(FOnError)) then
      FOnError(Success, 'Failed retrieves an IShellFolder object for a subfolder!');
    Success := Folder.EnumObjects(Application.Handle, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS,
      EnumList);
    if (Success <> S_OK) and (Assigned(FOnError)) then
      FOnError(Success, 'Failed Enum objects!');
    while EnumList.Next(1, ID, NumIDs) = S_OK do
    begin
      if not Channels and IsChannel(ChannelShortcut, Folder, ID) then
        continue;
      Item := New(PItem);
      Item.ID := CopyPidl(ID);
      Item.FullID := ConcatPIDLs(FullID, ID);
      Item.Folder := IsFolderEx(ChannelShortcut, Folder, ID);
      Item.Created := False;
      TempList.Add(Item);
    end;
    DisposePidl(ID);
    if TempList.Count = 0 then
    begin
      AddEmpty(Menu);
      TempList.Free;
      exit;
    end;
    TempList.Sort(SortFunc);
    for I := 0 to TempList.Count - 1 do
    begin
      List.Add(PItem(Templist[I]));
      MenuItem := TMenuItem.Create(Menu);
      with MenuItem do
      begin
        SubMenuImages := Images;
        OnClick := MenuClick;
        Tag := Counter;
        Caption := GetDisplayName(Folder, PItem(TempList[I])^.ID);
      end;
      if Length(MenuItem.Caption) > FMaxWidth then
        MenuItem.Caption := Copy(MenuItem.Caption, 1, FMaxWidth) + '...';
      J := GetImageIndex(PItem(TempList[I])^.FullID, False, False);
      if (J = 3) and (CheckShortcut(SFGAO_LINK, SFGAO_DISPLAYATTRMASK)) then
      begin
        ShGetFileInfo('*.htm', FILE_ATTRIBUTE_NORMAL, sfi, sizeOf(sfi),
          SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON);
        J := sfi.iIcon;
        DestroyIcon(sfi.iIcon);
      end;
      MenuItem.ImageIndex := J;
      Menu.Add(MenuItem);
      Inc(Counter);
      if PItem(TempList[I])^.Folder then
        AddDummy(MenuItem);
    end;
    TempList.Free;
  end;
end;

procedure TFavoritesPopup.MenuClick(Sender: TObject);
var
  Folder: IShellFOlder;
  FileInfo: TSHFileInfo;
  ID: PItemIDList;
  X: OleVariant;
  Url: string;
  Success: integer;
begin
  if FEnabled and not (csDesigning in ComponentState) then
  begin
    if PItem(list[(Sender as TMenuItem).Tag])^.folder
    then
    begin
      if
        not PItem(list[(Sender as TMenuItem).Tag]).Created then
      begin
        AddMenu(Sender as TMenuItem,
          PItem(list[(Sender as TMenuItem).Tag])^.FULLID);
        PItem(list[(Sender as TMenuItem).Tag]).Created := TRUE;
      end;
    end
    else
    begin
      id := CopyPidl(PItem(list[(Sender as TMenuItem).Tag])^.FULLID);
      StripLastID(ID);
      Success := Desktop.BindToObject(ID, nil, IID_IShellFolder, Pointer(Folder));
      if (Success <> S_OK) and (Assigned(FOnError)) then
        FOnError(Success, 'Failed Retrieves an IShellFolder object for a subfolder!');
      SHGetFileInfo(PChar(PItem(list[(Sender as TMenuItem).Tag])^.ID), 0,
        FileInfo, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_TYPENAME or SHGFI_ATTRIBUTES);
      if Fileinfo.szTypeName = ChannelShortcut then
        ResolveChannel(Folder, PItem(list[(Sender as TMenuItem).Tag])^.ID, Url)
      else
        if fileinfo.szTypeName = InternetShortcut then
        begin
          if FResolveUrl = IntshCut then
            Url := ResolveUrlIntShCut(GetFileName(Folder,
              PItem(list[(Sender as TMenuItem).Tag])^.ID))
          else
            Url := ResolveUrlIni(GetFileName(Folder,
              PItem(list[(Sender as TMenuItem).Tag])^.ID));
        end
        else
          Url := Resolvelink(GetFileName(Folder,
            PItem(list[(Sender as TMenuItem).Tag])^.ID));
      DisposePidl(ID);
      if Assigned(FOnUrlSelected) then
        FOnUrlSelected(Sender, Url)
      else
        if Assigned(FEmbeddedWB) then
          FEmbeddedWB.Navigate(Url, X, X, X, X);
    end;
  end;
end;

procedure TFavoritesPopup.BuildOptionsMenu;
begin
  if FEnabled and not (csDesigning in ComponentState) then
  begin
    if foAddFavorites in FOptions then
      FavoritesPopup.Add(NewItem(FLocalization.FAddFavorites, 0,
        False, True, addfavorite, 0, ''));
    if foOrganizeFavorites in FOptions then
      FavoritesPopup.Add(NewItem(FLocalization.FOrganizeFavorites, 0,
        False, True, organizefavorite, 0, ''));
    if FavoritesPopup.Count > 0 then
      FavoritesPopup.Add(NewItem('-', 0, False, True, nil, 0, ''));
    if IE5_Installed then
    begin
      if foImportFavorites in FOptions then
        FavoritesPopup.Add(NewItem(FLocalization.FImportFavorites, 0,
          False, True, FavoritesImport, 0, ''));
      if foExportFavorites in FOptions then
        FavoritesPopup.Add(NewItem(FLocalization.FExportFavorites, 0,
          False, True, FavoritesExport, 0, ''));
      if foImportExportWizard in FOptions then
        FavoritesPopup.Add(NewItem(FLocalization.FImportExportWizard, 0,
          False, True, ImportExportWizard, 0, ''));

      if (foImportFavorites in FOptions) or (foExportFavorites in FOptions)
        or (foImportExportWizard in FOptions) then
        FavoritesPopup.Add(NewItem('-', 0, False, True, nil, 0, ''));
    end;
  end;
end;

procedure TFavoritesPopup.CreateMenu(mi: TMenuItem);
var
  FileInfo: TSHFileInfo;
begin
  if FEnabled and not (csDesigning in ComponentState) then
  begin
    Counter := 0;
    List := TList.Create;
    Images := TImagelist.Create(self);
    with images do
    begin
      ShareImages := True;
      DrawingStyle := dsTransparent;
      Handle := SHGetFileInfo(Pchar(FavoritesPidl), 0, FileInfo,
        SizeOf(FileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    end;
    if mi = nil then
    begin
      FavoritesPopup := TMenuitem.Create(self);
      FavoritesPopup.Caption := FCaption;
    end
    else
      FavoritesPopup := mi;
    FavoritesPopup.SubmenuImages := Images;
    BuildOptionsMenu;
    AddMenu(FavoritesPopup, FavoritesPidl);
  end;
end;

procedure TFavoritesPopup.ReBuildMenu;
begin
  DestroyList;
  List := TList.Create;
  FavoritesPopup.Clear;
  BuildOptionsMenu;
  AddMenu(FavoritesPopup, FavoritesPidl);
end;

end.
