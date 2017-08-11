//******************************************************************
//                                                                 *
//                          TFavoritesMenu                         *                                                      *
//                     For Delphi 5 to XE                          *
//                     Freeware Component                          *
//                            by                                   *
//                     Per Lindsø Larsen                           *
//                   per.lindsoe@larsen.dk                         *
//                                                                 *
//                                                                 *
//  Contributions:                                                 *
//  Pete Morris (MrPMorris@Hotmail.com)                            *
//  Rob Young (rob@coolfocus.com)                                  *
//  Eran Bodankin (bsalsa) bsalsa@gmail.com                        *
//         -  D2005 update & added new functions                   *
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
//$Id: FavMenu.pas,v 1.4 2006/12/05 11:56:31 bsalsa Exp $

unit FavMenu;

interface

{$I EWB.inc}

uses

  Classes, Controls, Forms, ShlObj, Imglist, Menus, EmbeddedWB;


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

  TLocalization = class(TPersistent)
  private
    FAddFavorites: string;
    FOrganizeFavorites: string;
    FImportFavorites: string;
    FExportFavorites: string;
    FImportExportWizard: string;
    FEmptyCaption: string;
  published
    property AddFavorites: string read FAddFavorites write FAddFavorites;
    property OrganizeFavorites: string read FOrganizeFavorites write FOrganizeFavorites;
    property ImportFavorites: string read FImportFavorites write FImportFavorites;
    property ExportFavorites: string read FExportFavorites write FExportFavorites;
    property ImportExportWizard: string read FImportExportWizard write FImportExportWizard;
    property EmptyCaption: string read FEmptyCaption write FEmptyCaption;
  end;

  TFavoritesMenu = class(TComponent)
  private
    { Private declarations }
    Counter: Integer;
    Desktop: IShellFolder;
    FavoritesMenu: TMenuItem;
    FavoritesPidl: PItemIDList;
    FAbout: string;
    FCaption: string;
    FChannels: Boolean;
    FEmbeddedWB: TEmbeddedWB;
    FEnabled: Boolean;
    FOnError: TErrorEvent;
    FLocalization: TLocalization;
    FMainMenu: TMainmenu;
    FMaxWidth: Integer;
    FMenuPosition: Integer;
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
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateMenu(mi: TMenuItem); overload;
    procedure CreateMenu; overload;
    procedure ReBuildMenu;
  published
    { Published declarations }
    property About: string read fAbout write SetAbout;
    property Caption: string read FCaption write FCaption;
    property Channels: Boolean read FChannels write FChannels default False;
    property EmbeddedWB: TEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property Localization: TLocalization read FLocalization write FLocalization;
    property MainMenu: TMainMenu read FMainMenu write FMainMenu;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth default 50;
    property MenuPosition: Integer read FMenuPosition write FMenuPosition default 1;
    property OnAddFavorites: TOnAddFavoritesEvent read FOnAddFavorites write FOnAddFavorites;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnURLSelected: TOnURLSelectedEvent read FOnURLSelected write FOnURLSelected;
    property Options: TOptions read FOptions write FOptions default [foAddFavorites, foOrganizeFavorites];
    property ResolveUrl: TResolveUrl read FResolveUrl write FResolveUrl default IntShCut;
  end;

implementation

uses
  ImportFavorites, ExportFavorites, Windows, ShellApi, SysUtils, SHDocVw_EWB, ActiveX, Registry, EwbTools;

var
  ChannelShortcut, InternetShortcut: string;
  Folder: IShellFolder;
  Procedur: procedure(Handle: THandle; Path: PAnsiChar); stdcall;

function SortFunc(Item1, Item2: Pointer): Integer;
begin
  Result := SmallInt(Folder.CompareIDs(0, PItem(Item1).ID, PItem(Item2).ID));
end;

function CheckShortcut(Shortcut, Flag: Integer): Boolean;
begin
  Result := ShortCut and Flag <> 0;
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

constructor TFavoritesMenu.Create;
begin
  FAbout := 'TFavoritesMenu - from http://www.bsalsa.com/';
  FEnabled := True;
  FLocalization := TLocalization.Create;
  FLocalization.FAddFavorites := 'Add to Favorites';
  FLocalization.FOrganizeFavorites := 'Organize Favorites';
  FLocalization.FImportFavorites := 'Import Favorites';
  FLocalization.FExportFavorites := 'Export Favorites';
  FLocalization.FImportExportWizard := 'Import Export Favorites Wizard';
  FLocalization.FEmptyCaption := ' (empty) ';
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
      ChannelShortCut := 'Channel Shortcut';
    Closekey;
    if OpenKey('InternetShortcut', FALSE)
      then
      InternetShortCut := ReadString('')
    else
      InternetShortCut := 'Internet Shortcut';
    Closekey;
    Free;
  end;
  FMaxWidth := 50;
  FMenuPosition := 1;
  FOptions := [foAddFavorites, foOrganizeFavorites];
  inherited;
end;

procedure TFavoritesMenu.DestroyList;
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

destructor TFavoritesMenu.Destroy;
begin
  FLocalization.Free;
  DestroyList;
  inherited;
end;

procedure TFavoritesMenu.SetAbout(Value: string);
begin
  Exit;
end;

procedure TFavoritesMenu.FavoritesExport(Sender: TObject);
var
  ExportFav: TExportFavorite;
begin
  if FEnabled and Assigned(FEmbeddedWB) then
  begin
    if not FEmbeddedWB.DocumentLoaded then
      FEmbeddedWB.AssignEmptyDocument(True);
    ExportFAv := TExportFavorite.Create(Self);
    with ExportFav do
    begin
      ExportFavorites;
      Free;
    end;
  end;
end;

procedure TFavoritesMenu.FavoritesImport(Sender: TObject);
var
  ImportFav: TImportFavorite;
begin
  if FEnabled and Assigned(FEmbeddedWB) then
  begin
    if not FEmbeddedWB.DocumentLoaded then
      FEmbeddedWB.AssignEmptyDocument(True);
    ImportFav := TImportFavorite.Create(Self);
    with ImportFav do
    begin
      ImportFavorites;
      Free;
    end;
  end;
end;

procedure TFavoritesMenu.ImportExportWizard(Sender: TObject);
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

procedure TFavoritesMenu.OrganizeFavorite(Sender: Tobject);
var
  SpecialPath: array[0..MAX_PATH] of Char;
  H: Cardinal;
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
        if (not bGet) and Assigned(FOnError) then
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

procedure TFavoritesMenu.AddFavorite(Sender: TObject);
const
  CLSID_ShellUIHelper: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
var
  ShellUIHelper: ISHellUIHelper;
  Url, Title: OleVariant;
  Success: integer;
begin
  if (FEnabled) and (Assigned(FEmbeddedWB)) then
  begin
    Title := FEmbeddedWB.LocationName;
    Url := FEmbeddedWB.LocationUrl;
    if (Url <> EmptyStr) and (url <> 'about:blank') then
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
      FOnError(E_FAIL, 'Please assign a browser');
end;

procedure TFavoritesMenu.AddDummy(menu: TMenuItem);
var
  Dummy: TMenuItem;
begin
  Dummy := TMenuItem.Create(self);
  Dummy.Visible := False;
  Menu.Add(Dummy);
end;

procedure TFavoritesMenu.AddEmpty(menu: TMenuItem);
var
  Empty: TMenuItem;
begin
  Empty := TMenuItem.Create(self);
  Empty.Caption := FLocalization.FEmptyCaption;
  Empty.Enabled := False;
  Menu.add(Empty);
end;

procedure TFavoritesMenu.AddMenu(Menu: TMenuItem; FullID: PItemIDList);
var
  MenuItem: TMenuItem;
  EnumList: IEnumIDList;
  ID: PItemIDList;
  NumIDs: LongWord;
  TempList: TList;
  I, J: Integer;
  sfi: TShFileInfo;
  Success: integer;
begin
  if FEnabled then
  begin
    TempList := TList.Create;
    try
      Success := Desktop.BindToObject(FullID, nil, IID_IShellFolder, Pointer(Folder));
      if (Success <> S_OK) and (Assigned(FOnError)) then
        FOnError(Success, 'Failed to retrieve an IShellFolder object for a subfolder!');
      Success := Folder.EnumObjects(Application.Handle, SHCONTF_FOLDERS or SHCONTF_NONFOLDERS,
        EnumList);
      if (Success <> S_OK) and (Assigned(FOnError)) then
        FOnError(Success, 'Failed Enum objects!');
      while EnumList.Next(1, ID, NumIDs) = S_OK do
      begin
        if not Channels and IsChannel(ChannelShortCut, Folder, ID) then
          Continue;
        Item := New(PItem);
        Item.ID := CopyPidl(ID);
        Item.FullID := ConcatPIDLs(FullID, ID);
        Item.Folder := IsFolderEx(ChannelShortCut, Folder, ID);
        Item.Created := False;
        TempList.Add(Item);
      end;
      DisposePidl(ID);
      if TempList.Count = 0 then
      begin
        AddEmpty(Menu);
        Exit;
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
        if (J = 3) and (CheckShortCut(SFGAO_LINK, SFGAO_DISPLAYATTRMASK)) then
        begin
          ShGetFileInfo('*.htm', FILE_ATTRIBUTE_NORMAL, sfi, SizeOf(sfi),
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

    finally
      TempList.Free;
    end;
  end;
end;

procedure TFavoritesMenu.MenuClick(Sender: TObject);
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
    if PItem(list[(Sender as TMenuItem).Tag])^.folder then
    begin
      if not PItem(list[(Sender as TMenuItem).Tag]).Created then
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
        FOnError(Success, 'Failed to retrieve an IShellFolder object for a subfolder!');
      SHGetFileInfo(PChar(PItem(list[(Sender as TMenuItem).Tag])^.ID), 0,
        FileInfo, SizeOf(TSHFileInfo), SHGFI_PIDL or SHGFI_TYPENAME or SHGFI_ATTRIBUTES);
      if Fileinfo.szTypeName = ChannelShortcut then
        ResolveChannel(Folder, PItem(list[(Sender as TMenuItem).Tag])^.ID, Url)
      else
        if fileinfo.szTypeName = InternetShortCut then
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

procedure TFavoritesMenu.BuildOptionsMenu;
begin
  if FEnabled and not (csDesigning in ComponentState) then
  begin
    if foAddFavorites in FOptions then
      FavoritesMenu.Add(NewItem(FLocalization.FAddFavorites, 0,
        False, True, addfavorite, 0, ''));
    if foOrganizeFavorites in FOptions then
      FavoritesMenu.Add(NewItem(FLocalization.FOrganizeFavorites, 0,
        False, True, organizefavorite, 0, ''));
    if FavoritesMenu.Count > 0 then
      FavoritesMenu.Add(NewItem('-', 0, False, True, nil, 0, ''));
    if IE5_Installed then
    begin
      if foImportFavorites in FOptions then
        FavoritesMenu.Add(NewItem(FLocalization.FImportFavorites, 0,
          False, True, FavoritesImport, 0, ''));
      if foExportFavorites in FOptions then
        FavoritesMenu.Add(NewItem(FLocalization.FExportFavorites, 0,
          False, True, FavoritesExport, 0, ''));
      if foImportExportWizard in FOptions then
        FavoritesMenu.Add(NewItem(FLocalization.FImportExportWizard, 0,
          False, True, ImportExportWizard, 0, ''));

      if (foImportFavorites in FOptions) or (foExportFavorites in FOptions) then
        FavoritesMenu.Add(NewItem('-', 0, False, True, nil, 0, ''));
    end;
  end;
end;

procedure TFavoritesMenu.CreateMenu;
var
  FileInfo: TSHFileInfo;
begin
  if FEnabled and not (csDesigning in ComponentState) then
  begin
    Counter := 0;
    List := TList.Create;
    Images := TImagelist.Create(self);
    Images.ShareImages := True;
    Images.DrawingStyle := dsTransparent;
    Images.Handle := SHGetFileInfo(Pchar(FavoritesPidl), 0, FileInfo,
      SizeOf(FileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
    FavoritesMenu := TMenuitem.Create(self);
    with FavoritesMenu do
    begin
      SubMenuImages := Images;
      Caption := FCaption;
    end;
    if Assigned(FMainMenu) then
    begin
      if FMenuPosition > FMainMenu.Items.Count + 1 then
        FMenuPosition := FMainMenu.Items.Count + 1
      else
        if FMenuPosition <= 0 then
          FMenuPosition := 1;
      FMainMenu.Items.Insert(FMenuPosition - 1, FavoritesMenu);
    end;
    BuildOptionsMenu;
    AddMenu(FavoritesMenu, FavoritesPidl);
  end;
end;

procedure TFavoritesMenu.CreateMenu(mi: TMenuItem);
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
      FavoritesMenu := TMenuitem.Create(self);
      FavoritesMenu.Caption := FCaption;
    end
    else
      FavoritesMenu := mi;
    FavoritesMenu.SubmenuImages := Images;
    if Assigned(FMainMenu) then
    begin
      if FMenuPosition > FMainMenu.Items.Count + 1 then
        FMenuPosition := FMainMenu.Items.Count + 1
      else
        if FMenuPosition <= 0 then
          FMenuPosition := 1;
      FMainMenu.Items.Insert(FMenuPosition - 1, FavoritesMenu);
    end;
    BuildOptionsMenu;
    AddMenu(FavoritesMenu, FavoritesPidl);
  end;
end;

procedure TFavoritesMenu.ReBuildMenu;
begin
  DestroyList;
  List := TList.Create;
  FavoritesMenu.Clear;
  BuildOptionsMenu;
  AddMenu(FavoritesMenu, FavoritesPidl);
end;

end.

