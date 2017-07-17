//******************************************************************
//                                                                 *
//                       TFavoritesTree                            *
//                     Freeware Component                          *
//                     For Delphi 5 - DSelphi XE                   *
//                            by                                   *
//                       Pete Morris                               *
//                     and Eran Bodankin                           *
//                                                                 *
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
//$Id: FavoritesTree.pas,v 1.4 2006/12/05 11:56:31 bsalsa Exp $

unit FavoritesTree;

interface

{$I EWB.inc}

uses
  ShlObj, Messages, Windows, SysUtils, Classes, Forms, ComCtrls, DIRMonitor,
  iniFiles, EmbeddedWB, EwbCore, Controls, Imglist,
  ExportFavorites, ImportFavorites, EwbAcc, EWBTools;

type
  TNodeType = (ntRoot, ntItem, ntEmptyFolder, ntFolder, ntOrganizeFavorites,
    ntAddToFavorites, ntImportFavorites, ntExportFavorites, ntTools);
  TFavoriteOption = (foShowRoot, foShowItems, foShowOrganize, foShowAdd,
    foShowImport, foShowExport);
  TFavoriteOptions = set of TFavoriteOption;
  TNodeAddedEvent = procedure(Sender: TObject; const aNode: TTreeNode;
    aNodeType: TNodeType) of object;
  TNodeMissingEvent = procedure(Sender: TObject; const aNode: TTreeNode;
    aNodeType: TNodeType) of object;
  TNavigateEvent = procedure(Sender: TObject; const Url: string) of object;
  TPopupMenuMode = (pmm_System, pmm_PopupMenu);


  TLocalization = class(TPersistent)
  private
    FNodeOrganiseFavorites: string;
    FNodeAddToFavorites: string;
    FNodeImportFavorites: string;
    FNodeExportFavorites: string;
    FNodeTools: string;
    FNodeFavorites: string;
    FTextLinks: string;
    FTextImported: string;
  published
    property NodeOrganiseFavorites: string read FNodeOrganiseFavorites write FNodeOrganiseFavorites;
    property NodeAddToFavorites: string read FNodeAddToFavorites write FNodeAddToFavorites;
    property NodeImportFavorites: string read FNodeImportFavorites write FNodeImportFavorites;
    property NodeExportFavorites: string read FNodeExportFavorites write FNodeExportFavorites;
    property NodeTools: string read FNodeTools write FNodeTools;
    property NodeFavorites: string read FNodeFavorites write FNodeFavorites;
    property TextLinks: string read FTextLinks write FTextLinks;
    property TextImported: string read FTextImported write FTextImported;
  end;

  TCustomFavoritesTree = class(TCustomTreeView)
  private
    foldericon: Integer;
    myRootNode, myRootNode2: TTreeNode;
    lFolder: PItemIDList;
    lPath: array[0..MAX_PATH] of char;
    FavIndex: integer;
    FOptions: TFavoriteOptions;
    FPath: string;
    ImageList: TImageList;
    FEmbeddedWB: TCustomEmbeddedWB;
    fExportFavorites: TExportFavorite;
    FImportFavorites: TImportFavorite;
    FOnNavigate: TNavigateEvent;
    FOnNodeAdded: TNodeAddedEvent;
    FOnNodeMissing: TNodeMissingEvent;
    FOnFavoritesChanged: TNotifyEvent;
    FPopupMenuMode: TPopupMenuMode;
    FLocalization: TLocalization;
    procedure SetOption(const Value: TFavoriteOptions);
    procedure CallDoOrganizeFavDlg(const DllName: string);
  protected
    procedure DblClick; override;
    function InternalAdd(const aParent: TTreeNode; const aCaption: string; const
      aNodeType: TNodeType): TTreeNode; virtual;
    procedure Loaded; override;
    property Options: TFavoriteOptions read FOptions write SetOption;
    property OnFavoritesChanged: TNotifyEvent read FOnFavoritesChanged write
      FOnFavoritesChanged;
    property OnNavigate: TNavigateEvent read FOnNavigate write FOnNavigate;
    property OnNodeAdded: TNodeAddedEvent read FOnNodeAdded write FOnNodeAdded;
    property OnNodeMissing: TNodeMissingEvent read FOnNodeMissing write
      FOnNodeMissing;
    procedure PopupSystemContextMenu(Node: TTreeNode; Point: TPoint);
    procedure WMContextMenu(var Msg: TMessage); message WM_Contextmenu;
  public
    constructor Create(AOwner: TComponent); override;
    procedure GetSelectedIndex(Node: TTreeNode); override;
    procedure NodeAdded(Sender: TObject;
      const aNode: TTreeNode; aNodeType: TNodeType);
    destructor Destroy; override;
    procedure AddToFavorites;
    procedure ExportTheFavorites;
    function GetFileName(const aNode: TTreeNode): string;
    procedure ImportTheFavorites;
    procedure OrganizeFavorites;
    procedure Refresh; dynamic;
    procedure RefreshFolder(const aFolder: TTreeNode); dynamic;
    function NodeURL(const aNode: TTreeNode): string;
  published

    property PopupMenuMode: TPopupMenuMode read FPopupMenuMode write
      FPopupMenuMode;
    property EmbeddedWB: TCustomEmbeddedWB read FEmbeddedWB write FEmbeddedWB;
    property ImportFavorites: TImportFavorite read FImportFavorites write
      FImportFavorites;
    property ExportFavorites: TExportFavorite read fExportFavorites write
      FExportFavorites;
    property Localization: TLocalization read FLocalization write FLocalization;
  end;

  TFavoritesTree = class(TCustomFavoritesTree)
  private
  published
    //new properties
    property Options;
    //inherited properties
    property Align;
    property Anchors;
    property BiDiMode;
    property BorderStyle;
    property BorderWidth;
    property ChangeDelay;
    property Color;
    property Constraints;
    property Cursor;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    //  property Expand;
    property Font;
    property Height;
    property HelpContext;
    property HideSelection;
    property Hint;
    property HotTrack;
    property Images;
    property Indent;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property PopupMenu;
    property RightClickSelect;
    property RowSelect;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop;
    property Tag;
    property ToolTips;
    property Top;
    property Visible;
    property Width;
    //NewEvents
    property OnFavoritesChanged;
    //Called when you another app alters the favorites
    property OnNavigate; //When you need to navigate somewhere
    property OnNodeAdded;
    //When a node is added, so you can set the image indexes if you like
    property OnNodeMissing;
    //When a node is clicked, but someone else has deleted the file/folder
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnContextPopup;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEdited;
    property OnEditing;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

var
  Folder: IShellFolder;


implementation
uses
{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
  MenuContext, SHDocVw_EWB, ComObj, ActiveX, FileCtrl, Registry, ShellApi;

{ TCustomFavoritesTree }

procedure TCustomFavoritesTree.GetSelectedIndex(Node: TTreeNode);
begin
  inherited;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TCustomFavoritesTree.NodeAdded(Sender: TObject;
  const aNode: TTreeNode; aNodeType: TNodeType);
begin
  inherited;
end;

constructor TCustomFavoritesTree.Create(AOwner: TComponent);
var FileInfo: TSHFileInfo;
begin
  inherited;

  FLocalization := TLocalization.Create;
  FLocalization.NodeOrganiseFavorites := 'Organize favorites';
  FLocalization.NodeAddToFavorites := 'Add To favorites';
  FLocalization.NodeImportFavorites := 'Import favorites';
  FLocalization.NodeExportFavorites := 'Export favorites';
  FLocalization.NodeTools := 'Tools';
  FLocalization.NodeFavorites := 'Favorites';
  FLocalization.TextLinks := 'Links';
  FLocalization.TextImported := 'Imported';

  SHGetSpecialFolderLocation(0, CSIDL_FAVORITES, lFolder);
  SHGetPathFromIDList(lFolder, lPath);
  //SHGetSpecialFolderLocation(Application.Handle, CSIDL_FAVORITES, FavoritesPIDL);

  FPath := StrPas(lPath);
  if FPath[Length(FPath)] <> '\' then
    FPath := FPath + '\';
  FOptions := [foShowRoot, foShowItems, foShowOrganize, foShowAdd, foShowImport,
    foShowExport];
  ShowRoot := True;
  ImageList := TImagelist.Create(Self);
  ImageList.ShareImages := True;
//  ImageList.DrawingStyle := dsTransparent;
  ImageList.Handle := SHGetFileInfo(Pchar(lFolder), 0, FileInfo,
    SizeOf(FileInfo), SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_SMALLICON);
  Self.Images := ImageList;
  foldericon := -1;
end;

procedure TCustomFavoritesTree.Loaded;
begin
  inherited;
  ShowRoot := True;
  Refresh;
end;

destructor TCustomFavoritesTree.Destroy;
begin
  FLocalization.Free;
  ImageList.Free;
  inherited;
end;

function TCustomFavoritesTree.NodeURL(const aNode: TTreeNode): string;
const
  CLSID_InternetShortCut: TGUID = (d1: $FBF23B40; D2: $E3F0; D3: $101B; D4:
    ($84, $88, $00, $AA, $00, $3E, $56, $F8));
var
  FileName: string;
  FName: array[0..MAX_PATH] of WideChar;
  PURL: Pchar;
  IUrl: IUniformResourceLocator;
  PersistFile: IPersistFile;
begin
  FileName := GetFileName(aNode);
  IUrl := CreateComObject(CLSID_InternetShortCut) as IUniformResourceLocator;
  PersistFile := IUrl as IPersistFile;
  StringToWideChar(FileName, FName, MAX_PATH);
  PersistFile.Load(FName, STGM_READ);
  IUrl.GetURL(@PURL);
  Result := PURL;
end;

function TCustomFavoritesTree.GetFileName(const aNode: TTreeNode): string;
begin
  if (aNode = nil) or ((aNode = Items[FavIndex]) {and  (foShowRoot in Options)}) then
    Result := FPath
  else
  begin
    case TNodeType(aNode.Data) of
      ntItem: Result := GetFileName(aNode.Parent) + aNode.Text + '.Url';
      ntFolder: Result := GetFileName(aNode.Parent) + aNode.Text + '\';
      ntEmptyFolder: Result := GetFileName(aNode.Parent) + aNode.Text + '\';
      ntTools: Result := GetFileName(aNode.Parent) + aNode.Text + '\';
    end;
  end;
end;

procedure TCustomFavoritesTree.RefreshFolder(const aFolder: TTreeNode);
var
  CurrentPath: string;
  SR: TSearchRec;
  Found: Integer;
  newnode: TTreeNode;
  sfi: TShFileInfo;
  i: Integer;
begin
  CurrentPath := GetFileName(aFolder);
  Found := FindFirst(CurrentPath + '*.*', faDirectory, SR);

  while Found = 0 do
  begin
    if (SR.Attr and faDirectory <> 0) and (SR.Name <> '.') and (SR.Name <>
      '..') then
    begin
      newnode := InternalAdd(aFolder, SR.Name, ntEmptyFolder);
      ShGetFileInfo('*.*', FILE_ATTRIBUTE_DIRECTORY, sfi, sizeOf(sfi),
        SHGFI_SYSICONINDEX or SHGFI_USEFILEATTRIBUTES or SHGFI_SMALLICON);
      if foldericon < 0 then foldericon := sfi.iIcon;
      newnode.ImageIndex := sfi.iIcon;
      DestroyIcon(sfi.iIcon);
    end;
    Found := FindNext(SR);
  end;
  FindClose(SR);

  if foShowItems in Options then
  begin
    Found := FindFirst(CurrentPath + '*.Url', faAnyFile, SR);
    while Found = 0 do
    begin
      if (SR.Attr and faDirectory = 0) then
      begin
        newnode := InternalAdd(aFolder, Copy(SR.Name, 1, Length(SR.Name) - 4),
          ntItem);
        {$IFDEF UNICODE}
        ShGetFileInfo(LPWSTR((CurrentPath + SR.Name)), FILE_ATTRIBUTE_NORMAL, sfi, SizeOf(sfi),
          SHGFI_SYSICONINDEX or
          SHGFI_USEFILEATTRIBUTES or
          SHGFI_SMALLICON);
        {$ELSE}
        ShGetFileInfo(LPCSTR((CurrentPath + SR.Name)), FILE_ATTRIBUTE_NORMAL, sfi, SizeOf(sfi),
          SHGFI_SYSICONINDEX or
          SHGFI_USEFILEATTRIBUTES or
          SHGFI_SMALLICON);
            {$ENDIF}
        if sfi.iIcon > 3 then
        begin
          newnode.ImageIndex := sfi.iIcon;
        end
        else
        begin
          ShGetFileInfo('*.htm', FILE_ATTRIBUTE_NORMAL, sfi, SizeOf(sfi),
            SHGFI_SYSICONINDEX or
            SHGFI_USEFILEATTRIBUTES or
            SHGFI_SMALLICON);
          newnode.ImageIndex := sfi.iIcon;
        end;
        DestroyIcon(sfi.iIcon);
      end;
      Found := FindNext(SR);
    end;
    FindClose(SR);
  end;

  if aFolder <> nil then
    if TNodeType(aFolder.Data) = ntEmptyFolder then
      aFolder.Data := Pointer(ntFolder);

  if aFolder <> nil then
    if TNodeType(aFolder.Data) = ntFolder then
    begin
      aFolder.HasChildren := True;
      aFolder.Data := Pointer(ntFolder);
      aFolder.Expand(True);
    end;

  for i := 0 to myRootNode.Count do
    Self.Items[i].ImageIndex := foldericon;

  myRootNode.ImageIndex := foldericon;
  myRootNode2.ImageIndex := foldericon;
end;

function TCustomFavoritesTree.InternalAdd(const aParent: TTreeNode;
  const aCaption: string; const aNodeType: TNodeType): TTreeNode;
begin
  Result := Items.AddChild(aParent, aCaption);
  Result.Data := Pointer(aNodeType);
  if Assigned(OnNodeAdded) then
    OnNodeAdded(Self, Result, aNodeType);
end;

procedure TCustomFavoritesTree.Refresh;
var
  RootNode, RootNode2: TTreeNode;
begin
  Items.BeginUpdate;
  try
    while Items.Count > 0 do
      Items[0].Delete;

    if foShowRoot in Options then
      RootNode := InternalAdd(nil, FLocalization.NodeTools, ntTools)
    else
      RootNode := nil;
    if foShowOrganize in Options then
      InternalAdd(RootNode, FLocalization.NodeOrganiseFavorites, ntOrganizeFavorites);
    if (foShowAdd in Options) then
      InternalAdd(RootNode, FLocalization.NodeAddToFavorites, ntAddToFavorites);
    if (foShowImport in Options) then
      InternalAdd(RootNode, FLocalization.NodeImportFavorites, ntImportFavorites);
    if (foShowExport in Options) then
      InternalAdd(RootNode, FLocalization.NodeExportFavorites, ntExportFavorites);
    if (RootNode <> nil) then
      RootNode.Expanded := True;
    if (Pos(FLocalization.TextLinks, RootNode.Text) > 0) or
      (Pos(FLocalization.TextImported, RootNode.Text) > 0) then
      RootNode.HasChildren := True;

    RootNode2 := InternalAdd(nil, FLocalization.NodeFavorites, ntRoot);
    FavIndex := RootNode.Count + 1;

    myRootNode := RootNode;
    myRootNode2 := RootNode2;
    RefreshFolder(RootNode2);
    RootNode2.Expand(True);
  finally
    Items.EndUpdate;
  end;
end;

procedure TCustomFavoritesTree.SetOption(const Value: TFavoriteOptions);
begin
  FOptions := Value;
end;

procedure TCustomFavoritesTree.ExportTheFavorites;
begin
  if Assigned(ExportFavorites) then
    FExportFavorites.ExportFavorites
  else
    Application.MessageBox('Please assign a TExportFavorites component to use this feature' + #13#10 +
      'or set Options.foshowExport to False.', 'Hint', MB_OK);
end;

procedure TCustomFavoritesTree.ImportTheFavorites;
begin
  if Assigned(ImportFavorites) then
  begin
    FImportFavorites.ImportFavorites;
    Refresh;
  end else
    Application.MessageBox('Please assign a TImportFavorites component to use this feature' + #13#10 +
      'or set Options.foshowImport to False.', 'Hint',MB_OK);
end;

procedure TCustomFavoritesTree.CallDoOrganizeFavDlg(const DllName: string);
var
  H: HWND;
  OrganizeFavDlg: procedure(Handle: THandle; Path: PAnsiChar); stdcall;
begin
  H := LoadLibrary(PChar(DllName));
  if H <> 0 then
  begin
    OrganizeFavDlg := GetProcAddress(H, 'DoOrganizeFavDlg');
    if Assigned(OrganizeFavDlg) then
      OrganizeFavDlg(Application.Handle, PAnsiChar(AnsiString(FPath)));
  end;
  FreeLibrary(H);
end;

procedure TCustomFavoritesTree.OrganizeFavorites;
begin
  if GetIEVersionMajor > 6 then
    CallDoOrganizeFavDlg('ieframe.dll')
  else
    CallDoOrganizeFavDlg('shdocvw.dll');
  Refresh;
end;

procedure AddToFav(const Url, Title: string);
const
  CLSID_SHELLUIHELPER: TGUID = '{64AB4BB7-111E-11D1-8F79-00C04FC2FBE1}';
var
  ShellUIHelper: ISHellUIHelper;
  Url1, Title1: OleVariant;
begin
  Title1 := Title;
  Url1 := Url;
  CoCreateInstance(CLSID_SHELLUIHELPER, nil, CLSCTX_INPROC_SERVER,
    IID_IShellUIHelper, ShellUIHelper);
  ShellUIHelper.AddFavorite(Url1, Title1);
end;

procedure TCustomFavoritesTree.AddToFavorites;
begin
  if Assigned(EmbeddedWB) then
  begin
    AddToFav(EmbeddedWB.LocationURL, EmbeddedWB.LocationName);
    Refresh;
  end;
end;

function URLFromShortcut(const dotURL: string): string;
begin
  with TIniFile.Create(dotURL) do
  try
    try
      Result := ReadString('InternetShortcut', 'Url', '');
    except;
      Result := '';
    end;
  finally
    Free;
  end;
end;

procedure TCustomFavoritesTree.DblClick;
var
  URLPath: WideString;
  X: OleVariant;
  Url: string;
begin
  inherited;
  if Selected = nil then Exit;

  case TNodeType(Selected.Data) of
    ntFolder, ntEmptyFolder:
      if not {$IFDEF DELPHI6_UP}SysUtils.{$ENDIF}DirectoryExists(GetFileName(Selected)) then
      begin
        if Assigned(OnNodeMissing) then
          OnNodeMissing(Self, Selected, TNodeType(Selected.Data));
        Selected.Delete;
        Exit;
      end;

    ntItem:
      if not FileExists(GetFileName(Selected)) then
      begin
        if Assigned(OnNodeMissing) then
          OnNodeMissing(Self, Selected, TNodeType(Selected.Data));
        Selected.Delete;
        Exit;
      end;
  end;

  case TNodeType(Selected.Data) of
    ntAddToFavorites: AddToFavorites;
    ntOrganizeFavorites: OrganizeFavorites;
    ntImportFavorites: ImportTheFavorites;
    ntExportFavorites: ExportTheFavorites;
    ntTools: Selected.Expand(True);
    ntEmptyFolder:
      begin
        RefreshFolder(Selected);
        Selected.Expand(False);
      end;
    ntItem:
      begin
        if Assigned(OnNavigate) then
          FOnNavigate(Self, NodeURL(Selected));
        if Assigned(EmbeddedWB) then
        begin
          URLPath := GetFileName(Selected);
          Url := URLFromShortcut(URLPath);
          EmbeddedWB.Navigate(Url, X, X, X, X);
        end;
      end;
  end;
end;

procedure TCustomFavoritesTree.PopupSystemContextMenu(Node: TTreeNode; Point:
  TPoint);
var
  ISF: IShellFolder;
  Pidl: PItemIdList;
  Par: TTreeNode;
begin
  if Assigned(Node) then
  begin
    Par := Node.Parent;
    if Assigned(Par) then
    begin
      Pidl := Node.Data;
      try
        MenuContext.DisplayContextMenu(ISF, Pidl, 0, Application.Handle, Point, 1);
      except
        GetLastError;
      end;
    end;
  end;
end;

procedure TCustomFavoritesTree.WMContextMenu(var Msg: TMessage);
var
  Point: TPoint;
  R: TRect;
  NewSel, SelItem: TTreeNode;
begin
  NewSel := nil;
  if Msg.lparam = -1 then
  begin
    SelItem := TTreeNode(Selected);
    if not Assigned(SelItem) then
      Exit;
    R := SelItem.DisplayRect(True);
    Point.X := R.Left + ((R.Right - R.Left) div 2);
    Point.Y := R.Top - ((R.Top - R.Bottom) div 2);
  end
  else
  begin
    Point.X := LOWORD(Msg.lParam);
    Point.Y := HIWORD(Msg.lParam);
    Point := ScreenToClient(Point);
    SelItem := Selected //TListItem(GetPosition(P.X,P.Y));
  end;
  Point := ClientToScreen(Point);
  if (Selected <> SelItem) then
    NewSel := TTreeNode(Selected);
  Selected := SelItem;
  if PopupMenuMode = pmm_PopupMenu then
  begin
    if Assigned(Popupmenu) then
    begin
      PopupMenu.PopupComponent := Self;
      PopupMenu.Popup(Point.X, Point.Y)
    end;
  end
  else
    PopupSystemContextMenu(SelItem, Point);

  if Assigned(NewSel) then
    Selected := NewSel;
end;

end.
