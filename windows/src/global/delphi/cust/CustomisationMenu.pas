(*
  Name:             CustomisationMenu
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    14 Sep 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add MouseButton property
                    14 Sep 2006 - mcdurdin - Add Location property
*)
unit CustomisationMenu;

interface

uses
  Classes,
  custinterfaces,
  CustomisationStorage,
  Graphics,
  StockFileNames;


type
  TCustomisationMenuItem = class(TCollectionItem)
  private
    FCmdLine: string;
    FCaption: WideString;
    FAction: TCustomisationMenuItemAction;
    FPictureDefault: TPicture;
    FPictureSelected: TPicture;
    FHotKey: TShortCut;
    FItemType: TCustomisationMenuItemType;
    FFont: TFont;
    FIcon: TPicture;
    FLocation: TCustomisationMenuItemLocation;
    procedure SetPictureDefault(const Value: TPicture);
    procedure SetPictureSelected(const Value: TPicture);
    procedure SetFont(const Value: TFont);
    procedure SetIcon(const Value: TPicture);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property MouseButton: TCustomisationMenuItemLocation read FLocation write FLocation default milLeft;
    property Location: TCustomisationMenuItemLocation read FLocation write FLocation default milLeft;
    property ItemType: TCustomisationMenuItemType read FItemType write FItemType;
    property Caption: WideString read FCaption write FCaption;
    property Icon: TPicture read FIcon write SetIcon;
    property PictureDefault: TPicture read FPictureDefault write SetPictureDefault;
    property PictureSelected: TPicture read FPictureSelected write SetPictureSelected;
    property Action: TCustomisationMenuItemAction read FAction write FAction;
    property CmdLine: string read FCmdLine write FCmdLine;
    property HotKey: TShortCut read FHotKey write FHotKey;
    property Font: TFont read FFont write SetFont;
  end;

  TCustomisationMenuItems = class(TCollection)
  private
    function GetItem(Index: Integer): TCustomisationMenuItem;
    procedure SetItem(Index: Integer; const Value: TCustomisationMenuItem);
  public
    constructor Create; reintroduce;
    function Add: TCustomisationMenuItem;
    property Items[Index: Integer]: TCustomisationMenuItem read GetItem write SetItem; default;
  end;

  TCustomisationMenu = class(TComponent)
  private
    FCustStorage: TCustomisationStorage;
    FCustFile: TCustFile;
    FItems: TCustomisationMenuItems;
    procedure SetItems(const Value: TCustomisationMenuItems);
  public
    constructor Create(ACustStorage: TCustomisationStorage); reintroduce;
    destructor Destroy; override;
    procedure Load;
    procedure Save;
  published
    property Items: TCustomisationMenuItems read FItems write SetItems;
  end;

implementation

uses
  KLog,
  SysUtils;

{ TCustomisationMenuItem }

constructor TCustomisationMenuItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFont := TFont.Create;
  FIcon := TPicture.Create;
  FPictureDefault := TPicture.Create;
  FPictureSelected := TPicture.Create;
  FLocation := milLeft;
end;

destructor TCustomisationMenuItem.Destroy;
begin
  FreeAndNil(FPictureDefault);
  FreeAndNil(FPictureSelected);
  FreeAndNil(FIcon);
  FreeAndNil(FFont);
  inherited Destroy;
end;

procedure TCustomisationMenuItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomisationMenuItem.SetIcon(const Value: TPicture);
begin
  FIcon.Assign(Value);
end;

procedure TCustomisationMenuItem.SetPictureDefault(const Value: TPicture);
begin
  FPictureDefault.Assign(Value);
end;

procedure TCustomisationMenuItem.SetPictureSelected(const Value: TPicture);
begin
  FPictureSelected.Assign(Value);
end;

{ TCustomisationMenuItems }

function TCustomisationMenuItems.Add: TCustomisationMenuItem;
begin
  Result := inherited Add as TCustomisationMenuItem;
end;

constructor TCustomisationMenuItems.Create;
begin
  inherited Create(TCustomisationMenuItem);
end;

function TCustomisationMenuItems.GetItem(Index: Integer): TCustomisationMenuItem;
begin
  Result := inherited GetItem(Index) as TCustomisationMenuItem;
end;

procedure TCustomisationMenuItems.SetItem(Index: Integer; const Value: TCustomisationMenuItem);
begin
  inherited SetItem(Index, Value);
end;

{ TCustomisationMenu }

constructor TCustomisationMenu.Create(ACustStorage: TCustomisationStorage);
begin
  inherited Create(nil);
  FCustStorage := ACustStorage;
  FItems := TCustomisationMenuItems.Create;
  Load;
end;

destructor TCustomisationMenu.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TCustomisationMenu.Load;
var
  n: Integer;
  ms: TMemoryStream;
begin
  n := FCustStorage.CustFiles.IndexOfFileName(StockFileName_Menu);
  if n < 0 then
  begin
    with FCustStorage.CustFiles.AddCustFile do
    begin
      FileName := StockFileName_Menu;
    end;
    n := FCustStorage.CustFiles.IndexOfFileName(StockFileName_Menu);
  end;
  FCustFile := FCustStorage.CustFiles[n];

  FCustFile.Stream.Position := 0;
  if FCustFile.Stream.Size > 0 then
    try
      ms := TMemoryStream.Create;
      try
        ObjectTextToBinary(FCustFile.Stream, ms);
        ms.Position := 0;
        ms.ReadComponent(Self);
      finally
        ms.Free;
      end;
    except
      on E:Exception do
      begin
        //ExceptionHook.LogException;
        KL.LogError('Exception %s loading menu: %s', [E.ClassName, E.Message]);
      end;
    end;
end;

procedure TCustomisationMenu.Save;
begin
  FCustFile.Stream.Size := 0;
  FCustFile.Stream.WriteComponent(Self);
end;

procedure TCustomisationMenu.SetItems(const Value: TCustomisationMenuItems);
begin
  FItems.Assign(Value);
end;

end.
 
