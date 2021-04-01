(*
  Name:             keymanpackagecontentkeyboards
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Jan 2007

  Modified Date:    12 Mar 2010
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Jan 2007 - mcdurdin - Raise error if keyboard not found in collection with Get_Item
                    27 Mar 2008 - mcdurdin - I1192 - Fix crash starting multiple products
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
*)
unit keymanpackagecontentkeyboards;

interface

uses
  ComObj, ActiveX, keymanapi_TLB, internalinterfaces, KeymanContext, keymanautoobject, keymankeyboard, StdVcl, kmpinffile;

type
  TKeymanPackageContentKeyboards = class(TKeymanAutoCollectionObject, IKeymanPackageContentKeyboards)
  private
    FKeyboards: TKeyboardList;
  protected
    { IKeymanKeyboards }
    function IndexOf(const ID: WideString): Integer; safecall;

    { IKeymanPackageContentKeyboards }
    function Get_Items(Index: OleVariant): IKeymanKeyboard; safecall;
  public
    constructor Create(AContext: TKeymanContext; AOwnsObjects: Boolean); reintroduce;
    destructor Destroy; override;
  end;

  TKeymanPackageContentKeyboardsInstalled = class(TKeymanPackageContentKeyboards, IIntKeymanKeyboardsPackageInstalled)
  private
    FPackageName: WideString;
  protected
    procedure RefreshSource; { I1192 - 7.0.244.0 - fix crash when starting multiple products }
  public
    constructor Create(AContext: TKeymanContext; const APackageName: WideString); reintroduce;
  end;

  TKeymanPackageContentKeyboardsFile = class(TKeymanPackageContentKeyboards)
  public
    constructor Create(AContext: TKeymanContext; const ASourcePath: string; const kmpinf: TKMPInfFile); reintroduce;
  end;

implementation

uses
  Windows,
  utilkeyman,
  PackageInfo,
  ComServ, SysUtils, keymanerrorcodes, keymankeyboardinstalled, keymankeyboardfile, utilfiletypes, Variants;

{ TKeymanPackageContentKeyboards }

constructor TKeymanPackageContentKeyboards.Create(AContext: TKeymanContext; AOwnsObjects: Boolean);
begin
  _SetContext(AContext);
  FKeyboards := TKeyboardList.Create;
  inherited Create(AContext, IKeymanPackageContentKeyboards, FKeyboards);
end;

destructor TKeymanPackageContentKeyboards.Destroy;
begin
  FKeyboards.Free;
  inherited Destroy;
end;

function TKeymanPackageContentKeyboards.Get_Items(Index: OleVariant): IKeymanKeyboard;
var
  i: Integer;
begin
  if VarType(Index) = varOleStr
    then i := IndexOf(Index)
    else i := Index;

  if (i < Get_Count) and (i >= 0) then
    Result := FKeyboards[i] as IKeymanKeyboard
  else
    ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
end;

function TKeymanPackageContentKeyboards.IndexOf(const ID: WideString): Integer;
var
  i: Integer;
  s: string;
begin
  s := ExtractFileName(RemoveFileExtension(ID, Ext_KeymanFile));
  for i := 0 to FKeyboards.Count - 1 do
    if (AnsiCompareText((FKeyboards[i] as IKeymanKeyboard).ID, s) = 0) then
      Exit(i);

  Result := -1;
end;

{ TKeymanPackageContentKeyboardsInstalled }

constructor TKeymanPackageContentKeyboardsInstalled.Create(AContext: TKeymanContext; const APackageName: WideString);
begin
  inherited Create(AContext, False);
  FPackageName := APackageName;
  RefreshSource;
end;

{ I1192 - 7.0.244.0 - fix crash when starting multiple products }
procedure TKeymanPackageContentKeyboardsInstalled.RefreshSource;
var
  i: Integer;
begin
  FKeyboards.Clear;
  with Context do
    for i := 0 to (Keyboards as IKeymanKeyboardsInstalled).Count - 1 do
      with Keyboards.Items[i] as IIntKeymanKeyboardInstalled do
        if AnsiCompareText(RegKeyboard.PackageName, FPackageName) = 0 then
        begin
          FKeyboards.Add(Keyboards.Items[i]);
        end;
end;

{ TKeymanPackageContentKeyboardsFile }

constructor TKeymanPackageContentKeyboardsFile.Create(AContext: TKeymanContext; const ASourcePath: string; const kmpinf: TKMPInfFile);
var
  i: Integer;

  procedure AddKeyboard;
  var
    FPackageKeyboard: TPackageKeyboard;
  begin
    FPackageKeyboard := kmpinf.Keyboards.ItemByID(GetShortKeyboardName(kmpinf.Files[i].FileName));
    FKeyboards.Add(TKeymanKeyboardFile.Create(AContext, ASourcePath + kmpinf.Files[i].FileName, FPackageKeyboard));
  end;

begin
  inherited Create(AContext, True);

  for i := 0 to kmpinf.Files.Count - 1 do
    if kmpinf.Files[i].FileType = ftKeymanFile then
      AddKeyboard;
end;

end.

