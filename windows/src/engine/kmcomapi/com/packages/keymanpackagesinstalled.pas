(*
  Name:             keymanpackagesinstalled
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Fix IKeymanPackagesInstalled reference
                    04 Jan 2007 - mcdurdin - Test if package file exists when installing
                    19 Mar 2007 - mcdurdin - I701 - fix leaked registry object
                    23 Aug 2007 - mcdurdin - I923 - Handle corrupted files without crashing
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    29 Mar 2010 - mcdurdin - I2299 - Internal crash causes keyboard list to not refresh
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit keymanpackagesinstalled;

interface

uses
  Windows, ActiveX, ComObj, keymanapi_TLB, KeymanContext, internalinterfaces, keymanautoobject, keymanpackageinstalled, StdVcl;

type
  TPackageList = class(TAutoObjectList)
  private
    function GetItem(Index: Integer): IIntKeymanPackageInstalled;
    procedure SetItem(Index: Integer; const Value: IIntKeymanPackageInstalled);
  public
    property Items[Index: Integer]: IIntKeymanPackageInstalled read GetItem write SetItem; default;
  end;

  TKeymanPackagesInstalled = class(TKeymanAutoCollectionObject, IKeymanPackagesInstalled, IKeymanPackagesInstalled2)
  private
    FPackages: TPackageList;
  protected
    procedure DoRefresh; override;

    { IKeymanPackagesInstalled }
    function Get_Items(Index: OleVariant): IKeymanPackageInstalled; safecall;

    function GetPackageFromFile(const Filename: WideString): IKeymanPackageFile; safecall;
    function IndexOf(const ID: WideString): Integer; safecall;
    procedure Install(const Filename: WideString; Force: WordBool); safecall;
    function Install2(const Filename: WideString; Force: WordBool): IKeymanPackageInstalled; safecall;
  public
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  ErrorControlledRegistry,
  RegistryKeys,
  utilfiletypes,
  kpinstallpackage,
  keymanerrorcodes,
  keymanpackagefile,
  KLog;

constructor TKeymanPackagesInstalled.Create(AContext: TKeymanContext);
begin
  FPackages := TPackageList.Create;
  inherited Create(AContext, IKeymanPackagesInstalled, FPackages);
  Refresh;
end;

destructor TKeymanPackagesInstalled.Destroy;
begin
  FPackages.Free;
  inherited Destroy;
end;

function TKeymanPackagesInstalled.Get_Items(Index: OleVariant): IKeymanPackageInstalled;
var
  i: Integer;
begin
  if VarType(Index) = varOleStr
    then i := IndexOf(Index)
    else i := Index;

  if (i < Get_Count) and (i >= 0)
    then Result := FPackages[i] as IKeymanPackageInstalled
    else ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([Index]));
end;

function TKeymanPackagesInstalled.IndexOf(const ID: WideString): Integer;
var
  i: Integer;
  s: string;
begin
  s := ExtractFileName(RemoveFileExtension(ID, Ext_PackageFile));
  for i := 0 to FPackages.Count - 1 do
    if AnsiCompareText((FPackages[i] as IKeymanPackage).ID, s) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TKeymanPackagesInstalled.Install(const Filename: WideString; Force: WordBool);
var
  o: TKPInstallPackageOptions;
begin
  KL.MethodEnter(Self, 'Install', [Filename, Force]);
  with TKPInstallPackage.Create(Context) do
  try
    o := [];
    if Force then
      Include(o, ipForce);
    Execute(Filename, o);
  finally
    Free;
  end;
  KL.MethodExit(Self, 'Install');
end;

function TKeymanPackagesInstalled.Install2(const Filename: WideString;
  Force: WordBool): IKeymanPackageInstalled;
var
  o: TKPInstallPackageOptions;
begin
  KL.MethodEnter(Self, 'Install2', [Filename, Force]);
  with TKPInstallPackage.Create(Context) do
  try
    o := [ipDontInstallLanguages];
    if Force then
      Include(o, ipForce);
    Execute(Filename, o);
  finally
    Free;
  end;

  DoRefresh;
  Result := Get_Items(Filename);

  KL.MethodExit(Self, 'Install2');
end;

{ TPackageList }

function TPackageList.GetItem(Index: Integer): IIntKeymanPackageInstalled;
begin
  Result := inherited GetItem(Index) as IIntKeymanPackageInstalled;
end;

procedure TPackageList.SetItem(Index: Integer; const Value: IIntKeymanPackageInstalled);
begin
  inherited SetItem(Index, Value);
end;

procedure TKeymanPackagesInstalled.DoRefresh;
var
  keys: TStringList;
  i: Integer;
begin
  FPackages.Clear;
  keys := TStringList.Create;
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_InstalledPackages_LM) then
    begin
      GetKeyNames(keys);
      for i := 0 to keys.Count - 1 do FPackages.Add(TKeymanPackageInstalled.Create(Context, keys[i]));
    end;
  finally
    keys.Free;
    Free;
  end;
end;

function TKeymanPackagesInstalled.GetPackageFromFile(const Filename: WideString): IKeymanPackageFile;
var
  p: TKeymanPackageFile;
begin
  ClearErrors;
  p := nil;

  try
    if not FileExists(Filename) then
      raise Exception.Create('File does not exist.');
    p := TKeymanPackageFile.Create(Context, Filename);
  except
    on E:Exception do
      ErrorFmt(KMN_E_Install_InvalidFile, VarArrayOf([Filename, E.Message]));
  end;

  Result := p;
end;

end.

