unit keymankeyboardlanguagesfile;

interface

uses
  Windows, ActiveX, ComObj, keymanapi_TLB, StdVcl, keymanautoobject, KeymanContext,
  keymanerrorcodes,
  PackageInfo,
  internalinterfaces;

type
  TKeymanKeyboardLanguageFileList = TAutoObjectList;

  TKeymanKeyboardLanguagesFile = class(TKeymanAutoCollectionObject, IKeymanKeyboardLanguagesFile)   // I4169
  private
    FLanguages: TKeymanKeyboardLanguageFileList;
    FOwner: IKeymanKeyboardFile;
  protected
    procedure DoRefresh; override;

    { IKeymanKeyboardLanguagesFile }
    function Get_Items(Index: Integer): IKeymanKeyboardLanguage; safecall;
  public
    constructor Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardFile; APackageKeyboardLanguages: TPackageKeyboardLanguageList);
    destructor Destroy; override;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  System.Variants,

  ErrorControlledRegistry,

  keymankeyboardlanguagefile,
  KLog,
  RegistryKeys,
  utilkeyman;

{ TKeymanKeyboardLanguagesFile }

constructor TKeymanKeyboardLanguagesFile.Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardFile; APackageKeyboardLanguages: TPackageKeyboardLanguageList);
var
  i: Integer;
begin
  _SetContext(AContext);
  FOwner := AOwner;
  FLanguages := TKeymanKeyboardLanguageFileList.Create;
  inherited Create(AContext, IKeymanKeyboardLanguagesInstalled, FLanguages);
  // Just fill the list here because we never need to refresh static data anyway
  if not Assigned(APackageKeyboardLanguages) then
    Exit;
  for i := 0 to APackageKeyboardLanguages.Count - 1 do
  begin
    FLanguages.Add(TKeymanKeyboardLanguageFile.Create(AContext, AOwner, APackageKeyboardLanguages[i].ID, 0, APackageKeyboardLanguages[i].Name));
  end;
  Refresh;
end;

destructor TKeymanKeyboardLanguagesFile.Destroy;
begin
  FLanguages.Free;
  inherited Destroy;
end;

procedure TKeymanKeyboardLanguagesFile.DoRefresh;
begin
end;

function TKeymanKeyboardLanguagesFile.Get_Items(Index: Integer): IKeymanKeyboardLanguage;
begin
  if (Index < Get_Count) and (Index >= 0) then
    Result := FLanguages[Index] as IKeymanKeyboardLanguage
  else
    ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
end;

end.
