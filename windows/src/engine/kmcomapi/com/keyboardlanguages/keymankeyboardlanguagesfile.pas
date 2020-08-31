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
    function IndexOfBCP47Code(const BCP47Code: string): Integer;
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

  BCP47Tag,
  keymankeyboardlanguagefile,
  Keyman.System.CanonicalLanguageCodeUtils,
  KLog,
  RegistryKeys,
  utilkeyman;

{ TKeymanKeyboardLanguagesFile }

constructor TKeymanKeyboardLanguagesFile.Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardFile; APackageKeyboardLanguages: TPackageKeyboardLanguageList);
var
  i: Integer;
  FCanonicalBCP47Tag: string;
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
    FCanonicalBCP47Tag := TCanonicalLanguageCodeUtils.FindBestTag(APackageKeyboardLanguages[i].ID, True);
    if (FCanonicalBCP47Tag <> '') and (IndexOfBCP47Code(FCanonicalBCP47Tag) < 0) then
      FLanguages.Add(TKeymanKeyboardLanguageFile.Create(AContext, AOwner, FCanonicalBCP47Tag, 0,
        APackageKeyboardLanguages[i].Name));
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

function TKeymanKeyboardLanguagesFile.IndexOfBCP47Code(
  const BCP47Code: string): Integer;
var
  i: Integer;
begin
  for i := 0 to FLanguages.Count - 1 do
    if SameText(Get_Items(i).BCP47Code, BCP47Code) then
      Exit(i);
  Result := -1;
end;

function TKeymanKeyboardLanguagesFile.Get_Items(Index: Integer): IKeymanKeyboardLanguage;
begin
  if (Index < Get_Count) and (Index >= 0) then
    Result := FLanguages[Index] as IKeymanKeyboardLanguage
  else
    ErrorFmt(KMN_E_Collection_InvalidIndex, VarArrayOf([VarToStr(Index)]));
end;

end.
