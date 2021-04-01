unit keymankeyboardlanguagesfile;

interface

uses
  System.Win.ComObj,
  System.Win.StdVCL,
  Winapi.ActiveX,
  Winapi.Windows,

  internalinterfaces,
  keymanapi_TLB,
  keymanautoobject,
  KeymanContext,
  keymanerrorcodes,
  kmxfile,
  PackageInfo;

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
    constructor Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardFile; APackageKeyboardLanguages: TPackageKeyboardLanguageList;
      AKeyboardInfo: PKeyboardInfo);
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
  Keyman.System.MitigateWin10_1803LanguageInstall,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.LanguageCodeUtils,
  KLog,
  RegistryKeys,
  utilkeyman;

{ TKeymanKeyboardLanguagesFile }

constructor TKeymanKeyboardLanguagesFile.Create(AContext: TKeymanContext; AOwner: IKeymanKeyboardFile; APackageKeyboardLanguages: TPackageKeyboardLanguageList;
  AKeyboardInfo: PKeyboardInfo);
var
  i: Integer;
  FCanonicalBCP47Tag: string;
  langs: TArray<Integer>;
  lang: Integer;
  ml: TMitigateWin10_1803.TMitigatedLanguage;
  FCanonicalBCP4Tag: string;
  FLangID: Integer;
begin
  _SetContext(AContext);
  FOwner := AOwner;
  FLanguages := TKeymanKeyboardLanguageFileList.Create;
  inherited Create(AContext, IKeymanKeyboardLanguagesInstalled, FLanguages);
  // Just fill the list here because we never need to refresh static data anyway
  if Assigned(APackageKeyboardLanguages) then
  begin
    for i := 0 to APackageKeyboardLanguages.Count - 1 do
    begin
      FCanonicalBCP47Tag := TCanonicalLanguageCodeUtils.FindBestTag(APackageKeyboardLanguages[i].ID, True, True);
      if (FCanonicalBCP47Tag <> '') and (IndexOfBCP47Code(FCanonicalBCP47Tag) < 0) then
        FLanguages.Add(TKeymanKeyboardLanguageFile.Create(AContext, AOwner, FCanonicalBCP47Tag, 0,
          APackageKeyboardLanguages[i].Name));
    end;
  end
  else if Assigned(AKeyboardInfo) then
  begin
    langs := GetLanguageCodesFromKeyboard(AKeyboardInfo^);
    for lang in langs do
    begin
      if TMitigateWin10_1803.IsMitigationRequired(lang, ml) then
      begin
        FCanonicalBCP4Tag := ml.NewLanguage.BCP47;
        FLangID := ml.NewLanguage.Code;
      end
      else
      begin
        FCanonicalBCP47Tag := TLanguageCodeUtils.TranslateWindowsLanguagesToBCP47(lang);
        FLangID := lang;
      end;

      if FCanonicalBCP47Tag <> '' then
      begin
        FLanguages.Add(TKeymanKeyboardLanguageFile.Create(AContext, AOwner, FCanonicalBCP47Tag, FLangID, ''));
      end;
    end;
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
