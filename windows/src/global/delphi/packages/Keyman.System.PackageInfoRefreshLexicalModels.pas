unit Keyman.System.PackageInfoRefreshLexicalModels;

interface

uses
  System.Types,

  PackageInfo,
  Keyman.Developer.System.Project.ProjectLog,
  utilfiletypes;

type
  TPackageInfoRefreshLexicalModels = class
  private
    pack: TPackage;
    FOnError: TCompilePackageMessageEvent;

    procedure DoError(msg: string; State: TProjectLogState);
    function CheckLexicalModelLanguages: Boolean;
    function DoesFileMatchLexicalModelID(f: TPackageContentFile;
      const id: string): Boolean;
    function FindLexicalModelByFileName(const name: string): TPackageLexicalModel;
    function FindLexicalModelFileByID(const id: string): TPackageContentFile;
    function IsLexicalModelFileByContent(f: TPackageContentFile): Boolean;
    function IsLexicalModelFileByName(f: TPackageContentFile): Boolean;
  public
    constructor Create(Apack: TPackage);
    function Execute: Boolean;

    property OnError: TCompilePackageMessageEvent read FOnError write FOnError;
  end;

implementation

uses
  System.Classes,
  System.RegularExpressions,
  System.SysUtils,

  BCP47Tag,

  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.LexicalModelUtils,
  Keyman.System.LanguageCodeUtils,
  kmxfile;

const
  SError_LexicalModelMustHaveAVersion = 'A version number must be specified for lexical model ''%0:s''.';
  SError_LexicalModelMustHaveAtLeastOneLanguage = 'At least one language must be specified for lexical model ''%0:s''.';
  SError_LanguageTagIsNotValid = 'Lexical model with ID ''%0:s'' has a BCP 47 error: %2:s.';
  SWarning_LanguageTagIsNotCanonical = 'Lexical model with ID ''%0:s'' has a BCP 47 warning: %2:s.';

constructor TPackageInfoRefreshLexicalModels.Create(Apack: TPackage);
begin
  inherited Create;
  pack := Apack;
end;

procedure TPackageInfoRefreshLexicalModels.DoError(msg: string;
  State: TProjectLogState);
begin
  if Assigned(FOnError) then
    FOnError(Self, msg, State);
end;

function TPackageInfoRefreshLexicalModels.Execute: Boolean;
var
  i: Integer;
  lm: TPackageLexicalModel;
begin
  // Remove keyboards that do not have corresponding files
  for i := pack.LexicalModels.Count - 1 downto 0 do
  begin
    if FindLexicalModelFileByID(pack.LexicalModels[i].ID) = nil then
      pack.LexicalModels.Delete(i);
  end;

  // Add and update remaining keyboards
  for i := 0 to pack.Files.Count - 1 do
  begin
    if IsLexicalModelFileByName(pack.Files[i]) then
    begin
      // If the lexical model already exists, don't change any information because
      // it may have been edited by the user.

      lm := FindLexicalModelByFileName(pack.Files[i].FileName);
      if not Assigned(lm) then
      begin
        lm := TPackageLexicalModel.Create(pack);
        lm.ID := TLexicalModelUtils.LexicalModelFilenameToId(pack.Files[i].FileName);
        pack.LexicalModels.Add(lm);
      end;
    end;
  end;

  Result := CheckLexicalModelLanguages; // Always check all for comprehensive error messagess
end;

function TPackageInfoRefreshLexicalModels.CheckLexicalModelLanguages: Boolean;
var
  lm: TPackageLexicalModel;
  lang: TPackageKeyboardLanguage;
  msg: string;
begin
  Result := True;

  for lm in pack.LexicalModels do
  begin
    if lm.Languages.Count = 0 then
      DoError(Format(SError_LexicalModelMustHaveAtLeastOneLanguage, [lm.ID]), plsError);
    for lang in lm.Languages do
    begin
      with TBCP47Tag.Create(lang.ID) do
      try
        if not IsValid(True, msg) then
        begin
          DoError(Format(SError_LanguageTagIsNotValid, [lm.ID, lang.ID, msg]), plsError);
          Result := False;
        end
        else if not TCanonicalLanguageCodeUtils.IsCanonical(tag, msg) then
        begin

          DoError(Format(SWarning_LanguageTagIsNotCanonical, [lm.ID, lang.ID, msg]), plsWarning);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

function TPackageInfoRefreshLexicalModels.IsLexicalModelFileByContent(f: TPackageContentFile): Boolean;
begin
  // Look for "LMLayerWorker.loadModel"
  with TStringStream.Create('', TEncoding.UTF8) do
  try
    LoadFromFile(f.FileName);
    Result := TRegEx.IsMatch(DataString, '\bLMLayerWorker.loadModel\b', []);
  finally
    Free;
  end;
end;

function TPackageInfoRefreshLexicalModels.IsLexicalModelFileByName(f: TPackageContentFile): Boolean;
begin
  // Need to test if the JS is a valid lexical model file.
  // This is a bit of a pain... but we have to stick with .js for compat
  Result := TLexicalModelUtils.DoesJSFilenameFollowLexicalModelConventions(f.FileName) and
    (not FileExists(f.FileName) or IsLexicalModelFileByContent(f));
end;

function TPackageInfoRefreshLexicalModels.FindLexicalModelFileByID(const id: string): TPackageContentFile;
var
  i: Integer;
begin
  for i := 0 to pack.Files.Count - 1 do
  begin
    if DoesFileMatchLexicalModelID(pack.Files[i], id) then
      Exit(pack.Files[i]);
  end;

  Result := nil;
end;

function TPackageInfoRefreshLexicalModels.DoesFileMatchLexicalModelID(f: TPackageContentFile; const id: string): Boolean;
begin
  Result := TLexicalModelUtils.LexicalModelIDToFileName(id) = ExtractFileName(f.FileName);  // case sensitive match
end;

function TPackageInfoRefreshLexicalModels.FindLexicalModelByFileName(const name: string): TPackageLexicalModel;
var
  s: string;
begin
  s := TLexicalModelUtils.LexicalModelFileNameToID(name);
  Result := pack.LexicalModels.ItemByID(s);
end;

end.
