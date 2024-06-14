unit Keyman.System.PackageInfoRefreshKeyboards;

interface

uses
  System.Types,

  PackageInfo,
  Keyman.Developer.System.Project.ProjectLog,
  utilfiletypes;

type
  TPackageInfoRefreshKeyboards = class
  private
    pack: TPackage;
    FOnError: TCompilePackageMessageEvent;
    function FindKeyboardFileByID(const id: string): TPackageContentFile;
    function FindKeyboardFilesByID(const id: string): TIntegerDynArray;
    function IsKeyboardFileByName(f: TPackageContentFile): TKMFileType;
    function FindKeyboardByFileName(const name: string): TPackageKeyboard;

    procedure DoError(msg: string; State: TProjectLogState);
    function DoesFileMatchKeyboardID(f: TPackageContentFile;
      const id: string): Boolean;
    function IsKeyboardFileByContent(f: TPackageContentFile): Boolean;
    function CheckKeyboardLanguages: Boolean;
    function CheckKeyboardTargetVersions: Boolean;
    function IsModelFileByName(f: TPackageContentFile): Boolean;
  public
    type TPackageKeyboardInfo = record
      Name, ID, Version, MinKeymanVersion: string;
      RTL: Boolean;
    end;

    class function FillKeyboardDetails(f: TPackageContentFile;
      var pki: TPackageKeyboardInfo): Boolean; static;
    class procedure FillKeyboardLanguages(f: TPackageContentFile;
      k: TPackageKeyboard); static;

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
  Keyman.System.KMXFileLanguages,
  Keyman.System.KeyboardJSInfo,
  Keyman.System.KeyboardUtils,
  Keyman.System.LanguageCodeUtils,
  kmxfile;

const
  SError_KeyboardVersionsDoNotMatch = 'Keyboard with ID ''%0:s'' is included with two differing versions: ''%1:s'', ''%2:s''. The keyboard versions must be consistent.';
  SError_TooManyTargetFilesForKeyboardID = 'Keyboard with ID ''%0:s'' is included too many times (%1:d) in the package. You should have at most 1 .kmx and 1 .js file';
  SError_CannotHaveSameKeyboardTwiceWithSameTarget = 'Keyboard with ID ''%0:s'' should have at most 1 .kmx and 1 .js file in the package';
  SWarning_CannotFindKeyboardFile = 'Keyboard file ''%0:s'' cannot be found to verify file version. Skipping verification.';

  SError_LanguageTagIsNotValid = 'Keyboard with ID ''%0:s'' has a BCP 47 error: %2:s.';
  SWarning_LanguageTagIsNotCanonical = 'Keyboard with ID ''%0:s'' has a BCP 47 warning: %2:s.';

constructor TPackageInfoRefreshKeyboards.Create(Apack: TPackage);
begin
  inherited Create;
  pack := Apack;
end;

(**
  Adds new keyboards to the Keyboards object in the kmp.inf and
  removes any that are no longer in the list.

?  Note: if both a .js and a .kmx exist for a given keyboard id,
?  then both will be checked for version consistency, etc.??
*)
procedure TPackageInfoRefreshKeyboards.DoError(msg: string;
  State: TProjectLogState);
begin
  if Assigned(FOnError) then
    FOnError(Self, msg, State);
end;

function TPackageInfoRefreshKeyboards.Execute: Boolean;
var
  i: Integer;
  k: TPackageKeyboard;
  pki: TPackageKeyboardInfo;
begin
  // Remove keyboards that do not have corresponding files
  for i := pack.Keyboards.Count - 1 downto 0 do
  begin
    if FindKeyboardFileByID(pack.Keyboards[i].ID) = nil then
      pack.Keyboards.Delete(i);
  end;

  // Add and update remaining keyboards
  for i := 0 to pack.Files.Count - 1 do
  begin
    if IsKeyboardFileByName(pack.Files[i]) in [ftKeymanFile, ftJavascript] then
    begin
      // If the keyboard already exists, don't change language information because
      // it may have been edited by the user. However, do refresh the fields we
      // need to keep in sync: name, version and rtl (js keyboards only)

      k := FindKeyboardByFileName(pack.Files[i].FileName);
      if not Assigned(k) then
      begin
        k := TPackageKeyboard.Create(pack);
        FillKeyboardLanguages(pack.Files[i], k);
        pack.Keyboards.Add(k);
      end;

      if FillKeyboardDetails(pack.Files[i], pki) then
      begin
        k.Name := pki.Name;
        k.ID := pki.ID;
        k.Version := pki.Version;
        k.MinKeymanVersion := pki.MinKeymanVersion;
        if IsKeyboardFileByName(pack.Files[i]) = ftJavascript then
          // RTL flag is currently only relevant for KMW so although the
          // information can be read from .kmx we only copy it over for
          // js keyboards.
          k.RTL := pki.RTL;
      end;
    end;
  end;

  Result := CheckKeyboardTargetVersions;
  Result := CheckKeyboardLanguages and Result; // Always check all for comprehensive error messagess
end;

function TPackageInfoRefreshKeyboards.CheckKeyboardTargetVersions: Boolean;
var
  i, j: Integer;
  v: array[0..1] of TPackageKeyboardInfo;
  ids: TIntegerDynArray;
  FileNotFound: Boolean;
begin
  // Test that each keyboard has at most one target file for each platform and
  // that the versions match
  for i := 0 to pack.Keyboards.Count - 1 do
  begin
    ids := FindKeyboardFilesByID(pack.Keyboards[i].ID);
    if Length(ids) = 0 then
      // This can happen if a keyboard file is invalid
      Continue;

    if Length(ids) = 1 then
      Continue;

    if Length(ids) > 2 then
    begin
      DoError(Format(SError_TooManyTargetFilesForKeyboardID, [pack.Keyboards[i].ID, Length(ids)]), plsError);
      Exit(False);
    end;

    if pack.Files[ids[0]].FileType = pack.Files[ids[1]].FileType then
    begin
      DoError(Format(SError_CannotHaveSameKeyboardTwiceWithSameTarget, [pack.Keyboards[i].ID]), plsError);
      Exit(False);
    end;

    FileNotFound := False;
    for j := 0 to 1 do
      if not FillKeyboardDetails(pack.Files[ids[j]], v[j]) then
      begin
        DoError(Format(SWarning_CannotFindKeyboardFile, [pack.Files[ids[j]].FileName]), plsWarning);
        FileNotFound := True;
        Break;
      end;
    if FileNotFound then
      Continue;

    if v[1].Version <> v[0].Version then
    begin
      DoError(Format(SError_KeyboardVersionsDoNotMatch, [pack.Keyboards[i].ID, v[0].Version, v[1].Version]), plsError);
      Exit(False);
    end;
  end;

  Result := True;
end;

function TPackageInfoRefreshKeyboards.CheckKeyboardLanguages: Boolean;
var
  kbd: TPackageKeyboard;
  lang: TPackageKeyboardLanguage;
  msg: string;
begin
  Result := True;

  for kbd in pack.Keyboards do
  begin
    for lang in kbd.Languages do
    begin
      with TBCP47Tag.Create(lang.ID) do
      try
        if not IsValid(True, msg) then
        begin
          DoError(Format(SError_LanguageTagIsNotValid, [kbd.ID, lang.ID, msg]), plsError);
          Result := False;
        end
        else if not TCanonicalLanguageCodeUtils.IsCanonical(tag, msg, False, False) then
        begin
          DoError(Format(SWarning_LanguageTagIsNotCanonical, [kbd.ID, lang.ID, msg]), plsInfo);
        end;
      finally
        Free;
      end;
    end;
  end;
end;

function TPackageInfoRefreshKeyboards.IsKeyboardFileByContent(f: TPackageContentFile): Boolean;
var
  id: string;
begin
  id := TKeyboardUtils.GetKeymanWebCompiledNameFromFileName(f.FileName);
  // Look for Keyboard_<id>
  with TStringStream.Create('', TEncoding.UTF8) do
  try
    try
      LoadFromFile(f.FileName);
    except
      on E:EEncodingError do
      begin
        // If the file cannot be loaded as UTF-8, we'll ignore it; #11687
        Exit(False);
      end;
    end;
    Result := TRegEx.IsMatch(DataString, '\bKeyboard_'+id+'\b', []);
  finally
    Free;
  end;
end;

function TPackageInfoRefreshKeyboards.IsModelFileByName(f: TPackageContentFile): Boolean;
begin
  Result := TRegEx.IsMatch(f.FileName, '\.model\.js$', [roIgnoreCase]);
end;

function TPackageInfoRefreshKeyboards.IsKeyboardFileByName(f: TPackageContentFile): TKMFileType;
begin
  if f.FileType = ftKeymanFile then
  begin
    Exit(ftKeymanFile);
  end;

  if f.FileType <> ftJavascript then
    Exit(ftOther);

  // A lexical model file will typicaly have the extension .model.js. This prevents the
  // package editor from treating a lexical model as a keyboard when refreshing the list
  // of included keyboards
  if IsModelFileByName(f) then
    Exit(ftOther);

  // Need to test if the JS is a valid keyboard file.
  // This is a bit of a pain... but we have to stick with .js for compat
  if not FileExists(f.FileName) or IsKeyboardFileByContent(f) then
    Exit(ftJavascript);

  Exit(ftOther);
end;

class procedure TPackageInfoRefreshKeyboards.FillKeyboardLanguages(f: TPackageContentFile; k: TPackageKeyboard);
var
  codes: TStringDynArray;
  lang: TPackageKeyboardLanguage;
  i: Integer;
  s, t: string;
begin
  //
  k.Languages.Clear;
  if f.FileType = ftKeymanFile then
  begin
    try
      codes := TKMXFileLanguages.GetKMXFileBCP47Codes(f.FileName);
      for i := 0 to High(codes) do
      begin
        t := TCanonicalLanguageCodeUtils.FindBestTag(codes[i], False, False);
        if t = '' then
          // We won't add codes that are unrecognised
          Continue;

        lang := TPackageKeyboardLanguage.Create(f.Package);

        with TBCP47Tag.Create(t) do
        try
          Region := '';
          lang.ID := Tag;

          if not TLanguageCodeUtils.BCP47Languages.TryGetValue(Language, lang.Name) then
          begin
            lang.Name := Language;
          end;

          if Script <> '' then
          begin
            if TLanguageCodeUtils.BCP47Languages.TryGetValue(Script, s) then
              lang.Name := lang.Name + ' ('+s+')';
          end;
        finally
          Free;
        end;

        k.Languages.Add(lang);
      end;
    except
      on E:EKMXError do ;
      on E:EFOpenError do ;
    end;
  end
  else if f.FileType = ftJavascript then
  begin
    // There's nothing we can extract from the Javascript
  end;
end;

class function TPackageInfoRefreshKeyboards.FillKeyboardDetails(f: TPackageContentFile; var pki: TPackageKeyboardInfo): Boolean;
var
  ki: TKeyboardInfo;
begin
  if not FileExists(f.FileName) then
    Exit(False);

  Result := True;
  pki.ID := TKeyboardUtils.KeyboardFileNameToID(f.FileName);
  pki.RTL := False;

  if f.FileType = ftKeymanFile then
  begin
    try
      GetKeyboardInfo(f.FileName, False, ki, False);
      pki.Name := ki.KeyboardName;
      pki.Version := ki.KeyboardVersion;
      pki.MinKeymanVersion := ki.FileVersionAsString;
      pki.RTL := ki.KMW_RTL;
    except
      on E:EKMXError do Result := False;
      on E:EFOpenError do Result := False;
    end;
  end
  else if f.FileType = ftJavascript then
  begin
    try
      with TKeyboardJSInfo.Create(f.FileName) do
      try
        pki.Name := Name;
        pki.Version := Version;
        pki.RTL := RTL;
        pki.MinKeymanVersion := MinKeymanVersion;
      finally
        Free;
      end;
    except
      on E:EFOpenError do Result := False;
    end;
  end;
end;

(**
  Searches for a file that matches id.kmx or id-version.js. No attempt to test
  the file content is made; the search is just by name

  @params:  id   The identifier of the keyboard to search for

  @returns  file that matches the identifier, or nil if no matching file found
*)
function TPackageInfoRefreshKeyboards.FindKeyboardFileByID(const id: string): TPackageContentFile;
var
  i: Integer;
begin
  for i := 0 to pack.Files.Count - 1 do
  begin
    if DoesFileMatchKeyboardID(pack.Files[i], id) then
      Exit(pack.Files[i]);
  end;

  Result := nil;
end;

function TPackageInfoRefreshKeyboards.DoesFileMatchKeyboardID(f: TPackageContentFile; const id: string): Boolean;
var
  fn: string;
begin
  fn := ChangeFileExt(ExtractFileName(f.FileName), '');

  // If filename matches id.kmx
  if (f.FileType = ftKeymanFile) and SameText(fn, id) then
    Exit(True);

  // If filename matches id.js
  if (f.FileType = ftJavascript) and SameText(fn, id) then
    Exit(True);

  Result := False;
end;

function TPackageInfoRefreshKeyboards.FindKeyboardFilesByID(
  const id: string): TIntegerDynArray;
var
  i: Integer;
begin
  SetLength(Result, 0);
  for i := 0 to pack.Files.Count - 1 do
    if DoesFileMatchKeyboardID(pack.Files[i], id) then
    begin
      SetLength(Result, Length(Result)+1);
      Result[High(Result)] := i;
    end;
end;

function TPackageInfoRefreshKeyboards.FindKeyboardByFileName(const name: string): TPackageKeyboard;
var
  s: string;
begin
  s := TKeyboardUtils.KeyboardFileNameToID(name);
  Result := pack.Keyboards.ItemByID(s);
end;

end.
