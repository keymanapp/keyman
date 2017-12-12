unit Keyman.System.PackageInfoRefreshKeyboards;

interface

uses
  System.Types,

  PackageInfo,
  ProjectLog,
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
  public
    type TPackageKeyboardInfo = record
      Name, ID, Version: string;
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

  Keyman.System.KMXFileLanguages,
  Keyman.System.KeyboardJSInfo,
  Keyman.System.KeyboardUtils,
  kmxfile;

const
  SError_KeyboardVersionsDoNotMatch = 'Keyboard with ID ''%0:s'' is included with two differing versions: ''%1:s'', ''%2:s''. The keyboard versions must be consistent.';
  SError_TooManyTargetFilesForKeyboardID = 'Keyboard with ID ''%0:s'' is included too many times (%1:d) in the package. You should have at most 1 .kmx and 1 .js file';
  SError_CannotHaveSameKeyboardTwiceWithSameTarget = 'Keyboard with ID ''%0:s'' should have at most 1 .kmx and 1 .js file in the package';

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
  k0: TPackageKeyboard;
  ids: TIntegerDynArray;
  pki: TPackageKeyboardInfo;
  j: Integer;
  v: array[0..1] of TPackageKeyboardInfo;
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
      // need to keep in sync: name and version

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
      end;
    end;
  end;

  // Test that each keyboard has at most one target file for each platform and
  // that the versions match
  for i := 0 to pack.Keyboards.Count - 1 do
  begin
    ids := FindKeyboardFilesByID(pack.Keyboards[i].ID);
    Assert(Length(ids) > 0);

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

    for j := 0 to 1 do
      FillKeyboardDetails(pack.Files[ids[j]], v[j]);

    if v[1].Version <> v[0].Version then
    begin
      DoError(Format(SError_KeyboardVersionsDoNotMatch, [pack.Keyboards[i].ID, v[0].Version, v[1].Version]), plsError);
      Exit(False);
    end;
  end;

  Result := True;
end;

function TPackageInfoRefreshKeyboards.IsKeyboardFileByName(f: TPackageContentFile): TKMFileType;
var
  s: string;
begin
  if f.FileType = ftKeymanFile then
  begin
    Exit(ftKeymanFile);
  end;

  if f.FileType <> ftJavascript then
    Exit(ftOther);

  s := ChangeFileExt(ExtractFileName(f.FileName), '');
  if TRegEx.IsMatch(s, '^[a-z0-9_]+-([0-9]+)(\.[0-9+])*$', [roIgnoreCase]) then
    Exit(ftJavascript);

  Exit(ftOther);
end;

class procedure TPackageInfoRefreshKeyboards.FillKeyboardLanguages(f: TPackageContentFile; k: TPackageKeyboard);
var
  codes: TStringDynArray;
  lang: TPackageKeyboardLanguage;
  i: Integer;
begin
  //
  k.Languages.Clear;
  if f.FileType = ftKeymanFile then
  begin
    try
      codes := TKMXFileLanguages.GetKMXFileBCP47Codes(f.FileName);
      for i := 0 to High(codes) do
      begin
        lang := TPackageKeyboardLanguage.Create(f.Package);
        lang.ID := codes[i];
        // TODO: BCP47: Lookup default names
        lang.Name := codes[i];
        k.Languages.Add(lang);
      end;
    except
      on E:EKMXError do ;
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
  Result := True;
  pki.ID := TKeyboardUtils.KeyboardFileNameToID(f.FileName);

  if f.FileType = ftKeymanFile then
  begin
    try
      GetKeyboardInfo(f.FileName, False, ki, False);
      pki.Name := ki.KeyboardName;
      pki.Version := ki.KeyboardVersion;
    except
      on E:EKMXError do Result := False;
    end;
  end
  else if f.FileType = ftJavascript then
  begin
    try
      with TKeyboardJSInfo.Create(f.FileName) do
      try
        pki.Name := Name;
        pki.Version := Version;
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

  // If filename matches id-version.js
  if (f.FileType = ftJavascript) then
  begin
    if SameText(ExtractFileExt(f.FileName), '.js') and
        SameText(Copy(fn, 1, Length(id)), id) and
        (Copy(fn, Length(id)+1, 1) = '-') then
      Exit(True);
  end;

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
