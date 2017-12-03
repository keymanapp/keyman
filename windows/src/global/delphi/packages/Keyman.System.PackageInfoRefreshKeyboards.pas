unit Keyman.System.PackageInfoRefreshKeyboards;

interface

uses
  PackageInfo,
  utilfiletypes;

type
  TPackageInfoRefreshKeyboards = class
  private
    pack: TPackage;
    function FindKeyboardFileByID(const id: string): TPackageContentFile;
    class procedure FillKeyboardDetails(f: TPackageContentFile;
      k: TPackageKeyboard); static;
    class procedure FillKeyboardLanguages(f: TPackageContentFile;
      k: TPackageKeyboard); static;
    function IsKeyboardFileByName(f: TPackageContentFile): TKMFileType;
    function FindKeyboardByFileName(const name: string): TPackageKeyboard;
  public
    constructor Create(Apack: TPackage);
    procedure Execute;
  end;

implementation

uses
  System.RegularExpressions,
  System.SysUtils,

  Keyman.System.KMXFileLanguages,
  kmxfile;

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
procedure TPackageInfoRefreshKeyboards.Execute;
var
  i: Integer;
  k: TPackageKeyboard;
begin
  with TPackageInfoRefreshKeyboards.Create(pack) do
  try
    Execute;
  finally
    Free;
  end;
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

      FillKeyboardDetails(pack.Files[i], k);
    end;
  end;
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
  if TRegEx.IsMatch(s, '/^[a-z0-9_]+-([0-9]+)(\.[0-9+])+$/', [roIgnoreCase]) then
    Exit(ftJavascript);

  Exit(ftOther);
end;

class procedure TPackageInfoRefreshKeyboards.FillKeyboardLanguages(f: TPackageContentFile; k: TPackageKeyboard);
var
  ki: TKeyboardInfo;
  FEthnologueCodes: WideString;
begin
  //
  k.Languages.Clear;
  if f.FileType = ftKeymanFile then
  begin
    // ...
  end;
end;

class procedure TPackageInfoRefreshKeyboards.FillKeyboardDetails(f: TPackageContentFile; k: TPackageKeyboard);
begin
  //
      k.Name := ;;
      k.Version := ;;
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
  f: string;
begin
  for i := 0 to pack.Files.Count - 1 do
  begin
    f := ChangeFileExt(ExtractFileName(pack.Files[i].FileName), '');

    // If filename matches id.kmx
    if (pack.Files[i].FileType = ftKeymanFile) and SameText(f, id) then
      Exit(pack.Files[i]);

    // If filename matches id-version.js
    if SameText(ExtractFileExt(pack.Files[i].FileName), '.js') and
        SameText(Copy(f, 1, Length(id)), id) and
        (Copy(f, Length(id)+1, 1) = '-') then
      Exit(pack.Files[i]);
  end;

  Result := nil;
end;

function TPackageInfoRefreshKeyboards.FindKeyboardByFileName(const name: string): TPackageKeyboard;
var
  s: string;
  i: Integer;
begin
  s := ExtractFileName(name);
  if SameText(ExtractFileExt(s), '.js') then
  begin
    s := ChangeFileExt(s, '');
    s := Copy(s, 1, Pos('-', s)-1);
  end
  else
    s := ChangeFileExt(s, '');

  Result := pack.Keyboards.ItemByID(s);
end;

end.
