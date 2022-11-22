unit Keyman.System.KeyboardUtils;

interface

type
  TKeyboardUtils = class
  private
    class function CleanComponent(e: string): string; static;
  public
    const SKeyboardNameDoesNotFollowConventions_Message: string =
      'The keyboard file %0:s does not follow the recommended filename conventions. The name should be all lower case, '+
      'include only alphanumeric characters and underscore (_), and should not start with a digit.';
    const SKeyboardNameDoesNotFollowConventions_Prompt: string =
      'The keyboard file %0:s does not follow the recommended filename conventions. The name should be all lower case, '+
      'include only alphanumeric characters and underscore (_), and should not start with a digit.'+
      #13#10#13#10 +
      'Do you wish to continue?';

    const SFilenameDoesNotFollowConventions_Message: string =
      'The file %0:s does not follow the recommended filename conventions. The extension should be all lower case, '+
      'and the filename should include only alphanumeric characters, -, _, + and .';
    const SFilenameDoesNotFollowConventions_Prompt: string =
      'The file %0:s does not follow the recommended filename conventions. The extension should be all lower case, '+
      'and the filename should include only alphanumeric characters, -, _, + and .'+
      #13#10#13#10 +
      'Do you wish to continue?';

    const SPackageNameDoesNotFollowLexicalModelConventions_Message: string =
      'The package file %0:s does not follow the recommended model filename conventions. The name should be all lower case, '+
      'include only alphanumeric characters and underscore (_), and should have the structure <author>.<bcp47>.<uniq>.model.kps';

    class function KeyboardFileNameToID(filename: string): string;
    class function CleanKeyboardID(const Name: WideString): WideString; static;
    class function IsValidKeyboardID(const Name: string; EnforceCaseAndWhitespace: Boolean): Boolean; static;
    class function GetKeymanWebCompiledFileName(const FileName: WideString): WideString; static;
    class function GetKeymanWebCompiledNameFromFileName(const FileName: WideString): WideString; static;
    class function DoesFilenameFollowConventions(const Name: string): Boolean; static;
    class function DoesKeyboardFilenameFollowConventions(const Name: string): Boolean; static;

    class function IsValidVersionString(const Version: string): Boolean; static;
  end;

implementation

uses
  System.SysUtils;

{ TKeyboardUtil }

class function TKeyboardUtils.CleanKeyboardID(const Name: WideString): WideString;
var
  i: Integer;
const
  ValidChars: WideString = 'abcdefghijklmnopqrstuvwxyz0123456789_';
begin
  Result := LowerCase(Name);
  if Length(Result) = 0 then Exit;
  if Pos(Result[1], '0123456789') > 0 then Result := '_'+Result; // Can't have a number as initial
  for i := 1 to Length(Result) do
    if Pos(Result[i], ValidChars) = 0 then Result[i] := '_';
end;

class function TKeyboardUtils.GetKeymanWebCompiledNameFromFileName(const FileName: WideString): WideString;
begin
  Result := CleanKeyboardID(ChangeFileExt(ExtractFileName(FileName), ''));
end;

class function TKeyboardUtils.CleanComponent(e: string): string;
var
  i: Integer;
const
  ValidChars: WideString = 'abcdefghijklmnopqrstuvwxyz0123456789_.-+';
begin
  Result := e;
  if Length(Result) = 0 then Exit;
  for i := 1 to Length(Result) do
    if Pos(Result[i], ValidChars) = 0 then Result[i] := '_';
end;

class function TKeyboardUtils.DoesFilenameFollowConventions(const Name: string): Boolean;
var
  e, n: string;
begin
  e := ExtractFileExt(Name);
  n := LowerCase(ChangeFileExt(ExtractFileName(Name), ''));

  // The extension must be lowercase and not
  // use punctuation apart from ., _, -, and +.
  // The filename does not need to be lowercase
  Result := (CleanComponent(e) = e) and (CleanComponent(n) = n);
end;

class function TKeyboardUtils.DoesKeyboardFilenameFollowConventions(
  const Name: string): Boolean;
var
  e, n: string;
begin
  e := ExtractFileExt(Name);
  n := ChangeFileExt(ExtractFileName(Name), '');
  Result := (CleanComponent(e) = e) and IsValidKeyboardID(n, True);
end;

class function TKeyboardUtils.IsValidKeyboardID(const Name: string; EnforceCaseAndWhitespace: Boolean): Boolean;
begin
  if EnforceCaseAndWhitespace
    then Result := CleanKeyboardID(Name) = Name
    else Result := CleanKeyboardID(Name.Trim) = LowerCase(Name.Trim);
end;

class function TKeyboardUtils.IsValidVersionString(
  const Version: string): Boolean;
var
  p: PChar;
begin
  p := PChar(Version);
  while p^ <> #0 do
  begin
    if not CharInSet(p^, ['0'..'9']) then
      Exit(False);
    while CharInSet(p^, ['0'..'9']) do Inc(p);

    if p^ = '.' then
    begin
      Inc(p);
      if not CharInSet(p^, ['0'..'9']) then   // I4263
        Exit(False);
    end;
  end;

  Result := True;
end;

class function TKeyboardUtils.GetKeymanWebCompiledFileName(const FileName: WideString): WideString;   // I4140
begin
  Result :=
    ExtractFilePath(FileName) +
    GetKeymanWebCompiledNameFromFileName(FileName) +
    '.js';
end;

class function TKeyboardUtils.KeyboardFileNameToID(filename: string): string;
var
  ext: string;
begin
  ext := ExtractFileExt(FileName);
  if SameText(ext, '.kmx') or SameText(ext, '.kmp') or SameText(ext, '.js') then
  begin
    Result := CleanKeyboardID(ChangeFileExt(ExtractFileName(FileName), ''));
  end
  else
    Result := '';
end;

end.
