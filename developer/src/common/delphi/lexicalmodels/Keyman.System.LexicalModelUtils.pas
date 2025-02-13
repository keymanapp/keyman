unit Keyman.System.LexicalModelUtils;

interface

type
  TLexicalModelUtils = class
  public
    const SPackageNameDoesNotFollowLexicalModelConventions_Message: string =
      'The package file %0:s does not follow the recommended model filename conventions. The name should be all lower case, '+
      'include only alphanumeric characters and underscore (_), and should have the structure <author>.<bcp47>.<uniq>.model.kps';

    const SModelFileNameDoesNotFollowConventions_Message: string =
      'The model source file %0:s does not follow the recommended model filename conventions. The name should be all lower case, '+
      'include only alphanumeric characters and underscore (_), and should have the structure <author>.<bcp47>.<uniq>.model.ts';

    const SProjectFileNameDoesNotFollowLexicalModelConventions_Message: string =
      'The model project file %0:s does not follow the recommended model filename conventions. The name should be all lower case, '+
      'include only alphanumeric characters and underscore (_), and should have the structure <author>.<bcp47>.<uniq>.model.kpj';

    class function LexicalModelFileNameToID(filename: string): string;
    class function LexicalModelIDToFileName(id: string): string;
    class function DoesPackageFilenameFollowLexicalModelConventions(
      const Name: string): Boolean; static;
    class function DoesJSFilenameFollowLexicalModelConventions(
      const Name: string): Boolean; static;
    class function DoesTSFilenameFollowLexicalModelConventions(
      const Name: string): Boolean; static;
    class function DoesProjectFilenameFollowLexicalModelConventions(
      const Name: string): Boolean; static;
    class function CleanLexicalModelIDComponent(const Name: string): string; static;
    class function IsCleanLexicalModelIDComponent(const Name: string): Boolean; static;

    class function IsValidLexicalModelID(ID: string; EnforceCaseAndWhitespace: Boolean): Boolean; static;

    class function ExtractFileExt(filename: string): string; static;
    class function RemoveFileExt(filename: string): string; static;
    class function ChangeFileExt(filename, ext: string): string; static;
  end;

implementation

uses
  System.RegularExpressions,
  System.SysUtils,

  utilfiletypes;

{ TLexicalModelUtils }

const
  SLexicalModelExtension = '.model.js';

  // The model ID SHOULD adhere to this pattern (see also developer/src/kmc-model):
  //                           author           .bcp47            .uniq
  MODEL_ID_PATTERN         = '^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*$';
  MODEL_ID_PATTERN_JS      = '^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.js$';
  MODEL_ID_PATTERN_TS      = '^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.ts$';
  MODEL_ID_PATTERN_PACKAGE = '^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.(kps|kmp)$';
  MODEL_ID_PATTERN_PROJECT = '^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_-]*\.[a-z_][a-z0-9_]*\.model\.kpj$';

class function TLexicalModelUtils.IsValidLexicalModelID(ID: string;
  EnforceCaseAndWhitespace: Boolean): Boolean;
begin
  if not EnforceCaseAndWhitespace then
    ID := LowerCase(Trim(ID));
  Result := TRegEx.IsMatch(ID, MODEL_ID_PATTERN);
end;

class function TLexicalModelUtils.DoesJSFilenameFollowLexicalModelConventions(
  const Name: string): Boolean;
begin
  Result := TRegEx.IsMatch(ExtractFileName(Name), MODEL_ID_PATTERN_JS);
end;

class function TLexicalModelUtils.DoesTSFilenameFollowLexicalModelConventions(
  const Name: string): Boolean;
begin
  Result := TRegEx.IsMatch(ExtractFileName(Name), MODEL_ID_PATTERN_TS);
end;

class function TLexicalModelUtils.DoesPackageFilenameFollowLexicalModelConventions(
  const Name: string): Boolean;
// The model ID SHOULD adhere to this pattern (see also developer/src/kmc-model):
//                         author           .bcp47            .uniq
begin
  Result := TRegEx.IsMatch(ExtractFileName(Name), MODEL_ID_PATTERN_PACKAGE);
end;

class function TLexicalModelUtils.DoesProjectFilenameFollowLexicalModelConventions(
  const Name: string): Boolean;
begin
  Result := TRegEx.IsMatch(ExtractFileName(Name), MODEL_ID_PATTERN_PROJECT);
end;

class function TLexicalModelUtils.LexicalModelFileNameToID(filename: string): string;
begin
  Result := ExtractFileName(filename);
  if Result.EndsWith(SLexicalModelExtension) then
    Result := Result.Remove(Result.Length - SLexicalModelExtension.Length);
end;

class function TLexicalModelUtils.LexicalModelIDToFileName(id: string): string;
begin
  Result := id + SLexicalModelExtension;
end;

// Note: This isn't ideal. The dotted a.b.c.model.blah filename convention makes it impossible to
// generically extract an extension. This is the best we can do, adding special handling for specific
// file extensions

class function TLexicalModelUtils.RemoveFileExt(filename: string): string;
begin
  Result := Copy(filename, 1, Length(filename) - Length(TLexicalModelUtils.ExtractFileExt(filename)));
end;

class function TLexicalModelUtils.ChangeFileExt(filename, ext: string): string;
begin
  Result := Copy(filename, 1, Length(filename) - Length(TLexicalModelUtils.ExtractFileExt(filename))) + ext;
end;

class function TLexicalModelUtils.ExtractFileExt(filename: string): string;
var
  Second: string;
begin
  Result := System.SysUtils.ExtractFileExt(filename);
  if Result = '' then
    Exit;
  Second := System.SysUtils.ExtractFileExt(Copy(filename, 1, Length(filename)-Length(Result)));
  if (SameText(Result, Ext_LexicalModelSource) or
      SameText(Result, Ext_PackageSource) or
      SameText(Result, Ext_ProjectSource)) and
     SameText(Second, Ext_Model_Component) then
    Result := Second + Result;
end;

class function TLexicalModelUtils.IsCleanLexicalModelIDComponent(const Name: string): Boolean;
begin
  Result := Name = CleanLexicalModelIDComponent(Name);
end;

class function TLexicalModelUtils.CleanLexicalModelIDComponent(const Name: string): string;
var
  i: Integer;
const
  ValidChars: string = 'abcdefghijklmnopqrstuvwxyz0123456789_';
begin
  // Cloned from TKeyboardUtils.CleanKeyboardID
  Result := LowerCase(Name);
  if Length(Result) = 0 then Exit;
  if Pos(Result[1], '0123456789') > 0 then Result := '_'+Result; // Can't have a number as initial
  for i := 1 to Length(Result) do
    if Pos(Result[i], ValidChars) = 0 then Result[i] := '_';
end;

end.
