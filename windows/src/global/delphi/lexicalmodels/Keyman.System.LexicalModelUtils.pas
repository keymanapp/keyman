unit Keyman.System.LexicalModelUtils;

interface

type
  TLexicalModelUtils = class
  public
    const SPackageNameDoesNotFollowLexicalModelConventions_Message: string =
      'The package file %0:s does not follow the recommended model filename conventions. The name should be all lower case, '+
      'include only alphanumeric characters and underscore (_), and should have the structure <author>.<bcp47>.<uniq>.model.kps';

    class function LexicalModelFileNameToID(filename: string): string;
    class function LexicalModelIDToFileName(id: string): string;
    class function DoesPackageFilenameFollowLexicalModelConventions(
      const Name: string): Boolean; static;
    class function DoesJSFilenameFollowLexicalModelConventions(
      const Name: string): Boolean; static;
  end;

implementation

uses
  System.RegularExpressions,
  System.SysUtils;

{ TLexicalModelUtils }

const
  SLexicalModelExtension = '.model.js';

  // The model ID SHOULD adhere to this pattern (see also developer/js/index.ts):
  //                           author           .bcp47            .uniq
  MODEL_ID_PATTERN_JS      = '^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*\.model\.(js)$';
  MODEL_ID_PATTERN_PACKAGE = '^[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*\.[a-z_][a-z0-9_]*\.model\.(kps|kmp)$';

class function TLexicalModelUtils.DoesJSFilenameFollowLexicalModelConventions(
  const Name: string): Boolean;
begin
  Result := TRegEx.IsMatch(ExtractFileName(Name), MODEL_ID_PATTERN_JS);
end;

class function TLexicalModelUtils.DoesPackageFilenameFollowLexicalModelConventions(
  const Name: string): Boolean;
// The model ID SHOULD adhere to this pattern (see also developer/js/index.ts):
//                         author           .bcp47            .uniq
begin
  Result := TRegEx.IsMatch(ExtractFileName(Name), MODEL_ID_PATTERN_PACKAGE);
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

end.
