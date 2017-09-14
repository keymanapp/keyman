unit ValidateKeyboardInfo;

interface

uses
  compile;

type
  TValidateKeyboardInfo = class
    class function Execute(JsonFile: string; FDistribution, FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
  end;

implementation

uses
  System.SysUtils,
  Winapi.Windows;

{ TValidateKeyboardInfo }

const
  SKeyboardInfoSourceSchemaJson = 'keyboard_info.source.json';
  SKeyboardInfoDistSchemaJson = 'keyboard_info.distribution.json';

var
  GCallback: TCompilerCallback = nil;

type TValidateJsonMessageProc = function (offset: Int64; message: PAnsiChar): BOOL; stdcall;

function ValidateJsonFile(pwszSchemaFile, pwszJsonFile: PWideChar; MessageProc: TValidateJsonMessageProc): BOOL;
  stdcall; external 'kmcmpdll.dll';

function ValidateMessageProc(offset: Int64; message: PAnsiChar): BOOL; stdcall;
begin
  GCallback(-1, 0, message);
  Result := TRUE;
end;

class function TValidateKeyboardInfo.Execute(JsonFile: string; FDistribution, FSilent: Boolean; FCallback: TCompilerCallback): Boolean;
var
  SchemaFile: string;
begin
  GCallback := FCallback;
  if FDistribution
    then SchemaFile := ExtractFilePath(ParamStr(0)) + SKeyboardInfoDistSchemaJson
    else SchemaFile := ExtractFilePath(ParamStr(0)) + SKeyboardInfoSourceSchemaJson;
  Result := ValidateJsonFile(PWideChar(SchemaFile), PWideChar(JsonFile), ValidateMessageProc);
  if not FSilent then
  begin
    if Result
      then writeln('File '+JsonFile+' validated successfully.')
      else writeln('File '+JsonFile+' had errors.');
  end;
end;

end.
