unit TikeUnicodeData;

interface

uses
  UnicodeData;

procedure CreateTikeUnicodeData(UIManager: IUnicodeDataUIManager);

implementation

uses
  System.SysUtils,
  Winapi.Windows,

  ErrorControlledRegistry,
  RedistFiles,
  RegistryKeys;

procedure CreateTikeUnicodeData(UIManager: IUnicodeDataUIManager);
var
  FUnicodePath: string;
begin
  with TRegistryErrorControlled.Create do  // I3463
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(SRegKey_KeymanDeveloper_LM) and ValueExists(SRegValue_RootPath)
      then FUnicodePath := IncludeTrailingPathDelimiter(ReadString(SRegValue_RootPath))
      else FUnicodePath := '';
  finally
    Free;
  end;

  CreateUnicodeData(GetUnicodeDataSourcePath(FUnicodePath), UIManager, FUnicodePath);  // I3463
end;

end.
