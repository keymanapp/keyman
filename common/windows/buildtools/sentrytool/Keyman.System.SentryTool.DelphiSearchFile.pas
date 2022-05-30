unit Keyman.System.SentryTool.DelphiSearchFile;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TDelphiSearchFile = class
  private
    FFiles: TStringList;
    FPaths: TStringList;
    FPathIndex: Integer;
    procedure ParseDPR(const AProjectFile: string);
    procedure SearchPath(const Path: string);
  public
    constructor Create(const AProjectFile, ASearchPath: string);
    destructor Destroy; override;
    function FindFile(const UnitName: string): string;
    property Files: TStringList read FFiles;
  end;

implementation

uses
  System.RegularExpressions;

{ TDelphiSearchFile }

constructor TDelphiSearchFile.Create(const AProjectFile, ASearchPath: string);
begin
  FFiles := TStringList.Create;
  FPaths := TStringList.Create;
  FPaths.Delimiter := ';';
  FPaths.DelimitedText := ASearchPath;

  if AProjectFile <> '' then
    ParseDPR(AProjectFile);
end;

function ExpandRelativePath(const RootPath, Filename: string): string;
begin
  if not IsRelativePath(Filename) then
    Exit(Filename);

  Result := ExpandFileName(RootPath + Filename);
end;

procedure TDelphiSearchFile.ParseDPR(const AProjectFile: string);
var
  i: Integer;
  dpr: TStringList;
  re: TRegEx;
  m: TMatch;
  RootPath: string;
const
  pathMatch = '\s+([a-z_][a-z0-9_.]+)\s+in\s+''([^'']+)''';
begin
  RootPath := ExtractFilePath(AProjectFile);

  re := TRegEx.Create(pathMatch, [roIgnoreCase]);
  dpr := TStringList.Create;
  try
    dpr.LoadFromFile(AProjectFile);
    for i := 0 to dpr.Count - 1 do
    begin
      m := re.Match(dpr[i]);
      if m.Success then
        FFiles.AddPair(m.Groups[1].Value, ExpandRelativePath(RootPath, m.Groups[2].Value));
    end;
  finally
    dpr.Free;
  end;
end;

destructor TDelphiSearchFile.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FPaths);
  inherited;
end;

procedure TDelphiSearchFile.SearchPath(const Path: string);
var
  f: TSearchRec;
begin
  if FindFirst(IncludeTrailingPathDelimiter(Path) + '*.pas', 0, f) = 0 then
  begin
    repeat
      FFiles.AddPair(ChangeFileExt(f.Name, ''), IncludeTrailingPathDelimiter(Path) + f.Name);
    until FindNext(f) <> 0;
    FindClose(f);
  end;
end;

function TDelphiSearchFile.FindFile(const UnitName: string): string;
begin
  Result := FFiles.Values[UnitName];
  if Result <> '' then
    Exit;

  while FPathIndex < FPaths.Count - 1 do
  begin
    SearchPath(FPaths[FPathIndex]);
    Inc(FPathIndex);
    Result := FFiles.Values[UnitName];
    if Result <> '' then
      Exit;
  end;
end;

end.
