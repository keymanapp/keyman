unit Keyman.Developer.System.KeymanConvertMain;

interface

procedure Run;

implementation

uses
  System.SysUtils,
  Winapi.ActiveX,

  Keyman.Developer.System.KMConvertParameters,
  Keyman.Developer.System.ImportWindowsKeyboard,
  Keyman.Developer.System.KeyboardProjectTemplate,
  Keyman.Developer.System.LDMLKeyboardProjectTemplate,
  Keyman.Developer.System.ModelProjectTemplate,
  KeymanVersion,
  VersionInfo;

function DoRun: Boolean; forward;

procedure Run;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    if DoRun then
      ExitCode := 0
    else
      ExitCode := 1;
  finally
    CoUninitialize;
  end;
end;

function DoImportWindowsKeyboard(FParameters: TKMConvertParameters): Boolean;
var
  iwk: TImportWindowsKeyboard;
  FTargetFolder: string;
  v: Integer;
begin
  if not TryStrToInt('$'+FParameters.KLID, v) then
  begin
    writeln('ERROR: The format of the input parameter -klid '+FParameters.KLID+' is incorrect');
    Exit(False);
  end;

  iwk := TImportWindowsKeyboard.Create;
  try
    iwk.SourceKLID := FParameters.KLID;
    iwk.DestinationPath := FParameters.Destination;
    iwk.KeyboardIDTemplate := FParameters.KeyboardID;
    iwk.NameTemplate := FParameters.Name;
    iwk.Description := FParameters.Description;
    iwk.Copyright := FParameters.Copyright;
    iwk.FullCopyright := FParameters.FullCopyright;
    iwk.Version := FParameters.Version;
    iwk.BCP47Tags := FParameters.BCP47Tags;
    iwk.Author := FParameters.Author;

    FTargetFolder := ExtractFileDir(iwk.ProjectFilename);
    if DirectoryExists(FTargetFolder) then
    begin
      writeln('ERROR: The directory "'+FTargetFolder+'" already exists.');
      Exit(False);
    end;

    Result := iwk.Execute;
  finally
    iwk.Free;
  end;
end;

function DoCreateKeyboardTemplate(FParameters: TKMConvertParameters): Boolean;
var
  kpt: TKeyboardProjectTemplate;
  FTargetFolder: string;
begin
  kpt := TKeyboardProjectTemplate.Create(FParameters.Destination, FParameters.KeyboardID, FParameters.Targets);
  try
    if FParameters.Name = ''
      then kpt.Name := FParameters.KeyboardID
      else kpt.Name := FParameters.Name;
    kpt.Copyright := FParameters.Copyright;
    kpt.FullCopyright := FParameters.FullCopyright;
    kpt.Version := FParameters.Version;
    kpt.BCP47Tags := FParameters.BCP47Tags;
    kpt.Author := FParameters.Author;
    kpt.IncludeIcon := True;

    FTargetFolder := ExtractFileDir(kpt.ProjectFilename);
    if DirectoryExists(FTargetFolder) then
    begin
      writeln('ERROR: The directory "'+FTargetFolder+'" already exists.');
      Exit(False);
    end;

    try
      kpt.Generate;
    except
      on E:EKeyboardProjectTemplate do
      begin
        writeln(E.Message);
        Exit(False);
      end;
    end;
    Result := True;
  finally
    kpt.Free;
  end;
end;

function DoCreateLDMLKeyboardTemplate(FParameters: TKMConvertParameters): Boolean;
var
  kpt: TLDMLKeyboardProjectTemplate;
  FTargetFolder: string;
begin
  kpt := TLDMLKeyboardProjectTemplate.Create(FParameters.Destination, FParameters.KeyboardID);
  try
    if FParameters.Name = ''
      then kpt.Name := FParameters.KeyboardID
      else kpt.Name := FParameters.Name;
    kpt.Copyright := FParameters.Copyright;
    kpt.FullCopyright := FParameters.FullCopyright;
    kpt.Version := FParameters.Version;
    kpt.BCP47Tags := FParameters.BCP47Tags;
    kpt.Author := FParameters.Author;

    FTargetFolder := ExtractFileDir(kpt.ProjectFilename);
    if DirectoryExists(FTargetFolder) then
    begin
      writeln('ERROR: The directory "'+FTargetFolder+'" already exists.');
      Exit(False);
    end;

    try
      kpt.Generate;
    except
      on E:ELDMLKeyboardProjectTemplate do
      begin
        writeln(E.Message);
        Exit(False);
      end;
    end;
    Result := True;
  finally
    kpt.Free;
  end;
end;

function DoCreateModelTemplate(FParameters: TKMConvertParameters): Boolean;
var
  mpt: TModelProjectTemplate;
  FTargetFolder: string;
  ModelID: string;
begin
  ModelID := FParameters.ModelIdAuthor + '.' + FParameters.ModelIdLanguage + '.' + FParameters.ModelIdUniq;
  mpt := TModelProjectTemplate.Create(FParameters.Destination, ModelID);
  try
    mpt.Name := FParameters.Name;
    mpt.Copyright := FParameters.Copyright;
    mpt.FullCopyright := FParameters.FullCopyright;
    mpt.Version := FParameters.Version;
    mpt.BCP47Tags := FParameters.BCP47Tags;
    mpt.Author := FParameters.Author;

    FTargetFolder := ExtractFileDir(mpt.ProjectFilename);
    if DirectoryExists(FTargetFolder) then
    begin
      writeln('ERROR: The directory "'+FTargetFolder+'" already exists.');
      Exit(False);
    end;

    try
      mpt.Generate;
    except
      on E:EModelProjectTemplate do
      begin
        writeln(E.Message);
        Exit(False);
      end;
    end;
    Result := True;
  finally
    mpt.Free;
  end;
end;

procedure WriteBanner;
begin
  writeln(SKeymanDeveloperName + ' Conversion Utility');
  writeln('Version ' + CKeymanVersionInfo.VersionWithTag + ', ' + GetVersionCopyright);
  writeln;
end;

function CmdLineToArray: TArray<string>;
var
  i: Integer;
begin
  SetLength(Result, ParamCount);
  for i := 1 to ParamCount do
    Result[i-1] := ParamStr(i);
end;

function DoRun: Boolean;
var
  FParameters: TKMConvertParameters;
begin
  Result := False;

  // convert transforms keyboards from one format to another
  // and also generates template projects, etc.

  if not FParameters.CheckParams(CmdLineToArray) then
  begin
    if FParameters.EmitUsage then
    begin
      WriteBanner;
      FParameters.WriteUsage;
    end;
    Exit;
  end;

  if not FParameters.NoLogo then
    WriteBanner;

  case FParameters.Mode of
    cmImportWindows:
      Result := DoImportWindowsKeyboard(FParameters);
    cmTemplate:
      Result := DoCreateKeyboardTemplate(FParameters);
    cmLdmlKeyboard:
      Result := DoCreateLDMLKeyboardTemplate(FParameters);
    cmLexicalModel:
      Result := DoCreateModelTemplate(FParameters);
  end;
end;

end.
