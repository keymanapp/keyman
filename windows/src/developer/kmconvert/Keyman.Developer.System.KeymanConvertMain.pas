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
    iwk.Copyright := FParameters.Copyright;
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
    kpt.Name := FParameters.Name;
    kpt.Copyright := FParameters.Copyright;
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

procedure WriteBanner;
begin
  writeln(SKeymanDeveloperName + ' Conversion Utility');
  writeln('Version ' + GetVersionString + ', ' + GetVersionCopyright);
  writeln;
end;

function DoRun: Boolean;
var
  FParameters: TKMConvertParameters;
begin
  Result := False;

  // convert transforms keyboards from one format to another
  // and also generates template projects, etc.

  if not FParameters.CheckParams then
  begin
    WriteBanner;
    FParameters.WriteUsage;
    Exit;
  end;

  if not FParameters.NoLogo then
    WriteBanner;

  case FParameters.Mode of
    cmImportWindows:
      Result := DoImportWindowsKeyboard(FParameters);
    cmTemplate:
      Result := DoCreateKeyboardTemplate(FParameters);
  end;
end;

end.
