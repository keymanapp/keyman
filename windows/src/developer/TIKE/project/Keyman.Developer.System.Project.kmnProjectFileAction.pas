unit Keyman.Developer.System.Project.kmnProjectFileAction;

interface

uses
  System.SysUtils,

  Keyman.Developer.System.Project.kmnProjectFile,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType,
  UKeymanTargets;

type
  TkmnProjectFileAction = class(TkmnProjectFile)
  private
    function CompileVisualKeyboard(const AKVKSourceFile, AKVKTargetFile: string): Boolean;
  public
    function CompileKeyboard: Boolean;
    function Clean: Boolean;
  end;

implementation

uses
  System.Classes,
  System.Variants,
  Winapi.Windows,

  CompileKeymanWeb,
  compile,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.System.KeyboardUtils,
  VisualKeyboard;

function TkmnProjectFileAction.CompileVisualKeyboard(const AKVKSourceFile, AKVKTargetFile: string): Boolean;
begin
  with TVisualKeyboard.Create do
  try
    try
      LoadFromFile(AKVKSourceFile);
    except
      on E:Exception do
      begin
        OwnerProject.Log(plsError, AKVKSourceFile, 'Invalid visual keyboard: '+E.Message);
        Exit(False);
      end;
    end;
    if not SameFileName(AKVKSourceFile, AKVKTargetFile) then
      try
        Header.AssociatedKeyboard := ChangeFileExt(ExtractFileName(Self.FileName), '');
        SaveToFile(AKVKTargetFile, kvksfBinary);
      except
        on E:Exception do
        begin
          OwnerProject.Log(plsError, AKVKSourceFile, 'Could not save visual keyboard '+AKVKSourceFile+' to '+AKVKTargetFile+': '+E.ClassName+','+E.Message);
          Exit(False);
        end;
      end;
  finally
    Free;
  end;
  Result := True;
end;

function TkmnProjectFileAction.Clean: Boolean;
var
  FJS: string;
begin
  CleanFile(OutputFileName);

  if Targets * KMXKeymanTargets <> [] then
    CleanFile(ExtractFilePath(FileName) + ExtractFileName(ChangeFileExt(KVKFileName, '.kvk')), True);

  if Targets * KMWKeymanTargets <> [] then
  begin
    FJS := TKeyboardUtils.GetKeymanWebCompiledFileName(FileName);
    CleanFile(FJS); // keyboard-x.y.js
    CleanFile(ChangeFileExt(FJS, '') + '_load.js'); // keyboard-x.y_load.js
    CleanFile(ChangeFileExt(FJS, '.json'), True); // keyboard-x.y_load.js
  end;

  Result := True;
end;

function TkmnProjectFileAction.CompileKeyboard: Boolean;
var
  KMXFileName: String;
  FOutFileName: string;
  ckw: TCompileKeymanWeb;
  FKVKSourceFile: string;
  FKVKTargetFile: string;
begin
  TProject.CompilerMessageFile := Self;
  HasCompileWarning := False;   // I4706

  FKVKSourceFile := ExtractFilePath(FileName) + ExtractFileName(KVKFileName);
  FKVKTargetFile := OwnerProject.GetTargetFileName(ChangeFileExt(FKVKSourceFile, '.kvk'), FileName, FileVersion);

  try
    if Targets * KMXKeymanTargets <> [] then
    begin
      if Debug
        then Log(plsInfo, 'Compiling ' + FileName + ' with debug symbols for Windows, MacOSX...')   // I4504   // I4823
        else Log(plsInfo, 'Compiling ' + FileName + ' for Windows, MacOSX...');   // I4504

      //compile the keyboard
      KMXFileName := TargetFileName;
      ForceDirectories(ExtractFileDir(KMXFileName));
      //KMXFileName := ChangeFileExt(FileName, '.kmx');
      Result := CompileKeyboardFile(PChar(FileName), PChar(KMXFileName), Debug,
        OwnerProject.Options.CompilerWarningsAsErrors, OwnerProject.Options.WarnDeprecatedCode,   // I4865   // I4866
        ProjectCompilerMessage) > 0;

      if Result then
      begin
        if KVKFileName <> '' then
          Result := CompileVisualKeyboard(FKVKSourceFile, FKVKTargetFile);
      end;

      if HasCompileWarning and (WarnAsError or OwnerProject.Options.CompilerWarningsAsErrors) then Result := False;   // I4706

      if Result
        then Log(plsInfo, '''' + FileName + ''' compiled successfully for Windows to '''+KMXFileName+'''.')   // I4504
        else Log(plsError, '''' + FileName + ''' was not compiled successfully for Windows.')   // I4504
    end
    else
      Result := True;   // I4564

    // compile keyboard to web
    if Result and (Targets * KMWKeymanTargets <> []) then
    begin
      if Debug
        then Log(plsInfo, 'Compiling ' + FileName + ' with debug symbols for Web, iOS, Android, WinPhone...')   // I4504   // I4823
        else Log(plsInfo, 'Compiling ' + FileName + ' for Web, iOS, Android, WinPhone...');   // I4504

      ckw := TCompileKeymanWeb.Create;
      try
        FOutFilename := JSTargetFileName;
        ForceDirectories(ExtractFileDir(FOutFileName));

        if KVKFileName <> '' then
          Result := CompileVisualKeyboard(FKVKSourceFile, FKVKTargetFile);

        if Result then
          Result := ckw.Compile(OwnerProject, FileName, FOutFileName, Debug, ProjectCompilerMessage);   // I3681   // I4140   // I4865   // I4866

        if HasCompileWarning and (WarnAsError or OwnerProject.Options.CompilerWarningsAsErrors) then Result := False;   // I4706

        if Result
          then Log(plsInfo, '''' + FileName + ''' compiled successfully for Web, iOS, Android, WinPhone to '''+FOutFileName+'''.')   // I4140   // I4504
          else Log(plsError, '''' + FileName + ''' was not compiled successfully for Web, iOS, Android, WinPhone.');   // I4140   // I4504
      finally
        ckw.Free;
      end;
    end;
  finally
    TProject.CompilerMessageFile := nil;
  end;
end;

initialization
  RegisterProjectFileType('.kmn', TkmnProjectFileAction);
end.
