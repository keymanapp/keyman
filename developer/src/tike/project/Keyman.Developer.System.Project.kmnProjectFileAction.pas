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
    procedure CheckFilenameConventions;
  public
    function CompileKeyboard: Boolean;
    function Clean: Boolean;
    function DoSetCompilerOptions(addVersion,
      useLegacyCompiler: Boolean): Boolean;
  end;

implementation

uses
  System.Classes,
  System.StrUtils,
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
        OwnerProject.Log(plsError, AKVKSourceFile, 'Invalid visual keyboard: '+E.Message, CERR_ERROR, 0);
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
          OwnerProject.Log(plsError, AKVKSourceFile, 'Could not save visual keyboard '+AKVKSourceFile+' to '+AKVKTargetFile+': '+E.ClassName+','+E.Message, CERR_ERROR, 0);
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

procedure TkmnProjectFileAction.CheckFilenameConventions;
begin
  if not OwnerProject.Options.CheckFilenameConventions then
    Exit;

  if not TKeyboardUtils.DoesKeyboardFilenameFollowConventions(FileName) then
  begin
    HasCompileWarning := True;
    Log(plsWarning, Format(TKeyboardUtils.SKeyboardNameDoesNotFollowConventions_Message, [ExtractFileName(FileName)]), CERR_WARNING, 0);
  end;

  if KVKFileName <> '' then
  begin
    if not TKeyboardUtils.DoesKeyboardFilenameFollowConventions(KVKFileName) then
    begin
      HasCompileWarning := True;
      Log(plsWarning, Format(TKeyboardUtils.SKeyboardNameDoesNotFollowConventions_Message, [ExtractFileName(KVKFileName)]), CERR_WARNING, 0);
    end;
  end;
end;

function TkmnProjectFileAction.DoSetCompilerOptions(addVersion: Boolean; useLegacyCompiler: Boolean): Boolean;
var
  options: COMPILER_OPTIONS;
begin
  TProject.CompilerMessageFile := Self;
  options.dwSize := sizeof(COMPILER_OPTIONS);
  options.ShouldAddCompilerVersion := addVersion;
  // TODO: useLegacyCompiler means we switch to kmcmpdll vs kmc
  if not SetCompilerOptions(@options, ProjectCompilerMessage) then
  begin
    Log(plsFatal, 'Unable to set compiler options', CERR_FATAL, 0);
    Exit(False);
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
  TargetNames: string;
begin
  TProject.CompilerMessageFile := Self;
  HasCompileWarning := False;   // I4706

  FKVKSourceFile := ExtractFilePath(FileName) + ExtractFileName(KVKFileName);
  FKVKTargetFile := OwnerProject.GetTargetFileName(ChangeFileExt(FKVKSourceFile, '.kvk'), FileName, FileVersion);

  try
    CheckFilenameConventions;

    if Targets * KMXKeymanTargets <> [] then
    begin
      TargetNames := KeymanTargetsToNames(Targets * KMXKeymanTargets);
      Log(plsInfo, Format('Compiling ''%s'' %sfor %s...', [Filename, IfThen(Debug, 'with debug symbols ', ''), TargetNames]), 0, 0);

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
        then Log(plsSuccess, Format('''%s'' was compiled successfully for %s to ''%s''.', [FileName, TargetNames, KMXFileName]), 0, 0)   // I4504
        else Log(plsFailure, Format('''%s'' was not compiled successfully for %s.', [FileName, TargetNames]), 0, 0);   // I4504
    end
    else
      Result := True;   // I4564

    // compile keyboard to web
    if Result and (Targets * KMWKeymanTargets <> []) then
    begin
      TargetNames := KeymanTargetsToNames(Targets * KMWKeymanTargets);
      Log(plsInfo, Format('Compiling ''%s'' %sfor %s...', [Filename, IfThen(Debug, 'with debug symbols ', ''), TargetNames]), 0, 0);

      ckw := TCompileKeymanWeb.Create;
      try
        FOutFilename := JSTargetFileName;
        ForceDirectories(ExtractFileDir(FOutFileName));

        if KVKFileName <> '' then
          Result := CompileVisualKeyboard(FKVKSourceFile, FKVKTargetFile);

        if Result then
          Result := ckw.Compile(OwnerProject, FileName, FOutFileName, Debug, ProjectCompilerMessageW);   // I3681   // I4140   // I4865   // I4866

        if HasCompileWarning and (WarnAsError or OwnerProject.Options.CompilerWarningsAsErrors) then Result := False;   // I4706

        if Result
          then Log(plsSuccess, Format('''%s'' was compiled successfully for %s to ''%s''.', [FileName, TargetNames, FOutFileName]), 0, 0)   // I4504
          else Log(plsFailure, Format('''%s'' was not compiled successfully for %s.', [FileName, TargetNames]), 0, 0);   // I4504

        if Result and Assigned(OnCompileSuccess) then
          OnCompileSuccess(Self, FileName, FOutFileName);
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
