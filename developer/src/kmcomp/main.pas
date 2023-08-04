(*
  Name:             main
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    02 Aug 2006 - mcdurdin - Timeout when Beta expires
                    25 Jan 2007 - mcdurdin - Add -x option to encrypt kmx and kct files
                    30 Apr 2007 - mcdurdin - Initialize COM before building
                    30 May 2007 - mcdurdin - I817 - build bootstrap installer option
                    04 Jun 2007 - mcdurdin - I817 - support updating to a new msi file
                    27 Mar 2008 - mcdurdin - I1369 - Support compiling KeymanWeb keyboards in KMCOMP
                    25 May 2010 - mcdurdin - I2392 - Activation Client integration
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    13 Dec 2012 - mcdurdin - I3681 - V9.0 - KeymanWeb compiler should output formatted js when debug=1
                    05 May 2015 - mcdurdin - I4699 - V9.0 - Compile .kpj files from kmcomp
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    11 May 2015 - mcdurdin - I4707 - V9.0 - Add clean target to kmcomp
                    22 Jun 2015 - mcdurdin - I4763 - Package compiler (buildpkg) needs to specify username and password on command line
                    03 Aug 2015 - mcdurdin - I4825 - Make kmcomp command line details consistent
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile

*)
unit main;  // I3306

interface

procedure Run;

implementation

uses
  System.AnsiStrings,
  System.SysUtils,
  Winapi.ActiveX,
  Winapi.Windows,

  KeyboardParser,
  kmxfileconsts,
  Keyman.Developer.System.Project.ProjectLog,
  Keyman.Developer.System.Project.ProjectLogConsole,
  Keyman.Developer.System.ValidateRepoChanges,
  VersionInfo,
  compile,
  KCCompilePackage,
  KCCompileProject,
  KCCompileKVK,
  KeymanVersion,
  CompileKeymanWeb,
  JsonExtractKeyboardInfo,
  ValidateKeyboardInfo,
  MergeKeyboardInfo,
  UKeymanTargets;

function CompileKeyboard(FInFile, FOutFile: string; FDebug, FWarnAsError: Boolean): Boolean; forward;   // I4706
function KCSetCompilerOptions(const FInFile: string; FShouldAddCompilerVersion: Boolean): Boolean; forward;
//function CompilerMessage(line: Integer; msgcode: LongWord; text: PAnsiChar): Integer; stdcall; forward;
procedure FixupPathSlashes(var path: string); forward;

procedure Run;
var
  hOutfile: THandle;
  FNologo, FDebug, FSilent, FError: Boolean;
  s, FParamTarget, FParamInfile, FParamOutfile, FParamDebugfile: string;
  i: Integer;
  FInstaller: Boolean;
  FUpdateInstaller: Boolean;
  FInstallerMSI: string;
  FClean: Boolean;
  FValidateRepoChanges, FFullySilent: Boolean;
  FWarnAsError: Boolean;
  FCheckFilenameConventions: Boolean;
  FValidating: Boolean;
  FMerging: Boolean;
  FParamInfile2: string;
  FParamJsonFields: string;
  FJsonExtract: Boolean;
  FParamDistribution: Boolean;
  FMergingValidateIds: Boolean;
  FShouldAddCompilerVersion: Boolean;
  FJsonSchemaPath: string;
  FParamSourcePath: string;
  FParamHelpLink: string;
  FColorMode: TProjectLogConsole.TColorMode;
  cmd, spc: string;
begin
  FSilent := False;
  FFullySilent := False;
  FDebug := False;
  FError := False;
  FInstaller := False;
  FUpdateInstaller := False;
  FClean := False;
  FNologo := False;
  FValidateRepoChanges := False;
  FWarnAsError := False;
  FCheckFilenameConventions := False;
  FValidating := False;
  FJsonExtract := False;
  FMerging := False;
  FMergingValidateIds := False;
  FParamDistribution := False;
  FInstallerMSI := '';
  FColorMode := cmDefault;

  FShouldAddCompilerVersion := True;

  FParamInfile := '';
  FParamOutfile := '';
  FParamDebugfile := '';
  FParamTarget := '';   // I4699

  FJsonSchemaPath := ExtractFilePath(ParamStr(0));

  i := 1;
  while i <= ParamCount do
  begin
    s := LowerCase(ParamStr(i));
    if s = '-nologo' then   // I4706
      FNologo := True
    else if s = '-validate-repo-changes' then
      FValidateRepoChanges := True
    else if s = '-s' then FSilent := True   // I4706
    else if s = '-ss' then   // I4706
    begin
      FSilent := True;
      FFullySilent := True;
    end
    else if s = '-c' then FClean := True   // I4707
    else if s = '-u' then FUpdateInstaller := True
    else if s = '-d' then FDebug := True
    else if s = '-w' then FWarnAsError := True   // I4706
    else if s = '-cfc' then FCheckFilenameConventions := True
    else if s = '-t' then   // I4699
    begin
      Inc(i);
      if FParamTarget <> ''
        then FError := True
        else FParamTarget := ParamStr(i);
    end
    else if s = '-v' then FValidating := True
    else if s = '-vs' then FValidating := True
    else if s = '-vd' then begin FValidating := True; FParamDistribution := True; end
    else if s = '-m' then
    begin
      FMerging := True;
      if (FParamInfile <> '') and (FParamInfile2 = '') then
      begin
        Inc(i);
        FParamInfile2 := ParamStr(i);
      end;
    end
    else if s = '-source-path' then
    begin
      Inc(i);
      FParamSourcePath := ParamStr(i);
    end
    else if s = '-add-help-link' then
    begin
      Inc(i);
      FParamHelpLink := ParamStr(i);
    end
    else if s = '-schema-path' then
    begin
      Inc(i);
      FJsonSchemaPath := IncludeTrailingPathDelimiter(ParamStr(i));
    end
    else if s = '-m-validate-id' then
      FMergingValidateIds := True
    else if s = '-extract-keyboard-info' then
    begin
      FJsonExtract := True;
      Inc(i);
      FParamJsonFields := ParamStr(i);
    end
    else if s = '-color' then
      FColorMode := cmForceColor
    else if s = '-no-color' then
      FColorMode := cmForceNoColor
    else if s = '-no-compiler-version' then
      FShouldAddCompilerVersion := False
    else if (s = '-help') or (s = '-h') then
    begin
      // Force help
      FParamInfile := '';
      Break;
    end
    else if FParamInfile = '' then FParamInfile := ParamStr(i)
    else if FParamOutfile = '' then FParamOutfile := ParamStr(i)
    else if FParamDebugfile = '' then FParamDebugfile := ParamStr(i)
    else FError := True;
    Inc(i);
  end;

  if FUpdateInstaller and (FInstallerMSI = '') then
  begin
    writeln('Invalid arguments: -u cannot be specified if installer.msi is not.');
    ExitCode := 3;
    Exit;
  end;

  if (not FSilent and not FNologo) or FError then   // I4706
  begin
{$IFDEF WIN64}
    writeln(SKeymanDeveloperName + ' Compiler (64-bit)');
{$ELSE}
    writeln(SKeymanDeveloperName + ' Compiler (32-bit)');
{$ENDIF}
    writeln('Version ' + CKeymanVersionInfo.VersionWithTag + ', ' + GetVersionCopyright);
  end;

  if FError or (FParamInfile = '') then
  begin
    cmd := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
    spc := StringOfChar(' ', cmd.Length);
    writeln('');
    writeln('Usage: '+cmd+' [-s[s]] [-nologo] [-c] [-d] [-w] [-cfc] [-v[s|d]] [-source-path path] [-schema-path path] ');
    writeln('       '+spc+' [-m] infile [-m infile] [-t target] [outfile.kmx|outfile.js [error.log]]');   // I4699
    writeln('       '+spc+' [-add-help-link path] [-color|-no-color] [-no-compiler-version]');
    writeln('       '+spc+' [-extract-keyboard-info field[,field...]]');
    writeln('          infile        can be a .kmn file (Keyboard Source, .kps file (Package Source), or .kpj (project)');   // I4699   // I4825
    writeln('                        if -v specified, can also be a .keyboard_info file');
    writeln('          outfile.kmx   can only be specified for a .kmn infile');
    writeln('          outfile.js    write a KeymanWeb file');
    writeln('          error.log     write to an error log; outfile must be specified');   // I4825
    writeln;
    writeln('          -h, -help      print this help information');
    writeln('          -s             silent; don''t print information-level messages');
    writeln('          -ss            fully silent; don''t print anything (except fatal errors)');
    writeln('          -nologo        don''t print the compiler description');
    writeln('          -c             clean target (only for .kpj)');
    writeln('          -d             include debug information');
    writeln('          -w             treat warnings as errors');
    writeln('          -cfc           check filename conventions');
    writeln('          -t             build only the target file from the project (only for .kpj)');   // I4699
    writeln('          -add-help-link path to help file on https://help.keyman.com/keyboards');
    writeln;
    writeln('          -color         If specified, forces color log messages on');
    writeln('          -no-color      If specified, forces color log messages off. If neither specified,');
    writeln('                         uses console mode to determine whether color should be used.');
    writeln;
    writeln('          -no-compiler-version   Don''t embed the compiler version stores, useful for regression tests.');
    writeln;
    writeln(' JSON .keyboard_info compile targets:');
    writeln('          -v[s]    validate infile against source schema');
    writeln('          -vd      validate infile against distribution schema');
    writeln('          -m       merge information from infile (can be .kmp and .js) into .keyboard_info output file');
    writeln('          -m-validate-id  validate the id against the .js, .kmx and .kmp filenames when merging');
    writeln('          -extract-keyboard-info   print json data .keyboard_info for build script integration');
    writeln('          -source-path    specify path to add to the sourcePath field in the .keyboard_info output file');
    writeln('          -schema-path    specify path to the keyboard_info json schema definitions');
    writeln('                          if not specified, then defaults to same folder as kmcomp.exe');

    if FError
      then ExitCode := 2
      else ExitCode := 0;
    Exit;
  end;

  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    if not FSilent then writeln('');

    FixupPathSlashes(FParamDebugFile);
    FixupPathSlashes(FParamTarget);
    FixupPathSlashes(FParamInfile);
    FixupPathSlashes(FParamOutfile);
    FixupPathSlashes(FParamInfile2);
    FixupPathSlashes(FInstallerMSI);
    FixupPathSlashes(FJsonSchemaPath);

    if FParamDebugfile <> '' then
    begin
      hOutfile := CreateFile(PChar(FParamDebugfile), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, 0, 0);
      if hOutfile = INVALID_HANDLE_VALUE then hOutfile := 0;
    end
    else
      hOutfile := 0;

    TProjectLogConsole.Create(FSilent, FFullySilent, hOutfile, FColorMode);

    KCSetCompilerOptions(FParamInfile, FShouldAddCompilerVersion);

    if FValidateRepoChanges then
      FError := not TValidateRepoChanges.Execute(FParamInfile, FParamOutfile)
    else if FMerging then
      FError := not TMergeKeyboardInfo.Execute(FParamSourcePath, FParamInfile, FParamInfile2, FParamOutfile, FParamHelpLink, FMergingValidateIds, FSilent, TProjectLogConsole.Instance.Log)
    else if FValidating then
      FError := not TValidateKeyboardInfo.Execute(FParamInfile, FJsonSchemaPath, FParamDistribution, FSilent, TProjectLogConsole.Instance.Log)
    else if FJsonExtract then
      FError := not TJsonExtractKeyboardInfo.Execute(FParamInfile, FParamJsonFields, FSilent, TProjectLogConsole.Instance.Log)
    else if LowerCase(ExtractFileExt(FParamInfile)) = '.kpj' then   // I4699
      Ferror := not DoKCCompileProject(FParamInfile, FDebug, FClean, FWarnAsError, FCheckFilenameConventions, FParamTarget)   // I4706   // I4707
    else if LowerCase(ExtractFileExt(FParamInfile)) = '.kps' then
      FError := not DoKCCompilePackage(FParamInfile, FWarnAsError, FInstaller, FCheckFilenameConventions, FInstallerMSI, FUpdateInstaller, FJsonSchemaPath)   // I4706
    else
      FError := not CompileKeyboard(FParamInfile, FParamOutfile, FDebug, FWarnAsError);   // I4706

    if hOutfile <> 0 then CloseHandle(hOutfile);
  finally
    CoUninitialize();
  end;

  if FError then
    ExitCode := 1;
end;

function KCSetCompilerOptions(const FInFile: string; FShouldAddCompilerVersion: Boolean): Boolean;
var
  opt: TCompilerOptions;
begin
  TProjectLogConsole.Instance.Filename := FInFile;

  opt.dwSize := sizeof(TCompilerOptions);
  opt.ShouldAddCompilerVersion := FShouldAddCompilerVersion;

  Result := SetCompilerOptions(@opt, @CompilerMessageW);

  if not Result then
  begin
    TProjectLogConsole.Instance.Log(plsError, FInFile, 'Could not set compiler options', 0, 0);
  end;
end;

function CompileKeyboard(FInFile, FOutFile: string; FDebug, FWarnAsError: Boolean): Boolean;   // I4706
var
  FIsJS, FIsKMX: Boolean;
  kp: TKeyboardParser;
  FTargets: TKeymanTargets;
begin
  if ExtractFileExt(FOutFile) = '.*' then
  begin
    // Load the input .kmn and determine if it targets .js and .kmx
    kp := TKeyboardParser.Create;
    try
      kp.LoadFromFile(FInFile);

      // Compile targets - copied from kmnProjectFile
      FTargets := StringToKeymanTargets(kp.GetSystemStoreValue(ssTargets));
      if ktAny in FTargets then FTargets := AllKeymanTargets;
      if FTargets = [] then FTargets := [ktWindows];

      FIsJS := FTargets * KMWKeymanTargets <> [];
      FIsKMX := FTargets * KMXKeymanTargets <> [];
    finally
      kp.Free;
    end;
  end
  else
  begin
    FIsJS := SameText(ExtractFileExt(FOutFile), '.js');
    FIsKMX := not FIsJS;
  end;

  Result := True;

  if FIsJS then
  begin
    if FOutFile = '' then FOutFile := FInFile;
    FOutFile := ChangeFileExt(FOutFile, '.js');

    with TCompileKeymanWeb.Create do
    try
      Result := Result and Compile(nil, FInFile, FOutFile, FDebug, @CompilerMessageW);   // I3681   // I4865   // I4866
    finally
      Free;
    end;

    if TProjectLogConsole.Instance.HasWarning and FWarnAsError then Result := False;   // I4706
    if Result
      then TProjectLogConsole.Instance.Log(plsSuccess, FInFile, 'Keyboard '+FInFile+' compiled, output saved as '+FOutFile+'.', 0, 0)
      else TProjectLogConsole.Instance.Log(plsFailure, FInFile, 'Keyboard '+FInFile+' could not be compiled.', 0, 0);
  end;

  if Result and FIsKMX then
  begin
    if FOutFile = '' then FOutFile := FInFile;
    FOutFile := ChangeFileExt(FOutFile, '.kmx');
    Result := Result and (CompileKeyboardFile(PChar(FInFile), PChar(FOutFile), FDebug, FWarnAsError, True, @CompilerMessage) <> 0);   // I4865   // I4866
    Result := Result and CompileVisualKeyboardFromKMX(FInFile, FOutFile);

    if TProjectLogConsole.Instance.HasWarning and FWarnAsError then Result := False;   // I4706
    if Result
      then TProjectLogConsole.Instance.Log(plsSuccess, FInFile, 'Keyboard '+FInFile+' compiled, output saved as '+FOutFile+'.', 0, 0)
      else TProjectLogConsole.Instance.Log(plsFailure, FInFile, 'Keyboard '+FInFile+' could not be compiled.', 0, 0);
  end;
end;

procedure FixupPathSlashes(var path: string);
begin
  path := path.Replace('/', '\', [rfReplaceAll]);
end;


end.

