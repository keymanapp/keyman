(*
  Name:             CompilePackage
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    11 May 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Add message for missing files when compiling package
                    04 Jun 2007 - mcdurdin - Remove unused KeymanPath
                    20 Jun 2007 - mcdurdin - Remove unused ActivationManager reference
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    04 May 2015 - mcdurdin - I4690 - V9.0 - Pull keyboard version into package version when adding a keyboard
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
*)
unit CompilePackage;

interface

uses
  kpsfile, kmpinffile, PackageInfo,
  Keyman.Developer.System.Project.ProjectLog;

function DoCompilePackage(pack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent, ACheckFilenameConventions: Boolean; const AOutputFileName: string): Boolean;   // I4688

implementation

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.IniFiles,
  System.Zip,

  BCP47Tag,
  OnlineConstants,
  VisualKeyboard,
  utilfiletypes,
  ErrorControlledRegistry,
  RegistryKeys,
  kmxfile,
  KeymanDeveloperOptions,
  KeymanVersion,

  Keyman.System.KeyboardUtils,
  Keyman.System.LexicalModelUtils,
  Keyman.System.CanonicalLanguageCodeUtils,
  Keyman.System.PackageInfoRefreshKeyboards,
  Keyman.System.PackageInfoRefreshLexicalModels,

  RedistFiles,
  TempFileManager;

type
  TCompilePackage = class
  private
    FSilent: Boolean;
    FOnMessage: TCompilePackageMessageEvent;
    FTempPath: string;
    //FRuntimeSourcePath: string;
    pack: TKPSFile;

    FOutputFileName: string;
    FTempFiles: TTempFiles;
    FCheckFilenameConventions: Boolean;

    procedure FatalMessage(const msg: string);
    procedure WriteMessage(AState: TProjectLogState; const msg: string);   // I4706

    function BuildKMP: Boolean;

    constructor Create(APack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent, ACheckFilenameConventions: Boolean; const AOutputFileName: string);   // I4688
    destructor Destroy; override;
    function Compile: Boolean;
    procedure CheckForDangerousFiles;
    procedure CheckKeyboardVersions;
    procedure CheckKeyboardLanguages;
    procedure CheckFilenameConventions;
    function CheckLexicalModels: Boolean;
  end;

function DoCompilePackage(pack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent, ACheckFilenameConventions: Boolean; const AOutputFileName: string): Boolean;   // I4688
begin
  with TCompilePackage.Create(pack, AMessageEvent, ASilent, ACheckFilenameConventions, AOutputFileName) do   // I4688
  try
    Result := Compile;
  finally
    Free;
  end;
end;


constructor TCompilePackage.Create(APack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent, ACheckFilenameConventions: Boolean; const AOutputFileName: string);   // I4688
begin
  inherited Create;
  pack := APack;
  FOnMessage := AMessageEvent;
  FSilent := ASilent;
  FOutputFileName := AOutputFileName;
  FTempFiles := TTempFiles.Create;
  FCheckFilenameConventions := ACheckFilenameConventions;
end;

destructor TCompilePackage.Destroy;
begin
  FreeAndNil(FTempFiles);
  inherited Destroy;
end;

procedure TCompilePackage.CheckFilenameConventions;
var
  i: Integer;
begin
  if not FCheckFilenameConventions then
    Exit;

  if pack.LexicalModels.Count > 0 then
  begin
    if not TLexicalModelUtils.DoesPackageFilenameFollowLexicalModelConventions(pack.FileName) then
      WriteMessage(plsWarning, Format(TKeyboardUtils.SPackageNameDoesNotFollowLexicalModelConventions_Message, [ExtractFileName(pack.FileName)]));
  end
  else
  begin
    if not TKeyboardUtils.DoesKeyboardFilenameFollowConventions(pack.FileName) then
      WriteMessage(plsWarning, Format(TKeyboardUtils.SKeyboardNameDoesNotFollowConventions_Message, [ExtractFileName(pack.FileName)]));
  end;

  for i := 0 to pack.Files.Count - 1 do
  begin
    if not TKeyboardUtils.DoesFilenameFollowConventions(pack.Files[i].FileName) then
      WriteMessage(plsWarning, Format(TKeyboardUtils.SFilenameDoesNotFollowConventions_Message, [ExtractFileName(pack.Files[i].FileName)]));
  end;
end;

function TCompilePackage.CheckLexicalModels: Boolean;
begin
  if pack.LexicalModels.Count > 0 then
  begin
    if pack.Keyboards.Count > 0 then
    begin
      FatalMessage('The package contains both lexical models and keyboards, which is not permitted.');
      Exit(False);
    end;
  end;

  Exit(True);
end;

function TCompilePackage.Compile: Boolean;
var
  buf: array[0..260] of Char;
  n: Integer;
  f: TSearchRec;
begin
  Result := False;

  if pack.FileName = '' then
  begin
    FatalMessage('You need to save the package before building.');
    Exit;
  end;

  WriteMessage(plsInfo, 'Compiling package ' + ExtractFileName(pack.FileName) + '...');

  if Trim(pack.Info.Desc[PackageInfo_Name]) = '' then
  begin
    FatalMessage('You need to fill in the package name before building.');
    Exit;
  end;

  CheckFilenameConventions;
  CheckForDangerousFiles;
  CheckKeyboardVersions;
  CheckKeyboardLanguages;
  if not CheckLexicalModels then Exit;

  GetTempPath(260, buf);
  FTempPath := buf;

  GetTempFileName(PChar(FTempPath), 'kmn', 0, buf);
  FTempPath := buf;
  if FileExists(FTempPath) then DeleteFile(FTempPath);

  CreateDir(FTempPath);
  try
    Result := BuildKMP;
  finally
    n := FindFirst(FTempPath + '\*.*', 0, f);
    if n = 0 then
    begin
      while n = 0 do
      begin
        DeleteFile(FTempPath + '\' + f.Name);
        n := FindNext(f);
      end;
      FindClose(f);
    end;
    RemoveDirectory(PChar(FTempPath));
  end;
end;


function TCompilePackage.BuildKMP: Boolean;
var
  kmpinf: TKMPInfFile;
  psf: TPackageContentFile;
  FPackageVersion: string;
  i: Integer;
  f: TTempFile;
begin
  Result := False;

  kmpinf := TKMPInfFile.Create;
  try
    { Create KMP.INF and KMP.JSON }

    kmpinf.Assign(pack);

    // Add keyboard information to the package 'for free'
    // Note: this does not get us very far for mobile keyboards as
    // they still require the .js to be added by the developer at this stage.
    // But it ensures that all keyboards in the package are listed in the
    // {Keyboards} section

    with TPackageInfoRefreshKeyboards.Create(kmpinf) do
    try
      OnError := Self.FOnMessage;
      if not Execute then
      begin
        WriteMessage(plsError, 'The package build was not successful.');
        Exit;
      end;
    finally
      Free;
    end;

    with TPackageInfoRefreshLexicalModels.Create(kmpinf) do
    try
      OnError := Self.FOnMessage;
      if not Execute then
      begin
        WriteMessage(plsError, 'The package build was not successful.');
        Exit;
      end;
    finally
      Free;
    end;

    //
    // Update the package version to the current compiled
    // keyboard version.
    //

    if pack.KPSOptions.FollowKeyboardVersion then
    begin
      if kmpinf.Keyboards.Count = 0 then
      begin
        FatalMessage('The option "Follow Keyboard Version" is set but there are no keyboards in the package.');
        Exit;
      end;

      FPackageVersion := kmpinf.Keyboards[0].Version;
      for i := 1 to kmpinf.Keyboards.Count - 1 do
        if kmpinf.Keyboards[i].Version <> FPackageVersion then
        begin
          FatalMessage(
            'The option "Follow Keyboard Version" is set but the package contains more than one keyboard, '+
            'and the keyboards have mismatching versions.');
          Exit;
        end;

      kmpinf.Info.Desc[PackageInfo_Version] := FPackageVersion;
    end;

    kmpinf.RemoveFilePaths;

    if kmpinf.LexicalModels.Count = 0
      then kmpinf.Options.FileVersion := SKeymanVersion70
      else kmpinf.Options.FileVersion := SKeymanVersion120;

    if kmpinf.Options.FileVersion = SKeymanVersion70 then
    begin
      psf := TPackageContentFile.Create(kmpinf);
      psf.FileName := 'kmp.inf';
      psf.Description := 'Package information';
      psf.CopyLocation := pfclPackage;

      kmpinf.Files.Add(psf);
    end;

    psf := TPackageContentFile.Create(kmpinf);
    psf.FileName := 'kmp.json';
    psf.Description := 'Package information (JSON)';
    psf.CopyLocation := pfclPackage;

    kmpinf.Files.Add(psf);

    kmpinf.FileName := FTempPath + '\kmp.inf';
    kmpinf.SaveIni;

    kmpinf.FileName := FTempPath + '\kmp.json';
    kmpinf.SaveJSON;
  finally
    kmpinf.Free;
  end;

  try
    ForceDirectories(ExtractFileDir(FOutputFilename));
    if FileExists(FOutputFilename) then
      DeleteFile(FOutputFilename);

    { Create output file }

    try

      if FileExists(FOutputFilename) then DeleteFile(FOutputFilename);

      with TZipFile.Create do
      try
        Open(FOutputFilename, TZipMode.zmWrite);
        Add(FTempPath + '\kmp.inf');
        Add(FTempPath + '\kmp.json');
        for i := 0 to pack.Files.Count - 1 do
        begin
          if not FileExists(pack.Files[i].FileName) then
            WriteMessage(plsWarning, 'File '+pack.Files[i].FileName+' does not exist.')
          else
          begin
            // When compiling the package, save the kvk keyboard into binary for delivery
            if pack.Files[i].FileType = ftVisualKeyboard then
            begin
              f := TTempFileManager.Get('.kvk');
              FTempFiles.Add(f);
              with TVisualKeyboard.Create do
              try
                try
                  LoadFromFile(pack.Files[i].FileName);
                except
                  on E:EVisualKeyboardLoader do
                  begin
                    WriteMessage(plsError, pack.Files[i].FileName+' is invalid: '+E.Message);
                    Continue;
                  end;
                end;
                SaveToFile(f.Name, kvksfBinary);
              finally
                Free;
              end;
              Add(f.Name, ExtractFileName(pack.Files[i].FileName));
            end
            else
              Add(pack.Files[i].FileName);
          end;
        end;
        if FileCount < pack.Files.Count + 2 then
          WriteMessage(plsError, 'The build was not successful. Some files were skipped.')
        else
        begin
          Result := True;
        end;
        Close;
      finally
        Free;
      end;
    except
      on E:EZipException do
      begin
        WriteMessage(plsError, E.Message);
      end;
    end;
  finally
    if not Result then
      if FileExists(FOutputFilename) then DeleteFile(FOutputFilename);
  end;
end;

procedure TCompilePackage.FatalMessage(const msg: string);
begin
  FOnMessage(Self, msg, plsFatal);   // I4706
end;

procedure TCompilePackage.WriteMessage(AState: TProjectLogState; const msg: string);   // I4706
begin
  FOnMessage(Self, msg, AState);   // I4706
end;

const
  SKKeymanRedistFileShouldNotBeInPackage = 'The Keyman system file ''%0:s'' should not be compiled into the package.  Use a redistributable installer instead.';
  SKDocFilesDangerous = 'Microsoft Word .doc or .docx files (''%0:s'') are not portable.  You should instead use HTML or PDF format.';

procedure TCompilePackage.CheckForDangerousFiles;
var
  i, j: Integer;
  s: string;
begin
  for i := 0 to pack.Files.Count - 1 do
  begin
    s := LowerCase(ExtractFileName(pack.Files[i].FileName));
    for j := Low(CRedistFiles) to High(CRedistFiles) do
      if s = CRedistFiles[j].FileName then
        WriteMessage(plsWarning, Format(SKKeymanRedistFileShouldNotBeInPackage, [s]));
    for j := Low(CRuntimeFiles) to High(CRuntimeFiles) do
      if s = CRuntimeFiles[j].FileName then
        WriteMessage(plsWarning, Format(SKKeymanRedistFileShouldNotBeInPackage, [s]));
    if (ExtractFileExt(s) = '.doc') or (ExtractFileExt(s) = '.docx') then WriteMessage(plsWarning, Format(SKDocFilesDangerous, [s]));
  end;
end;

const
  SKKeyboardPackageVersionMismatch = 'The keyboard %0:s has version %1:s, which differs from the package version %2:s.';

procedure TCompilePackage.CheckKeyboardVersions;   // I4690
var
  n, i: Integer;
  ki: TKeyboardInfo;
begin
  n := 0;

  // The version information is checked separately if we use
  // 'follow keyboard version'
  if pack.KPSOptions.FollowKeyboardVersion then
    Exit;

  for i := 0 to pack.Files.Count - 1 do
    if pack.Files[i].FileType = ftKeymanFile then Inc(n);

  // We only test for keyboard <> package version if 1 keyboard in package
  // For multi-keyboard packages, it isn't sensible to do the same
  if n <> 1 then
    Exit;

  for i := 0 to pack.Files.Count - 1 do
  begin
    if pack.Files[i].FileType <> ftKeymanFile then Continue;
    if not FileExists(pack.Files[i].FileName) then Continue;

    GetKeyboardInfo(pack.Files[i].FileName, True, ki);
    if ki.KeyboardVersion <> pack.Info.Desc[PackageInfo_Version] then
      WriteMessage(plsWarning, Format(SKKeyboardPackageVersionMismatch,
        [ExtractFileName(pack.Files[i].FileName), ki.KeyboardVersion, pack.Info.Desc[PackageInfo_Version]]));
    ki.MemoryDump.Free;
  end;
end;

const
  SKKeyboardPackageLanguageNonCanonical = 'The keyboard %0:s has a non-canonical language ID "%1:s" (%2:s), should be "%3:s".';

procedure TCompilePackage.CheckKeyboardLanguages;
var
  k: TPackageKeyboard;
  l: TPackageKeyboardLanguage;
  NewID: string;
  Tag, NewTag: TBCP47Tag;
begin
  for k in pack.Keyboards do
  begin
    for l in k.Languages do
    begin
      Tag := TBCP47Tag.Create(l.ID);
      NewID := TCanonicalLanguageCodeUtils.FindBestTag(l.ID, False);
      if NewID = '' then
      begin
        // We don't have enough data to validate this tag
        Continue;
      end;

      NewTag := TBCP47Tag.Create(NewID);
      try
        if (Tag.Script = '') and (NewTag.Script <> '') then
        begin
          // Only give non-canonical warning if the script tag is missing but should
          // be present.
          NewTag.Region := Tag.Region; // We don't care about region, don't give unhelpful info to developer
          WriteMessage(plsWarning, Format(SKKeyboardPackageLanguageNonCanonical, [k.ID, l.ID, l.Name, NewTag.Tag]));
        end;
      finally
        NewTag.Free;
        Tag.Free;
      end;
    end;
  end;
end;

end.

