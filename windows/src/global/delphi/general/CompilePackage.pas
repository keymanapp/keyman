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
  kpsfile, kmpinffile, PackageInfo, ProjectLog;

type
  TCompilePackageMessageEvent = procedure(Sender: TObject; msg: string; State: TProjectLogState) of object;

function DoCompilePackage(pack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent: Boolean; const AOutputFileName: string): Boolean;   // I4688

implementation

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.IniFiles,
  System.Zip,

  OnlineConstants,
  VisualKeyboard,
  utilfiletypes,
  ErrorControlledRegistry,
  RegistryKeys,
  kmxfile,
  KeymanDeveloperOptions,
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

    procedure FatalMessage(const msg: string);
    procedure WriteMessage(AState: TProjectLogState; const msg: string);   // I4706

    function BuildKMP: Boolean;

    constructor Create(APack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent: Boolean; const AOutputFileName: string);   // I4688
    destructor Destroy; override;
    function Compile: Boolean;
    procedure CheckForDangerousFiles;
    procedure CheckKeyboardVersions;
  end;

function DoCompilePackage(pack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent: Boolean; const AOutputFileName: string): Boolean;   // I4688
begin
  with TCompilePackage.Create(pack, AMessageEvent, ASilent, AOutputFileName) do   // I4688
  try
    Result := Compile;
  finally
    Free;
  end;
end;


constructor TCompilePackage.Create(APack: TKPSFile; AMessageEvent: TCompilePackageMessageEvent; ASilent: Boolean; const AOutputFileName: string);   // I4688
begin
  inherited Create;
  pack := APack;
  FOnMessage := AMessageEvent;
  FSilent := ASilent;
  FOutputFileName := AOutputFileName;
  FTempFiles := TTempFiles.Create;
end;

destructor TCompilePackage.Destroy;
begin
  FreeAndNil(FTempFiles);
  inherited Destroy;
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

  if pack.Info.Desc['Name'] = '' then
  begin
    FatalMessage('You need to fill in the package name before building.');
    Exit;
  end;

  CheckForDangerousFiles;
  CheckKeyboardVersions;   // I4690

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
  i: Integer;
  f: TTempFile;
begin
  Result := False;

  kmpinf := TKMPInfFile.Create;
  try
    { Create KMP.INF }

    kmpinf.Assign(pack);
    kmpinf.RemoveFilePaths;
    kmpinf.FileName := FTempPath + '\kmp.inf';

    psf := TPackageContentFile.Create(kmpinf);
    psf.FileName := 'kmp.inf';
    psf.Description := 'Package information';
    psf.CopyLocation := pfclPackage;
    kmpinf.Files.Add(psf);
    kmpinf.SaveIni;
  finally
    kmpinf.Free;
  end;

  try
    ForceDirectories(ExtractFileDir(FOutputFilename));
    if FileExists(FOutputFilename) then
      DeleteFile(FOutputFilename);

    { Create output file }

    try
      with TZipFile.Create do
      try
        Open(FOutputFilename, TZipMode.zmWrite);
        Add(FTempPath + '\kmp.inf');
        for i := 0 to pack.Files.Count - 1 do
        begin
          if not FileExists(pack.Files[i].FileName) then
            WriteMessage(plsWarning, 'Warning: File '+pack.Files[i].FileName+' does not exist.')
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
        if FileCount < pack.Files.Count + 1 then
          WriteMessage(plsError, 'The build was not successful. Some files were skipped.')
        else
        begin
          Result := True;
          WriteMessage(plsInfo, 'Build successfully completed.');
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

end.

