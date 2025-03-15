(*
  Name:             Keyman.Developer.UI.Project.ProjectUI
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 May 2015

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes

*)
unit Keyman.Developer.UI.Project.ProjectUI;   // I4687

interface

uses
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI;

function GetGlobalProjectUI: TProjectUI;
function LoadGlobalProjectUI(pt: TProjectType; AFilename: string): TProjectUI;
procedure FreeGlobalProjectUI;
function IsGlobalProjectUIReady: Boolean;
function CreateTempGlobalProjectUI(pt: TProjectType): TProjectUI;

implementation

uses
  System.Hash,
  System.SysUtils,
  Winapi.Windows,

  Keyman.Developer.System.KeymanDeveloperPaths,
  Keyman.Developer.System.TikeMultiProcess,
  Keyman.Developer.System.Project.Project,
  utildir;

function LockProject(const AFilename: string): Boolean; forward;
procedure UnlockProject; forward;

function GetGlobalProjectUI: TProjectUI;
begin
  Result := FGlobalProject as TProjectUI;
end;

function IsGlobalProjectUIReady: Boolean;
begin
  Result := Assigned(FGlobalProject);
end;

procedure FreeGlobalProjectUI;
begin
  try
    FreeAndNil(FGlobalProject);
    TikeMultiProcess.CloseProject;
  finally
    UnlockProject;
  end;
end;

function CreateTempGlobalProjectUI(pt: TProjectType): TProjectUI;
var
  kpj: TProject;
  AFilename: string;
begin
  Assert(not Assigned(FGlobalProject));

  AFileName := KGetTempFileName('.kpj');
  System.SysUtils.DeleteFile(AFileName);

  if not LockProject(AFilename) then
  begin
    Exit(nil);
  end;

  kpj := TProject.Create(pt, AFilename, False);
  try
    kpj.Options.Version := pv10;
    kpj.Options.BuildPath := ''; //'$SOURCEPATH';
    kpj.Options.WarnDeprecatedCode := True;
    kpj.Options.CompilerWarningsAsErrors := True;
    kpj.Options.CheckFilenameConventions := False;
    kpj.Options.SkipMetadataFiles := True;
    kpj.Save;
  finally
    kpj.Free;
  end;

  Result := TProjectUI.Create(pt, AFilename, True);
  Result.IsTemporary := True;

  FGlobalProject := Result;
  TikeMultiProcess.OpenProject(
    '', // We use the empty string to designate a temp project (* is no project)
    FGlobalProject.GetTargetFilename(FGlobalProject.Options.SourcePath,
      '', '')
  );
end;

function LoadGlobalProjectUI(pt: TProjectType; AFilename: string): TProjectUI;
begin
  Assert(not Assigned(FGlobalProject));

  if not LockProject(AFilename) then
  begin
    Exit(nil);
  end;

  Result := TProjectUI.Create(pt, AFilename, True);   // I4687
  FGlobalProject := Result;
  TikeMultiProcess.OpenProject(
    FGlobalProject.FileName,
    FGlobalProject.GetTargetFilename(FGlobalProject.Options.SourcePath,
      '', '')
  );
end;

var
  hLockFile: THandle = INVALID_HANDLE_VALUE;

function LockProject(const AFilename: string): Boolean;
var
  LockFilename: string;
begin
  Assert(hLockFile = INVALID_HANDLE_VALUE);

  if not ForceDirectories(TKeymanDeveloperPaths.LockFilePath) then
  begin
    Exit(False);
  end;

  LockFilename := TKeymanDeveloperPaths.LockFilePath + THashSHA1.GetHashString(AFilename.ToLower) + '.lock';

  hLockFile := CreateFile(PChar(LockFilename), GENERIC_READ or GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_FLAG_DELETE_ON_CLOSE, 0);
  Result := hLockFile <> INVALID_HANDLE_VALUE;
end;

procedure UnlockProject;
begin
  if not hLockFile = INVALID_HANDLE_VALUE then
  begin
    Exit;
  end;

  CloseHandle(hLockFile);
  hLockFile := INVALID_HANDLE_VALUE;
end;

end.
