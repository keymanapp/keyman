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
  System.SysUtils,
  Winapi.Windows,

  Keyman.Developer.System.TikeMultiProcess,
  Keyman.Developer.System.Project.Project,
  utildir;

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
  FreeAndNil(FGlobalProject);
  TikeMultiProcess.CloseProject;
end;

function CreateTempGlobalProjectUI(pt: TProjectType): TProjectUI;
var
  kpj: TProject;
  AFilename: string;
begin
  Assert(not Assigned(FGlobalProject));

  AFileName := KGetTempFileName('.kpj');
  System.SysUtils.DeleteFile(AFileName);

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
  Result := TProjectUI.Create(pt, AFilename, True);   // I4687
  FGlobalProject := Result;
  TikeMultiProcess.OpenProject(
    FGlobalProject.FileName,
    FGlobalProject.GetTargetFilename(FGlobalProject.Options.SourcePath,
      '', '')
  );
end;

end.
