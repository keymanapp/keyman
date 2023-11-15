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

implementation

uses
  System.SysUtils,

  Keyman.Developer.System.Project.Project;

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
end;

function LoadGlobalProjectUI(pt: TProjectType; AFilename: string): TProjectUI;
begin
  Assert(not Assigned(FGlobalProject));
  if DirectoryExists(AFilename) then
  begin
    // Load a directory-based project
    if AFilename.EndsWith('\') then
      AFilename := AFilename.Substring(0, AFilename.Length-1);
    AFilename := AFilename + '\' + ExtractFileName(AFilename) + '.kpj';
  end;
  Result := TProjectUI.Create(pt, AFilename);   // I4687
  FGlobalProject := Result;
end;

end.
