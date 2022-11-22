(*
  Name:             Keyman.Developer.System.Project.kvkProjectFile
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Apr 2015

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:
  Todo:
  Notes:
  History:          30 Apr 2015 - mcdurdin - I4682 - V9.0 - Installing a keyboard with an OSK from Developer fails to install the OSK
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
*)
unit Keyman.Developer.System.Project.kvkProjectFile;   // I4682   // I4687

interface

uses
  System.SysUtils,

  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.System.Project.ProjectFiles,
  Keyman.Developer.System.Project.ProjectFileType;

type
  TkvkProjectFile = class;

  TkvkProjectFile = class(TOpenableProjectFile)
  protected
    function GetRelativeOrder: Integer; override;
  end;

type
  TkvksProjectFile = class;

  TkvksProjectFile = class(TOpenableProjectFile)
  protected
    function GetRelativeOrder: Integer; override;
  end;

implementation

uses
  System.Classes;

{-------------------------------------------------------------------------------
 - TkvkProjectFile                                                             -
 -------------------------------------------------------------------------------}

function TkvkProjectFile.GetRelativeOrder: Integer;
begin
  Result := 20;
end;

{-------------------------------------------------------------------------------
 - TkvksProjectFile                                                             -
 -------------------------------------------------------------------------------}

function TkvksProjectFile.GetRelativeOrder: Integer;
begin
  Result := 20;
end;

initialization
  RegisterProjectFileType('.kvk', TkvkProjectFile);
  RegisterProjectFileType('.kvks', TkvksProjectFile);
end.

