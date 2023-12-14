(*
  Name:             Keyman.Developer.System.Project.ProjectFiles
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      1 Aug 2006

  Modified Date:    4 May 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          01 Aug 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Test if file is open - WindowOpen
                    16 Jan 2009 - mcdurdin - I1769 - Add support for HTM, HTLM, XML to open for edit in Keyman Developer
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    04 May 2015 - mcdurdin - I4687 - V9.0 - Split project UI actions into separate classes
                    04 May 2015 - mcdurdin - I4694 - V9.0 - Split UI actions from non-UI actions in projects
*)
unit Keyman.Developer.System.Project.ProjectFiles;  // I3306   // I4687

interface

uses
  Keyman.Developer.System.Project.ProjectFile;

type
  TShellProjectFile = class(TProjectFile)
  protected
    function GetRelativeOrder: Integer; override;
    procedure GetFileParameters; override;
  end;

  TOpenableProjectFile = class(TShellProjectFile)
  protected
    function GetRelativeOrder: Integer; override;
  end;

implementation

uses
  Keyman.Developer.System.Project.ProjectFileType;

function TOpenableProjectFile.GetRelativeOrder: Integer;
begin
  Result := 10;
end;

function TShellProjectFile.GetRelativeOrder: Integer;
begin
  Result := 0;
end;

procedure TShellProjectFile.GetFileParameters;
begin
end;

initialization
  RegisterProjectFileType('*', TShellProjectFile);
  RegisterProjectFileType('.bmp', TOpenableProjectFile);
  RegisterProjectFileType('.ico', TOpenableProjectFile);
  RegisterProjectFileType('.txt', TOpenableProjectFile);
  RegisterProjectFileType('.md', TOpenableProjectFile);
  RegisterProjectFileType('.model_info', TOpenableProjectFile);
  RegisterProjectFileType('.htm', TOpenableProjectFile);   // I1769
  RegisterProjectFileType('.html', TOpenableProjectFile);  // I1769
  RegisterProjectFileType('.xml', TOpenableProjectFile);   // I1769
  RegisterProjectFileType('.js', TOpenableProjectFile);
  RegisterProjectFileType('.kpj', TOpenableProjectFile);
  RegisterProjectFileType('.user', TOpenableProjectFile);
  RegisterProjectFileType('.keyman-touch-layout', TOpenableProjectFile);
  RegisterProjectFileType('.keyboard_info', TOpenableProjectFile);
end.

