unit Keyman.Developer.UI.Project.wordlistTsvProjectFileUI;

interface

uses
  System.UITypes,

  Keyman.Developer.System.Project.wordlistTsvProjectFile,
  Keyman.Developer.UI.Project.ProjectFilesUI,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.UI.Project.ProjectUIFileType,
  UfrmMessages;

type
  TwordlistTsvProjectFileUI = class(TOpenableProjectFileUI)
  private
    function GetProjectFile: TwordlistTsvProjectFile;

  public
    function DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean; override;
    property ProjectFile: TwordlistTsvProjectFile read GetProjectFile;
  end;

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  Vcl.Dialogs,
  Vcl.Graphics,
  Vcl.Controls,

  dmActionsMain,
  UfrmMain,
  UfrmMDIEditor,
  KeymanDeveloperUtils,
  KeymanDeveloperOptions,
  System.Classes,
  System.Variants,
  utilsystem;

function TwordlistTsvProjectFileUI.DoAction(action: TProjectFileAction; FSilent: Boolean): Boolean;
begin
  Result := False;
end;

function TwordlistTsvProjectFileUI.GetProjectFile: TwordlistTsvProjectFile;
begin
  Result := FOwner as TwordlistTsvProjectFile;
end;

initialization
  RegisterProjectFileUIType(TwordlistTsvProjectFile, TwordlistTsvProjectFileUI);
end.




