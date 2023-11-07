unit Keyman.Developer.UI.Project.UpgradeProject;

interface

uses
  Keyman.Developer.System.Project.ProjectFile;

type
  TUpgradeResult = (urNoAction, urUpgraded, urCancelled);

function TryUpgradeProject(Project: TProject): TUpgradeResult;

implementation

uses
  System.UITypes,
  Vcl.Controls,
  Vcl.Dialogs,

  KeymanDeveloperOptions;

function TryUpgradeProject(Project: TProject): TUpgradeResult;
begin
  Result := urNoAction;

  if Project.Options.Version = pv20 then
  begin
    // We are already up to date
    Exit;
  end;

{  if not FKeymanDeveloperOptions.PromptForProjectUpgrade then
  begin
    // User wishes to stick with v1.0 projects
    Exit;
  end;}

  if not Project.CanUpgrade then
  begin
    // Project has restrictions, such as files in wrong folders, so
    // we cannot upgrade. Show a message for the user
    ShowMessage('The current project cannot be upgraded to v2.0. The following errors were encountered:'#13#10+
      Project.UpgradeMessages.Text);
    Exit;
  end;

  case MessageDlg('The current project can be upgraded to Keyman Developer 17.0 format. Do you wish to upgrade it (recommended)?'#13#10#13#10+
      'Note: upgraded projects will not be readable by older versions of Keyman Developer.',
      mtConfirmation, mbYesNoCancel, 0) of
    mrNo: Exit;
    mrCancel: Exit(urCancelled);
  end;

  // .. do the upgrade

  Project.Upgrade;

  Result := urUpgraded;
end;

end.
