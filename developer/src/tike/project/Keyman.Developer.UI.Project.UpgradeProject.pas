unit Keyman.Developer.UI.Project.UpgradeProject;

interface

uses
  Keyman.Developer.System.Project.ProjectFile;

type
  TUpgradeResult = (urNoAction, urUpgraded, urCancelled);

function TryUpgradeProject(Project: TProject): TUpgradeResult;

implementation

uses
  System.Classes,
  System.UITypes,
  Vcl.Controls,
  Vcl.Dialogs,

  UfrmMessages,
  Keyman.Developer.System.Project.ProjectLog,
  KeymanDeveloperOptions;

function TryUpgradeProject(Project: TProject): TUpgradeResult;
var
  msg: string;
begin
  Result := urNoAction;

  if Project.Options.Version = pv20 then
  begin
    // We are already up to date
    Exit;
  end;

{ TODO:
 if not FKeymanDeveloperOptions.PromptForProjectUpgrade then
  begin
    // User wishes to stick with v1.0 projects
    Exit;
  end;
}

  frmMessages.Clear;

  if not Project.CanUpgrade then
  begin
    // Project has restrictions, such as files in wrong folders, so
    // we cannot upgrade. Show a message for the user
    for msg in Project.UpgradeMessages do
    begin
      Project.Log(plsError, Project.FileName, msg, 0, 0);
    end;
    MessageDlg('Some issues must be addressed before the current project can '+
      'be upgraded to Keyman Developer 17 format.  '+
      'Details of the issues are listed in the Messages panel.', mtError, [mbOk], 0);
    Exit;
  end;

  case MessageDlg('The current project can be upgraded to Keyman Developer 17.0 format.  '+
      'Do you wish to upgrade it (recommended)?'#13#10#13#10+
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
