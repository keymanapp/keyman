unit Keyman.Developer.UI.dmActionsModelEditor;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Actions,
  Vcl.ActnList;

type
  TmodActionsModelEditor = class(TDataModule)
    actionsModelEditor: TActionList;
    actModelCompile: TAction;
    actModelTest: TAction;
    actModelIncludeDebugInformation: TAction;
    procedure actModelCompileExecute(Sender: TObject);
    procedure actModelIncludeDebugInformationExecute(Sender: TObject);
    procedure actModelTestExecute(Sender: TObject);
    procedure actModelCompileUpdate(Sender: TObject);
    procedure actModelTestUpdate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  modActionsModelEditor: TmodActionsModelEditor;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

uses
  Vcl.Menus,
  Winapi.Windows,

  UfrmMain,
  UfrmMessages,
  Keyman.Developer.System.Project.ProjectFile,
  Keyman.Developer.UI.Project.ProjectFileUI,
  Keyman.Developer.System.Project.modelTsProjectFile,
  Keyman.Developer.UI.UfrmModelEditor;

{$R *.dfm}

function ActiveModelEditor: TfrmModelEditor;
begin
  if Assigned(frmKeymanDeveloper.ActiveChild) and (frmKeymanDeveloper.ActiveChild is TfrmModelEditor)
    then Result := frmKeymanDeveloper.ActiveChild as TfrmModelEditor
    else Result := nil;
end;

function ActiveModelProjectFile: TmodelTsProjectFile;
begin
  Result := ActiveModelEditor.ProjectFile as TmodelTsProjectFile;
end;

procedure TmodActionsModelEditor.actModelCompileExecute(Sender: TObject);
begin
  frmMessages.Clear;   // I4686

  if ActiveModelEditor <> nil then
  begin
    (ActiveModelProjectFile.UI as TProjectFileUI).DoAction(pfaCompile, False);
  end;
end;

procedure TmodActionsModelEditor.actModelCompileUpdate(Sender: TObject);
begin
  actModelCompile.Enabled := ActiveModelEditor <> nil;
  if actModelCompile.Enabled
    then actModelCompile.ShortCut := Vcl.Menus.Shortcut(VK_F7, [])
    else actModelCompile.ShortCut := scNone;
  frmKeymanDeveloper.mnuModel.Visible := actModelCompile.Enabled;
end;

procedure TmodActionsModelEditor.actModelIncludeDebugInformationExecute(
  Sender: TObject);
begin
// TODO: (ActiveModelProjectFile.UI as TmodeltsProjectFileUI).Debug := not (ActiveModelProjectFile.UI as TmodeltsProjectFileUI).Debug;   // I4687
end;

procedure TmodActionsModelEditor.actModelTestExecute(Sender: TObject);
begin
  (ActiveModelProjectFile.UI as TProjectFileUI).DoAction(pfaTestKeymanWeb, False);   // I4687
end;

procedure TmodActionsModelEditor.actModelTestUpdate(Sender: TObject);
begin
  actModelTest.Enabled := ActiveModelEditor <> nil;
end;

end.
