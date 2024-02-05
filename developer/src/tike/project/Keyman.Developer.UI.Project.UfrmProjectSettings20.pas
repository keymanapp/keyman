(*
  Name:             Keyman.Developer.UI.Project.UfrmProjectSettings
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 May 2015

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile

*)
unit Keyman.Developer.UI.Project.UfrmProjectSettings20;   // I4688

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmProjectSettings20 = class(TForm)
    lblOutputPath: TLabel;
    editOutputPath: TEdit;
    Label2: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    Label3: TLabel;
    Label6: TLabel;
    chkCompilerWarningsAsErrors: TCheckBox;
    chkWarnDeprecatedCode: TCheckBox;
    chkCheckFilenameConventions: TCheckBox;
    lblSourcePath: TLabel;
    editSourcePath: TEdit;
    chkBuildMetadataFiles: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  Keyman.Developer.System.Project.Project,
  utildir;

procedure TfrmProjectSettings20.cmdOKClick(Sender: TObject);
var
  path: string;
begin
  path := Trim(DosSlashes(editOutputPath.Text));
  if (path <> '') and (not path.StartsWith('$PROJECTPATH') or (path.CountChar('\') > 1)) then
  begin
    ShowMessage('Output path "'+path+'" should start with "$PROJECTPATH" and be no more than one level deep.');
    Exit;
  end;

  path := Trim(DosSlashes(editSourcePath.Text));
  if (path <> '') and (not path.StartsWith('$PROJECTPATH') or (path.CountChar('\') > 1)) then
  begin
    ShowMessage('Source path "'+path+'" should start with "$PROJECTPATH" and be no more than one level deep.');
    Exit;
  end;

  FGlobalProject.Options.BuildPath := Trim(DosSlashes(editOutputPath.Text));
  FGlobalProject.Options.SourcePath := Trim(DosSlashes(editSourcePath.Text));
  FGlobalProject.Options.SkipMetadataFiles := not chkBuildMetadataFiles.Checked;
  FGlobalProject.Options.CompilerWarningsAsErrors := chkCompilerWarningsAsErrors.Checked;   // I4865
  FGlobalProject.Options.WarnDeprecatedCode := chkWarnDeprecatedCode.Checked;   // I4866
  FGlobalProject.Options.CheckFilenameConventions := chkCheckFilenameConventions.Checked;
  FGlobalProject.Save;
  ModalResult := mrOk;
end;

procedure TfrmProjectSettings20.FormCreate(Sender: TObject);
begin
  editOutputPath.Text := FGlobalProject.Options.BuildPath;
  editSourcePath.Text := FGlobalProject.Options.SourcePath;
  chkBuildMetadataFiles.Checked := not FGlobalProject.Options.SkipMetadataFiles;
  chkCompilerWarningsAsErrors.Checked := FGlobalProject.Options.CompilerWarningsAsErrors;   // I4865
  chkWarnDeprecatedCode.Checked := FGlobalProject.Options.WarnDeprecatedCode;   // I4866
  chkCheckFilenameConventions.Checked := FGlobalProject.Options.CheckFilenameConventions;
  if editOutputPath.Text = '' then editOutputPath.Text := '$PROJECTPATH';
end;

end.
