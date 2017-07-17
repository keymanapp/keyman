(*
  Name:             UfrmProjectSettings
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
unit UfrmProjectSettings;   // I4688

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmProjectSettings = class(TForm)
    lblOutputPath: TLabel;
    editOutputPath: TEdit;
    Label2: TLabel;
    cmdOK: TButton;
    cmdCancel: TButton;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    chkCompilerWarningsAsErrors: TCheckBox;
    chkWarnDeprecatedCode: TCheckBox;
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
  Project;

procedure TfrmProjectSettings.cmdOKClick(Sender: TObject);
begin
  FGlobalProject.Options.BuildPath := Trim(editOutputPath.Text);
  FGlobalProject.Options.CompilerWarningsAsErrors := chkCompilerWarningsAsErrors.Checked;   // I4865
  FGlobalProject.Options.WarnDeprecatedCode := chkWarnDeprecatedCode.Checked;   // I4866
  FGlobalProject.Save;
  ModalResult := mrOk;
end;

procedure TfrmProjectSettings.FormCreate(Sender: TObject);
begin
  editOutputPath.Text := FGlobalProject.Options.BuildPath;
  chkCompilerWarningsAsErrors.Checked := FGlobalProject.Options.CompilerWarningsAsErrors;   // I4865
  chkWarnDeprecatedCode.Checked := FGlobalProject.Options.WarnDeprecatedCode;   // I4866
  if editOutputPath.Text = '' then editOutputPath.Text := '$SOURCEPATH';
end;

end.
