(*
  Name:             UfrmEditLanguageExample
  Copyright:        Copyright (C) SIL International.
  Date:             14 Aug 2023
  Authors:          mcdurdin
*)
unit Keyman.Developer.UI.UfrmEditLanguageExample;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Variants,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,

  UfrmTike;

type
  TfrmEditLanguageExample = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    lblLanguageTag: TLabel;
    lblExampleNote: TLabel;
    editExampleNote: TEdit;
    editLanguageID: TEdit;
    lblExampleKeys: TLabel;
    editExampleKeys: TEdit;
    lblExampleText: TLabel;
    editExampleText: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FieldChange(Sender: TObject);
  private
    procedure EnableControls;
    function GetExampleKeys: string;
    function GetExampleNote: string;
    function GetExampleText: string;
    function GetLanguageID: string;
    procedure SetExampleKeys(const Value: string);
    procedure SetExampleNote(const Value: string);
    procedure SetExampleText(const Value: string);
    procedure SetLanguageID(const Value: string);
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    procedure SetTitle(IsAdding: Boolean);
    property LanguageID: string read GetLanguageID write SetLanguageID;
    property ExampleKeys: string read GetExampleKeys write SetExampleKeys;
    property ExampleText: string read GetExampleText write SetExampleText;
    property ExampleNote: string read GetExampleNote write SetExampleNote;
  end;

implementation

uses
  System.Generics.Collections,

  Keyman.Developer.System.HelpTopics;

{$R *.dfm}

{ TfrmEditLanguageExample }

procedure TfrmEditLanguageExample.FieldChange(Sender: TObject);
begin
  inherited;
  EnableControls;
end;

procedure TfrmEditLanguageExample.FormCreate(Sender: TObject);
begin
  inherited;
  EnableControls;
end;

function TfrmEditLanguageExample.GetExampleKeys: string;
begin
  Result := Trim(editExampleKeys.Text);
end;

function TfrmEditLanguageExample.GetExampleNote: string;
begin
  Result := Trim(editExampleNote.Text);
end;

function TfrmEditLanguageExample.GetExampleText: string;
begin
  Result := Trim(editExampleText.Text);
end;

function TfrmEditLanguageExample.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_EditLanguageExample;
end;

function TfrmEditLanguageExample.GetLanguageID: string;
begin
  Result := Trim(editLanguageID.Text);
end;

procedure TfrmEditLanguageExample.EnableControls;
begin
  cmdOK.Enabled :=
    (LanguageID <> '') and
    (ExampleKeys <> '') and
    (ExampleText <> '');
end;

procedure TfrmEditLanguageExample.SetExampleKeys(const Value: string);
begin
  editExampleKeys.Text := Value.Trim;
  EnableControls;
end;

procedure TfrmEditLanguageExample.SetExampleNote(const Value: string);
begin
  editExampleNote.Text := Value.Trim;
  EnableControls;
end;

procedure TfrmEditLanguageExample.SetExampleText(const Value: string);
begin
  editExampleText.Text := Value.Trim;
  EnableControls;
end;

procedure TfrmEditLanguageExample.SetLanguageID(const Value: string);
begin
  editLanguageID.Text := Value.Trim;
  EnableControls;
end;

procedure TfrmEditLanguageExample.SetTitle(IsAdding: Boolean);
begin
  if IsAdding then
    Caption := 'Add Language Example';
end;

end.
