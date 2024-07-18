(*
  Name:             UfrmEditRelatedPackage
  Copyright:        Copyright (C) SIL International.
  Date:             14 Aug 2023
  Authors:          mcdurdin
*)
unit Keyman.Developer.UI.UfrmEditRelatedPackage;

interface

uses
  System.Classes,
  System.SysUtils,
  Winapi.Messages,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.StdCtrls,

  UfrmTike;

type
  TfrmEditRelatedPackage = class(TTikeForm)
    cmdOK: TButton;
    cmdCancel: TButton;
    lblPackageID: TLabel;
    editPackageID: TEdit;
    chkDeprecates: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FieldChange(Sender: TObject);
  private
    procedure EnableControls;
    function GetDeprecates: Boolean;
    function GetPackageID: string;
    procedure SetDeprecates(const Value: Boolean);
    procedure SetPackageID(const Value: string);
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    procedure SetTitle(IsAdding: Boolean);
    property PackageID: string read GetPackageID write SetPackageID;
    property Deprecates: Boolean read GetDeprecates write SetDeprecates;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics;

{$R *.dfm}

{ TfrmEditRelatedPackage }

procedure TfrmEditRelatedPackage.FieldChange(Sender: TObject);
begin
  inherited;
  EnableControls;
end;

procedure TfrmEditRelatedPackage.FormCreate(Sender: TObject);
begin
  inherited;
  EnableControls;
end;

function TfrmEditRelatedPackage.GetDeprecates: Boolean;
begin
  Result := chkDeprecates.Checked;
end;

function TfrmEditRelatedPackage.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_EditRelatedPackage;
end;

function TfrmEditRelatedPackage.GetPackageID: string;
begin
  Result := Trim(editPackageID.Text);
end;

procedure TfrmEditRelatedPackage.EnableControls;
begin
  cmdOK.Enabled := (PackageID <> '');
end;

procedure TfrmEditRelatedPackage.SetDeprecates(const Value: Boolean);
begin
  chkDeprecates.Checked := Value;
end;

procedure TfrmEditRelatedPackage.SetPackageID(const Value: string);
begin
  editPackageID.Text := Value.Trim;
end;

procedure TfrmEditRelatedPackage.SetTitle(IsAdding: Boolean);
begin
  if IsAdding then
    Caption := 'Add Related Package';
end;

end.
