unit UfrmTIKEDock;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmTike, JvComponentBase,
  JvDockControlForm;

type
  TTIKEDockForm = class(TTikeForm)
    dockClient: TJvDockClient;
    procedure dockClientFormHide(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
  public
    procedure DoHideForm;
    procedure DoShowForm;
    procedure ToggleVisibility;
  end;

implementation

{$R *.dfm}

uses
  UfrmMain;

procedure TTIKEDockForm.dockClientFormHide(Sender: TObject);
begin
  Hide;
end;

procedure TTIKEDockForm.FormHide(Sender: TObject);
begin
  inherited;
  dockClient.HideParentForm;
end;

procedure TTIKEDockForm.ToggleVisibility;
begin
  if Visible and ContainsControl(Screen.ActiveControl) then
  begin
    DoHideForm;
  end
  else
  begin
    DoShowForm;
    SetFocus;
  end;
end;

procedure TTIKEDockForm.DoHideForm;
begin
  Visible := False;
  dockClient.HideParentForm;
  frmKeymanDeveloper.FocusActiveChild;
end;

procedure TTIKEDockForm.DoShowForm;
begin
  Visible := True;
  dockClient.ShowParentForm;
end;


end.
