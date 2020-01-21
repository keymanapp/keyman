(*
  Name:             UfrmOnlineUpdateSetup
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jan 2007

  Modified Date:    30 May 2007
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jan 2007 - mcdurdin - Rework to use GetProxySettings
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
*)
unit Keyman.Developer.UI.UfrmTikeOnlineUpdateSetup;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TfrmTikeOnlineUpdateSetup = class(TForm)
    lblHTTPProxy: TLabel;
    lblPort: TLabel;
    editProxy: TEdit;
    editPort: TEdit;
    cmdOK: TButton;
    cmdCancel: TButton;
    lblUsername: TLabel;
    editUsername: TEdit;
    Label2: TLabel;
    editPassword: TEdit;
    procedure cmdOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

implementation

uses
  GlobalProxySettings;

{$R *.DFM}

procedure TfrmTikeOnlineUpdateSetup.cmdOKClick(Sender: TObject);
begin
  GetProxySettings.Save(editProxy.Text, StrToIntDef(editPort.Text, 0), editUsername.Text, editPassword.Text);
  ModalResult := mrOk;
end;

procedure TfrmTikeOnlineUpdateSetup.FormCreate(Sender: TObject);
begin
  inherited;
  editProxy.Text := GetProxySettings.Server;
  editPort.Text := IntToStr(GetProxySettings.Port);
  editUsername.Text := GetProxySettings.Username;
  editPassword.Text := GetProxySettings.Password;
end;

end.

