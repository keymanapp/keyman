(*
  Name:             UfrmOnlineUpdateNewVersion
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Nov 2007

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    31 Mar 2011 - mcdurdin - I2849 - Developer online update check fails because it looks for a patch update
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit Keyman.Developer.UI.UfrmTikeOnlineUpdateNewVersion;  // I3306

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ShellAPI, ExtCtrls;

type
  TfrmTikeOnlineUpdateNewVersion = class(TForm)
    lblNewVersion: TLabel;
    lblVersionInfo: TLabel;
    panDownload: TPanel;
    panDownloadDetail: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    lblURL: TLabel;
    cmdVisitNow: TButton;
    cmdDownloadLater: TButton;
    cmdInstallLater: TButton;
    cmdInstallNow: TButton;
    Label1: TLabel;
    lblInstallDetail: TLabel;
    procedure lblURLClick(Sender: TObject);
    procedure cmdVisitNowClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FNewVersion: string;
    FCurrentVersion: string;
    FAppTitle: string;                                                               
    FInstallURL: string;  // I2849
    FInstallSize: Integer;  // I2849
    procedure OpenURL(url: string);
  public
    property CurrentVersion: string read FCurrentVersion write FCurrentVersion;
    property NewVersion: string read FNewVersion write FNewVersion;
    property InstallURL: string read FInstallURL write FInstallURL;  // I2849
    property InstallSize: Integer read FInstallSize write FInstallSize;  // I2849
    property AppTitle: string read FAppTitle write FAppTitle;
  end;

function OnlineUpdateNewVersion(AOwner: TComponent): TfrmTikeOnlineUpdateNewVersion;

implementation

uses
  UtilExecute;

function OnlineUpdateNewVersion(AOwner: TComponent): TfrmTikeOnlineUpdateNewVersion;
begin
  Result := TfrmTikeOnlineUpdateNewVersion.Create(AOwner);
end;

{$R *.DFM}

procedure TfrmTikeOnlineUpdateNewVersion.lblURLClick(Sender: TObject);
begin
  OpenURL(lblURL.Caption);
end;

procedure TfrmTikeOnlineUpdateNewVersion.cmdVisitNowClick(Sender: TObject);
begin
  OpenURL(lblURL.Caption);
  ModalResult := mrOk;
end;

procedure TfrmTikeOnlineUpdateNewVersion.FormShow(Sender: TObject);
begin
  lblVersionInfo.Caption := 'The new version is '+FNewVersion+' (you are currently running version '+FCurrentVersion+')';
  if FInstallURL <> '' then  // I2849
  begin
    panDownload.Visible := False;
    lblInstallDetail.Caption := 'A '+Format('%d', [FInstallSize div 1024 div 1024])+'MB upgrade can be automatically downloaded and installed.';  // I2849
  end;
end;

procedure TfrmTikeOnlineUpdateNewVersion.OpenURL(url: string);
begin
  if not TUtilExecute.URL(url) then  // I3349
    ShowMessage(SysErrorMessage(GetLastError));
end;

end.

