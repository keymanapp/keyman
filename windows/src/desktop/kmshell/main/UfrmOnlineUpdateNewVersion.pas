(*
  Name:             UfrmOnlineUpdateNewVersion
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      5 Dec 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          05 Dec 2006 - mcdurdin - Initial version
                    12 Dec 2006 - mcdurdin - Make 'installnow' work
                    12 Dec 2006 - mcdurdin - Fix CurrentVersion tag, capitalize form name
                    27 Mar 2008 - mcdurdin - Refer to FActiveProduct when creating form
                    16 Jan 2009 - mcdurdin - I1730 - Online update of keyboards
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmOnlineUpdateNewVersion;  // I3306

interface

uses
  System.Contnrs,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ShellAPI, UfrmWebContainer, OnlineUpdateCheck;

type
  TfrmOnlineUpdateNewVersion = class(TfrmWebContainer)
    procedure TntFormShow(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
  private
    FParams: TOnlineUpdateCheckParams;
    PageTag: Integer;
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  public
    property Params: TOnlineUpdateCheckParams read FParams write FParams;
  end;

function OnlineUpdateNewVersion(AOwner: TComponent): TfrmOnlineUpdateNewVersion;

implementation

uses
  kmint,
  Keyman.Configuration.System.UmodWebHttpServer,
  MessageIdentifiers,
  MessageIdentifierConsts,
  strutils,
  utilxml;

{$R *.DFM}

function OnlineUpdateNewVersion(AOwner: TComponent): TfrmOnlineUpdateNewVersion;
begin
  Result := TfrmOnlineUpdateNewVersion.Create(AOwner);
end;

procedure TfrmOnlineUpdateNewVersion.FireCommand(const command: WideString; params: TStringList);
var
  n: Integer;
begin
  //if command='openwebsite' then OpenURL(FDownloadURL)
  //else if command='visitwebsitenow' then begin OpenURL(FDownloadURL); ModalResult := mrOk; end
  //else if command='visitwebsitelater' then ModalResult := mrCancel
  //else
  if command='installnow' then ModalResult := mrYes
  else if command='installlater' then ModalResult := mrCancel
  else if (command='tickupdate') or (command='untickupdate') then
  begin
    n := StrToIntDef(params.Values['id'],-1);
    if n = 0 then
      FParams.Keyman.Install := command='tickupdate'
    else if (n > 0) and (n <= Length(FParams.Packages)) then
      FParams.Packages[n-1].Install := command='tickupdate';
  end
  else
    inherited;
end;

procedure TfrmOnlineUpdateNewVersion.TntFormDestroy(Sender: TObject);
begin
  inherited;
  modWebHttpServer.SharedData.Remove(PageTag);
end;

procedure TfrmOnlineUpdateNewVersion.TntFormShow(Sender: TObject);
var
  i: Integer;
  Data: IOnlineUpdateSharedData;
begin
  inherited;

  if FParams.Keyman.DownloadURL <> '' then
    FParams.Keyman.Install := True;

  for i := 0 to High(FParams.Packages) do
    FParams.Packages[i].Install := True;

  Data := TOnlineUpdateSharedData.Create(FParams);
  PageTag := modWebHttpServer.SharedData.Add(Data);

  FRenderPage := 'onlineupdate';
  Content_Render(False, 'tag='+IntToStr(PageTag));
end;

end.

