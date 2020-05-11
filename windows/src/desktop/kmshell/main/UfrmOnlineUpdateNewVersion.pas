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
  private
    FParams: TOnlineUpdateCheckParams;
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  public
    property Params: TOnlineUpdateCheckParams read FParams write FParams;
  end;

function OnlineUpdateNewVersion(AOwner: TComponent): TfrmOnlineUpdateNewVersion;

implementation

uses
  kmint,
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

procedure TfrmOnlineUpdateNewVersion.TntFormShow(Sender: TObject);
var
  xml: WideString;
  i: Integer;
begin
  inherited;

  // TODO: refactor data
  xml := '';

  if (FParams.Keyman.DownloadURL <> '') then
  begin
    FParams.Keyman.Install := True;
    xml := xml +
      '<Update>'+
        '<index>0</index>'+
        IfThen(not kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
        '<Keyman>'+
          '<Text>'+xmlencode(MsgFromIdFormat(SKUpdate_KeymanText, [FParams.Keyman.NewVersion]))+'</Text>'+
          '<NewVersion>'+xmlencode(FParams.Keyman.NewVersion)+'</NewVersion>'+
          '<OldVersion>'+xmlencode(FParams.Keyman.OldVersion)+'</OldVersion>'+
          '<DownloadSize>'+xmlencode(Format('%d', [FParams.Keyman.DownloadSize div 1024]))+'KB</DownloadSize>'+
          '<DownloadURL>'+xmlencode(FParams.Keyman.DownloadURL)+'</DownloadURL>'+
        '</Keyman>'+
      '</Update>';
  end;

  for i := 0 to High(FParams.Packages) do
  begin
    FParams.Packages[i].Install := True;
    xml := xml +
      '<Update>'+
        '<index>'+IntToStr(i+1)+'</index>'+
        IfThen(not kmcom.SystemInfo.IsAdministrator, '<RequiresAdmin />')+
        '<Package>'+
          '<Text>'+xmlencode(MsgFromIdFormat(SKUpdate_PackageText, [FParams.Packages[i].Description, FParams.Packages[i].NewVersion]))+'</Text>'+
          '<NewVersion>'+xmlencode(FParams.Packages[i].NewVersion)+'</NewVersion>'+
          '<OldVersion>'+xmlencode(FParams.Packages[i].OldVersion)+'</OldVersion>'+
          '<DownloadSize>'+xmlencode(Format('%d', [FParams.Packages[i].DownloadSize div 1024]))+'KB</DownloadSize>'+
          '<DownloadURL>'+xmlencode(FParams.Packages[i].DownloadURL)+'</DownloadURL>'+
        '</Package>'+
      '</Update>';

//    '<NewVersionText>'+xmlencode(MsgFromIdFormat(SKUpdate_NewVersionText, [FNewVersion, FCurrentVersion]))+'</NewVersionText>'+
//    '<PatchText>'+xmlencode(MsgFromIdFormat(SKUpdate_PatchText, [FPatchSize div 1024]))+'</PatchText>';
  end;

  FRenderPage := 'onlineupdate';
  Content_Render;
end;

end.

