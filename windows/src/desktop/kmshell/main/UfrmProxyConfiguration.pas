(*
  Name:             UfrmProxyConfiguration
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jan 2007

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jan 2007 - mcdurdin - Initial version
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmProxyConfiguration;  // I3306

interface

uses
  System.Contnrs,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmWebContainer, OleCtrls, SHDocVw, EmbeddedWB, SHDocVw_EWB,
  EwbCore, KeymanEmbeddedWB;

type
  TfrmProxyConfiguration = class(TfrmWebContainer)
    procedure TntFormCreate(Sender: TObject);
  private
    procedure Footer_OK(params: TStringList);
    procedure Footer_Cancel;
  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  end;

implementation

uses
  GenericXMLRenderer,
  GlobalProxySettings,
  utilxml;

{$R *.dfm}

procedure TfrmProxyConfiguration.FireCommand(const command: WideString;
  params: TStringList);
begin
  if command = 'footer_ok' then Footer_OK(params)
  else if command = 'footer_cancel' then Footer_Cancel
  else inherited;
end;

procedure TfrmProxyConfiguration.Footer_Cancel;
begin
  ModalResult := mrCancel;
end;

procedure TfrmProxyConfiguration.Footer_OK(params: TStringList);
var
  FPassword: WideString;
begin
  if params.Values['password'] = '##PASSWORD##'
    then FPassword := GetProxySettings.Password
    else FPassword := params.Values['password'];

  GetProxySettings.Save(params.Values['server'], StrToIntDef(params.Values['port'], 0),
    params.Values['username'], FPassword);
  ModalResult := mrOk;
end;

procedure TfrmProxyConfiguration.TntFormCreate(Sender: TObject);
var
  FPassword, xml: WideString;
begin
  inherited;

  if GetProxySettings.Password <> ''
    then FPassword := '##PASSWORD##'
    else FPassword := '';

  xml :=
    '<Proxy>'+
      '<Server>'+XMLEncode(GetProxySettings.Server)+'</Server>'+
      '<Port>'+IntToStr(GetProxySettings.Port)+'</Port>'+
      '<Username>'+XMLEncode(GetProxySettings.Username)+'</Username>'+
      '<Password>'+XMLEncode(FPassword)+'</Password>'+
    '</Proxy>';

  XMLRenderers.RenderTemplate := 'ProxyConfiguration.xsl';
  XMLRenderers.Add(TGenericXMLRenderer.Create(xml));

  Content_Render;
end;

end.
