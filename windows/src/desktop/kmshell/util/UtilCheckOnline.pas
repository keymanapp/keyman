(*
  Name:             UtilCheckOnline
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      15 Apr 2015

  Modified Date:    15 Apr 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          15 Apr 2015 - mcdurdin - I4658 - V9.0 - Add Keep in Touch screen
*)
unit UtilCheckOnline;

interface

uses
  HTTPUploader;

function IsOnline: Boolean;

implementation

uses
  System.SysUtils,

  GlobalProxySettings,
  Upload_Settings,
  VersionInfo;

type
  TCheckOnline = class
    function Run: Boolean;
  end;


function IsOnline: Boolean;
begin
  with TCheckOnline.Create do
  try
    Result := Run;
  finally
    Free;
  end;
end;
{ TCheckOnline }

function TCheckOnline.Run: Boolean;
begin
  try
    with THTTPUploader.Create(nil) do
    try
      Proxy.Server := GetProxySettings.Server;
      Proxy.Port := GetProxySettings.Port;
      Proxy.Username := GetProxySettings.Username;
      Proxy.Password := GetProxySettings.Password;
      Request.Agent := API_UserAgent;
      Request.SetURL(MakeAPIURL(API_Path_IsOnline));
      Upload;

      Result := Response.StatusCode = 200;
    finally
      Free;
    end;
  except
    on E:Exception do
    begin
      Result := False;
    end;
  end;
end;

end.
