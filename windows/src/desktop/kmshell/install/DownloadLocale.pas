(*
  Name:             DownloadLocale
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      5 Nov 2010

  Modified Date:    15 Sep 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          05 Nov 2010 - mcdurdin - Fixup for UILanguageList call
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    15 Sep 2016 - mcdurdin - I4989 - Download locale not working correctly
                    15 Sep 2016 - mcdurdin - 9.0.525.0
*)
unit DownloadLocale;  // I3306

interface

uses
  Windows,
  Forms,
  Controls,
  UfrmDownloadProgress;

type
  TDownloadLocale = class
  private
    procedure DoDownload(AOwner: TfrmDownloadProgress; var Result: Boolean);
  public
    function Download(Owner: TForm): Boolean;
  end;

implementation

uses
  Classes,
  custinterfaces,
  Dialogs,
  httpuploader,
  Keyman.Configuration.UI.InstallFile,
  kmint,
  Upload_Settings,
  GlobalProxySettings,
  VersionInfo,
  UfrmWebContainer, // for FOnDownloadLocale
  utildir,
  RegistryKeys,
  SysUtils,
  ErrorControlledRegistry,
  UILanguages;

procedure TDownloadLocale.DoDownload(AOwner: TfrmDownloadProgress; var Result: Boolean);
var
  FDepth: Integer;

  procedure DownloadFile(const URL: WideString = '');
  var
    FTempFileName: WideString;
    FTempDir: WideString;
    i: Integer;
    n: Integer;
  begin
    if FDepth > 10 then
    begin
      ShowMessage('Too many redirections on attempting to download updated locale files');
      Exit;
    end;

    FTempDir := IncludeTrailingPathDelimiter(CreateTempPath);  // I1679

    Inc(FDepth);
    try
      with THTTPUploader.Create(AOwner) do
      try
        OnCheckCancel := AOwner.HTTPCheckCancel;
        OnStatus := AOwner.HTTPStatus;
        Request.Agent := API_UserAgent;
        Proxy.Server := GetProxySettings.Server;
        Proxy.Port := GetProxySettings.Port;
        Proxy.Username := GetProxySettings.Username;
        Proxy.Password := GetProxySettings.Password;
        if URL = '' then
        begin
          Request.Protocol := API_Protocol;
          Request.HostName := API_Server;
          Request.UrlPath := API_Path_DownloadLocale;
        end
        else
          Request.SetURL(URL);
        Fields.Add('Version', ansistring(GetVersionString));
        Upload;
        if Response.StatusCode = 200 then
        begin
          n := LastDelimiter('/', Request.UrlPath);
          if n = 0 then
          begin
            ShowMessage('Unable to download file - not a recognised Keyman file: '+Request.URL);
            Exit;
          end;

          if Response.ContentDispositionFilename <> '' then
            FTempFileName := FTempDir + string(Response.ContentDispositionFilename)
          else
            FTempFileName := FTempDir + Copy(Request.UrlPath, n+1, Length(Request.UrlPath));
          n := Pos('?', FTempFileName);
          if n > 0 then Delete(FTempFileName, n, MAXINT);

          try
            try
              with TFileStream.Create(FTempFileName, fmCreate) do
              try
                Write(Response.PMessageBody^, Response.MessageBodyLength);
              finally
                Free;
              end;
            except
              on E:EFOpenError do
              begin
                ShowMessage('Unable to download file: '+E.Message);
                Exit;
              end;
            end;

            if TInstallFile.Execute(AOwner, FTempFileName, False, True, '', '') then Result := True;   // I4989
          finally
            if FileExists(FTempFileName) then
              DeleteFile(FTempFileName);
          end;
        end
        else if Response.StatusCode = 302 then
        begin
          with TStringList.Create do
          try
            Text := string(Response.Headers);
            for i := 0 to Count - 1 do
            begin
              n := Pos(':', Strings[i]);
              if n > 0 then
                Strings[i] := Copy(Strings[i], 1, n-1)+'='+Trim(Copy(Strings[i], n+1, Length(Strings[i])));
            end;

            if Values['Location'] <> '' then
            begin
              DownloadFile(Values['Location']);
              Exit;
            end;
          finally
            Free;
          end;
        end
        else
          ShowMessage('Unable to load : '+Request.URL+': '+IntToStr(Response.StatusCode));
      finally
        Free;
      end;
    finally
      Dec(FDepth);
      DeleteTempPath(FTempDir);
    end;
  end;

begin
  Result := False;
  FDepth := 0;
  DownloadFile;
end;

function TDownloadLocale.Download(Owner: TForm): Boolean;
begin
  with TfrmDownloadProgress.Create(Owner) do
  try
    Callback := DoDownload;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure RefreshLanguagesAvailable;
var
  FLanguagesAvailableString: WideString;
  FCurrentLanguage: WideString;
begin
  with kmint.KeymanCustomisation.CustMessages do
  begin
    FLanguagesAvailableString := GetAvailableLanguages;
    FCurrentLanguage := LanguageCode;
  end;

  with TRegistryErrorControlled.Create do  // I2890
  try
    if not OpenKey(SRegKey_KeymanDesktop_CU, True) then  // I2890
      RaiseLastRegistryError;
    WriteString(SRegValue_AvailableLanguages, FLanguagesAvailableString);
    WriteString(SRegValue_CurrentLanguage, FCurrentLanguage);
  finally
    Free;
  end;

  kmint.KeymanCustomisation.CustMessages.LanguageCode := FCurrentLanguage;

  UILanguageList.Refresh;
end;

function DoDownloadLocale(Owner: TForm): Boolean;
begin
  with TDownloadLocale.Create do
  try
    Result := Download(Owner);
    if Result then
    begin
      RefreshLanguagesAvailable;
    end;
  finally
    Free;
  end;
end;

initialization
  FOnDownloadLocale := DoDownloadLocale;
end.
