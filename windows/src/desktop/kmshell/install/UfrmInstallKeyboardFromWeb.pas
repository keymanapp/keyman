(*
  Name:             UfrmInstallKeyboardFromWeb
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Oct 2006

  Modified Date:    2 Oct 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Oct 2006 - mcdurdin - Initial version
                    05 Dec 2006 - mcdurdin - Refactor using XML-Renderer
                    12 Dec 2006 - mcdurdin - Capitalize form name
                    04 Jan 2007 - mcdurdin - Add proxy support
                    15 Jan 2007 - mcdurdin - Use name of file in Content-Disposition
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    17 Sep 2007 - mcdurdin - I1070 - Add Download-Locale flag
                    27 Mar 2008 - mcdurdin - I1366 - Add version information to InstallKeyboardFromWeb so we can show appropriate content online
                    14 Jun 2008 - mcdurdin - I1496 - Fix filename when content disposition is missing [some proxy servers do this]
                    16 Jan 2009 - mcdurdin - I1679 - Fix crash installing keyboard when file is locked in temp dir
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    02 Oct 2014 - mcdurdin - I4414 - V9.0 - Download Keyboard dialog does not display correctly
*)
unit UfrmInstallKeyboardFromWeb;  // I3306   // I4414

interface

uses
  System.Contnrs,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, UfrmWebContainer, UfrmDownloadProgress, UfrmKeymanBase;

type
  TfrmInstallKeyboardFromWeb = class(TfrmWebContainer)
    dlgSaveFile: TSaveDialog;
    procedure TntFormShow(Sender: TObject);
  private
    FURL: WideString;
    FFileName: WideString;
    procedure Download(params: TStringList);
    procedure DoDownload(AOwner: TfrmDownloadProgress; var Result: Boolean);

  protected
    procedure FireCommand(const command: WideString; params: TStringList); override;
  end;

implementation

uses
  httpuploader,
  GlobalProxySettings,
  UfrmInstallKeyboard,
  Upload_Settings,
  utildir,
  VersionInfo,
  WideStrings;

{$R *.dfm}

{ TfrmInstallKeyboardFromWeb }

procedure TfrmInstallKeyboardFromWeb.DoDownload(AOwner: TfrmDownloadProgress;
    var Result: Boolean);
var
  FDepth: Integer;

  procedure DownloadFile(const URL: WideString);
  var
    FTempFileName: WideString;
    FTempDir: WideString;
    i: Integer;
    n: Integer;
  begin
    if FDepth > 10 then
    begin
      ShowMessage('Too many redirections on attempting to load keyboard ' + URL);
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
        //Request.Protocol := Upload_Protocol;
        //Request.HostName := Upload_Server;
        Request.SetURL(URL);// UrlPath := URL;
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
            FTempFileName := FTempDir + string(Response.ContentDispositionFilename)  // I3310
          else if FFileName <> '' then
            FTempFileName := FTempDir + FFileName
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

            if InstallFile(Self, FTempFileName, False, False, '') then Result := True;   // I4414
          finally
            if FileExists(FTempFileName) then
              DeleteFile(FTempFileName);
          end;
        end
        else if Response.StatusCode = 302 then
        begin
          with TStringList.Create do
          try
            Text := string(Response.Headers);  // I3310
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
          ShowMessage('Unable to load : '+Request.URL+' ... '+IntToStr(Response.StatusCode));
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
  DownloadFile(FURL);
end;

procedure TfrmInstallKeyboardFromWeb.Download(params: TStringList);
begin
  FURL := params.Values['url'];
  FFilename := params.Values['filename'];

  with TfrmDownloadProgress.Create(Self) do
  try
    Callback := DoDownload;
    if ShowModal=mrOk then Self.ModalResult := mrOk;
  finally
    Free;
  end;
end;

procedure TfrmInstallKeyboardFromWeb.FireCommand(const command: WideString;
  params: TStringList);
begin
  if command = 'download' then
  begin
    Download(params);
  end
  else if command = 'footer_cancel' then
    ModalResult := mrCancel
  else
    inherited;
end;

procedure TfrmInstallKeyboardFromWeb.TntFormShow(Sender: TObject);
begin
  // Ensures keyman.com hosted site opens locally
  cef.ShouldOpenRemoteUrlsInBrowser := False;
  FRenderPage := 'downloadkeyboard';

  Content_Render;
  inherited;
end;

end.
