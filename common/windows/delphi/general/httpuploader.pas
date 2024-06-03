(*
  Name:             httpuploader
  Copyright:        Copyright (C) SIL International.
  Documentation:    gl1
  Description:      Upload a file to http or https site using Windows Inet functions
  Create Date:      13 May 2005

  Modified Date:    15 Sep 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          13 May 2005 - mcdurdin - Initial version
                    06 Oct 2006 - mcdurdin - Add support for GET and URL parsing
                    04 Dec 2006 - mcdurdin - Add progress support
                    12 Dec 2006 - mcdurdin - Improve performance
                    04 Jan 2007 - mcdurdin - Add proxy support
                    15 Jan 2007 - mcdurdin - Add Content-Disposition checking
                    22 Jan 2007 - mcdurdin - Remove TntDialogs and utilhttp dependencies
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    14 Jun 2008 - mcdurdin - I1465 - Fix proxy support
                    14 Jun 2009 - mcdurdin - I1704 - Activation through a firewall
                    31 Mar 2011 - mcdurdin - I2855 - Keyman Developer online update crashes with Integer Overflow
                    04 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    04 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    01 Sep 2014 - mcdurdin - I4399 - V9.0 - HTTP download should report progress more cleanly
                    15 Sep 2016 - mcdurdin - I4989 - Download locale not working correctly
                    15 Sep 2016 - mcdurdin - 9.0.525.0
*)
unit httpuploader;  // I3306

interface

uses
  Classes, Contnrs, SysUtils, Windows, WinInet;

type
  THTTPUploader = class;

  EHTTPUploader = class(Exception)
  private
    FErrorCode: Integer;
  public
    constructor Create(const msg: string; ErrorCode: Integer);
    property ErrorCode: Integer read FErrorCode;
  end;

  EHTTPUploaderCancel = class(Exception);

  THTTPUploaderCheckCancelEvent = procedure(Sender: THTTPUploader; var Cancel: Boolean) of object;
  THTTPUploaderStatusEvent = procedure(Sender: THTTPUploader) of object;
  THTTPUploaderStatusStringEvent = procedure(Sender: THTTPUploader; const Data: string) of object;
  THTTPUploaderStatusDwordEvent = procedure(Sender: THTTPUploader; const Data: DWord) of object;
  THTTPUploaderStatusFileCompleteEvent = procedure(Sender: THTTPUploader; const FileName: string; Successful: Boolean) of object;
  THTTPUploaderStatusFileProgressEvent = procedure(Sender: THTTPUploader; const FileName: string; dwFileBytesSent, dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond: DWord) of object;
  THTTPUploaderStatusSimpleEvent = procedure(Sender: THTTPUploader; const Message: string; Position, Total: Int64) of object; // I2855

  THTTPUploaderRequest = class
    Agent: string;
    HostName: string;
    UrlPath: string;
    Protocol: string;
    Verb: string;
  private
    function GetURL: string;
  public
    procedure SetURL(URL: string);
    property URL: string read GetURL;
  end;

  THTTPUploaderResponse = class
  private
    FHeaders: AnsiString;  // I3310
    procedure SetHeaders(const Value: AnsiString);  // I3310
  public
    StatusCode: Integer;
    PMessageBody: PAnsiChar;  // I3310
    MessageBodyLength: Integer;
    ContentDispositionFilename: AnsiString;  // I3310
    function MessageBodyAsString: AnsiString;  // I3310
    destructor Destroy; override;
    property Headers: AnsiString read FHeaders write SetHeaders;  // I3310
  end;

  THTTPUploaderField = class
    Name, Value: AnsiString;  // I3310
  end;

  THTTPUploaderFile = class
    FieldName, ContentType: AnsiString;  // I3310
    PathName: string;
  end;

  THTTPUploaderFields = class(TObjectList)
  private
    function GetItem(Index: Integer): THTTPUploaderField;
    procedure SetItem(Index: Integer; const Value: THTTPUploaderField);
  public
    function Add(const AName, AValue: AnsiString): Integer;  // I3310
    property Items[Index: Integer]: THTTPUploaderField read GetItem write SetItem; default;
  end;

  THTTPUploaderFiles = class(TObjectList)
  private
    function GetItem(Index: Integer): THTTPUploaderFile;
    procedure SetItem(Index: Integer; const Value: THTTPUploaderFile);
  public
    function Add(const AFieldName: AnsiString; const APathName: string; const AContentType: AnsiString = ''): Integer;  // I3310
    property Items[Index: Integer]: THTTPUploaderFile read GetItem write SetItem; default;
  end;

  THTTPUploaderProxy = class
    Server: string;
    Port: Integer;
    Username: string;
    Password: string;
    function Describe(HTTPS: Boolean): string;
  end;

  THTTPUploader_MessageProcessor = class
  public
    procedure ProcessMessages; virtual; abstract;
  end;

  THTTPUploader = class(TComponent)
  private
    FCachedBytesPerSecond: DWord;
    FOnRequestSent: THTTPUploaderStatusDWordEvent;
    FOnResponseReceived: THTTPUploaderStatusDWordEvent;
    FOnClosingConnection: THTTPUploaderStatusEvent;
    FOnReceivingResponse: THTTPUploaderStatusEvent;
    FOnConnectionClosed: THTTPUploaderStatusEvent;
    FOnSendingRequest: THTTPUploaderStatusEvent;
    FOnFileComplete: THTTPUploaderStatusFileCompleteEvent;
    FOnFileProgress: THTTPUploaderStatusFileProgressEvent;
    FOnConnectingToServer: THTTPUploaderStatusStringEvent;
    FOnNameResolved: THTTPUploaderStatusStringEvent;
    FOnConnectedToServer: THTTPUploaderStatusStringEvent;
    FOnStatus: THTTPUploaderStatusSimpleEvent;
    FOnResolvingName: THTTPUploaderStatusStringEvent;
    FRequest: THTTPUploaderRequest;
    FResponse: THTTPUploaderResponse;
    FFields: THTTPUploaderFields;
    FFiles: THTTPUploaderFiles;
    FOnFileBegin: THTTPUploaderStatusStringEvent;
    FOnCheckCancel: THTTPUploaderCheckCancelEvent;
    FProxy: THTTPUploaderProxy;
    FShowUI: Boolean;
    function CalculateContentLength(const strBoundary, strFieldName, strFieldValue: AnsiString): DWord; overload;  // I3310
    function CalculateContentLength(const strBoundary, strFieldName: AnsiString; const strPathName: string; const strContentType: AnsiString; var dwOverallBytesTotal: DWord): DWord; overload;  // I3310
    function CalculateContentLength(const strBoundary: AnsiString; var dwOverallBytesTotal: DWord): DWord; overload;  // I3310
    function GenerateBodyTrailer(const strBoundary: AnsiString): AnsiString;  // I3310
    function GenerateContentTypeHeader(const strBoundary: AnsiString): AnsiString;  // I3310
    function GenerateFieldHeader(const strBoundary, strFieldName: AnsiString): AnsiString;  // I3310
    function GenerateFieldTrailer: AnsiString;  // I3310
    function GenerateFileHeader(const strBoundary, strFieldName: AnsiString; const strPathName: string; const strContentType: AnsiString): AnsiString;  // I3310
    function GenerateFileTrailer: AnsiString;  // I3310
    function GenerateMultipartBoundary: AnsiString;  // I3310
    //procedure InternetStatusCallback(hInternet: HINTERNET; dwInternetStatus: DWORD; lpvStatusInformation: Pointer; dwStatusInformationLength: DWord);
    procedure ReadResponseBody(hRequest: HINTERNET);
    procedure ReadResponseHeaders(hRequest: HINTERNET);
    procedure ReadStatusCode(hRequest: HINTERNET; var pStatusCode: DWord);
    procedure UploadField(hRequest: HINTERNET; const strBoundary, strFieldName, strFieldValue: AnsiString);  // I3310
    procedure UploadFile(hRequest: HINTERNET; const strBoundary, strFieldName: AnsiString; const strPathName: string; const strContentType: AnsiString; var dwOverallBytesSent: DWord; dwOverallBytesTotal: DWord);  // I3310
    procedure UploadFileContent(hRequest: HINTERNET; const strPathName: string; hFile: THandle; var dwOverallBytesSent: DWord; dwOverallBytesTotal: DWORD);
    procedure DoProcessMessages;
  protected
    procedure DoStatus(const Message: string; Position: Int64 = 0; Total: Int64 = 0); virtual; // I2855


    procedure DoFileBegin(const Data: string); virtual;
(*
    procedure DoResolvingName(const Data: string); virtual;
    procedure DoNameResolved(const Data: string); virtual;
    procedure DoConnectingToServer(const Data: string); virtual;
    procedure DoConnectedToServer(const Data: string); virtual;
    procedure DoSendingRequest; virtual;
    procedure DoRequestSent(const Data: DWord); virtual;
    procedure DoReceivingResponse; virtual;
    procedure DoResponseReceived(const Data: DWord); virtual;
    procedure DoClosingConnection; virtual;
    procedure DoConnectionClosed; virtual;
*)
    procedure DoFileComplete(const FileName: string; Successful: Boolean); virtual;

    procedure DoFileProgress(const FileName: string; dwFileBytesSent, dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond: DWord); virtual;

    function CheckCancel: Boolean; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Upload;

    property ShowUI: Boolean read FShowUI write FShowUI default true;

    property Files: THTTPUploaderFiles read FFiles;
    property Fields: THTTPUploaderFields read FFields;

    property Request: THTTPUploaderRequest read FRequest;
    property Response: THTTPUploaderResponse read FResponse;

    property Proxy: THTTPUploaderProxy read FProxy;

    property OnStatus: THTTPUploaderStatusSimpleEvent read FOnStatus write FOnStatus;
    property OnCheckCancel: THTTPUploaderCheckCancelEvent read FOnCheckCancel write FOnCheckCancel;

    property OnFileBegin: THTTPUploaderStatusStringEvent read FOnFileBegin write FOnFileBegin;
    property OnResolvingName: THTTPUploaderStatusStringEvent read FOnResolvingName write FOnResolvingName;
    property OnNameResolved: THTTPUploaderStatusStringEvent read FOnNameResolved write FOnNameResolved;
    property OnConnectingToServer: THTTPUploaderStatusStringEvent read FOnConnectingToServer write FOnConnectingToServer;
    property OnConnectedToServer: THTTPUploaderStatusStringEvent read FOnConnectedToServer write FOnConnectedToServer;
    property OnSendingRequest: THTTPUploaderStatusEvent read FOnSendingRequest write FOnSendingRequest;
    property OnRequestSent: THTTPUploaderStatusDWordEvent read FOnRequestSent write FOnRequestSent;
    property OnReceivingResponse: THTTPUploaderStatusEvent read FOnReceivingResponse write FOnReceivingResponse;
    property OnResponseReceived: THTTPUploaderStatusDWordEvent read FOnResponseReceived write FOnResponseReceived;
    property OnClosingConnection: THTTPUploaderStatusEvent read FOnClosingConnection write FOnClosingConnection;
    property OnConnectionClosed: THTTPUploaderStatusEvent read FOnConnectionClosed write FOnConnectionClosed;
    property OnFileComplete: THTTPUploaderStatusFileCompleteEvent read FOnFileComplete write FOnFileComplete;
    property OnFileProgress: THTTPUploaderStatusFileProgressEvent read FOnFileProgress write FOnFileProgress;
  end;

var
  FHTTPUploader_MessageProcessor: THTTPUploader_MessageProcessor = nil;

implementation

uses
  System.StrUtils,
  VersionInfo,
  wininet5;

procedure StaticInternetStatusCallback(hInternet: HINTERNET; dwContext: PDWord; dwInternetStatus: DWord;
  lpvStatusInformation: Pointer; dwStatusInformationLength: DWord); stdcall; forward;

function THTTPUploader.GenerateMultipartBoundary: AnsiString;  // I3310
begin
  Result := '---------------------------' + AnsiString(Format('%0.4X%0.4X%0.4X', [Random(65536), Random(65536), Random(65536)]));  // I3310
end;

function THTTPUploader.GenerateContentTypeHeader(const strBoundary: AnsiString): AnsiString;  // I3310
begin
  Result := AnsiString(Format('Content-Type: multipart/form-data; boundary=%s', [strBoundary]));  // I3310
end;

function THTTPUploader.GenerateFileHeader(const strBoundary, strFieldName: AnsiString; const strPathName: string; const strContentType: AnsiString): AnsiString;  // I3310
var
  contentDispositionHeader: AnsiString;
  contentTypeHeader: AnsiString;
begin
  contentDispositionHeader := AnsiString(Format('Content-Disposition: form-data; name="%s"; filename="%s"', [strFieldName, ExtractFileName(strPathName)]));  // I3310
	contentTypeHeader := AnsiString(Format('Content-Type: %s', [strContentType]));
  Result := AnsiString(Format('--%s'#13#10, [strBoundary])) +  // I3310
	  contentDispositionHeader + #13#10 +
	  contentTypeHeader + #13#10 +
    #13#10;
end;

function THTTPUploader.GenerateFileTrailer: AnsiString;  // I3310
begin
  Result := #13#10;
end;

function THTTPUploader.GenerateBodyTrailer(const strBoundary: AnsiString): AnsiString;  // I3310
begin
  Result := AnsiString(Format('--%s--'#13#10, [strBoundary]));  // I3310
end;

function THTTPUploader.GenerateFieldHeader(const strBoundary, strFieldName: AnsiString): AnsiString;  // I3310
var
  contentDispositionHeader: AnsiString;  // I3310
begin
	contentDispositionHeader := AnsiString(Format('Content-Disposition: form-data; name="%s"', [strFieldName]));  // I3310

	Result := AnsiString(Format('--%s'#13#10, [strBoundary])) +  // I3310
    contentDispositionHeader + #13#10 +
    #13#10;
end;

function THTTPUploader.GenerateFieldTrailer: AnsiString;  // I3310
begin
  Result := #13#10;
end;

function THTTPUploader.CalculateContentLength(const strBoundary, strFieldName, strFieldValue: AnsiString): DWord;  // I3310
var
  fieldHeader, fieldTrailer: AnsiString;  // I3310
begin
	fieldHeader := GenerateFieldHeader(strBoundary, strFieldName);
	fieldTrailer := GenerateFieldTrailer;

	Result := Length(fieldHeader) + Length(strFieldValue) + Length(fieldTrailer);
end;

function THTTPUploader.CalculateContentLength(const strBoundary, strFieldName: AnsiString; const strPathName: string; const strContentType: AnsiString; var dwOverallBytesTotal: DWord): DWord;  // I3310
var
  fileTrailer, fileHeader: AnsiString;
  fileLength: DWord;
  hFile: THandle;
begin
	fileHeader := GenerateFileHeader(strBoundary, strFieldName, strPathName, strContentType);

	hFile := CreateFile(PChar(strPathName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
	fileLength := GetFileSize(hFile, nil);
	CloseHandle(hFile);

	dwOverallBytesTotal := dwOverallBytesTotal + fileLength;

	fileTrailer := GenerateFileTrailer;

	Result := DWord(Length(fileHeader)) + fileLength + DWord(Length(fileTrailer));
end;

function THTTPUploader.CalculateContentLength(const strBoundary: AnsiString; var dwOverallBytesTotal: DWord): DWord;  // I3310
var
  contentLength: DWord;
  i: Integer;
  bodyTrailer: AnsiString;
begin
	contentLength := 0;
	dwOverallBytesTotal := 0;

  for i := 0 to FFields.Count - 1 do
    contentLength := contentLength + CalculateContentLength(strBoundary, FFields[i].Name, FFields[i].Value);

  for i := 0 to FFiles.Count - 1 do
    contentLength := contentLength + CalculateContentLength(strBoundary, FFiles[i].FieldName, FFiles[i].PathName, FFiles[i].ContentType,
      dwOverallBytesTotal);

  if (FFields.Count > 0) or (FFiles.Count > 0) then
  begin
  	bodyTrailer := GenerateBodyTrailer(strBoundary);
	  contentLength := contentLength + DWord(Length(bodyTrailer));
  end;

  Result := contentLength;
end;

procedure THTTPUploader.Upload;
var
  i: Integer;
  FHTTPS: Boolean;
  hInet, hConnect, hRequest: HINTERNET;
  bodyTrailer, strBoundary: AnsiString;
  contentTypeHeader: AnsiString;
	dwStatusCode, dwBytesWritten, dwOverallBytesSent, dwOverallBytesTotal, contentLength: DWord;
  buffersIn: INTERNET_BUFFERS;
  FFlags: DWORD;
  dwError: Cardinal;
  pvData: Pointer;
  RetryCount: Integer;
  dwFlags,
  dwLength: DWord;
  dwReserved: DWord;
  FProxyOptions: INTERNET_PER_CONN_OPTION_LIST;
  FProxyOption: array[0..1] of INTERNET_PER_CONN_OPTION;
  Port: Integer;
  PortIndex: Integer;
  HostName: string;
begin
  FHTTPS := FRequest.Protocol = 'https';

	hInet := InternetOpen(PChar(FRequest.Agent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0 {INTERNET_FLAG_ASYNC});
	if hInet = nil then
    raise EHTTPUploader.Create('InternetOpen failed', GetLastError);
  try
    FProxyOptions.dwSize := SizeOf(FProxyOptions);
    FProxyOptions.pszConnection := nil;
    FProxyOptions.dwOptionError := 0;
    FProxyOptions.pOptions := @FProxyOption[0];

    if FProxy.Server <> '' then
    begin
      FProxyOptions.dwOptionCount := 2;
      FProxyOption[0].dwOption := INTERNET_PER_CONN_PROXY_SERVER;
      FProxyOption[0].pszValue := PChar('http://' + FProxy.Server + ':' + IntToStr(FProxy.Port));
      FProxyOption[1].dwOption := INTERNET_PER_CONN_FLAGS;
      FProxyOption[1].dwValue := PROXY_TYPE_PROXY;
      InternetSetOption(hInet, INTERNET_OPTION_PER_CONNECTION_OPTION, @FProxyOptions, SizeOf(FProxyOptions));
    end;

    InternetSetStatusCallback(hInet, @StaticInternetStatusCallback);

    if FHTTPS
      then Port := INTERNET_DEFAULT_HTTPS_PORT
      else Port := INTERNET_DEFAULT_HTTP_PORT;

    HostName := FRequest.HostName;
    PortIndex := HostName.IndexOf(':');
    if PortIndex >= 0 then
    begin
      Port := StrToIntDef(HostName.Substring(PortIndex + 1), Port);
      HostName := HostName.Substring(0, PortIndex);
    end;

    hConnect := InternetConnect(hInet, PChar(HostName), Port, nil, nil, INTERNET_SERVICE_HTTP, 0, DWord(Self));
    if hConnect = nil then
      raise EHTTPUploader.Create('InternetConnect failed', GetLastError);

    try
      FFlags := INTERNET_FLAG_NO_CACHE_WRITE or INTERNET_FLAG_DONT_CACHE;
      if FHTTPS then FFlags := FFlags or INTERNET_FLAG_SECURE;

      if FRequest.Verb = '' then
        if FFields.Count > 0 then FRequest.Verb := 'POST' else FRequest.Verb := 'GET';

      hRequest := HttpOpenRequest(hConnect, PChar(FRequest.Verb), PChar(FRequest.UrlPath), nil, nil, nil, FFlags, DWord(Self));
      if hRequest = nil then
        raise EHTTPUploader.Create('HttpOpenRequest failed', GetLastError);
      try
        // Just like Chrome, we ignore certificate revocation. This is important
        // because sometimes Windows has trouble with its revocation cache and
        // this can lead to error 12057
        dwFlags := SECURITY_FLAG_IGNORE_REVOCATION;
        if not InternetSetOption(hRequest, INTERNET_OPTION_SECURITY_FLAGS, @dwFlags, sizeof(dwFlags)) then
          raise EHTTPUploader.Create('InternetSetOption SECURITY_FLAGS failed', GetLastError);

        strBoundary := GenerateMultipartBoundary;
        contentTypeHeader := GenerateContentTypeHeader(strBoundary);

        if not HttpAddRequestHeadersA(hRequest, PAnsiChar(contentTypeHeader), $FFFFFFFF, HTTP_ADDREQ_FLAG_ADD) then  // I3310
          raise EHTTPUploader.Create('HttpAddRequestHeaders failed', GetLastError);

        dwOverallBytesTotal := 0;
        contentLength := CalculateContentLength(strBoundary, dwOverallBytesTotal);

        RetryCount := 0;
        while True do
        begin
          Inc(RetryCount);
          FillChar(buffersIn, sizeof(INTERNET_BUFFERS), 0);
          buffersIn.dwStructSize := sizeof(INTERNET_BUFFERS);
          buffersIn.dwBufferTotal := contentLength;

          if not HttpSendRequestEx(hRequest, @buffersIn, nil, HSR_INITIATE, Dword(Self)) then
            // If GetLastError() == 6 (ERROR_INVALID_HANDLE), then this often means that
            // the server wasn't actually up.  Unfortunately, I don't know of a better
            // way to get more information.
            raise EHTTPUploader.Create('HttpSendRequestEx failed', GetLastError);

          dwLength := 4;
          dwReserved := 0;
          if HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @dwStatusCode, dwLength, dwReserved) and
            (dwStatusCode = HTTP_STATUS_PROXY_AUTH_REQ) then
          begin
            // Proxy is present and requires authentication to continue
            if FProxy.Username <> '' then
            begin
              InternetSetOption(hRequest, INTERNET_OPTION_PROXY_USERNAME, PChar(FProxy.Username), Length(FProxy.Username)+1);
              if FProxy.Password <> '' then
                InternetSetOption(hRequest, INTERNET_OPTION_PROXY_PASSWORD, PChar(FProxy.Password), Length(FProxy.Password)+1);
              if RetryCount < 5 then Continue;
            end
            else
            begin
              // We don't have configured proxy options, so we'll prompt the
              // user
              if FShowUI then
              begin
                // Present the standard Windows proxy authentication dialog
                dwError := InternetErrorDlg(GetDesktopWindow, hRequest, ERROR_INTERNET_INCORRECT_PASSWORD,
                                           FLAGS_ERROR_UI_FILTER_FOR_ERRORS or
                                           FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or
                                           FLAGS_ERROR_UI_FLAGS_GENERATE_DATA,
                                           pvData);
                if dwError = ERROR_INTERNET_FORCE_RETRY then
                  // This indicates that the user clicked OK in the dialog
                  Continue
                else
                  raise EHTTPUploader.Create('Unable to authenticate with proxy', dwError);
              end
              else
                raise EHTTPUploader.Create('Proxy login required but no UI specified', 0);
            end;
          end;

          for i := 0 to FFields.Count - 1 do
            UploadField(hRequest, strBoundary, FFields[i].Name, FFields[i].Value);

          dwOverallBytesSent := 0;

          for i := 0 to FFiles.Count - 1 do
            UploadFile(hRequest, strBoundary, FFiles[i].FieldName, FFiles[i].PathName, FFiles[i].ContentType, dwOverallBytesSent, dwOverallBytesTotal);

          if (FFields.Count > 0) or (FFiles.Count > 0) then
          begin
            // After the last file:
            bodyTrailer := GenerateBodyTrailer(strBoundary);
            if not InternetWriteFile(hRequest, PAnsiChar(bodyTrailer), Length(bodyTrailer), dwBytesWritten) then  // I3310
              raise EHTTPUploader.Create('InternetWriteFile failed', GetLastError);
          end;

          if not HttpEndRequest(hRequest, nil, 0, 0) then
          begin
            dwError := GetLastError;
            if dwError <> ERROR_INTERNET_FORCE_RETRY then
              raise EHTTPUploader.Create('HttpEndRequest failed', GetLastError);
          end;
            // TODO: return S_OK anyway?  It's probably too late to do anything about it now.
            //raise EHTTPUploader.Create('HttpEndRequest failed', GetLastError);

          ReadStatusCode(hRequest, dwStatusCode);

          case dwStatusCode of
            HTTP_STATUS_PROXY_AUTH_REQ: // Proxy Authentication Required
              begin
              // Insert code to set strUsername and strPassword.
                if FProxy.Username <> '' then
                begin
                  InternetSetOption(hRequest, INTERNET_OPTION_PROXY_USERNAME, PChar(FProxy.Username), Length(FProxy.Username)+1);
                  if FProxy.Password <> '' then
                    InternetSetOption(hRequest, INTERNET_OPTION_PROXY_PASSWORD, PChar(FProxy.Password), Length(FProxy.Password)+1);
                  if RetryCount < 5 then Continue;
                end
                else
                begin
                  dwError := InternetErrorDlg(GetDesktopWindow, hRequest, ERROR_INTERNET_INCORRECT_PASSWORD,
                                           FLAGS_ERROR_UI_FILTER_FOR_ERRORS or
                                           FLAGS_ERROR_UI_FLAGS_CHANGE_OPTIONS or
                                           FLAGS_ERROR_UI_FLAGS_GENERATE_DATA,
                                           pvData);

                  if dwError = ERROR_INTERNET_FORCE_RETRY then Continue;
                  //InternetErrorDlg(GetActiveWindow,  hRequest, dwError, dwFlags, ...);
                end;
              end;
          end;
          Break;
        end;

        FResponse.StatusCode := dwStatusCode;

        ReadResponseHeaders(hRequest);
        ReadResponseBody(hRequest);
      finally
        InternetCloseHandle(hRequest);
      end;
    finally
      InternetCloseHandle(hConnect);
    end;
  finally
    InternetSetOption(hInet, INTERNET_OPTION_CONTEXT_VALUE, nil, 4);
    InternetCloseHandle(hInet);
  end;
end;

procedure THTTPUploader.ReadStatusCode(hRequest: HINTERNET; var pStatusCode: DWord);
var
	dwStatusCodeSize, dwReserved: DWord;
begin
  dwReserved := 0;
	dwStatusCodeSize := sizeof(DWord);
	if not HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @pStatusCode, dwStatusCodeSize, dwReserved) then
    raise EHttpUploader.Create('ReadStatusCode failed', GetLastError);
end;

procedure THTTPUploader.ReadResponseHeaders(hRequest: HINTERNET);
var
  dwReserved, dwInfoBufferLength: DWord;
  pInfoBuffer: PWideChar;   // I4989
begin
  dwReserved := 0;
	dwInfoBufferLength := 10;
	pInfoBuffer := AllocMem(dwInfoBufferLength+1);
	while not HttpQueryInfo(hRequest, HTTP_QUERY_RAW_HEADERS_CRLF, pInfoBuffer, dwInfoBufferLength, dwReserved) do
  begin
    if GetLastError = ERROR_INSUFFICIENT_BUFFER then
    begin
      FreeMem(pInfoBuffer);
      pInfoBuffer := AllocMem(dwInfoBufferLength+1);
		end
		else
		  raise EHttpUploader.Create('HttpQueryInfo failed', GetLastError);
  end;

  FResponse.Headers := AnsiString(WideString(pInfoBuffer));   // I4989
  FreeMem(pInfoBuffer);
end;

procedure THTTPUploader.ReadResponseBody(hRequest: HINTERNET);
var
  ShortURL: string;
  SplitURL: TArray<string>;
	dwBufferLength, dwReserved, dwBytesRead, dwTotalBytes, dwBytesAvailable: DWORD;
  pMessageBody, p: PAnsiChar;
begin
  DoProcessMessages;
  FResponse.PMessageBody := nil;
  FResponse.MessageBodyLength := 0;


  dwTotalBytes := 0;
  dwBufferLength := sizeof(dwTotalBytes); dwReserved := 0;
  if not HttpQueryInfoA(hRequest, HTTP_QUERY_CONTENT_LENGTH or HTTP_QUERY_FLAG_NUMBER, @dwTotalBytes, dwBufferLength, dwReserved) then   // I4989
    dwTotalBytes := 0;

  SplitURL := Request.URL.Split(['?'], MaxInt);
  SplitURL := SplitURL[0].Split(['/'], MaxInt);
  ShortURL := SplitURL[High(SplitURL)];
  DoStatus('Downloading '+ShortURL, 0, dwTotalBytes);

  dwBytesAvailable := 65536;
  dwBytesRead := 1;
  pMessageBody := AllocMem(dwBytesAvailable+1);
  try
    while dwBytesRead > 0 do //InternetQueryDataAvailable(hRequest, dwBytesAvailable, 0, 0) do
    begin
      if CheckCancel then
        raise EHttpUploaderCancel.Create('');

      if not InternetReadFile(hRequest, pMessageBody, dwBytesAvailable, dwBytesRead) then
        raise EHttpUploader.Create('InternetReadFile failed', GetLastError);

      if dwBytesRead = 0 then Break;	// End of File.

      //DoFileProgress( 'Downloading', FResponse.MessageBodyLength, dwTotalBytes, 0, FResponse.MessageBodyLength, dwTotalBytes, 0, 0);

      p := AllocMem(FResponse.MessageBodyLength + Integer(dwBytesRead) + 1);
      if FResponse.MessageBodyLength > 0 then
      begin
        CopyMemory(p, FResponse.PMessageBody, FResponse.MessageBodyLength);
        FreeMem(FResponse.PMessageBody);
      end;
      FResponse.PMessageBody := p;
      Inc(p, FResponse.MessageBodyLength);
      CopyMemory(p, pMessageBody, dwBytesRead);
      FResponse.MessageBodyLength := FResponse.MessageBodyLength + Integer(dwBytesRead);
      DoStatus('Downloading '+ShortURL, FResponse.MessageBodyLength, dwTotalBytes);

      DoProcessMessages;
    end;
  finally
    FreeMem(pMessageBody);
  end;
end;

procedure THTTPUploader.UploadFile(hRequest: HINTERNET; const strBoundary, strFieldName: AnsiString; const strPathName: string; const strContentType: AnsiString; var dwOverallBytesSent: DWord; dwOverallBytesTotal: DWord);  // I3310
var
  fileTrailer, fileHeader: AnsiString;
	dwBytesWritten: DWORD;
  hFile: THandle;
begin
  fileHeader := GenerateFileHeader(strBoundary, strFieldName, strPathName, strContentType);

	if not InternetWriteFile(hRequest, PAnsiChar(fileHeader), Length(fileHeader), dwBytesWritten) then  // I3310
    raise EHttpUploader.Create('InternetWriteFile failed', GetLastError);

	hFile := CreateFile(PChar(strPathName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
	if hFile = INVALID_HANDLE_VALUE then
		raise EHttpUploader.Create('CreateFile failed', GetLastError);
  try
    DoFileBegin(strPathName);
    try
      UploadFileContent(hRequest, strPathName, hFile, dwOverallBytesSent, dwOverallBytesTotal);
    except
      DoFileComplete(strPathName, FALSE);
      raise;
    end;
	  DoFileComplete(strPathName, TRUE);
  finally
		CloseHandle(hFile);
  end;

	// After the file:
	fileTrailer := GenerateFileTrailer;
	if not InternetWriteFile(hRequest, PAnsiChar(fileTrailer), Length(fileTrailer), dwBytesWritten) then  // I3310
    raise EHttpUploader.Create('InternetWriteFile failed', GetLastError);
end;

procedure THTTPUploader.UploadField(hRequest: HINTERNET; const strBoundary, strFieldName, strFieldValue: AnsiString);  // I3310
var
  fieldTrailer, fieldHeader: AnsiString;
	dwBytesWritten: DWORD;
begin
  fieldHeader := GenerateFieldHeader(strBoundary, strFieldName);

	if not InternetWriteFile(hRequest, PAnsiChar(fieldHeader), Length(fieldHeader), dwBytesWritten) then  // I3310
    raise EHttpUploader.Create('InternetWriteFile failed', GetLastError);

  if Length(strFieldValue) > 0 then
  	if not InternetWriteFile(hRequest, PAnsiChar(AnsiString(strFieldValue)), Length(AnsiString(strFieldValue)), dwBytesWritten) then  // I3310
	  	raise EHttpUploader.Create('InternetWriteFile failed', GetLastError);

	fieldTrailer := GenerateFieldTrailer;
	if not InternetWriteFile(hRequest, PAnsiChar(fieldTrailer), Length(fieldTrailer), dwBytesWritten) then  // I3310
    raise EHttpUploader.Create('InternetWriteFile failed', GetLastError);
end;

procedure THTTPUploader.UploadFileContent(hRequest: HINTERNET; const strPathName: string; hFile: THandle; var dwOverallBytesSent: DWord; dwOverallBytesTotal: DWORD);
const
  PROGRESS_INTERVAL = 250;	// Report progress twice a second.   // I4399
	// If a write takes less time than this, then increase the buffer size.
	LOW_WRITE_THRESHOLD = 200;   // I4399
	// If a write takes more time than this, then decrease the buffer size.
	HIGH_WRITE_THRESHOLD = 1250;

	MIN_BUFFER_SIZE = 8192;
	INITIAL_BUFFER_SIZE = 32*1024;   // I4399
	MAX_BUFFER_SIZE = 16 * 1024 * 1024;
var
  dwOverallBytesRemaining, dwFileBytesRemaining, dwSecondsTaken, dwTimeNow, dwWriteEndTime, dwWriteTicks, dwBytesWritten, dwWriteStartTime,
  dwBytesRead, dwSecondsToOverallCompletion, dwSecondsToFileCompletion, cbBuffer, dwLocalFileSize,
  dwFileBytesSent, dwBytesPerSecond, dwTimeStarted, dwTimeLast: DWord;
  pBuffer: PAnsiChar;
begin
	dwLocalFileSize := GetFileSize(hFile, nil);
	dwFileBytesSent := 0;
	dwBytesPerSecond := FCachedBytesPerSecond;
	dwTimeStarted := GetTickCount div PROGRESS_INTERVAL;
	dwTimeLast := dwTimeStarted;

	{* The buffer size is a tradeoff.  Too small and the transfer rate will drop.
	 * Too large and each write will take too long, and you'll not get useful progress
	 * reporting.
	 * What would have been nice is if the InternetStatusCallback was notified
	 * on partial writes, but it's only notified when the entire buffer is flushed.
	 * This makes it useless for adding progress to a large buffer.
	 * Instead, we'll dynamically adjust the buffer size, so that each write is
	 * taking about a second.
	 *}

	cbBuffer := INITIAL_BUFFER_SIZE;
	pBuffer := AllocMem(cbBuffer);
  try
    if dwBytesPerSecond = 0 then dwSecondsToFileCompletion := $FFFFFFFF else dwSecondsToFileCompletion := dwLocalFileSize div dwBytesPerSecond;
    if dwBytesPerSecond = 0 then dwSecondsToOverallCompletion := $FFFFFFFF else dwSecondsToOverallCompletion := dwOverallBytesTotal div dwBytesPerSecond;
    DoFileProgress(strPathName, 0, dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond);
    while True do
    begin
      if CheckCancel then
        raise EHttpUploaderCancel.Create('');

      if not ReadFile(hFile, pBuffer^, cbBuffer, dwBytesRead, nil) then
        raise EHttpUploader.Create('ReadFile failed', GetLastError);

      if dwBytesRead = 0 then Break;

      dwWriteStartTime := GetTickCount;

      // Write that to the other end:
      if not InternetWriteFile(hRequest, pBuffer, dwBytesRead, dwBytesWritten) then
        {* One cause I've found for InternetWriteFile failing, when writing to a PHP script,
         * is that max_execution_time is too small, and the file is too large.
         *
         * The PHP script gets killed, and we see ERROR_INTERNET_CONNECTION_ABORTED.
         *}
        raise EHttpUploader.Create('InternetWriteFile failed', GetLastError);

      dwWriteEndTime := GetTickCount;
      dwWriteTicks := dwWriteEndTime - dwWriteStartTime;

      if dwWriteTicks < LOW_WRITE_THRESHOLD then
      begin
        //TRACE("Writing %d bytes took %d ms.\n", cbBuffer, dwWriteTicks);
        //TRACE("Increasing buffer size.\n");
        // Increase the buffer size for increased performance.
        if cbBuffer < MAX_BUFFER_SIZE then
        begin
          cbBuffer := cbBuffer * 2;
          if cbBuffer > MAX_BUFFER_SIZE then cbBuffer := MAX_BUFFER_SIZE;
          FreeMem(pBuffer); pBuffer := nil;
          pBuffer := AllocMem(cbBuffer);
        end
      end
      else if dwWriteTicks > HIGH_WRITE_THRESHOLD then
      begin
        //TRACE("Writing %d bytes took %d ms.\n", cbBuffer, dwWriteTicks);
        //TRACE("Decreasing buffer size.\n");
        // Decrease the buffer size for more regular progress reporting.
        if cbBuffer > MIN_BUFFER_SIZE then
        begin
          cbBuffer := cbBuffer div 2;
          if cbBuffer < MIN_BUFFER_SIZE then cbBuffer := MIN_BUFFER_SIZE;
          FreeMem(pBuffer); pBuffer := nil;
          pBuffer := AllocMem(cbBuffer);
        end;
      end;

      Inc(dwFileBytesSent, dwBytesWritten);
      Inc(dwOverallBytesSent, dwBytesWritten);

      // Uncomment this to slow it down a bit, if you're testing against a local server:
      //Sleep(50);

      // Avoid reporting progress too often.
      dwTimeNow := GetTickCount div PROGRESS_INTERVAL;
      if dwTimeNow > dwTimeLast then
      begin
        dwSecondsTaken := dwTimeNow - dwTimeStarted;
        dwBytesPerSecond := dwFileBytesSent div dwSecondsTaken;

        dwFileBytesRemaining := dwLocalFileSize - dwFileBytesSent;
        dwSecondsToFileCompletion := dwFileBytesRemaining div dwBytesPerSecond;

        dwOverallBytesRemaining := dwOverallBytesTotal - dwOverallBytesSent;
        dwSecondsToOverallCompletion := dwOverallBytesRemaining div dwBytesPerSecond;

        DoFileProgress(strPathName, dwFileBytesSent, dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond);
        dwTimeLast := dwTimeNow;
      end;
    end;

    FCachedBytesPerSecond := dwBytesPerSecond;
    DoFileProgress(strPathName, dwLocalFileSize, dwLocalFileSize, dwSecondsToFileCompletion, dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond);
  finally
  	if Assigned(pBuffer) then FreeMem(pBuffer);
  end;
end;

{* static *}
procedure StaticInternetStatusCallback(hInternet: HINTERNET; dwContext: PDWord; dwInternetStatus: DWord;
  lpvStatusInformation: Pointer; dwStatusInformationLength: DWord); stdcall;
begin
end;

procedure THTTPUploader.DoFileComplete(const FileName: string; Successful: Boolean);
begin
  if Assigned(FOnFileComplete)
    then FOnFileComplete(Self, FileName, Successful)
    else DoStatus('File '+ExtractFileName(FileName)+' complete');
end;

procedure THTTPUploader.DoFileProgress(const FileName: string;
  dwFileBytesSent, dwLocalFileSize, dwSecondsToFileCompletion,
  dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion,
  dwBytesPerSecond: DWord);
begin
  if Assigned(FOnFileProgress)
    then FOnFileProgress(Self, FileName, dwFileBytesSent, dwLocalFileSize, dwSecondsToFileCompletion,
      dwOverallBytesSent, dwOverallBytesTotal, dwSecondsToOverallCompletion, dwBytesPerSecond)
    else DoStatus(Format('Uploading file %s (%d of %d total bytes sent%s)',   // I4399
      [ExtractFileName(FileName), dwFileBytesSent, dwLocalFileSize,
      IfThen(dwSecondsToFileCompletion=$FFFFFFFF, '', ', '+IntToStr(dwSecondsToFileCompletion)+' seconds remaining')]),
      dwOverallBytesSent, dwOverallBytesTotal);
end;

procedure THTTPUploader.DoProcessMessages;
begin
  if Assigned(FHTTPUploader_MessageProcessor) then
    FHTTPUploader_MessageProcessor.ProcessMessages;
end;

procedure THTTPUploader.DoStatus(const Message: string; Position, Total: Int64); // I2855
begin
  if Assigned(FOnStatus) then FOnStatus(Self, Message, Position, Total);
end;

constructor THTTPUploader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowUI := True;
  FFiles := THTTPUploaderFiles.Create;
  FFields := THTTPUploaderFields.Create;
  FRequest := THTTPUploaderRequest.Create;
  FResponse := THTTPUploaderResponse.Create;
  FProxy := THTTPUploaderProxy.Create;
  FRequest.Agent := 'THTTPUploader/1.0';
end;

destructor THTTPUploader.Destroy;
begin
  FreeAndNil(FFiles);
  FreeAndNil(FFields);
  FreeAndNil(FRequest);
  FreeAndNil(FResponse);
  FreeAndNil(FProxy);
  inherited Destroy;
end;

{ THTTPUploaderFields }

function THTTPUploaderFields.Add(const AName, AValue: AnsiString): Integer;  // I3310
var
  f: THTTPUploaderField;
begin
  f := THTTPUploaderField.Create;
  f.Name := AName; f.Value := AValue;
  Result := inherited Add(f);
end;

function THTTPUploaderFields.GetItem(Index: Integer): THTTPUploaderField;
begin
  Result := inherited GetItem(Index) as THTTPUploaderField;
end;

procedure THTTPUploaderFields.SetItem(Index: Integer; const Value: THTTPUploaderField);
begin
  inherited SetItem(Index, Value);
end;

{ THTTPUploaderFiles }

function THTTPUploaderFiles.Add(const AFieldName: AnsiString; const APathName: string;  // I3310
  const AContentType: AnsiString): Integer;
var
  f: THTTPUploaderFile;
begin
  f := THTTPUploaderFile.Create;
  f.FieldName := AFieldName;
  f.PathName := APathName;
  if AContentType = ''
    then f.ContentType := 'application/octet-stream'
    else f.ContentType := AContentType;
  Result := inherited Add(f);
end;

function THTTPUploaderFiles.GetItem(Index: Integer): THTTPUploaderFile;
begin
  Result := inherited GetItem(Index) as THTTPUploaderFile;
end;

procedure THTTPUploaderFiles.SetItem(Index: Integer; const Value: THTTPUploaderFile);
begin
  inherited SetItem(Index, Value);
end;

procedure THTTPUploader.DoFileBegin(const Data: string);
begin
  if Assigned(FOnFileBegin)
    then FOnFileBegin(Self, Data)
    else DoStatus('File '+Data+' beginning upload');
end;

function THTTPUploader.CheckCancel: Boolean;
begin
  if Assigned(FOnCheckCancel) then FOnCheckCancel(Self, Result) else Result := False;
end;

{ EHTTPUploader }

constructor EHTTPUploader.Create(const msg: string; ErrorCode: Integer);
  function InetErrorMessage(ErrorCode: Integer): string;
  var
    Buffer: array[0..255] of Char;
  var
    Len: Integer;
  begin
    Len := FormatMessage(FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_IGNORE_INSERTS or
      FORMAT_MESSAGE_ARGUMENT_ARRAY, Pointer(GetModuleHandle('wininet.dll')), ErrorCode, 0, Buffer,
      Length(Buffer), nil);  // I3310
    while (Len > 0) and (CharInSet(Buffer[Len - 1], [#0..#32, '.'])) do Dec(Len);
    SetString(Result, Buffer, Len);
  end;

begin
  FErrorCode := ErrorCode;
  inherited Create(msg+Format(', error = %d (%s)', [ErrorCode, InetErrorMessage(ErrorCode)]));
end;

{ THTTPUploaderResponse }

destructor THTTPUploaderResponse.Destroy;
begin
  FreeMem(PMessageBody);
  inherited Destroy;
end;

function THTTPUploaderResponse.MessageBodyAsString: AnsiString;  // I3310
begin
  Result := PMessageBody;
end;

procedure THTTPUploaderResponse.SetHeaders(const Value: AnsiString);   // I7914  // I3310
    function MakeSafe(path: string): AnsiString;
    var
      i: Integer;
    begin
      I := LastDelimiter('/' + PathDelim + DriveDelim, path);
      Result := AnsiString(Copy(path, I + 1, MaxInt));  // I3310

      for I := Length(Result) downto 1 do
        if CharInSet(Result[I], [#0..#31, '?', '*', '"', '<', '>', '|']) then Delete(Result, I, 1);  // I3310
    end;

var
  s: string;
  n: Integer;
  i: Integer;
begin
  FHeaders := Value;
  with TStringList.Create do
  try
    Text := string(FHeaders);  // I3310
    s := '';
    for i := 0 to Count - 1 do
    begin
      if AnsiCompareText(Copy(Trim(Strings[i]), 1, Length('Content-Disposition')), 'Content-Disposition') = 0 then
      begin
        {s := Trim(Strings[i]);
        System.Delete(s, 1, Length('Content-Disposition'));
        s := Trim(Strings[i]);
        System.Delete(s, 1, 1);}
        s := Trim(Strings[i]);
        Break;
      end;
    end;
    if s <> '' then
    begin
      n := Pos(';', s);
      while n > 0 do
      begin
        System.Delete(s,1,n);
        s := Trim(s);
        if Copy(s,1,8) = 'filename' then
        begin
          System.Delete(s,1,8);
          s := Trim(s);
          if Copy(s,1,1) = '=' then
          begin
            System.Delete(s,1,1);
            s := Trim(s);
            if Copy(s,1,1) = '"' then
            begin
              System.Delete(s,1,1);
              n := Pos('"', s);
              if n > 0 then
                ContentDispositionFilename := MakeSafe(Copy(s, 1, n-1));
            end
            else
            begin
              n := Pos(' ', s);
              if n = 0 then n := Pos(';', s);
              if n = 0 then n := Length(s) + 1;
              ContentDispositionFileName := MakeSafe(Copy(s, 1, n-1));
            end;
            Exit;
          end;
        end;
        n := Pos(';', s);
      end;
    end;
  finally
    Free;
  end;
end;

{ THTTPUploaderRequest }

function THTTPUploaderRequest.GetURL: string;
begin
  Result := Protocol + '://' + HostName + UrlPath;
end;

procedure THTTPUploaderRequest.SetURL(URL: string);
var
  n: Integer;
begin
  n := Pos('://', URL);
  if n = 0 then Exit;

  Protocol := Copy(URL, 1, n-1);
  Delete(URL, 1, n+2);

  n := Pos('/', URL);
  if n = 0 then Exit;
  HostName := Copy(URL, 1, n-1);
  Delete(URL, 1, n-1);

  UrlPath := URL;
end;

{ THTTPUploaderProxy }

function THTTPUploaderProxy.Describe(HTTPS: Boolean): string;
begin
  Result := Server;
  if Port > 0 then
    Result := Result + ':' + IntToStr(Port);

//  if HTTPS then
//    Result := 'https=https://'+Result;
end;

end.

