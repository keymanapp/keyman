//*************************************************************************
//                                                                        *
//                       IEDownload Demo 2009                            *                                                      *
//                            by                                          *
//             Eran Bodankin - bsalsa - (bsalsa@gmail.com)               *
//  Updated versions:                                                     *
//               http://www.bsalsa.com                                    *
//*************************************************************************
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. BSALSA PRODUCTIONS DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. BSALSA PRODUCTIONS SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 3 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit Downloaddemo_U;

interface

uses
  dialogs, Windows, SysUtils, Classes, Forms, UrlMon, StdCtrls, ActiveX,
  Controls, ComCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB, ExtCtrls, OleCtrls,
  IEDownload, IEDownloadAcc, IEDownloadTools, IEAddress;

type
  TForm1 = class(TForm)
    IEDownload1: TIEDownload;
    Panel1: TPanel;
    btnStart: TButton;
    btnStop: TButton;
    Panel3: TPanel;
    Panel4: TPanel;
    ListView: TListView;
    rgBind: TRadioGroup;
    rgBindMethod: TRadioGroup;
    GroupBox1: TGroupBox;
    ProgressBar1: TProgressBar;
    lblProgress: TLabel;
    GroupBox2: TGroupBox;
    edtFile: TEdit;
    GroupBox3: TGroupBox;
    lblState: TLabel;
    GroupBox4: TGroupBox;
    cbOverWrite: TCheckBox;
    cbOpenFolder: TCheckBox;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    EmbeddedWB1: TEmbeddedWB;
    TabSheet4: TTabSheet;
    memPreviewData: TMemo;
    TabSheet9: TTabSheet;
    memPreviewStream: TMemo;
    TabSheet3: TTabSheet;
    Panel5: TPanel;
    memEvents: TMemo;
    TabSheet5: TTabSheet;
    memResponse: TMemo;
    TabSheet6: TTabSheet;
    memProgress: TMemo;
    TabSheet7: TTabSheet;
    memHeaders: TMemo;
    TabSheet8: TTabSheet;
    memErrors: TMemo;
    TabSheet10: TTabSheet;
    memDetails: TMemo;
    TabSheet2: TTabSheet;
    memSession: TMemo;
    memAddress: TMemo;
    btnStopAll: TButton;
    btnGoList: TButton;
    rgFileExists: TRadioGroup;
    cbAsyn: TCheckBox;
    btnToCache: TButton;
    btnToFile: TButton;
    TabSheet11: TTabSheet;
    lvThread: TListView;
    Panel2: TPanel;
    btnGo: TButton;
    IEAddress1: TIEAddress;
    procedure rgBindClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function IEDownload1Response(Sender: TBSCB; dwResponseCode: Cardinal;
      szResponseHeaders, szRequestHeaders: PWideChar;
      out szAdditionalRequestHeaders: PWideChar): HRESULT;
    procedure IEDownload1Resume(Sender: TBSCB; FileName: string;
      var Action: Cardinal);
    function IEDownload1GetWindow(Sender: TBSCB; const GUIDReason: TGUID;
      out hwnd: Cardinal): HRESULT;
    procedure IEDownload1DataAvailable(Sender: TBSCB; var Buffer: PByte;
      var BufLength: Cardinal);
    function IEDownload1BeginningTransaction(Sender: TBSCB; szURL,
      szHeaders: PWideChar; dwReserved: Cardinal;
      out szAdditionalHeaders: PWideChar): HRESULT;
    procedure btnStopClick(Sender: TObject);
    procedure btnStartClick(Sender: TObject);
    function IEDownload1SecurityProblem(Sender: TBSCB; dwProblem: Cardinal;
      Problem: string): HRESULT;
    procedure IEDownload1GetBindResults(var Sender: TBSCB;
      out clsidProtocol: TGUID; out dwResult: Cardinal; out szResult: PWideChar;
      const stResult: string);
    procedure IEDownload1StateChange(State: TState);
    procedure IEDownload1Error(const ErrorCode: Integer; const stError: string);
    function IEDownload1CodeInstallProblem(Sender: TBSCB;
      ulStatusCode: Cardinal; szDestination, szSource: PWideChar;
      dwReserved: Cardinal; stResult: string): HRESULT;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    function IEDownload1GetRootSecurityId(var SecurityIdBuffer: TByteArray;
      var BufferSize: Cardinal): HRESULT;
    procedure IEDownload1Redirect(Sender: TBSCB; var AbortRedirect: Boolean;
      const FromUrl, DestUrl: string);
    procedure IEDownload1DataAvailableInfo(Sender: TBSCB; grfBSCF: Cardinal;
      Status: string);
    procedure btnStopAllClick(Sender: TObject);
    procedure btnGoListClick(Sender: TObject);
    procedure IEDownload1FileExists(var Action: TFileExistsOption;
      const aFileName: WideString; var NewFileName: WideString);
    procedure IEDownload1StopBinding(Sender: TBSCB; HRESULT: HRESULT;
      szError: PWideChar);
    function IEDownload1GetBindInfo(Sender: TBSCB; out grfBINDF: Cardinal;
      var BindInfo: _tagBINDINFO): HRESULT;
    procedure btnToCacheClick(Sender: TObject);
    procedure btnToFileClick(Sender: TObject);
    procedure IEDownload1Connect(Sender: TBSCB; Res: HRESULT;
      stMessage: string);
    procedure IEDownload1StreamComplete(Sender: TBSCB; Stream: TStream;
      Result: HRESULT);
    procedure IEDownload1Terminate(const Sender: TBSCB; const ThreadId: Integer;
      const aFileName: WideString; var bCancel: Boolean);
    function IEDownload1GetBindInfoEx(Sender: TBSCB; out grfBINDF: Cardinal;
      pbindinfo: _tagBINDINFO; out grfBINDF2: Cardinal): HRESULT;
    function IEDownload1GetSerializedClientCertContext(var Sender: TBSCB;
      out ppbCert: Byte; var pcbCert: Cardinal): HRESULT;
    function IEDownload1PutProperty(Sender: TBSCB; mkp: _MONIKERPROPERTY;
      val: PWideChar): HRESULT;
    procedure IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded,
      ElapsedTime, Speed, RemainingTime, Status, Percent: string);
    procedure IEDownload1StartBinding(Sender: TBSCB; var Cancel: Boolean;
      pib: IBinding; const FileName: WideString; const FileSize: Integer);
    procedure IEDownload1Authenticate(Sender: TBSCB; var tmpHWND: HWND;
      var szUserName, szPassWord: WideString; var Rezult: HRESULT);
    procedure IEDownload1AuthenticateEx(Sender: TBSCB; var tmpHWND: HWND;
      var szUserName, szPassWord: WideString; pauthinfo: _tagAUTHENTICATEINFO;
      var Rezult: HRESULT);
    procedure IEDownload1Complete(Sender: TCustomIEDownload; aFileNameAndPath,
      aFileName, aFolderName, aExtension: WideString;
      const ActiveConnections: Integer);
    procedure IEDownload1BeforeDownload(Sender: TInfoData; const Url, FileName,
      FileExtension, Host, DownloadFolder: string; const FileSize: Integer;
      var Cancel: Boolean);
    procedure EmbeddedWB1FileDownload(Sender: TCustomEmbeddedWB; pmk: IMoniker;
      pbc: IBindCtx; dwBindVerb, grfBINDF: Cardinal; pBindInfo: PBindInfo;
      pszHeaders, pszRedir: PWideChar; uiCP: Cardinal; var Rezult: HRESULT);
    procedure btnGoClick(Sender: TObject);
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure EmbeddedWB1NavigateComplete2(ASender: TObject;
      const pDisp: IDispatch; var URL: OleVariant);
  private
    { Private declarations }
    tmpString: string;
    counter: integer;
    procedure ClearComponents;
    procedure InitialComponents;
    procedure UpdateThreadDetails(aSender: TBSCB);
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.btnStopAllClick(Sender: TObject);
begin
  IEDownload1.CancelAll;
  counter := 0;
end;

procedure TForm1.btnStopClick(Sender: TObject);
begin
  IEDownload1.Cancel;
  counter := 0;
end;

procedure TForm1.btnToCacheClick(Sender: TObject);
begin
  ClearComponents;
  InitialComponents;
  IEdownload1.UrlDownloadToCacheFile(memAddress.Lines[0]);
end;

procedure TForm1.btnToFileClick(Sender: TObject);
begin
  ClearComponents;
  InitialComponents;
  IEdownload1.UrlDownloadToFile(memAddress.Lines[0]);
end;

procedure TForm1.btnGoClick(Sender: TObject);
begin
   EmbeddedWB1.Go(IEAddress1.Text);
end;

procedure TForm1.btnGoListClick(Sender: TObject);
var
  sl: TStrings;
begin
  ClearComponents;
  InitialComponents;
  sl := TStringList.Create;
  try
    with sl do
    begin
      Add('http://www.bsalsa.com/Downloads/2.exe');
      Add('http://www.bsalsa.com/Downloads/2.rar');
      Add('http://www.bsalsa.com/support.html');
    end;
    memAddress.Clear;
    memAddress.Lines := sl;
    IEdownload1.GoList(sl);
  finally
    sl.Free;
  end;
end;

procedure TForm1.btnStartClick(Sender: TObject);
begin
  ClearComponents;
  InitialComponents;
  IEdownload1.Go(memAddress.Lines[0]);
  MemSession.Lines:= IEdownload1.ItemsManager.SessionList;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IEDownload1.ActiveConnections > 0 then
  begin
  if MessageDlg('Threads active. Do you still want to quit?',
      mtWarning, [mbYes, mbNo], 0) = mrNo then
      CanClose:= False
      else
      IEDownload1.CancelAll;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  PageControl1.ActivePageIndex := 0;
  cbOverWrite.Checked := True;
  cbAsyn.Checked := True;
  cbOpenFolder.Checked := True;
  rgBindMethod.ItemIndex := 0;
  rgBind.ItemIndex := 0;
  rgFileExists.ItemIndex := 0;
end;

procedure TForm1.rgBindClick(Sender: TObject);
begin
  cbOpenFolder.Enabled := not (rgBind.ItemIndex = 1);
end;

procedure TForm1.IEDownload1DataAvailable(Sender: TBSCB; var Buffer: PByte;
  var BufLength: Cardinal);
{Use the OnData event to get the data for parsing/editing '
 on the fly'. }
begin
  inc(counter);
  if counter = 1 then {Just so you will not see the event fires several times}
    memEvents.Lines.Add('Data Available Event.');
  memPreviewData.Lines.Add(string(PAnsiChar(Buffer)));
end;

procedure TForm1.IEDownload1DataAvailableInfo(Sender: TBSCB; grfBSCF: Cardinal;
  Status: string);
begin
  if tmpString <> Status then
    {Just so you will not see the event fires several times}
    memEvents.Lines.Add('Data Available Info Event:' + Status);
  tmpString := status;
end;

procedure TForm1.IEDownload1Error(const ErrorCode: Integer;
  const stError: string);
begin
  memEvents.Lines.Add('Error Event.' + stError);
  memErrors.Lines.Add('An error accrued:  ' + stError);
  PageControl1.ActivePageIndex := 7;
end;

procedure TForm1.IEDownload1FileExists(var Action: TFileExistsOption;
  const aFileName: WideString; var NewFileName: WideString);

begin
  memEvents.Lines.Add('File Exists Event: ' + aFileName);
  case rgFileExists.ItemIndex of
    0: Action := feOverWrite;
    1:
      begin
        Action := feRename;
        TimeSeparator := '_';
        NewFileName := Inputbox('IE Downloader - File exist warning',
          'Please type a new file name', TimeToStr(now) + '_' +
          IEDownload1.FileName);
        if NewFileName = '' then
          Action := feSkip;
      end;
    2:
      begin
        showmessage('The file exists. Operation aborted.');
        EmbeddedWb1.LoadFromString('The file exists. Operation aborted.');
        Action := feSkip;
      end;
  end;
end;

procedure TForm1.IEDownload1Authenticate(Sender: TBSCB; var tmpHWND: HWND;
  var szUserName, szPassWord: WideString; var Rezult: HRESULT);
begin
    memEvents.Lines.Add('Authenticate Event.' + #13#10 +
    ResponseCodeToStr(Rezult));
  if (szUserName <> '') or (szPassWord <> '') then
  begin
    memEvents.Lines.Add('UserName : ' + szUserName);
    memEvents.Lines.Add('Password : ' + szPassWord)
  end;
end;

procedure TForm1.IEDownload1AuthenticateEx(Sender: TBSCB; var tmpHWND: HWND;
  var szUserName, szPassWord: WideString; pauthinfo: _tagAUTHENTICATEINFO;
  var Rezult: HRESULT);
begin
    memEvents.Lines.Add('AuthenticateEx Event.' + #13#10 +
    ResponseCodeToStr(Rezult));
  if (szUserName <> '') or (szPassWord <> '') then
  begin
    memEvents.Lines.Add('UserName : ' + szUserName);
    memEvents.Lines.Add('Password : ' + szPassWord)
  end;
end;

procedure TForm1.IEDownload1BeforeDownload(Sender: TInfoData; const Url,
  FileName, FileExtension, Host, DownloadFolder: string;
  const FileSize: Integer; var Cancel: Boolean);
begin
   memEvents.Lines.Add('Before Download Event. ' + FileName);
end;

function TForm1.IEDownload1BeginningTransaction(Sender: TBSCB; szURL,
  szHeaders: PWideChar; dwReserved: Cardinal;
  out szAdditionalHeaders: PWideChar): HRESULT;
{Use OnBeginningTransaction to add additionally headers to the
 transaction.}
begin
  UpdateThreadDetails(Sender);
  memHeaders.Lines.Add('AdditionalHeaders:' + #13#10 + szAdditionalHeaders);
  memEvents.Lines.Add('Beginning Transaction Event.');
  Result := S_OK;
end;

function TForm1.IEDownload1CodeInstallProblem(Sender: TBSCB;
  ulStatusCode: Cardinal; szDestination, szSource: PWideChar;
  dwReserved: Cardinal; stResult: string): HRESULT;
begin {The event will fire only under this terms:
  http://msdn.microsoft.com/en-us/library/ms775136(VS.85).aspx}
  memEvents.Lines.Add('Code Install Problem Event.');
  memErrors.Lines.Add('An error accrued:  ' + stResult);
  Result := S_OK;
end;

procedure TForm1.IEDownload1Complete(Sender: TCustomIEDownload;
  aFileNameAndPath, aFileName, aFolderName, aExtension: WideString;
  const ActiveConnections: Integer);
 var
  lvTItem: TListItem;
  idx: integer;
begin
  memEvents.Lines.Add('Complete Event:' + aFileName);
  with memDetails.Lines do
  begin
    Add(#13#10 + 'Downloading from: ' + IEDownload1.ServerAddress);
    Add(#13#10 + 'Server IP: ' + IEDownload1.ServerIP);
    Add(#13#10 + 'File mime type: ' + IEDownload1.MimeType);
    Add(#13#10 + 'Display Name: ' + IEDownload1.DisplayName);
    case IEDownload1.DownloadMethod of
      dmStream:Add(#13#10 + 'Download method: Stream');
      dmFile:
        begin
          Add(#13#10 + 'Download method: File');
          Add(#13#10 + 'Download folder: ' + aFolderName);
          Add(#13#10 + 'Downloaded File name: ' + aFileName);
          Add(#13#10 + 'Downloaded File Extension: ' + aExtension);
          Add(#13#10 + 'File size: ' + (FormatSize(IEDownload1.FileSize)));
          if not FileExists(aFileNameAndPath) then
            memErrors.Lines.Add('Can not Locate The File.')
          else
          begin
          if IEDownload1.MimeType = 'text/html' then
            EmbeddedWB1.Go(aFileNameAndPath)
          else
            EmbeddedWB1.LoadFromString('Done.');
          end;
        end;
    end;
    Add(#13#10 + 'Total Downloads: ' + IntToStr(IEDownload1.DownloadsCounter));
    edtFile.Text := IEDownload1.DownloadFolder + IEDownload1.FileName;
  end;
    for idx := 0 to lvThread.Items.Count - 1 do
    if (lvThread.Items[idx].Caption = IEDownload1.FileName) then
    begin
      lvTItem := lvThread.Items[idx];
      if (Assigned(lvTItem)) then
        lvTItem.SubItems[3] := IntToStr(IEDownload1.ActiveConnections);
   end;
end;

procedure TForm1.IEDownload1Connect(Sender: TBSCB; Res: HRESULT;
  stMessage: string);
begin
  memEvents.Lines.Add('Connect Event:' + stMessage);
end;

function TForm1.IEDownload1SecurityProblem(Sender: TBSCB; dwProblem: Cardinal;
  Problem: string): HRESULT;
begin
  memEvents.Lines.Add('SecurityProblem Event.');
  Result := S_OK;
end;

procedure TForm1.IEDownload1StartBinding(Sender: TBSCB; var Cancel: Boolean;
  pib: IBinding; const FileName: WideString; const FileSize: Integer);
begin
   memEvents.Lines.Add('Start Binding Event.' + FileName);
end;

procedure TForm1.IEDownload1StateChange(State: TState);
begin
  btnStart.Enabled := not IEDownload1.Busy;
  btnGoList.Enabled := not IEDownload1.Busy;
  btnToCache.Enabled := not IEDownload1.Busy;
  btnToFile.Enabled := not IEDownload1.Busy;
  btnStop.Enabled := IEDownload1.Busy;
  btnStopAll.Enabled := IEDownload1.Busy;
  case State of
    sBusy:
      begin
        memEvents.Lines.Add('StateChange: Busy');
        lblState.Caption := 'Busy';
      end;
    sReady:
      begin
        memEvents.Lines.Add('StateChange: Ready');
        lblState.Caption := 'Ready';
      end;
    sStopped:
      begin
        memEvents.Lines.Add('StateChange: Stopped');
        lblState.Caption := 'Stopped';
      end;
  end;
end;

procedure TForm1.IEDownload1StopBinding(Sender: TBSCB; HRESULT: HRESULT;
  szError: PWideChar);
begin
  memEvents.Lines.Add('StopBinding Event. ' + ResponseCodeToStr(HRESULT));
  if HRESULT <> S_OK then
    memErrors.Lines.Add('On Stop Binding Error: ' + szError);
end;

procedure TForm1.IEDownload1StreamComplete(Sender: TBSCB; Stream: TStream;
  Result: HRESULT);
var
  MS: TMemoryStream;
begin
  memEvents.Lines.Add('DownloadComplete Event.');
  counter := 0;
  MS := TMemoryStream.Create;
  with ms do
  begin
    Seek(0, 0);
    LoadFromStream(Stream);
  end;
  memPreviewStream.Lines.LoadFromStream(MS);
  MS.Free;
  if IEDownload1.DownloadMethod = dmStream then
     EmbeddedWB1.LoadFromStream(Stream);
end;

procedure TForm1.IEDownload1Terminate(const Sender: TBSCB;
  const ThreadId: Integer; const aFileName: WideString; var bCancel: Boolean);
begin
   UpdateThreadDetails(Sender);
   memEvents.Lines.Add('Termination Event.');
  // if IEDownload1.ProcessCounter > 0 then
  begin
 //   if MessageDlg('Threads active. Do you still want to quit?',
  //    mtWarning, [mbYes, mbNo], 0) = mrNo then
      //  CanClose := false;
  end;
end;

function TForm1.IEDownload1GetBindInfo(Sender: TBSCB; out grfBINDF: Cardinal;
  var BindInfo: _tagBINDINFO): HRESULT;
begin
  memEvents.Lines.Add('GetBindInfo Event.');
  Result := S_OK;
end;

function TForm1.IEDownload1GetBindInfoEx(Sender: TBSCB; out grfBINDF: Cardinal;
  pbindinfo: _tagBINDINFO; out grfBINDF2: Cardinal): HRESULT;
begin
  memEvents.Lines.Add('GetBindInfoEx Event.');
  Result := S_OK;
end;

procedure TForm1.IEDownload1GetBindResults(var Sender: TBSCB;
  out clsidProtocol: TGUID; out dwResult: Cardinal; out szResult: PWideChar;
  const stResult: string);
begin
  memEvents.Lines.Add('GetBindResults Event. ' + stResult);
  if dwResult <> S_OK then
    memErrors.Lines.Add(szResult + ' GetBindResults.' + stResult);
end;

function TForm1.IEDownload1GetRootSecurityId(var SecurityIdBuffer: TByteArray;
  var BufferSize: Cardinal): HRESULT;
begin
  Result := S_OK;
  memEvents.Lines.Add('Get Root Security Id Event.');
end;

function TForm1.IEDownload1GetSerializedClientCertContext(var Sender: TBSCB;
  out ppbCert: Byte; var pcbCert: Cardinal): HRESULT;
begin
  memEvents.Lines.Add('GetSerializedClientCertContext Event.');
  Result := S_OK;
end;

function TForm1.IEDownload1GetWindow(Sender: TBSCB; const GUIDReason: TGUID;
  out hwnd: Cardinal): HRESULT;
begin
  memEvents.Lines.Add('GetWindow Event.');
  Result := S_OK;
end;

procedure TForm1.IEDownload1Progress(Sender: TBSCB; ulProgress, ulProgressMax,
  ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded,
  ElapsedTime, Speed, RemainingTime, Status, Percent: string);
var
  ListItem: TListItem;
begin
  memProgress.Lines.Add(szStatusText);
  memProgress.Lines.Add(Status);
  lblProgress.Caption := Format('Downloaded %d of %d bytes', [ulProgress,
    ulProgressMax]) + '   |  in KB: ' + FormatSize(IEDownload1.FileSize);
  ProgressBar1.Max := ulProgressMax;
  ProgressBar1.Position := ulProgress;
  if (ulStatusCode = BINDSTATUS_BEGINDOWNLOADDATA) then
    memEvents.Lines.Add('Progress Event.');
  if (ulStatusCode = BINDSTATUS_DOWNLOADINGDATA) or (ulStatusCode =
    BINDSTATUS_ENDDOWNLOADDATA) then
  begin
    with ListView do
    begin
      Items.BeginUpdate;
      try
        ListItem := ListView.Items.Add;
        ListItem.Caption := IEDownload1.FileName;
        with ListItem.SubItems do
        begin
          Add(Speed);
          Add(Downloaded);
          Add(RemainingTime);
          Add(ElapsedTime);
          Add(Status);
          Add(' (' + IntToStr(ulProgress) + ' of: ' + IntToStr(ulProgressMax) +
            ')');
          Add(Percent);
        end;
      finally
        Items.EndUpdate;
      end;
    end;
  end;
end;

function TForm1.IEDownload1PutProperty(Sender: TBSCB; mkp: _MONIKERPROPERTY;
  val: PWideChar): HRESULT;
begin
   memEvents.Lines.Add('Put Property Event');
   Result:= S_OK;
end;

procedure TForm1.IEDownload1Redirect(Sender: TBSCB; var AbortRedirect: Boolean;
  const FromUrl, DestUrl: string);
begin
  AbortRedirect := False;
  memEvents.Lines.Add('Redirect Event from address:' + FromUrl + #13#10 +
    'To address: ' + DestUrl);
end;

function TForm1.IEDownload1Response(Sender: TBSCB; dwResponseCode: Cardinal;
  szResponseHeaders, szRequestHeaders: PWideChar;
  out szAdditionalRequestHeaders: PWideChar): HRESULT;
{Use OnResponse to get response headers and eventually add additional request headers.}
begin
  memEvents.Lines.Add('Response Event.');
  with memResponse.Lines do
  begin
    Add('dwResponseCode:');
    Add(ResponseCodeToStr(dwResponseCode));
    Add('Response szResponseHeaders:');
    Add(szResponseHeaders);
  end;
  Result := S_OK;
end;

procedure TForm1.IEDownload1Resume(Sender: TBSCB; FileName: string;
  var Action: Cardinal);
begin
  //Useless, the event is not supprted by Microsoft yet.
  memEvents.Lines.Add('Response Event.');
end;

{Some procedures-----------------------------------------------------------------}

procedure TForm1.ClearComponents;
begin
  lvThread.Clear;
  memHeaders.Lines.Clear;
  memEvents.Lines.Clear;
  memPreviewData.Lines.Clear;
  memPreviewStream.Lines.Clear;
  memResponse.Lines.Clear;
  memProgress.Lines.Clear;
  MemErrors.Lines.Clear;
  ListView.Items.Clear;
  MemDetails.Lines.Clear;
  edtFile.Text := '';
end;

procedure TForm1.EmbeddedWB1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
begin
  IEAddress1.Text:= URL;
end;

procedure TForm1.EmbeddedWB1FileDownload(Sender: TCustomEmbeddedWB;
  pmk: IMoniker; pbc: IBindCtx; dwBindVerb, grfBINDF: Cardinal;
  pBindInfo: PBindInfo; pszHeaders, pszRedir: PWideChar; uiCP: Cardinal;
  var Rezult: HRESULT);
begin
   Rezult:= S_OK;
   IEDownload1.Download(pmk, pbc);
end;

procedure TForm1.EmbeddedWB1NavigateComplete2(ASender: TObject;
  const pDisp: IDispatch; var URL: OleVariant);
begin
  IEAddress1.Text:= EmbeddedWB1.LocationURL;
end;

procedure TForm1.InitialComponents;
begin
  EmbeddedWB1.AddHtmlToAboutBlank('Working. Please wait..');
  //PageControl1.ActivePageIndex := 0;
  ProgressBar1.Position := 0;
  counter := 0;
  with IEDownload1 do
  begin
    case rgBind.ItemIndex of
      0: BindVerb := Get;
      1: BindVerb := Post;
      2: BindVerb := Put;
      3: BindVerb := Custom;
    end;
    case rgBind.ItemIndex of
      0: DownloadMethod := dmFile;
      1: DownloadMethod := dmStream;
    end;
    if (cbOpenFolder.Checked) and (DownloadMethod = dmFile) then
      OpenDownloadFolder := True
    else
      OpenDownloadFolder := False;

    if cbAsyn.Checked then
      BindF := BindF  + [Asynchronous]
    else
      BindF := BindF - [Asynchronous];
  end;
end;

procedure TForm1.UpdateThreadDetails(aSender: TBSCB);
  var
  lvTItem: TListItem;
begin
  if (aSender <> nil) then
  begin
    lvTItem := lvThread.Items.Add;
     with lvTItem do
     begin
       Caption := IEDownload1.FileName;
       SubItems.Add(IntToStr(aSender.ThreadID));
       SubItems.Add(IntToStr(aSender.Handle));
       case aSender.ThreadStatus of
         tsRunning:  SubItems.Add('Running');
         tsSuspended:  SubItems.Add('Suspended');
         tsWaiting:  SubItems.Add('Waiting');
         tsTerminated:  SubItems.Add('Terminated');
       end;
       SubItems.Add(IntToStr(IEDownload1.ActiveConnections));
     end;
  end;
end;
end.

