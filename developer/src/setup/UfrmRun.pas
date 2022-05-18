(*
  Name:             UfrmRun
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jun 2007

  Modified Date:    11 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jun 2007 - mcdurdin - Initial version
                    05 Jun 2007 - mcdurdin - I817 - Fix install label display when no keyboards installing
                    08 Jun 2007 - mcdurdin - I876 - Use labels from setup.inf
                    08 Jun 2007 - mcdurdin - I877 - Don't attempt to downgrade from Pro to Light
                    08 Jun 2007 - mcdurdin - I878 - Always start Welcome screen
                    08 Jun 2007 - mcdurdin - I879 - Bootstrapper should exit after install finishes successfully
                    19 Jun 2007 - mcdurdin - I817 - Translate to Unicode and remove Forms dependence
                    23 Aug 2007 - mcdurdin - Initial version from Desktop
                    17 Sep 2007 - mcdurdin - I1071 - Fix setup crash when downloading update
                    17 Sep 2007 - mcdurdin - I1072 - Fix non-silent silent mode
                    17 Sep 2007 - mcdurdin - I1073 - Fix non-silent silent mode
                    14 Jun 2008 - mcdurdin - I1440 - Avoid update check stopping install
                    18 Mar 2011 - mcdurdin - I2680 - Bootstrapper needs to allow downloading full installer upgrade
                    18 Mar 2011 - mcdurdin - I1901 - Bootstrapper should close after install
                    31 Mar 2011 - mcdurdin - I2856 - Keyman Developer 8.0 setup incorrectly looks for version 7.0 update
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3326 - V9.0 - Add missing MsiEnumRelatedProducts from jwamsi
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    17 Aug 2012 - mcdurdin - I3377 - KM9 - Update code references from 8.0 to 9.0
                    15 Jun 2012 - mcdurdin - I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    03 Nov 2012 - mcdurdin - I3500 - V9.0 - Merge of I3355 - Keyman Developer (and Desktop) sometimes reboot automatically with auto upgrade
                    11 Aug 2015 - mcdurdin - I4858 - Fix XML files not upgrading in Keyman Developer
*)
unit UfrmRun;  // I3306

interface

uses
  resource, CommCtrl,
  Windows, Messages, SysUtils, Variants, Classes,
  SetupStrings,
  SetupForm, UfrmDownloadProgress;

type
  TMSIInfo = record
    Version, ProductCode: WideString;
    InstallURL, Filename: WideString;  // I2680
    InstallSize: Integer;
  end;

  TfrmRun = class(TSetupForm)
  public
    procedure cmdExitClick(Sender: TObject);
    procedure lblTitleClick(Sender: TObject);
    procedure cmdInstallKeymanDesktopClick(Sender: TObject);

    procedure FormCreate(Sender: TObject); override;
    procedure FormShow(Sender: TObject); override;
    procedure DlgMain(var Message: TMessage); override;
    function ProcessCommand(ID, NotificationCode: Integer; hControl: HWND): Boolean; override;
  private
    FBitmap, FLogo: THandle;
    StatusMax: Integer;
    FDownloadFilename: WideString;
    FDownloadURL: WideString;
    FNewVersion, FInstalledVersion, FInstallerVersion: TMSIInfo;
    //FInstalledVersion: WideString;
    FContinueSetup: Boolean;
    FWhiteBrush: THandle;
    FErrorLog: TStream;
    FPromptForReboot: Boolean;  // I3355   // I3500
    FSilent: Boolean;
    FOfflineByRequest: Boolean;
    FOnline: Boolean;
    //FInstalledProductCode: WideString;
    //FInstallerProductCode: WideString;
    //FNewProductCode: WideString;

    procedure LogError(const msg: WideString; ShowDialogIfNotSilent: Boolean = True);

    procedure WMUserFormShown(var Message: TMessage); //message WM_USER_FormShown;
    procedure SetupMSI;
    procedure CheckNewVersion;
    function InstallMSI: Boolean;
    procedure Status(const Text: WideString = '');
    procedure DownloadRedistFile(AOwner: TfrmDownloadProgress; var Result: Boolean);
    function InstallNewVersion: Boolean;
    function IsNewerVersionInstalled(const NewVersion: WideString): Boolean;
    procedure CheckInstalledVersion;
    procedure PrepareForReboot(res: Cardinal);
    function RestartWindows: Boolean;
    procedure CheckInternetConnectedState;
    procedure SetFontProperties(id, weight: Integer; ssSize, ssName: TInstallInfoText);
    function CacheMSIFile(const SrcMSIFileName: WideString): WideString;
    procedure FinishCacheMSIFile(const SrcMSIFileName: WideString; InstallSuccess: Boolean);
    procedure SetOfflineByRequest(const Value: Boolean);
    procedure WaitFor(hProcess: THandle; var Waiting, Cancelled: Boolean);  // I3349

  public
    function DialogID: Integer; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DoInstall(Silent, PromptForReboot: Boolean): Boolean;  // I3355   // I3500
    property ContinueSetup: Boolean read FContinueSetup write FContinueSetup;
    property Offline: Boolean read FOfflineByRequest write SetOfflineByRequest;
  end;

var
  frmRun: TfrmRun;

procedure CheckMSIResult(res: UINT);

implementation

uses
  Unicode, utilexecute,
  utilsystem, shlobj,
  OnlineConstants,
  TntDialogHelp,
  types, upload_settings,
  httpuploader,
  KeymanVersion,
  Keyman.System.UpdateCheckResponse,
  VersionInfo, GetOSVersion,
  SFX,
  bootstrapmain, jwawintype, jwamsi, ErrorControlledRegistry, RegistryKeys;

{$EXTERNALSYM MsiEnumRelatedProducts}

const
  msilib = 'msi.dll';
  {$IFDEF UNICODE}
  AWSuffix = 'W';
  {$ELSE}
  AWSuffix = 'A';
  {$ENDIF UNICODE}

var
  _MsiEnumRelatedProducts: Pointer;

{$WARNINGS OFF} // Because return value seems to be undefined due to JMP

function MsiEnumRelatedProducts(lpUpgradeCode: LPCTSTR; dwReserved: DWORD;
  iProductIndex: DWORD; lpProductBuf: LPTSTR): UINT; stdcall;  // I3326
begin
  GetProcedureAddress(_MsiEnumRelatedProducts, msilib, 'MsiEnumRelatedProducts' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_MsiEnumRelatedProducts]
  end;
end;

{$WARNINGS ON}

procedure TfrmRun.DownloadRedistFile(AOwner: TfrmDownloadProgress; var Result: Boolean);
begin
  Result := False;
  try
    with THTTPUploader.Create(AOwner) do
    try
      OnCheckCancel := AOwner.HTTPCheckCancel;
      OnStatus := AOwner.HTTPStatus;
      Request.Agent := API_UserAgent_Developer;
      //Request.Protocol := Upload_Protocol;
      //Request.HostName := Upload_Server;
      Request.SetURL(FDownloadURL);// UrlPath := URL;
      Upload;
      if Response.StatusCode = 200 then
      begin
        with TFileStream.Create(ExtPath + FDownloadFilename, fmCreate) do  // I3310
        try
          Write(Response.PMessageBody^, Response.MessageBodyLength);
        finally
          Free;
        end;
        Result := True;
      end
      else
        LogError(FInstallInfo.Text(ssErrorDownloadingUpdate, [Response.StatusCode]));
    finally
      Free;
    end;
  except
    on E:EHTTPUploader do
    begin
      if (E.ErrorCode = 12007) or (E.ErrorCode = 12029)
        then LogError(FInstallInfo.Text(ssErrorUnableToContactServer))
        else LogError(FInstallInfo.Text(ssErrorUnableToContactServerDetailed, [E.Message]));
      Result := False;
    end;
  end;
end;

procedure TfrmRun.WaitFor(hProcess: THandle; var Waiting, Cancelled: Boolean);  // I3349
var
  msg: TMsg;
begin
  case MsgWaitForMultipleObjects(1, hProcess, FALSE, INFINITE, QS_ALLINPUT) of
    WAIT_OBJECT_0 + 1:
      while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
      begin
        if not IsDialogMessage(Handle, Msg) then
        begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end;
      end;
    WAIT_OBJECT_0, WAIT_ABANDONED_0:
      Waiting := False;
  end;
end;

function TfrmRun.IsNewerVersionInstalled(const NewVersion: WideString): Boolean;
begin
  Result := (FInstalledVersion.Version <> '') and (CompareVersions(NewVersion, FInstalledVersion.Version) >= 0);
end;

function TfrmRun.InstallNewVersion: Boolean;
begin
  Result := False;
  if not FSilent and (MessageDlgW(FInstallInfo.Text(ssQueryUpdateVersion, [FNewVersion.InstallSize div 1024, FNewVersion.Version]),  // I2680
      mtConfirmation, mbOkCancel, 0) = mrCancel) then
    Exit;

  FDownloadURL := FNewVersion.InstallURL;  // I2680
  FDownloadFilename := FNewVersion.Filename;

  { Download the redistributable }
  with TfrmDownloadProgress.Create(Self) do
  try
    Callback := DownloadRedistFile;
    Result := ShowModal = mrOk;
  finally
    Free;
  end;
end;

procedure TfrmRun.CheckNewVersion;
var
  ucr: TUpdateCheckResponse;
begin
  with THTTPUploader.Create(nil) do
  try
    if FInstalledVersion.Version = ''
      then Fields.Add('version', AnsiString(FInstallInfo.Version))
      else Fields.Add('version', AnsiString(FInstalledVersion.Version));
    Fields.Add('tier', ansistring(CKeymanVersionInfo.Tier));
    Fields.Add('manual', '1');

    Request.HostName := API_Server;
    Request.Protocol := API_Protocol;
    Request.UrlPath := API_Path_UpdateCheck_Developer;

    Upload;
    if Response.StatusCode = 200 then
    begin
      if ucr.Parse(Response.MessageBodyAsString, 'developer', FInstallInfo.Version) then
      begin
        if ucr.Status = ucrsUpdateReady then
        begin
          FNewVersion.Version := ucr.NewVersion;
          FNewVersion.InstallURL := ucr.InstallURL;
          FNewVersion.InstallSize := ucr.InstallSize;
          FNewVersion.Filename := ExtractFileName(StringReplace(FNewVersion.InstallURL, '/', '\', [rfReplaceAll]));  // I1917
        end;
      end
      else
        raise Exception.Create(ucr.ErrorMessage);
    end
    else
      raise Exception.Create('Error '+IntToStr(Response.StatusCode));
  finally
    Free;
  end;
end;

procedure TfrmRun.SetupMSI;
var
  FHWnd: THandle;
begin
  FHWnd := Handle;
  MsiSetInternalUI(INSTALLUILEVEL_NONE, @FHWnd);
end;

procedure TfrmRun.Status(const Text: WideString = '');
var
  FMax: Integer;
begin
  if Text = '' then
  begin
    ShowWindow(GetDlgItem(Handle, IDC_PROGRESS1), SW_HIDE);
  end
  else
  begin
    ShowWindow(GetDlgItem(Handle, IDC_PROGRESS1), SW_SHOW);
    FMax := SendDlgItemMessage(Handle, IDC_PROGRESS1, PBM_GETRANGE, 0, 0);
    if StatusMax <> FMax then
    begin
      SendDlgItemMessage(Handle, IDC_PROGRESS1, PBM_SETPOS, 1, 0);
      SendDlgItemMessage(Handle, IDC_PROGRESS1, PBM_SETRANGE, 0, MAKELONG(0, StatusMax));
      SendDlgItemMessage(Handle, IDC_PROGRESS1, PBM_SETSTEP, 1, 0);
    end
    else
      SendDlgItemMessage(Handle, IDC_PROGRESS1, PBM_STEPIT, 0, 0);
  end;

  SetWindowText(GetDlgItem(Handle, IDC_STATUS), PWideChar(Text));
  UpdateWindow(GetDlgItem(Handle, IDC_STATUS));
  UpdateWindow(GetDlgItem(Handle, IDC_PROGRESS1));
  UpdateWindow(Handle);
end;

procedure TfrmRun.WMUserFormShown(var Message: TMessage);
begin
  if FContinueSetup then
    DoInstall(False, True);  // I3355   // I3500
end;

procedure TfrmRun.cmdInstallKeymanDesktopClick(Sender: TObject);
begin
  if DoInstall(False, True) then	// I1901 and I2680.4  // I3355   // I3500
    ModalResult := mrOk;
end;

constructor TfrmRun.Create(AOwner: TComponent);
begin
  inherited;
  FWhiteBrush := CreateSolidBrush(RGB(255,255,255));
end;

destructor TfrmRun.Destroy;
begin
  inherited;
  DeleteObject(FBitmap);
  DeleteObject(FLogo);
  FreeAndNil(FErrorLog);
end;

function TfrmRun.DialogID: Integer;
begin
  Result := 102;
end;

procedure TfrmRun.DlgMain(var Message: TMessage);
var
  r: TRect;
  FoldBitmap, hdc, FBitmapHDC: THandle;
begin
  case Message.Msg of
    WM_CTLCOLORDLG, WM_CTLCOLORSTATIC, WM_CTLCOLOREDIT:
      begin
        Message.Result := Integer(FWhiteBrush);
      end;
    WM_USER_FormShown:
      WMUserFormShown(Message);
    WM_ERASEBKGND:
      begin
        GetClientRect(Handle, r);
        hdc := THandle(Message.wParam);
        FillRect(hdc, r, (COLOR_WINDOW+1));
        GetWindowRect(GetDlgItem(Handle, IDC_IMAGE), r);
        ScreenToClient(Handle, r.TopLeft);
        ScreenToClient(Handle, r.BottomRight);

        FBitmapHDC := CreateCompatibleDC(hdc);
        try
          FOldBitmap := SelectObject(FBitmapHDC, FBitmap);
          BitBlt(hdc, r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top, FBitmapHDC, 0, 0, SRCCOPY);
          SelectObject(FBitmapHDC, FOldBitmap);

          GetWindowRect(GetDlgItem(Handle, IDC_LOGO), r);
          ScreenToClient(Handle, r.TopLeft);
          ScreenToClient(Handle, r.BottomRight);

          FOldBitmap := SelectObject(FBitmapHDC, FLogo);
          BitBlt(hdc, r.Left, r.Top, r.Right - r.Left, r.Bottom - r.Top, FBitmapHDC, 0, 0, SRCCOPY);
          SelectObject(FBitmapHDC, FOldBitmap);
        finally
          DeleteDC(FBitmapHDC);
        end;

        Message.Result := 1;
      end;
  else inherited;
  end;
end;

function TfrmRun.DoInstall(Silent, PromptForReboot: Boolean): Boolean;  // I1901  // I3355   // I3500
begin
  FPromptForReboot := PromptForReboot;  // I3355   // I3500
  FSilent := Silent;
  

  SetCursor(LoadCursor(0, IDC_WAIT));
  try
    EnableWindow(GetDlgItem(Handle, IDC_MESSAGE), False);
    EnableWindow(GetDlgItem(Handle, IDC_LOGOMESSAGE), False);
    EnableWindow(GetDlgItem(Handle, IDOK), False);
    EnableWindow(GetDlgItem(Handle, IDCANCEL), False);
    EnableWindow(GetDlgItem(Handle, IDC_CHECK1), False);

    StatusMax := 6;

    SetupMSI;

    CheckInstalledVersion;

    if SendDlgItemMessage(Handle, IDC_CHECK1, BM_GETCHECK, 0, 0) = BST_CHECKED then
    begin
      Status(FInstallInfo.Text(ssStatusCheckingForUpdates));
      try
        CheckNewVersion;
      except
        on E:Exception do // I1440 - avoid update check failure stopping install
        begin
          LogError(FInstallInfo.Text(ssCheckForUpdatesError, [E.Message]));
        end;
      end;
    end;
    Status(FInstallInfo.Text(ssStatusInstalling));
    Result := InstallMSI;  // I1901
  finally
    EnableWindow(GetDlgItem(Handle, IDC_MESSAGE), True);
    EnableWindow(GetDlgItem(Handle, IDC_LOGOMESSAGE), True);
    EnableWindow(GetDlgItem(Handle, IDOK), True);
    EnableWindow(GetDlgItem(Handle, IDCANCEL), True);
    EnableWindow(GetDlgItem(Handle, IDC_CHECK1), True);

    SendDlgItemMessage(Handle, IDOK, BM_SETSTYLE, BS_PUSHBUTTON, 1);
    SendMessage(Handle, DM_SETDEFID, IDCANCEL, 0);

    SetFocus(GetDlgItem(Handle, IDCANCEL));
    SetCursor(LoadCursor(0, IDC_ARROW));
    Status();
  end;
end;

procedure TfrmRun.cmdExitClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmRun.SetFontProperties(id, weight: Integer; ssSize, ssName: TInstallInfoText); // size, weight: Integer; const FontName: WideString);
var
  h: HWND;
  hFont: THandle;
  lf: LOGFONT;
  s: string{TNT-ALLOW string};
begin

  h := GetDlgItem(Handle, id);
  hFont := THandle(SendMessage(h, WM_GETFONT, 0, 0));
  GetObject(hFont, sizeof(lf), @lf);
  lf.lfWeight := weight;
  lf.lfHeight := StrToIntDef(FInstallInfo.Text(ssSize), StrToInt(FDefaultStrings[ssSize]));
  s := FInstallInfo.Text(ssName);
  if s <> '' then StrPCopy{TNT-ALLOW StrPCopy}(lf.lfFaceName, s);
  hFont := CreateFontIndirect(lf);
  SendMessage(h, WM_SETFONT, WPARAM(hFont), 0);
end;

procedure TfrmRun.SetOfflineByRequest(const Value: Boolean);
begin
  FOfflineByRequest := Value;

  if FOnline and not FOfflineByRequest
    then SendDlgItemMessage(Handle, IDC_CHECK1, BM_SETCHECK, BST_CHECKED, 0)
    else SendDlgItemMessage(Handle, IDC_CHECK1, BM_SETCHECK, BST_UNCHECKED, 0);
end;

procedure TfrmRun.FormCreate(Sender: TObject);
begin
  CheckInternetConnectedState;

  FBitmap := LoadBitmap(HInstance, MAKEINTRESOURCE(20));
  FLogo := LoadBitmap(HInstance, MAKEINTRESOURCE(21));

  SetWindowText(Handle, PWideChar(FInstallInfo.Text(ssApplicationTitle)));
  
  { Change the font size for the title }
  SetFontProperties(IDC_TITLE, FW_BOLD, ssFontSize_Title, ssFontName_Title);
  SetFontProperties(IDOK, FW_BOLD, ssFontSize_InstallButton, ssFontName_InstallButton); //e StrToIntDef(FInstallInfo.Text(ssFontSize_InstallButton), 18), FW_BOLD, FInstallInfo.Text(ssFontName_InstallButton));

  SetFontProperties(IDC_MESSAGE, FW_NORMAL, ssFontSize_Dialog, ssFontName_Dialog);
  SetFontProperties(IDC_LOGOMESSAGE, FW_NORMAL, ssFontSize_Dialog, ssFontName_Dialog);
  SetFontProperties(IDC_STATUS, FW_NORMAL, ssFontSize_Dialog, ssFontName_Dialog);
  SetFontProperties(IDC_CHECK1, FW_NORMAL, ssFontSize_Dialog, ssFontName_Dialog);
  SetFontProperties(IDCANCEL, FW_NORMAL, ssFontSize_Dialog, ssFontName_Dialog);

  SetWindowText(GetDlgItem(Handle, IDC_TITLE), PWideChar(FInstallInfo.Text(ssTitle, [FInstallInfo.EditionTitle])));
  SetWindowText(GetDlgItem(Handle, IDC_LOGOMESSAGE), PWideChar(FInstallInfo.Text(ssSIL, [FInstallInfo.EditionTitle])));
  SetWindowText(GetDlgItem(Handle, IDC_MESSAGE), PWideChar(FInstallInfo.Text(ssWelcome_Plain, [FInstallInfo.EditionTitle])));

  SetWindowText(GetDlgItem(Handle, IDC_CHECK1), PWideChar(FInstallInfo.Text(ssCheckForUpdates)));
  SetWindowText(GetDlgItem(Handle, IDOK), PWideChar(FInstallInfo.Text(ssInstallButton)));
  SetWindowText(GetDlgItem(Handle, IDCANCEL), PWideChar(FInstallInfo.Text(ssExitButton)));

  ShowWindow(GetDlgItem(Handle, IDC_STATUS), SW_HIDE);
  ShowWindow(GetDlgItem(Handle, IDC_PROGRESS1), SW_HIDE);
end;

procedure TfrmRun.CheckInternetConnectedState;
type
  TInternetGetConnectedState = function(lpdwFlags: LPDWORD; dwReserved: DWORD): BOOL; stdcall;
var
  hModule: THandle;
  FInternetGetConnectedState: TInternetGetConnectedState;
  flags: DWord;
begin
  FOnline := False;
  hModule := LoadLibrary('wininet.dll');
  if hModule <> 0 then
  begin
    FInternetGetConnectedState := GetProcAddress(hModule, 'InternetGetConnectedState');
    if Assigned(FInternetGetConnectedState) then
    begin
      FOnline := FInternetGetConnectedState(@flags, 0);
    end;
    FreeLibrary(hModule);
  end;

  if FOnline and not FOfflineByRequest
    then SendDlgItemMessage(Handle, IDC_CHECK1, BM_SETCHECK, BST_CHECKED, 0)
    else SendDlgItemMessage(Handle, IDC_CHECK1, BM_SETCHECK, BST_UNCHECKED, 0);
end;

procedure TfrmRun.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

procedure CheckMSIResult(res: UINT);
begin
  if res <> ERROR_SUCCESS then
    raise Exception.Create('Failed to install '+FInstallInfo.MSIFileName+': ['+IntToStr(res)+'] '+SysErrorMessage(res));
end;

function TfrmRun.CacheMSIFile(const SrcMSIFileName: WideString): WideString;
var
  path: WideString;
begin
  path := GetFolderPath(CSIDL_PROGRAM_FILES) + SFolder_CachedInstallerFiles+'\'+FInstallerVersion.ProductCode;

  if not SysUtils.ForceDirectories(path) then
    raise Exception.Create('Failed to cache installer MSI file (code 1,'+IntToStr(GetLastError)+'): '+SysErrorMessage(GetLastError));

  Result := path+'\'+ExtractFileName(SrcMSIFileName);

  if FileExists(PWideChar(Result)) then
  begin
    // Backup the existing cached installer until after the upgrade - it isn't needed for the upgrade because this is a full, not a patch upgrade
    RenameFile(Result, Result + '.1');
  end;

  if not CopyFile(PWideChar(SrcMSIFileName), PWideChar(Result), False) then
    raise Exception.Create('Failed to cache installer MSI file (code 2,'+IntToStr(GetLastError)+'): '+SysErrorMessage(GetLastError));
end;

procedure TfrmRun.FinishCacheMSIFile(const SrcMSIFileName: WideString; InstallSuccess: Boolean);
var
  newmsi, path: WideString;
begin
  path := GetFolderPath(CSIDL_PROGRAM_FILES) + SFolder_CachedInstallerFiles+'\'+FInstallerVersion.ProductCode;
  newmsi := path+'\'+ExtractFileName(SrcMSIFileName);

  if SysUtils.FileExists(newmsi+'.1') then
  begin
    if InstallSuccess then
      DeleteFile(newmsi+'.1')
    else
    begin
      DeleteFile(newmsi);
      RenameFile(newmsi+'.1', newmsi);
    end;
  end
  else if not InstallSuccess then
  begin
    DeleteFile(newmsi);
    RemoveDir(path);
    RemoveDir(ExtractFileDir(path)); // may fail if other products cached there, we don't care
    RemoveDir(ExtractFileDir(ExtractFileDir(path))); // may fail if other products installed there, we don't care
  end;
end;


procedure TfrmRun.CheckInstalledVersion;
var
  hProduct: MSIHANDLE;
  buf: array[0..64] of WideChar;
  sz: DWord;
  UpgradeCode: WideString;
  i: DWord;
begin
  FInstalledVersion.Version := '';
  CheckMSIResult(MsiOpenPackageExW(PWideChar(ExtPath + FInstallInfo.MSIFileName), MSIOPENPACKAGEFLAGS_IGNOREMACHINESTATE, hProduct));  // I3310
  try

    sz := 64;
    CheckMSIResult(MsiGetProductPropertyW(hProduct, 'UpgradeCode', buf, @sz));
    UpgradeCode := buf;
    sz := 64;
    CheckMSIResult(MsiGetProductPropertyW(hProduct, 'ProductCode', buf, @sz));
    FInstallerVersion.ProductCode := buf;
  finally
    MsiCloseHandle(hProduct);
  end;

  //FNewVersion.PackageCode := GetMSIPackageCode(ExtPath + FInstallInfo.MSIFileName);

  i := 0;
  if MsiEnumRelatedProducts(PWideChar(UpgradeCode), 0, i, buf) = ERROR_SUCCESS then
  begin
    sz := 64;
    FInstalledVersion.ProductCode := buf;
    CheckMSIResult(MsiGetProductInfoW(PWideChar(FInstalledVersion.ProductCode), INSTALLPROPERTY_VERSIONSTRING, buf, @sz));
    FInstalledVersion.Version := buf;
  end;
end;

procedure DoOutputDebugString(s: string);
begin
  OutputDebugString(PWideChar('KEYMANDEVELOPER.SETUP.EXE: '+s+#13#10));
end;

function TfrmRun.InstallMSI: Boolean;
var
  res: Cardinal;
  ReinstallMode: WideString;
  FCacheFileName: WideString;
begin
  Result := True;

  DoOutputDebugString('InstallMSI');
  DoOutputDebugString('  NewVersion.Version=<'+FNewVersion.Version+'>');
  DoOutputDebugString('  InstallInfo.Version=<'+FInstallInfo.Version+'>');
  { We need to check if the included installer or the patch available is a newer version than the currently installed version }
  if FNewVersion.Version <> '' then
  begin
    DoOutputDebugString('NewVersion.Version <> ''''');
    if IsNewerVersionInstalled(FNewVersion.Version) then
    begin
      DoOutputDebugString('  IsNewerVersionInstalled=True');
      Exit;
      { The patch is older than the installed version }
    end;

    if not InstallNewVersion then
    begin
      { We need to check if the included installer is a newer version than the currently installed version }
      FNewVersion.Filename := '';
      FNewVersion.Version := '';
    end;
  end;

  DoOutputDebugString('Testing IsNewerVersionInstalled('''+FInstallInfo.Version+''')');
  if not IsNewerVersionInstalled(FInstallInfo.Version) then
  begin
    DoOutputDebugString('not IsNewerVersionInstalled');
    ReinstallMode := 'REBOOTPROMPT=S REBOOT=ReallySuppress'; // I2754 - Auto update is too silent  // I3355   // I3500
    if (FInstalledVersion.Version <> '') and (FInstalledVersion.ProductCode = FInstallerVersion.ProductCode)
      then ReinstallMode := ReinstallMode + ' REINSTALLMODE=vdmus REINSTALL=ALL';  // I3355   // I3500   // I4858

    { Cache the msi file to avoid source dependencies for repair, patch, uninstall, bleagh }

    FCacheFileName := CacheMSIFile(ExtPath + FInstallInfo.MSIFileName);  // I3310

    if FSilent then
      MsiSetInternalUI(INSTALLUILEVEL_NONE, nil)
    else
      MsiSetInternalUI(INSTALLUILEVEL_FULL, nil);
    res := MsiInstallProductW(PWideChar(FCacheFileName), PWideChar('DISABLEFINALOPTIONS=1 '+ReinstallMode));

    FinishCacheMSIFile(ExtPath + FInstallInfo.MSIFileName,  // I3310
      (res = ERROR_SUCCESS_REBOOT_REQUIRED) or (res = ERROR_SUCCESS_REBOOT_INITIATED) or (res = ERROR_SUCCESS));

    case res of
      ERROR_SUCCESS: ;
      ERROR_SUCCESS_REBOOT_REQUIRED, ERROR_SUCCESS_REBOOT_INITIATED:
        begin
          PrepareForReboot(res); // We need to continue this install after reboot
          Result := False;
          Exit;
        end;
      ERROR_INSTALL_USEREXIT: begin Result := False; Exit; end;
    else
      CheckMSIResult(res);
    end;
  end;

  if not FSilent then  
    MsiSetInternalUI(INSTALLUILEVEL_FULL, nil);
  
  if FNewVersion.Filename <> '' then
  begin
    //Run the setup app silently  // I2680.5
    TUtilExecute.WaitForProcess('"'+ExtPath + FNewVersion.FileName+'" -s -o', ExtPath, SW_SHOW, WaitFor);  // I3349
    //if not FSilent then
    //  MsiSetInternalUI(INSTALLUILEVEL_BASIC, nil);
    //CheckMSIResult(MsiApplyPatchW(PWideChar(ExtPath + FNewVersion.Filename), nil, INSTALLTYPE_DEFAULT, ''));
  end;
end;

function TfrmRun.ProcessCommand(ID, NotificationCode: Integer;
  hControl: HWND): Boolean;
begin
  Result := True;
  case ID of
    IDOK: cmdInstallKeymanDesktopClick(Self);
    IDCANCEL: cmdExitClick(Self);
    else Result := False;
  end;
end;

procedure TfrmRun.lblTitleClick(Sender: TObject);
begin
  TUtilExecute.URL(MakeKeymanURL(URLPath_KeymanHome));  // I3349
end;

procedure TfrmRun.LogError(const msg: WideString; ShowDialogIfNotSilent: Boolean = True);
const
  nl: WideString = #13#10;
begin
  if not FSilent and ShowDialogIfNotSilent then
    ShowMessageW(msg);
  if not Assigned(FErrorLog) then
  begin
    if FileExists('setup.log') then
    begin
      FErrorLog := TFileStream.Create('setup.log', fmOpenReadWrite);
      FErrorLog.Seek(0, soFromEnd);
    end
    else
      FErrorLog := TFileStream.Create('setup.log', fmCreate);
  end;

  FErrorLog.Write(PWideChar(msg+nl)^, Length(msg+nl)*2);
end;

procedure TfrmRun.PrepareForReboot(res: Cardinal);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKey(SRegKey_WindowsRunOnce_CU, True) then
      WriteString(SRegValue_WindowsRunOnce_Setup, '"'+ParamStr(0)+'" -c');
  finally
    Free;
  end;

  if res = ERROR_SUCCESS_REBOOT_REQUIRED then
  begin
    if FPromptForReboot and (MessageDlgW(FInstallInfo.Text(ssQueryRestart), mtConfirmation, mbOkCancel, 0) = mrOk) then  // I3355   // I3500
    begin
      if not RestartWindows then
        LogError(FInstallInfo.Text(ssErrorUnableToAutomaticallyRestart));
    end
    else
      LogError(FInstallInfo.Text(ssMustRestart), False);
  end;
end;

type
  PTOKEN_PRIVILEGES = ^TOKEN_PRIVILEGES;
  {$EXTERNALSYM PTOKEN_PRIVILEGES}
  _TOKEN_PRIVILEGES = record
    PrivilegeCount: DWORD;
    Privileges: array [0..0] of LUID_AND_ATTRIBUTES;
  end;

  TAdjustTokenPrivileges = function (TokenHandle: THandle; DisableAllPrivileges: BOOL;
    const NewState: TTokenPrivileges; BufferLength: DWORD;
    PreviousState: PTokenPrivileges; var ReturnLength: DWORD): BOOL; stdcall;

  TOpenProcessToken = function (ProcessHandle: THandle; DesiredAccess: DWORD;
    var TokenHandle: THandle): BOOL; stdcall;

  TLookupPrivilegeValue = function (lpSystemName, lpName: PWideChar;
    var lpLuid: TLargeInteger): BOOL; stdcall;

function TfrmRun.RestartWindows: Boolean;
var
  hToken: THandle;
  tkp: TOKEN_PRIVILEGES;
  FAdjustTokenPrivileges: TAdjustTokenPrivileges;
  FOpenProcessToken: TOpenProcessToken;
  FLookupPrivilegeValue: TLookupPrivilegeValue;
  hAdvApi32: Cardinal;
  retlen: Cardinal;
const
  SE_SHUTDOWN_NAME: WideString = 'SeShutdownPrivilege';
  SHTDN_REASON_MAJOR_APPLICATION     = $00040000;
  SHTDN_REASON_MINOR_INSTALLATION    = $00000002;
  SHTDN_REASON_FLAG_PLANNED      = DWORD($80000000);
begin
  Result := False;

  hAdvApi32 := LoadLibrary('advapi32.dll');
  if hAdvApi32 = 0 then Exit;

  FOpenProcessToken := GetProcAddress(hAdvApi32, 'OpenProcessToken');
  if not Assigned(FOpenProcessToken) then Exit;

  FLookupPrivilegeValue := GetProcAddress(hAdvApi32, 'LookupPrivilegeValueW');
  if not Assigned(FLookupPrivilegeValue) then Exit;

  FAdjustTokenPrivileges := GetProcAddress(hAdvApi32, 'AdjustTokenPrivileges');
  if not Assigned(FAdjustTokenPrivileges) then Exit;

  // Get a token for this process.
  if not FOpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, hToken) then Exit;

  // Get the LUID for the shutdown privilege.
  if not FLookupPrivilegeValue(nil, PWideChar(SE_SHUTDOWN_NAME), tkp.Privileges[0].Luid) then Exit;

  tkp.PrivilegeCount := 1;  // one privilege to set
  tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;

  // Get the shutdown privilege for this process.

  FAdjustTokenPrivileges(hToken, False, tkp, 0, nil, retlen);
  if GetLastError() <> ERROR_SUCCESS then Exit;

  ExitWindowsEx(EWX_REBOOT, SHTDN_REASON_MAJOR_APPLICATION or SHTDN_REASON_MINOR_INSTALLATION or SHTDN_REASON_FLAG_PLANNED);
end;

end.
