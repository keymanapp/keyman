(*
  Name:             KeymanControl
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:      Manages the connection with keyman32.dll.  All Keyman 7 compliant
                    applications should be using this unit to control Keyman, not connecting
                    directly to keyman32.dll.
                    Note that StartKeyman32Engine and StopKeyman32Engine are usually called
                    by the Keyman Controller application rather than by client applications.
                    Client applications should usually call kmcomapi.Products[n].Start/Stop
  Create Date:      20 Jun 2006

  Modified Date:    25 Oct 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Add AutoApply functionality
                    14 Sep 2006 - mcdurdin - Store wm_keyman_refresh value
                    04 Dec 2006 - mcdurdin - Add StartKeyman32 (as distinct from LoadKeyman32)
                    04 Dec 2006 - mcdurdin - Add IsProductLoaded
                    04 Dec 2006 - mcdurdin - When RefreshKeyman is called, ensure refresh gets to all windows before app closes
                    12 Dec 2006 - mcdurdin - Disable and Enable user interface support
                    04 Jan 2007 - mcdurdin - Cleanup Keyman Engine shutdown - including PostMessage process
                    16 May 2007 - mcdurdin - Fixed FStartedKeyman32 to false on ShutdownKeyman32Engine
                    30 May 2007 - mcdurdin - I863 - Really free keyman32.dll when shutting down Keyman32 engine
                    19 Jun 2007 - mcdurdin - I819 - Restart Keyman Engine
                    13 Jul 2007 - mcdurdin - I910 - Flag why Keyman Engine is not starting
                    27 Mar 2008 - mcdurdin - I1267 - ActiveKeyboard returning wrong information when no keyboard active
                    27 Mar 2008 - mcdurdin - I1287 - Switch keyboard and language together
                    20 Jul 2008 - mcdurdin - I1412 - Keyman Engine starts multiple times in Tutorial
                    12 Mar 2010 - mcdurdin - I2230 - Resolve crashes due to incorrect reference counting
                    29 Jun 2010 - mcdurdin - I2435 - Send instead of Post KM_EXIT to get engine to exit more reliably
                    29 Jun 2010 - mcdurdin - I2435 - Post an unused KM_EXITFLUSH to trigger GetMessage hook to detach from all processes (as far as possible)
                    29 Jun 2010 - mcdurdin - I2435 - Use thread enumeration to post KM_EXITFLUSH for greater reliability
                    03 Oct 2011 - mcdurdin - I3092 - Keyman Engine does not restart nicely when shutdown uncleanly
                    08 Jun 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    24 Jan 2012 - mcdurdin - I3212 - Keyboards fail to switch from macros, when associated with a language
                    04 Nov 2012 - mcdurdin - I3541 - V9.0 - Merge of I3212 - Keyboards fail to switch from macros, when associated with a language
                    28 Nov 2012 - mcdurdin - I3598 - V9.0 - Debug_Keyman32Path was not always tested when loading keyman32.dll
                    03 Jul 2014 - mcdurdin - I4315 - V9.0 - Switching keyboards with kmcomapi needs to use TSF
                    17 Aug 2014 - mcdurdin - I4381 - V9.0 - Keyman keyboards should be removed from language bar when Keyman exits
                    28 Mar 2016 - mcdurdin - I5018 - Keyman fails to exit if a system compat flag gets set
                    25 Oct 2016 - mcdurdin - I5133 - Keyman.exe starts and exits because Control.StartKeyman32Engiene is passed a zero ProductID
                    25 Oct 2016 - mcdurdin - I5125 - Failure to start Keyman due to path errors

                    25 Oct 2016 - mcdurdin - I5134 - Crash starting Keyman due to 'IUnknown' interface not listed in Type Library

*)

unit keymancontrol;

interface

uses
  Messages, Windows, SysUtils, Classes, ComObj, ActiveX, keymanapi_TLB, internalinterfaces,
  customisationstorage,
  custinterfaces, keymancontext, keymanautoobject, StdVcl, contnrs, KeymanEngineControl;

type
  TKeyman32InitialiseFunction = function(Handle: HWND; FSingleApp: BOOL): BOOL; stdcall;
  TKeyman32ExitFunction = function: BOOL; stdcall;
  TKeyman32GetActiveKeymanIDFunction = function: DWORD; stdcall;
  TKeyman32GetLastFocusWindowFunction = function: HWND; stdcall;
  TKeyman32GetLastActiveWindowFunction = function: HWND; stdcall;
  TKeyman32RegisterMasterControllerFunction = function(Value: HWND): BOOL; stdcall;
  TKeyman32UnregisterMasterControllerFunction = function: BOOL; stdcall;
  TKeyman32RegisterControllerThreadFunction = function(Value: DWORD): BOOL; stdcall;
  TKeyman32UnregisterControllerThreadFunction = function(Value: DWORD): BOOL; stdcall;
  TKeyman32GetInitialisedFunction = function(var FSingleApp: BOOL): BOOL; stdcall;
  TKeyman32ControllerSendMessageFunction = function(msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
  TKeyman32ControllerPostMessageFunction = procedure(msg: UINT; wParam: WPARAM; lParam: LPARAM); stdcall;
  TKeyman32UpdateTouchPanelVisibilityFunction = procedure(Value: BOOL); stdcall;

  TKeymanControl = class(TKeymanAutoObject, IKeymanCustomisationAccess, IIntKeymanControl, IKeymanControl, IKeymanEngineControl)
  private
{$IFNDEF WIN64}
    hlibKeyman32: THandle;
    FStartedKeyman32, FMustExitKeyman32: Boolean;
    FKeyman_Initialise: TKeyman32InitialiseFunction;
    FKeyman_ResetInitialisation: TKeyman32ExitFunction;  // I3092
    FKeyman_Exit, FKeyman_RestartEngine: TKeyman32ExitFunction;
    FKeyman_GetActiveKeymanID: TKeyman32GetActiveKeymanIDFunction;
    FKeyman_GetLastFocusWindow: TKeyman32GetLastFocusWindowFunction;
    FKeyman_GetLastActiveWindow: TKeyman32GetLastActiveWindowFunction;
    FKeyman_RegisterMasterController: TKeyman32RegisterMasterControllerFunction;
    FKeyman_UnregisterMasterController: TKeyman32UnregisterMasterControllerFunction;
    FKeyman_RegisterControllerThread: TKeyman32RegisterControllerThreadFunction;
    FKeyman_UnregisterControllerThread: TKeyman32UnregisterControllerThreadFunction;
    FKeyman_GetInitialised: TKeyman32GetInitialisedFunction;
    FKeyman_SendMasterController: TKeyman32ControllerSendMessageFunction;
    FKeyman_PostMasterController: TKeyman32ControllerPostMessageFunction;
    FKeyman_StartExit: TKeyman32ExitFunction;  // I3092
    FKeyman_UpdateTouchPanelVisibility: TKeyman32UpdateTouchPanelVisibilityFunction;
    procedure LoadKeyman32;
    procedure StartKeyman32;
    procedure Do_Keyman_Exit;
    procedure Keyman32Bit_Destroy;
{$ENDIF}

  private
    wm_keyman_control: UINT;
    wm_kmselectlang: UINT;
    FAutoApply: Boolean;
    FKeymanCustomisation: IKeymanCustomisation;
    FLastRefreshToken: IntPtr;

    function RunKeymanConfiguration(const filename: string): Boolean;
    procedure ApplyToRunningKeymanEngine;
    function FindMasterControllerWindow: THandle;
  protected
    { IKeymanControl }
    function Get_ActiveLanguage: IKeymanLanguage; safecall;
    procedure Set_ActiveLanguage(const Value: IKeymanLanguage); safecall;

    function Get_LastActiveWindow: LongWord; safecall;    // 32 bit only
    function Get_LastFocusWindow: LongWord; safecall;    // 32 bit only

    function IsConfigurationOpen: WordBool; safecall;
    function IsKeymanRunning: WordBool; safecall;
    function IsOnlineUpdateCheckOpen: WordBool; safecall;
    function IsTextEditorOpen: WordBool; safecall;
    function IsVisualKeyboardOpen: WordBool; safecall;
    procedure OpenConfiguration; safecall;
    procedure OpenDiagnostics; safecall;
    procedure OpenHelp(const Topic: WideString); safecall;
    procedure OpenTextEditor; safecall;
    procedure OpenUpdateCheck; safecall;
    procedure ShowKeyboardWelcome(const Keyboard: IKeymanKeyboardInstalled);   safecall;
    procedure StartKeyman; safecall;
    procedure StartVisualKeyboard; safecall;
    procedure StopKeyman; safecall;
    procedure StopVisualKeyboard; safecall;

    { IKeymanEngineControl }
    procedure RestartEngine; safecall;    // 32 bit only
    procedure ShutdownKeyman32Engine; safecall;    // 32 bit only
    procedure StartKeyman32Engine; safecall;    // 32 bit only
    procedure StopKeyman32Engine; safecall;    // 32 bit only
    procedure ResetKeyman32Engine; safecall;    // 32 bit only
    procedure RegisterControllerWindow(Value: LongWord); safecall;     // deprecated in #5060
    procedure UnregisterControllerWindow(Value: LongWord); safecall;    // deprecated in #5060
    procedure DisableUserInterface; safecall;
    procedure EnableUserInterface; safecall;
    procedure UpdateTouchPanelVisibility(Value: Boolean); safecall;

    procedure DiagnosticTestException; safecall;

    function LastRefreshToken: IntPtr; safecall;

    // #5060:
    procedure RegisterMasterController(Value: LongWord); safecall;     // 32 bit only
    procedure UnregisterMasterController(Value: LongWord); safecall;    // 32 bit only
    procedure RegisterControllerThread(Value: LongWord); safecall;     // 32 bit only
    procedure UnregisterControllerThread(Value: LongWord); safecall;    // 32 bit only

    { IIntKeymanControl }
    procedure AutoApplyKeyman;
    procedure ApplyKeyman;
    function GetAutoApply: Boolean;
    procedure SetAutoApply(Value: Boolean);
    function CurrentUILanguage: string;
    procedure Refresh;

    { IKeymanCustomisationAccess }
    function KeymanCustomisation: IKeymanCustomisation; safecall;
  public
    constructor Create(AContext: TKeymanContext);
    destructor Destroy; override;
  end;

implementation

uses
  KeymanControlMessages,
  KeymanCustomisation,
  KeymanMutex,
  Winapi.msctf,
  keyman_msctf,
  tlhelp32, ErrorControlledRegistry, RegistryKeys, DebugPaths,
  ComServ,
  glossary,
  utilsystem,
  utilkeyman,
  utiltsf,
  KeymanPaths,
  keymanerrorcodes, psapi, Variants, KLog;

var
  wm_keyman: Integer = 0;

{$IFNDEF WIN64}
const
  SKeyman32Filename = 'keyman32.dll';
{$ENDIF}

const
  SWnd_MasterController = 'TfrmKeyman7Main';
  SWnd_VisualKeyboard = 'TfrmVisualKeyboard';
  SWnd_OnlineUpdateIcon = 'TfrmOnlineUpdateIcon';
  SWnd_OnlineUpdateNewVersion = 'TfrmOnlineUpdateNewVersion';

function TKeymanControl.GetAutoApply: Boolean;
begin
  Result := FAutoApply;
end;

function TKeymanControl.Get_ActiveLanguage: IKeymanLanguage;
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: keyman_msctf.ITfInputProcessorProfileMgr;
  i: Integer;
  profile: keyman_msctf.TF_INPUTPROCESSORPROFILE;
begin
  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');

  OleCheck(pInputProcessorProfileMgr.GetActiveProfile(GUID_TFCAT_TIP_KEYBOARD, profile));

//  if (profile.dwProfileType <> TF_PROFILETYPE_INPUTPROCESSOR) or not IsEqualGUID(profile.clsid, c_clsidKMTipTextService) then
//    Exit(nil);

  with (Context.Languages as IKeymanLanguages) do
    for i := 0 to Count-1 do
    begin
      case profile.dwProfileType of
        TF_PROFILETYPE_KEYBOARDLAYOUT:
          if IsEqualGUID(Items[i].ProfileGUID, GUID_NULL) and (profile.HKL = Items[i].HKL) then
            Exit(Items[i]);
        TF_PROFILETYPE_INPUTPROCESSOR:
          if IsEqualGUID(Items[i].ProfileGUID, profile.guidProfile) then
            Exit(Items[i]);
      end;
    end;

  Result := nil;
end;

procedure TKeymanControl.SetAutoApply(Value: Boolean);
begin
  FAutoApply := Value;
end;

procedure TKeymanControl.Set_ActiveLanguage(const Value: IKeymanLanguage);   // I4315
var
  pInputProcessorProfiles: ITfInputProcessorProfiles;
  pInputProcessorProfileMgr: ITfInputProcessorProfileMgr;
  ClassID: TGUID;
  ProfileGUID: TGUID;
begin
  OleCheck(CoCreateInstance(CLASS_TF_InputProcessorProfiles, nil, CLSCTX_INPROC_SERVER,
                          IID_ITfInputProcessorProfiles, pInputProcessorProfiles));

  if not Supports(pInputProcessorProfiles, IID_ITfInputProcessorProfileMgr, pInputProcessorProfileMgr) then   // I3743
    raise Exception.Create('Missing interface IID_ITfInputProcessorProfileMgr');

  if not Assigned(Value) then
    raise EOleException.Create('Invalid argument', E_INVALIDARG, '', '', 0);

  pInputProcessorProfiles.ChangeCurrentLanguage(Value.LangID);

  ClassID := Value.ClassID;
  ProfileGUID := Value.ProfileGUID;

  if IsEqualGUID(Value.ProfileGUID, GUID_NULL) then
    pInputProcessorProfileMgr.ActivateProfile(TF_PROFILETYPE_KEYBOARDLAYOUT, Value.LangID, ClassID, ProfileGUID,
      Value.LangID, TF_IPPMF_DONTCARECURRENTINPUTLANGUAGE)
  else
    pInputProcessorProfiles.ActivateLanguageProfile(ClassID, Value.LangID, ProfileGUID);
end;

function TKeymanControl.IsConfigurationOpen: WordBool;
begin
  with TKeymanMutex.Create('KeymanConfiguration') do
  try
    Result := MutexOwned;
  finally
    Free;
  end;
end;

function TKeymanControl.IsKeymanRunning: WordBool;
begin
  Result := (FindMasterControllerWindow <> 0);
end;

function TKeymanControl.IsOnlineUpdateCheckOpen: WordBool;
begin
  Result := (FindWindow(SWnd_OnlineUpdateIcon, nil) <> 0) or
    (FindWindow(SWnd_OnlineUpdateNewVersion, nil) <> 0);
end;

function TKeymanControl.IsTextEditorOpen: WordBool;
begin
  with TKeymanMutex.Create('KeymanTextEditor') do
  try
    Result := MutexOwned;
  finally
    Free;
  end;
end;

function TKeymanControl.IsVisualKeyboardOpen: WordBool;
begin
  Result := (FindWindow(SWnd_VisualKeyboard, nil) <> 0);
end;

function TKeymanControl.FindMasterControllerWindow: THandle;
begin
  Result := FindWindow(SWnd_MasterController, nil);
end;

function TKeymanControl.KeymanCustomisation: IKeymanCustomisation;
begin
  if not Assigned(FKeymanCustomisation) then
    FKeymanCustomisation := TCustomisationAutoObject.Create(Context, IKeymanUserInterface,
    TKeymanPaths.KeymanCustomisationPath);
  Result := FKeymanCustomisation;
end;

procedure TKeymanControl.StartVisualKeyboard;
var
  h: THandle;
begin
  h := FindMasterControllerWindow;
  if h <> 0 then
    PostMessage(h, wm_keyman_control, KMC_ONSCREENKEYBOARD, 1); // := ProcAddr('Keyman_PostMasterController');
  //(Context as TKeymanContext).Controller.ShowVisualKeyboard;
end;

procedure TKeymanControl.StopVisualKeyboard;
var
  h: THandle;
begin
  h := FindMasterControllerWindow;
  if h <> 0 then
    PostMessage(h, wm_keyman_control, KMC_ONSCREENKEYBOARD, 0); // := ProcAddr('Keyman_PostMasterController');
  //(Context as TKeymanContext).Controller.HideVisualKeyboard;
end;

procedure TKeymanControl.StopKeyman;
begin
  Context.Controller.StopKeyman;
end;

procedure TKeymanControl.AutoApplyKeyman;
begin
  if FAutoApply then
    ApplyToRunningKeymanEngine;
end;

procedure TKeymanControl.ApplyKeyman;
begin
  ApplyToRunningKeymanEngine;
end;

function TKeymanControl.LastRefreshToken: IntPtr;
begin
  Result := FLastRefreshToken;
end;

type
  TRefreshThread = class(TThread)
  private
    FLastRefreshToken: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(ALastRefreshToken: Integer);
  end;

procedure TKeymanControl.ApplyToRunningKeymanEngine;
begin
  FLastRefreshToken := Random(MaxInt);
  TRefreshThread.Create(FLastRefreshToken).Start;
end;

constructor TKeymanControl.Create(AContext: TKeymanContext);
begin
  inherited Create(AContext, IKeymanControl);
  wm_keyman_control := RegisterWindowMessage('WM_KEYMAN_CONTROL');
  wm_kmselectlang := RegisterWindowMessage('WM_KMSELECTLANG');
  FAutoApply := True;
end;

function TKeymanControl.CurrentUILanguage: string;
begin
  Result := KeymanCustomisation.CustMessages.LanguageCode;
end;

destructor TKeymanControl.Destroy;
begin
{$IFNDEF WIN64}
  Keyman32Bit_Destroy;
{$ENDIF}
  inherited Destroy;
end;

procedure TKeymanControl.OpenConfiguration;
begin
  RunKeymanConfiguration('-c');
end;

procedure TKeymanControl.OpenDiagnostics;
var
  msg, path: string;
begin
  path := TKeymanPaths.KeymanEngineInstallPath(TKeymanPaths.S_TSysInfoExe);
  ExecuteProgram('"'+path+'"', ExtractFileDir(path), msg);
end;

procedure TKeymanControl.OpenHelp(const Topic: WideString);
begin
  RunKeymanConfiguration('-h "'+Topic+'"');
end;

procedure TKeymanControl.OpenTextEditor;
begin
  RunKeymanConfiguration('-t');
end;

procedure TKeymanControl.OpenUpdateCheck;
begin
  RunKeymanConfiguration('-buc');
end;

procedure TKeymanControl.StartKeyman;
begin
  Context.Controller.StartKeyman;
end;

function TKeymanControl.RunKeymanConfiguration(const filename: string): boolean;
var
  err: string;
begin
  Result := ExecuteProgram('"'+TKeymanPaths.KeymanDesktopInstallPath(TKeymanPaths.S_KMShell)+'" '+filename,
    TKeymanPaths.KeymanDesktopInstallDir, err);
end;

procedure TKeymanControl.Refresh;
begin
  if Assigned(FKeymanCustomisation) then
    FKeymanCustomisation.Refresh;
end;

procedure TKeymanControl.DiagnosticTestException;
begin
  raise Exception.Create('Testing safecall wrappering of exception for Sentry');
end;

procedure TKeymanControl.DisableUserInterface;
const
  KM_DISABLEUI = 1;
begin
  if wm_keyman = 0 then
    wm_keyman := RegisterWindowMessage('wm_keyman');
  PostMessage(HWND_BROADCAST, wm_keyman, KM_DISABLEUI, 0);
end;

procedure TKeymanControl.EnableUserInterface;
const
  KM_ENABLEUI = 2;
begin
  if wm_keyman = 0 then
    wm_keyman := RegisterWindowMessage('wm_keyman');
  PostMessage(HWND_BROADCAST, wm_keyman, KM_ENABLEUI, 0);
end;

procedure TKeymanControl.ShowKeyboardWelcome(const Keyboard: IKeymanKeyboardInstalled);
var
  pkg: IKeymanPackageInstalled;
begin
  pkg := Keyboard.OwnerPackage;
  if Assigned(pkg) then
    RunKeymanConfiguration('-kw "'+pkg.ID+'"');
end;


{**
  Control of startup and shutdown of keyman32.dll -- implemented only by 32 bit;
  generally these functions are intended to be used only by keyman.exe.
*}

procedure TKeymanControl.StartKeyman32Engine;   // I5133
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  StartKeyman32;
{$ENDIF}
end;

procedure TKeymanControl.StopKeyman32Engine;   // I5133
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ENDIF}
end;

function TKeymanControl.Get_LastActiveWindow: LongWord;
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  Result := FKeyman_GetLastActiveWindow;
{$ENDIF}
end;

function TKeymanControl.Get_LastFocusWindow: LongWord;
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  Result := FKeyman_GetLastFocusWindow;
{$ENDIF}
end;

procedure TKeymanControl.RegisterControllerThread(Value: LongWord);
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  if not FKeyman_RegisterControllerThread(Value) then
    Error(KMN_E_KeymanControl_CannotRegisterControllerWindow);
{$ENDIF}
end;

procedure TKeymanControl.RegisterControllerWindow(Value: LongWord);
begin
  // no-op (removed as part of #5060)
end;

procedure TKeymanControl.RegisterMasterController(Value: LongWord);
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  if not FKeyman_RegisterMasterController(Value) then
    Error(KMN_E_KeymanControl_CannotRegisterControllerWindow);
{$ENDIF}
end;

procedure TKeymanControl.ResetKeyman32Engine;   // I5133
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  FKeyman_ResetInitialisation;
{$ENDIF}
end;

procedure TKeymanControl.RestartEngine;
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  if not FKeyman_RestartEngine then
    ErrorFmt(KMN_E_KeymanControl_CannotLoadKeyman32, VarArrayOf([Integer(GetLastError), 'Failed to restart Keyman Engine: '+SysErrorMessage(GetLastError)]));
{$ENDIF}
end;

procedure TKeymanControl.UnregisterControllerThread(Value: LongWord);
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  if hlibKeyman32 = 0 then
    KL.LogError('UnregisterControllerThread: keyman32.dll is not loaded')
  else if not FKeyman_UnregisterControllerThread(Value) then
    KL.LogError('UnregisterControllerThread: Could not unregister controller thread %x', [Value]);
{$ENDIF}
end;

procedure TKeymanControl.UnregisterControllerWindow(Value: LongWord);
begin
  // no-op (removed as part of #5060)
end;

procedure TKeymanControl.UnregisterMasterController(Value: LongWord);
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  if hlibKeyman32 = 0 then
    KL.LogError('UnregisterMasterController: keyman32.dll is not loaded')
  else if not FKeyman_UnregisterMasterController then
    KL.LogError('UnregisterMasterController: Could not unregister controller window %x', [Value]);
{$ENDIF}
end;

procedure TKeymanControl.UpdateTouchPanelVisibility(Value: Boolean);
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  LoadKeyman32;
  FKeyman_UpdateTouchPanelVisibility(Value);
{$ENDIF}
end;

procedure TKeymanControl.ShutdownKeyman32Engine;
begin
{$IFDEF WIN64}
  Error(Cardinal(E_NOTIMPL));
{$ELSE}
  KL.MethodEnter(Self, 'ShutdownKeyman32Engine', [FMustExitKeyman32]);
  if FMustExitKeyman32 then
  begin
    StopKeyman32Engine;   // I5133
    Do_Keyman_Exit;
    if hlibKeyman32 <> 0 then   // I5018
    begin
      FreeLibrary(hlibKeyman32);   // I5018
      hlibKeyman32 := 0;
    end;
    FMustExitKeyman32 := False;
    FStartedKeyman32 := False;
  end;
  KL.MethodExit(Self, 'ShutdownKeyman32Engine');
{$ENDIF}
end;

{**
  32-bit helper functions for initialising keyman32.dll.
*}

{$IFNDEF WIN64}
procedure TKeymanControl.Keyman32Bit_Destroy;
begin
  if hlibKeyman32 <> 0 then
  begin
    StopKeyman32Engine;   // I5133
    if FMustExitKeyman32 then
      Do_Keyman_Exit;
  end;
end;

procedure TKeymanControl.Do_Keyman_Exit;
var
  hSnap: THandle;
  te: THREADENTRY32;
const
  KM_EXITFLUSH = 8;
begin

  if wm_keyman = 0 then
    wm_keyman := RegisterWindowMessage('wm_keyman');

  FKeyman_StartExit; // I3092

  { Tell Keyman to shut down its hooks }

  FKeyman_Exit;

  { Keyman32 can stay attached because the GetMessage hook is not cleared from the thread
    until a message is posted to the thread message queue.  So enumerate all threads and
    post a flush message to them to force keyman32 to detach.  This should work because
    we have elevated UIPI.  Threads without a message queue will fail but they will not
    have an attached getmessage so it won't matter }

  hSnap := CreateToolhelp32Snapshot(TH32CS_SNAPTHREAD, 0);
  if hSnap = INVALID_HANDLE_VALUE then RaiseLastOSError;
  FillChar(te, sizeof(te), 0);  // I3310! was buggy in v8 but probably didn't matter
  te.dwSize := sizeof(te);

  if Thread32First(hSnap, te) then
  begin
    repeat
      PostThreadMessage(te.th32ThreadID, wm_keyman, KM_EXITFLUSH, 0);
    until not Thread32Next(hSnap, &te);
  end;
  CloseHandle(hSnap);

end;

procedure TKeymanControl.LoadKeyman32;

    function GetKeymanInstallPath: string;   // I3598
    var
      buf: array[0..260] of char;
      RootPath: string;
    begin
      RootPath := '';
      with TRegistryErrorControlled.Create do  // I2890
      try
        RootKey := HKEY_LOCAL_MACHINE;
        if OpenKeyReadOnly(SRegKey_KeymanEngine_LM) and ValueExists(SRegValue_RootPath) then
            RootPath := ReadString(SRegValue_RootPath);
      finally
        Free;
      end;

      RootPath := GetDebugPath('Debug_Keyman32Path', RootPath);  // I2825

      if RootPath = '' then
      begin
        GetModuleFileName(HInstance, buf, 260);
        RootPath := ExtractFilePath(buf);
      end;

      Result := IncludeTrailingPathDelimiter(RootPath);
    end;

    function ProcAddr(const Name: string): FARPROC;
    begin
      Result := GetProcAddress(hlibKeyman32, PChar(Name));
      if not Assigned(Result) then
        ErrorFmt(KMN_E_KeymanControl_CannotLoadKeyman32, VarArrayOf([Integer(GetLastError), 'Failed to GetProcAddress for "'+Name+'", '+SysErrorMessage(GetLastError)]));
    end;
var
  s: string;
begin
  if hlibKeyman32 = 0 then
  begin

    s := GetKeymanInstallPath+SKeyman32Filename;   // I3598
    if not FileExists(s) then
      ErrorFmt(KMN_E_KeymanControl_CannotLoadKeyman32, VarArrayOf([Integer(GetLastError), 'Failed to find '+SKeyman32Filename+' at "'+s+'", '+SysErrorMessage(GetLastError)]));

    hlibKeyman32 := LoadLibrary(PChar(s));
    if hlibKeyman32 = 0 then
      ErrorFmt(KMN_E_KeymanControl_CannotLoadKeyman32, VarArrayOf([Integer(GetLastError), 'Failed to LoadLibrary for "'+s+'", '+SysErrorMessage(GetLastError)]));

    @FKeyman_GetInitialised      := ProcAddr('Keyman_GetInitialised');
    @FKeyman_Initialise          := ProcAddr('Keyman_Initialise');
    @FKeyman_ResetInitialisation := ProcAddr('Keyman_ResetInitialisation');  // I3092
    @FKeyman_Exit                := ProcAddr('Keyman_Exit');
    @FKeyman_StartExit           := ProcAddr('Keyman_StartExit');  // I3092
    @FKeyman_RestartEngine       := ProcAddr('Keyman_RestartEngine');
    @FKeyman_GetActiveKeymanID   := ProcAddr('GetActiveKeymanID');
    @FKeyman_GetLastFocusWindow  := ProcAddr('Keyman_GetLastFocusWindow');
    @FKeyman_GetLastActiveWindow := ProcAddr('Keyman_GetLastActiveWindow');
    @FKeyman_RegisterMasterController := ProcAddr('Keyman_RegisterMasterController');
    @FKeyman_UnregisterMasterController := ProcAddr('Keyman_UnregisterMasterController');
    @FKeyman_RegisterControllerThread := ProcAddr('Keyman_RegisterControllerThread');
    @FKeyman_UnregisterControllerThread := ProcAddr('Keyman_UnregisterControllerThread');
    @FKeyman_SendMasterController := ProcAddr('Keyman_SendMasterController');
    @FKeyman_PostMasterController := ProcAddr('Keyman_PostMasterController');
    @FKeyman_UpdateTouchPanelVisibility := ProcAddr('Keyman_UpdateTouchPanelVisibility');
  end;
end;

procedure TKeymanControl.StartKeyman32;
var
  FSingleApp: BOOL;
begin
  KL.MethodEnter(Self, 'StartKeyman32', []);
  try
    if FStartedKeyman32 then Exit;

    if FKeyman_GetInitialised(FSingleApp) then FMustExitKeyman32 := False
    else
      {TODO: Init: pass a handover window handle - so that Keyman can PostMessage it when shutting down from another place }
      if FKeyman_Initialise(0,False) then FMustExitKeyman32 := True
      else ErrorFmt(KMN_E_KeymanControl_CannotLoadKeyman32, VarArrayOf([Integer(GetLastError), 'Failed to Keyman_Initialise, '+SysErrorMessage(Integer(GetLastError))]));

    FStartedKeyman32 := True;
  finally
    KL.MethodExit(Self, 'StartKeyman32');
  end;
end;
{$ENDIF}

{ TRefreshThread}

constructor TRefreshThread.Create(ALastRefreshToken: Integer);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FLastRefreshToken := ALastRefreshToken;
end;

procedure TRefreshThread.Execute;
const
  KR_REQUEST_REFRESH = 0;
  KR_SETTINGS_CHANGED = 3;
var
  msg: TMsg;
  wm_keyman_refresh: UINT;
  RefreshHandle: THandle;
begin
  // This convoluted way of refreshing keyman ensures that km is init for the thread.
  // Other methods would work but this is easiest

  wm_keyman_refresh := RegisterWindowMessage('WM_KEYMANREFRESH');
  RefreshHandle := AllocateHWnd(nil);

  // We currently have two announcements because the KR_REQUEST_REFRESH
  // announcement is actioned only when Keyman Engine is running, while we
  // still need to tell any apps listening for changes that the settings
  // have changed. In future, we should probably refactor this to use a
  // single broadcast.

  // We use a random number here to so that multiple processes can generate
  // hopefully unique tokens for refresh, and we share this token with our
  // current consumer so they can ignore notifications that they have
  // generated

  // First, post out a KR_SETTINGS_CHANGED for the benefit of Keyman
  // Configuration and other apps that want it
  PostMessage(HWND_BROADCAST, wm_keyman_refresh, KR_SETTINGS_CHANGED, FLastRefreshToken);

  // Then, post out a refresh to Keyman Engine and process it in this
  // thread so that Keyman Engine will grab it and broadcast it, if it is
  // currently running
  PostMessage(RefreshHandle, wm_keyman_refresh, KR_REQUEST_REFRESH, 0);

  // Flush the queue but don't stall the thread
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
  begin
    DispatchMessage(msg);
  end;
  DeallocateHWnd(RefreshHandle);
end;

end.
