(*
  Name:             UfrmSplash
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      20 Jun 2006

  Modified Date:    15 Apr 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          20 Jun 2006 - mcdurdin - Initial version
                    01 Aug 2006 - mcdurdin - Keyman 7 splash
                    14 Sep 2006 - mcdurdin - Use custom splash from pxx
                    12 Dec 2006 - mcdurdin - Rework as XSL
                    04 Jan 2007 - mcdurdin - Add proxy support
                    30 May 2007 - mcdurdin - I825 - Added proxy username and password
                    07 Nov 2007 - mcdurdin - I922, I1100 - Work around crash when starting Keyman Desktop on Vista with security software blocking it
                    27 Mar 2008 - mcdurdin - Use TfrmKeymanBase
                    14 Jun 2008 - mcdurdin - I1400 - Check language settings on startup
                    16 Jan 2009 - mcdurdin - I1730 - Online update of keyboards
                    30 Jan 2009 - mcdurdin - I1818 - Don't show configuration when starting Keyman when it is already running
                    05 Nov 2010 - mcdurdin - Fix crash starting Keyman Desktop
                    30 Dec 2010 - mcdurdin - I2562 - Clean up installation and show splash after upgrade
                    11 Jan 2011 - mcdurdin - I2643 - Use Always-On-Top for Keyman startup until it activates so it is always visible to the user
                    28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman Desktop splash from showing multiple copies
                    18 Mar 2011 - mcdurdin - I2392 - Activation server integration
                    18 Mar 2011 - mcdurdin - I2786 - Application title
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    28 May 2014 - mcdurdin - I4222 - V9.0 - Deprecate osWin2000, osWinXP, osWin2003Server
                    03 Jul 2014 - mcdurdin - I3730 - V9.0 - I3710 fail - script error dialog appears behind splash dialog
                    03 Aug 2014 - mcdurdin - I4356 - V9.0 - If splash screen is minimized, it cannot be restored
                    01 Sep 2014 - mcdurdin - I4393 - V9.0 - Keyman Desktop Free Edition polish
                    01 Sep 2014 - mcdurdin - I4396 - V9.0 - When configuration run from Splash and license key entered, splash doesn't refresh
                    15 Apr 2015 - mcdurdin - I4658 - V9.0 - Add Keep in Touch screen
*)
unit UfrmSplash;  // I3306

interface

uses
  System.Contnrs,
  System.UITypes,
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, UfrmKeymanBase, keymanapi_TLB, jpeg,
  UfrmWebContainer, OleCtrls, SHDocVw, EmbeddedWB, SHDocVw_EWB,
  UserMessages,
  EwbCore, KeymanEmbeddedWB, Vcl.AppEvnts;

type
  TfrmSplash = class(TfrmWebContainer)
    ApplicationEvents1: TApplicationEvents;
    procedure TntFormCreate(Sender: TObject);
    procedure Command_Start;
    procedure Command_Exit;
    procedure TntFormShow(Sender: TObject);
    procedure TntFormActivate(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
  private
    FShouldDisplay: Boolean;
    FShowConfigurationOnLoad: Boolean;
    procedure WMUser_FormShown(var Message: TMessage); message WM_USER_FormShown;
    procedure WMUser(var Message: TMessage); message WM_USER;
  protected
    procedure FireCommand(const command: WideString; params: TStringList);
      override;

    class function ShouldRegisterWindow: Boolean; override; // I2720
    function ShouldSetAppTitle: Boolean; override;  // I2786
  public
    procedure Do_Content_Render(FRefreshKeyman: Boolean); override;
    property ShowConfigurationOnLoad: Boolean read FShowConfigurationOnLoad write FShowConfigurationOnLoad;
  end;

procedure StartKeyman(ForceSplash, Silent, StartWithConfiguration: Boolean);  // I2562
procedure ShowSplash;

implementation

uses
  ComObj,
  custinterfaces,
  GenericXMLRenderer,
  GetOSVersion,
  initprog,
  KeyboardListXMLRenderer,
  KeymanControlMessages,
  KeymanOptionNames,
  kmcomapi_errors,
  kmint,
  MessageIdentifierConsts,
  MessageIdentifiers,
  KeymanMutex,
  OnlineUpdateCheck,
  PngImage,
  ErrorControlledRegistry,
  RegistryKeys,
  UfrmKeepInTouch,
  UfrmTextEditor,
  Upload_Settings,
  utilexecute,
  utilfocusappwnd,
  utilkmshell,
  utilxml,
  VersionInfo, ActiveX, MSHTML_TLB;

{$R *.DFM}

procedure ShowSplash;
begin
  with TfrmSplash.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmSplash.TntFormActivate(Sender: TObject);
begin
  inherited;
  if FormStyle = fsStayOnTop then  // I2643
    FormStyle := fsNormal;
end;

procedure TfrmSplash.TntFormCreate(Sender: TObject);
begin
  inherited;
  FShouldDisplay := True;
  Position := poScreenCenter;
end;

procedure TfrmSplash.TntFormShow(Sender: TObject);
begin
  Do_Content_Render(False);
  inherited;
end;

function EscapeString(const str: WideString): WideString;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(str) do
  begin
    case str[i] of
      '"':     Result := Result + '\"';
      #0..#31: Result := Result + '\x'+IntToHex(Ord(str[i]), 2);
      '\':     Result := Result + '\\';
      else     Result := Result + str[i];
    end;
  end;
end;

procedure TfrmSplash.Do_Content_Render(FRefreshKeyman: Boolean);
var
  xml: WideString;
begin
  xml := '';

  xml := xml + '<Version>'+xmlencode(MsgFromIdFormat(SKSplashVersion, [GetVersionString]))+'</Version>';
//  xml := xml + '<Keyboards)Count>'+IntToStr(kmcom.Keyboards.Count)+'</KeyboardCount>';

  XMLRenderers.RenderTemplate := 'Splash.xsl';
  XMLRenderers.Clear;
  XMLRenderers.Add(TGenericXMLRenderer.Create(xml));
  XMLRenderers.Add(TKeyboardListXMLRenderer.Create);

  Content_Render;
end;

procedure TfrmSplash.WMUser(var Message: TMessage);
begin
  Do_Content_Render(False);
end;

procedure TfrmSplash.WMUser_FormShown(var Message: TMessage);
begin
  if (GetForegroundWindow <> Handle) and
    (GetWindowThreadProcessId(GetForegroundWindow) <> GetCurrentThreadId) then   // I3730
  begin
    BringToFront;
    SetFocus;
    FormStyle := fsStayOnTop; // I2643
  end;

  inherited;

  if FShowConfigurationOnLoad then
  begin
    Main(Self);
    Do_Content_Render(True);
  end;
end;

procedure TfrmSplash.FireCommand(const command: WideString; params: TStringList);
begin
  if command = 'start' then Command_Start
  else if command = 'config' then begin Main(Self); Do_Content_Render(True); end   // I4393   // I4396
  else if command = 'hidesplash' then FShouldDisplay := False
  else if command = 'showsplash' then FShouldDisplay := True
  else if command = 'exit' then Command_Exit
  else inherited;
end;

class function TfrmSplash.ShouldRegisterWindow: Boolean; // I2720
begin
  Result := True;
end;

function TfrmSplash.ShouldSetAppTitle: Boolean;  // I2786
begin
  Result := True;
end;

procedure TfrmSplash.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);   // I4356
begin
  if (Msg.message = WM_SYSCOMMAND) and (Msg.wParam = SC_RESTORE) then
  begin
    // Handle the case where Win+M pressed, window never restores
    // Only really happens with Splash.
    PostMessage(Handle, WM_SHOWWINDOW, 1, SW_PARENTOPENING);
  end;
end;

procedure TfrmSplash.Command_Start;
begin
  ModalResult := mrOk;
  if kmcom.Options[KeymanOptionName(TUtilKeymanOption.koShowStartup)].Value <> FShouldDisplay then
  begin
    kmcom.Options[KeymanOptionName(TUtilKeymanOption.koShowStartup)].Value := FShouldDisplay;
    kmcom.Options.Apply;
  end;
end;

procedure TfrmSplash.Command_Exit;
begin
  ModalResult := mrCancel;
end;

procedure StartKeyman(ForceSplash, Silent, StartWithConfiguration: Boolean);  // I2562
var
  hKeymanControl: THandle;
  FMutex: TKeymanMutex;
  function HasLoadedKeyboards: Boolean;
  var
    i: Integer;
  begin
    for i := 0 to kmcom.Keyboards.Count - 1 do
      if kmcom.Keyboards[i].Loaded then
        Exit(True);
    Result := False;
  end;
begin
  if kmcom.Control.IsKeymanRunning then   // I1818 - don't show Configuration if Keyman is already running
  begin
    hKeymanControl := FindWindow('TApplication', 'keyman');
    if hKeymanControl <> 0 then
      PostMessage(hKeymanControl, RegisterWindowMessage('WM_KEYMAN_CONTROL'), MAKELONG(KMC_NOTIFYWELCOME, NW_SENDBALLOON), NWB_IDENTIFYICON);

//      if Value[KeymanDesktopOptions.koShowWelcome] then
//        Splash(nil, False);
  end
  else
  begin
    FMutex := TKeymanMutex.Create('KeymanSplash');
    try
      if not FMutex.MutexOwned then
      begin
        FocusSplash;  // I2562
        Exit;
      end;

      if (not Silent and kmcom.Options[KeymanOptionName(TUtilKeymanOption.koShowStartup)].Value) or ForceSplash then
      begin
        with TfrmSplash.Create(nil) do
        try
          ShowConfigurationOnLoad := StartWithConfiguration;
          if ShowModal = mrCancel then
            Exit;
        finally
          Free;
        end;
      end;
    finally
      FMutex.Free;
    end;

    repeat
      try
        kmcom.Control.StartKeyman;
        Break;
      except
        on E:EOleException do
        begin
          if kmcom.Errors.Count > 0 then
          begin
            if Cardinal(kmcom.Errors[0].ErrorCode) = KMN_E_KeymanControl_CannotStartProduct then
            begin
              case MessageDlg(MsgFromIdFormat(SKCannotStartProduct, [kmcom.Errors[0].Description]), mtError,
                  mbYesNoCancel, 0) of
                mrYes: ;
                mrNo, mrCancel: Exit;
              end;
            end;
          end
          else raise;
        end;
      end;

    until False;

    if kmcom.SystemInfo.RebootRequired then
      RunReboot('Windows must be restarted for changes to complete.  Restart now?',
        'Windows did not initiate the restart successfully.  You will need to restart manually.');

    if kmcom.Options[KeymanOptionName(TUtilKeymanOption.koCheckForUpdates)].Value then
    begin
      if not kmcom.Control.IsOnlineUpdateCheckOpen then
        RunConfiguration(0, '-ouc -s');
    end;
  end;
end;

end.

