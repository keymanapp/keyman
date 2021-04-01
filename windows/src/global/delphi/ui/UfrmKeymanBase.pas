(*
  Name:             UfrmKeymanBase
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    2 Jun 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    14 Sep 2006 - mcdurdin - Base forms on TTntForm
                    04 Jan 2007 - mcdurdin - Add help support
                    27 Mar 2008 - mcdurdin - Relocated
                    28 Feb 2011 - mcdurdin - I2720 - Prevent Keyman splash from showing multiple copies
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    02 Jun 2015 - mcdurdin - I4565 - CrashID:kmshell.exe_9.0.476.0_2C469740_ERegistryException
*)
unit UfrmKeymanBase;  // I3306

interface

uses
  System.Classes,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Winapi.Messages,
  Winapi.Windows,

  UserMessages;

type
  TfrmKeymanBase = class(TForm)
    function TntFormHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure TntFormShow(Sender: TObject);
    procedure TntFormCreate(Sender: TObject);
    procedure TntFormDestroy(Sender: TObject);
  private
    FHelpTopic: string;
    procedure RegisterWindow;  // I2720
    procedure DeregisterWindow;  // I2720
    class function RegisteredName: string;  // I2720
  protected
    class function ShouldRegisterWindow: Boolean; virtual;  // I2720
    procedure CreateHandle; override;  // I2720
  public
    class function GetRegisteredHandle: HWND;  // I2720
    property HelpTopic: string read FHelpTopic write FHelpTopic;
  end;

implementation

uses
  System.Win.Registry,

  ErrorControlledRegistry,
  Keyman.System.KeymanSentryClient,
  RegistryKeys;

{$R *.DFM}

procedure TfrmKeymanBase.TntFormCreate(Sender: TObject);
begin
  TKeymanSentryClient.Breadcrumb('user', 'Created form '+ClassName, 'form-create');
  RegisterWindow;  // I2720
end;

procedure TfrmKeymanBase.TntFormDestroy(Sender: TObject);
begin
  DeregisterWindow;  // I2720
  TKeymanSentryClient.Breadcrumb('user', 'Destroyed form '+ClassName, 'form-destroy');
end;

function TfrmKeymanBase.TntFormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean): Boolean;
begin
  Result := True;
  if Command in [HELP_CONTEXT, HELP_CONTEXTPOPUP] then
  begin
    CallHelp := False;
    Application.HelpJump(FHelpTopic);
  end;
end;

procedure TfrmKeymanBase.TntFormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

var  // I2720
  FAppName: string = '';
function AppName: string;
begin
  if FAppName = '' then
    FAppName := ChangeFileExt(ExtractFileName(Application.ExeName),'');
  Result := FAppName;
end;

class function TfrmKeymanBase.RegisteredName: string;  // I2720
begin
  Result := AppName + ':' + ClassName;
end;

procedure TfrmKeymanBase.CreateHandle;
begin
  inherited;
  RegisterWindow;  // I2720
end;

procedure TfrmKeymanBase.DeregisterWindow;  // I2720
begin
  if ShouldRegisterWindow then
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      try   // I4565
        if OpenKey(SRegKey_KeymanRegisteredWindows_CU, True) then
          if ValueExists(RegisteredName) then
            DeleteValue(RegisteredName);
      except
        on E:ERegistryException do ;  // Minor issue   // I4565
      end;
    finally
      Free;
    end;
  end;
end;

class function TfrmKeymanBase.GetRegisteredHandle: HWND;  // I2720
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly(SRegKey_KeymanRegisteredWindows_CU) and ValueExists(RegisteredName)
      then Result := ReadInteger(RegisteredName)
      else Result := 0;
  finally
    Free;
  end;
end;

procedure TfrmKeymanBase.RegisterWindow;  // I2720
begin
  if ShouldRegisterWindow then
  begin
    with TRegistryErrorControlled.Create do  // I2890
    try
      try   // I4565
        if OpenKey(SRegKey_KeymanRegisteredWindows_CU, True) then
          WriteInteger(RegisteredName, Handle);
      except
        on E:ERegistryException do ;  // Minor issue   // I4565
      end;
    finally
      Free;
    end;
  end;
end;

class function TfrmKeymanBase.ShouldRegisterWindow: Boolean;  // I2720
begin
  Result := False;
end;


end.

