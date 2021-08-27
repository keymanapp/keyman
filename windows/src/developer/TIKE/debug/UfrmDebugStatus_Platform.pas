unit UfrmDebugStatus_Platform;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, UfrmDebugStatus_Child, Vcl.StdCtrls;

type
  TfrmDebugStatus_Platform = class(TfrmDebugStatus_Child)
    lblPlatformOS: TLabel;
    lblPlatformUI: TLabel;
    lblPlatformFormFactor: TLabel;
    lblPlatformApplication: TLabel;
    lblPlatformBrowser: TLabel;
    cbPlatformOS: TComboBox;
    cbPlatformUI: TComboBox;
    cbPlatformFormFactor: TComboBox;
    cbPlatformApplication: TComboBox;
    cbPlatformBrowser: TComboBox;
    procedure cbPlatformClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure SetPlatform;
    procedure EnableControlsAndSetValues;
  protected
    procedure DebugKeyboardChanged; override;
    procedure DebugEventChanged; override;
  end;

implementation

{$R *.dfm}

// Matches https://help.keyman.com/developer/language/reference/platform

const
  PlatformUI: array[0..1] of string = (
    'Touch',
    'Hardware'
  );
  PLATFORM_UI_TOUCH = 0;
  PLATFORM_UI_HARDWARE = 1;

  PlatformOS: array[0..4] of string = (
    'Windows',
    'macOS',
    'Linux',
    'Android',
    'iOS'
  );
  PLATFORM_OS_WINDOWS = 0;
  PLATFORM_OS_MAC = 1;
  PLATFORM_OS_LINUX = 2;
  PLATFORM_OS_ANDROID = 3;
  PLATFORM_OS_IOS = 4;

  PlatformFormFactor: array[0..2] of string = (
    'Desktop',
    'Tablet',
    'Phone'
  );
  PLATFORM_FF_DESKTOP = 0;
  PLATFORM_FF_TABLET = 1;
  PLATFORM_FF_PHONE = 2;

  PlatformApplication: array[0..1] of string = (
    'Native',
    'Web'
  );
  PLATFORM_APP_NATIVE = 0;
  PLATFORM_APP_WEB = 1;

  PlatformBrowser: array[0..6] of string = (
    '',
    'IE',
    'Chrome',
    'Firefox',
    'Safari',
    'Opera',
    'Edge'
  );
  PLATFORM_BROWSER_NA = 0;
  PLATFORM_BROWSER_IE = 1;
  PLATFORM_BROWSER_CHROME = 2;
  PLATFORM_BROWSER_FIREFOX = 3;
  PLATFORM_BROWSER_SAFARI = 4;
  PLATFORM_BROWSER_OPERA = 5;
  PLATFORM_BROWSER_EDGE = 6;

procedure TfrmDebugStatus_Platform.cbPlatformClick(Sender: TObject);
begin
  EnableControlsAndSetValues;
  SetPlatform;
  memoDebug.SetFocus;
end;

procedure TfrmDebugStatus_Platform.DebugEventChanged;
begin
  inherited;
  EnableControlsAndSetValues;
end;

procedure TfrmDebugStatus_Platform.DebugKeyboardChanged;
begin
  if debugkeyboard = nil then
    Exit;
  EnableControlsAndSetValues;
  SetPlatform;
end;

procedure TfrmDebugStatus_Platform.EnableControlsAndSetValues;
var
  e: Boolean;
begin
  // Here we enable the specific allowable combinations of various
  // platform variables. These may change over time, e.g. if we eventually
  // support touch on Windows

  e := CurrentEvent = nil;

  // If we are a single-step debug state, disable all combos
  cbPlatformUI.Enabled := e;
  cbPlatformOS.Enabled := e;
  cbPlatformFormFactor.Enabled := e;
  cbPlatformApplication.Enabled := e;
  cbPlatformBrowser.Enabled := e;
  if not e then
     Exit;

  case cbPlatformOS.ItemIndex of
    PLATFORM_OS_WINDOWS, PLATFORM_OS_MAC, PLATFORM_OS_LINUX:
    begin
      cbPlatformUI.ItemIndex := PLATFORM_UI_HARDWARE;
      cbPlatformUI.Enabled := False;
      cbPlatformFormFactor.ItemIndex := PLATFORM_FF_DESKTOP;
      cbPlatformFormFactor.Enabled := False;
    end;
    PLATFORM_OS_IOS:
    begin
      cbPlatformUI.ItemIndex := PLATFORM_UI_HARDWARE;
      cbPlatformUI.Enabled := False;
      if cbPlatformFormFactor.ItemIndex = PLATFORM_FF_DESKTOP then
        cbPlatformFormFactor.ItemIndex := PLATFORM_FF_TABLET;
      cbPlatformFormFactor.Enabled := True;
    end;
    PLATFORM_OS_ANDROID:
    begin
      cbPlatformUI.Enabled := True;
      if cbPlatformFormFactor.ItemIndex = PLATFORM_FF_DESKTOP then
        cbPlatformFormFactor.ItemIndex := PLATFORM_FF_TABLET;
      cbPlatformFormFactor.Enabled := True;
    end;
  end;

  if cbPlatformApplication.ItemIndex = PLATFORM_APP_NATIVE then
  begin
    cbPlatformBrowser.Enabled := False;
    cbPlatformBrowser.ItemIndex := PLATFORM_BROWSER_NA;
  end
  else
  begin
    cbPlatformBrowser.Enabled := True;
    if cbPlatformBrowser.ItemIndex = PLATFORM_BROWSER_NA then
      cbPlatformBrowser.ItemIndex := PLATFORM_BROWSER_CHROME;
  end;
end;

procedure TfrmDebugStatus_Platform.FormCreate(Sender: TObject);
  procedure Fill(cb: TComboBox; const a: array of string);
  var
    s: string;
  begin
    cb.Clear;
    cb.Items.BeginUpdate;
    for s in a do
      cb.Items.Add(s);
    cb.Items.EndUpdate;
    cb.ItemIndex := 0;
  end;
begin
  inherited;
  Fill(cbPlatformOS, PlatformOS);
  Fill(cbPlatformUI, PlatformUI);
  Fill(cbPlatformFormFactor, PlatformFormFactor);
  Fill(cbPlatformApplication, PlatformApplication);
  Fill(cbPlatformBrowser, PlatformBrowser);
end;

procedure TfrmDebugStatus_Platform.SetPlatform;
var
  p: string;
begin
  p := cbPlatformOS.Text;

  if p.StartsWith('mac') then
    // We need to allow all three synonyms here, because the compiler does not
    // validate these platform strings, but they are defined as valid
    // alternatives
    p := 'mac macos macosx';

  p := p + ' ' +
    cbPlatformUI.Text + ' ' +
    cbPlatformFormFactor.Text + ' ' +
    cbPlatformApplication.Text + ' ' +
    cbPlatformBrowser.Text;

  DebugCore.KMXPlatform := p.ToLower;
end;

end.
