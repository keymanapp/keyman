{
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * Wrapper for Keyman Core debug interfaces
}
unit Keyman.System.Debug.DebugCore;

interface

uses
  System.SysUtils,

  Keyman.System.KeymanCore,
  Keyman.System.KeymanCoreDebug;

type
  EDebugCore = class(Exception);

  TDebugCore = class
  private
    FKeyboard: pkm_kbp_keyboard;
    FState: pkm_kbp_state;
    class var KeymanCoreLoaded: Boolean;
    class procedure InitKeymanCore; static;
    function GetKMXPlatform: string;
    procedure SetKMXPlatform(const Value: string);
  public
    constructor Create(const Filename: string; EnableDebug: Boolean);
    destructor Destroy; override;
    function GetOption(const name: string): string;
    procedure SetOption(const name, value: string);
    property KMXPlatform: string read GetKMXPlatform write SetKMXPlatform;
    property Keyboard: pkm_kbp_keyboard read FKeyboard;
    property State: pkm_kbp_state read FState;
  end;

implementation

uses
  KeymanPaths;

{ TDebugCore }

constructor TDebugCore.Create(const Filename: string; EnableDebug: Boolean);
var
  status: km_kbp_status;
begin
  inherited Create;

  InitKeymanCore;

  FKeyboard := nil;
  FState := nil;

  status := km_kbp_keyboard_load(PChar(FileName), FKeyboard);
  if status <> KM_KBP_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to start debugger -- keyboard load failed with error %x', [Ord(status)]);

  status := km_kbp_state_create(FKeyboard, @KM_KBP_OPTIONS_END, FState);
  if status <> KM_KBP_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to start debugger -- state creation failed with error %x', [Ord(status)]);

  if EnableDebug then
  begin
    status := km_kbp_state_debug_set(FState, 1);
    if status <> KM_KBP_STATUS_OK then
      raise EDebugCore.CreateFmt('Unable to start debugger -- enabling debug failed with error %x', [Ord(status)]);
  end;
end;

destructor TDebugCore.Destroy;
begin
  if FState <> nil then
    km_kbp_state_dispose(FState);
  FState := nil;
  if FKeyboard <> nil then
    km_kbp_keyboard_dispose(FKeyboard);
  FKeyboard := nil;
  inherited Destroy;
end;

class procedure TDebugCore.InitKeymanCore;
var
  path: string;
begin
  if not KeymanCoreLoaded then
  begin
    path := TKeymanPaths.KeymanCoreLibraryPath(kmnkbp0);
    _km_kbp_set_library_path(path);
    KeymanCoreLoaded := True;
  end;
end;

function TDebugCore.GetKMXPlatform: string;
var
  p: pkm_kbp_cp;
  status: km_kbp_status;
begin
  status := km_kbp_state_option_lookup(
    FState,
    KM_KBP_OPT_ENVIRONMENT,
    pkm_kbp_cp(PWideChar(KM_KBP_KMX_ENV_PLATFORM)),
    p
  );
  if status <> KM_KBP_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to locate platform, error %x', [Ord(status)]);
  Result := PWideChar(p);
end;

procedure TDebugCore.SetKMXPlatform(const Value: string);
var
  options: array[0..1] of km_kbp_option_item;
  status: km_kbp_status;
begin
  options[0].key := pkm_kbp_cp(PWideChar(KM_KBP_KMX_ENV_PLATFORM));
  options[0].value := pkm_kbp_cp(PWideChar(Value));
  options[0].scope := KM_KBP_OPT_ENVIRONMENT;
  options[1] := KM_KBP_OPTIONS_END;
  status := km_kbp_state_options_update(FState, @options[0]);
  if status <> KM_KBP_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to set platform, error %x', [Ord(status)]);
end;

function TDebugCore.GetOption(const name: string): string;
var
  p: pkm_kbp_cp;
  status: km_kbp_status;
begin
  status := km_kbp_state_option_lookup(
    FState,
    KM_KBP_OPT_KEYBOARD,
    pkm_kbp_cp(PWideChar(name)),
    p
  );
  if status <> KM_KBP_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to locate option %s, error %x', [name, Ord(status)]);
  Result := PWideChar(p);
end;

procedure TDebugCore.SetOption(const name, value: string);
var
  options: array[0..1] of km_kbp_option_item;
  status: km_kbp_status;
begin
  options[0].key := pkm_kbp_cp(PWideChar(Name));
  options[0].value := pkm_kbp_cp(PWideChar(Value));
  options[0].scope := KM_KBP_OPT_KEYBOARD;
  options[1] := KM_KBP_OPTIONS_END;
  status := km_kbp_state_options_update(FState, @options[0]);
  if status <> KM_KBP_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to set option %s, error %x', [name, Ord(status)]);
end;

end.
