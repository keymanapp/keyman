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
    FKeyboard: pkm_core_keyboard;
    FState: pkm_core_state;
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
    property Keyboard: pkm_core_keyboard read FKeyboard;
    property State: pkm_core_state read FState;
  end;

implementation

uses
  System.Classes,

  KeymanPaths;

{ TDebugCore }

constructor TDebugCore.Create(const Filename: string; EnableDebug: Boolean);
var
  status: km_core_status;
  fs: TFileStream;
  Buffer: Pointer;
  BufferSize: NativeInt;
begin
  inherited Create;

  InitKeymanCore;

  FKeyboard := nil;
  FState := nil;

  Buffer := nil;
  try
    fs := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      BufferSize := fs.Size;
      Buffer := AllocMem(BufferSize);
      if fs.Read(Buffer^, BufferSize) <> BufferSize then
        raise EDebugCore.Create('Unable to start debugger -- failed to read file from disk');
    finally
      fs.Free;
    end;

    status := km_core_keyboard_load_from_blob(PChar(FileName), Buffer, BufferSize, FKeyboard);
  finally
    FreeMem(Buffer);
  end;

  if status <> KM_CORE_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to start debugger -- keyboard load failed with error %x', [Ord(status)]);

  status := km_core_state_create(FKeyboard, @KM_CORE_OPTIONS_END, FState);
  if status <> KM_CORE_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to start debugger -- state creation failed with error %x', [Ord(status)]);

  if EnableDebug then
  begin
    status := km_core_state_debug_set(FState, 1);
    if status <> KM_CORE_STATUS_OK then
      raise EDebugCore.CreateFmt('Unable to start debugger -- enabling debug failed with error %x', [Ord(status)]);
  end;
end;

destructor TDebugCore.Destroy;
begin
  if FState <> nil then
    km_core_state_dispose(FState);
  FState := nil;
  if FKeyboard <> nil then
    km_core_keyboard_dispose(FKeyboard);
  FKeyboard := nil;
  inherited Destroy;
end;

class procedure TDebugCore.InitKeymanCore;
var
  path: string;
begin
  if not KeymanCoreLoaded then
  begin
    path := TKeymanPaths.KeymanCoreLibraryPath(keymancore);
    try
      _km_core_set_library_path(path);
    except
      on E:Exception do
        raise EDebugCore.CreateFmt('Unable to load Keyman Core library at %s: %s %s', [path, E.ClassName, E.Message]);
    end;
    KeymanCoreLoaded := True;
  end;
end;

function TDebugCore.GetKMXPlatform: string;
var
  p: pkm_core_cu;
  status: km_core_status;
begin
  status := km_core_state_option_lookup(
    FState,
    KM_CORE_OPT_ENVIRONMENT,
    pkm_core_cu(PWideChar(KM_CORE_KMX_ENV_PLATFORM)),
    p
  );
  if status <> KM_CORE_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to locate platform, error %x', [Ord(status)]);
  Result := PWideChar(p);
end;

procedure TDebugCore.SetKMXPlatform(const Value: string);
var
  options: array[0..1] of km_core_option_item;
  status: km_core_status;
begin
  options[0].key := pkm_core_cu(PWideChar(KM_CORE_KMX_ENV_PLATFORM));
  options[0].value := pkm_core_cu(PWideChar(Value));
  options[0].scope := KM_CORE_OPT_ENVIRONMENT;
  options[1] := KM_CORE_OPTIONS_END;
  status := km_core_state_options_update(FState, @options[0]);
  if status <> KM_CORE_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to set platform, error %x', [Ord(status)]);
end;

function TDebugCore.GetOption(const name: string): string;
var
  p: pkm_core_cu;
  status: km_core_status;
begin
  status := km_core_state_option_lookup(
    FState,
    KM_CORE_OPT_KEYBOARD,
    pkm_core_cu(PWideChar(name)),
    p
  );
  if status <> KM_CORE_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to locate option %s, error %x', [name, Ord(status)]);
  Result := PWideChar(p);
end;

procedure TDebugCore.SetOption(const name, value: string);
var
  options: array[0..1] of km_core_option_item;
  status: km_core_status;
begin
  options[0].key := pkm_core_cu(PWideChar(Name));
  options[0].value := pkm_core_cu(PWideChar(Value));
  options[0].scope := KM_CORE_OPT_KEYBOARD;
  options[1] := KM_CORE_OPTIONS_END;
  status := km_core_state_options_update(FState, @options[0]);
  if status <> KM_CORE_STATUS_OK then
    raise EDebugCore.CreateFmt('Unable to set option %s, error %x', [name, Ord(status)]);
end;

end.
