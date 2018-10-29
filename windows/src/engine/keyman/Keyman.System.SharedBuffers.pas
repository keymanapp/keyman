unit Keyman.System.SharedBuffers;

interface

uses
  Winapi.Windows;

const
  MAX_SELECTKEYBOARDTSF_CIRCULARBUFFER_SIZE = 16;
  GLOBAL_SHAREDBUFFER_FILE_MAPPING_NAME = 'KeymanEngine_SharedBuffer';

type
  {$ALIGN 4}
  TSelectKeyboardBuffer = record
    LangID: WORD;
    CLSID: TGUID;
    GUIDProfile: TGUID;
  end;

  {$ALIGN 4}
  TSharedBuffer = record
    SelectKeyboardBuffer: array[0..MAX_SELECTKEYBOARDTSF_CIRCULARBUFFER_SIZE-1] of TSelectKeyboardBuffer;
  end;

  PSharedBuffer = ^TSharedBuffer;

  TSharedBufferManager = class
  private
    class var FIdentity: TSharedBufferManager;
  private
    FSelectKeyboardIndex: Integer;
    hMMF: THandle;
    pSharedData: PSharedBuffer;
  public
    constructor Create;
    destructor Destroy; override;
    function WriteSelectKeyboardBuffer(skb: TSelectKeyboardBuffer): Integer;
    class function Identity: TSharedBufferManager;
  end;

implementation

uses
  System.SysUtils,

  Keyman.System.Security;

{ TSharedBufferManager }

constructor TSharedBufferManager.Create;
begin
  inherited Create;
  hMMF := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE or SEC_COMMIT, 0, sizeof(TSharedBuffer),
    PChar(GLOBAL_SHAREDBUFFER_FILE_MAPPING_NAME));
  if hMMF = 0 then
    RaiseLastOSError;

  SetObjectToLowIntegrity(hMMF);
  // Most apps only need read access. TODO: reduce general access permission to READ for other processes;
  // but doing so for application packages is a good start
  GrantPermissionToAllApplicationPackages(hMMF, FILE_MAP_READ);

  pSharedData := PSharedBuffer(MapViewOfFile(hMMF, FILE_MAP_ALL_ACCESS, 0, 0, sizeof(TSharedBuffer)));
  if not Assigned(pSharedData) then
    RaiseLastOSError;
end;

destructor TSharedBufferManager.Destroy;
begin
  if Assigned(pSharedData) then
  begin
    if not UnmapViewOfFile(pSharedData) then
      RaiseLastOSError;
  end;

  if hMMF <> 0 then
  begin
    if not CloseHandle(hMMF) then
      RaiseLastOSError;
  end;

  inherited Destroy;
end;

class function TSharedBufferManager.Identity: TSharedBufferManager;
begin
  if not Assigned(FIdentity) then
    FIdentity := TSharedBufferManager.Create;
  Result := FIdentity;
end;

//
// This comes from the Windows SDK and is a simple way of guaranteeing
// that the processor cache is flushed on a multiprocessor machine, so
// that we can be assured that the circular buffer is coherent before
// we notify the target thread. (Given that we post a message to the
// target thread and don't use synchronisation primitives, this is
// probably not going to be a problem anyway but it's the right way to
// do it.
//
procedure MemoryBarrier; inline;
var
  Barrier: LONG;
begin
  InterlockedOr(Barrier, 0);
end;

function TSharedBufferManager.WriteSelectKeyboardBuffer(
  skb: TSelectKeyboardBuffer): Integer;
begin
  Result := FSelectKeyboardIndex;

  pSharedData.SelectKeyboardBuffer[Result] := skb;

  MemoryBarrier;

  Inc(FSelectKeyboardIndex);
  if FSelectKeyboardIndex = MAX_SELECTKEYBOARDTSF_CIRCULARBUFFER_SIZE then
    FSelectKeyboardIndex := 0;
end;

initialization
finalization
  FreeAndNil(TSharedBufferManager.FIdentity);
end.
