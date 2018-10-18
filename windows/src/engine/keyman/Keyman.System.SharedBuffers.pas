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
    FSelectKeyboardIdentity: Integer;
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
    UnmapViewOfFile(pSharedData);
  end;

  if hMMF <> 0 then
  begin
    CloseHandle(hMMF);
  end;

  inherited Destroy;
end;

class function TSharedBufferManager.Identity: TSharedBufferManager;
begin
  if not Assigned(FIdentity) then
    FIdentity := TSharedBufferManager.Create;
  Result := FIdentity;
end;

procedure MemoryBarrier; inline;
var
  Barrier: LONG;
begin
  InterlockedOr(Barrier, 0);
end;

function TSharedBufferManager.WriteSelectKeyboardBuffer(
  skb: TSelectKeyboardBuffer): Integer;
begin
  Result := FSelectKeyboardIdentity;

  pSharedData.SelectKeyboardBuffer[Result] := skb;

  MemoryBarrier;

  Inc(FSelectKeyboardIdentity);
  if FSelectKeyboardIdentity = MAX_SELECTKEYBOARDTSF_CIRCULARBUFFER_SIZE then
    FSelectKeyboardIdentity := 0;
end;

initialization
finalization
  FreeAndNil(TSharedBufferManager.FIdentity);
end.
