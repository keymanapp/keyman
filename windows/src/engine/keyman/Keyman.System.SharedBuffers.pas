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
  AclApi,
  Accctrl,
  System.SysUtils;

const LOW_INTEGRITY_SDDL_SACL_W: WideString = 'S:(ML;;NW;;;LW)';
const LABEL_SECURITY_INFORMATION = $00000010;
const SDDL_REVISION_1 = 1;

function ConvertStringSecurityDescriptorToSecurityDescriptor(
    {IN} StringSecurityDescriptor: LPCWSTR;
    {IN} StringSDRevision: DWORD;
    {OUT} var SecurityDescriptor: PSECURITY_DESCRIPTOR;
    {OUT} SecurityDescriptorSize: PULONG {OPTIONAL}
    ): BOOL; stdcall; external 'advapi32.dll' name 'ConvertStringSecurityDescriptorToSecurityDescriptorW';

function SetObjectToLowIntegrity(hObject: THandle; _type: SE_OBJECT_TYPE = SE_KERNEL_OBJECT): BOOL;
var
  dwErr: DWORD;
  pSD: PSECURITY_DESCRIPTOR;
  pSacl: PACL;
  fSaclPresent: BOOL;
  fSaclDefaulted: BOOL;
begin
  //BOOL bRet = FALSE;
  //dwErr := ERROR_SUCCESS;
  Result := False;

  pSD := nil;
  pSacl := nil;
  fSaclPresent := FALSE;
  fSaclDefaulted := FALSE;

  if LOBYTE(LOWORD(GetVersion())) >= 6 then
  begin
    if ConvertStringSecurityDescriptorToSecurityDescriptor(PWideChar(LOW_INTEGRITY_SDDL_SACL_W), SDDL_REVISION_1, pSD, nil) then
    begin
      if GetSecurityDescriptorSacl(pSD, fSaclPresent, pSacl, fSaclDefaulted) then
      begin
        dwErr := SetSecurityInfo(
                hObject, _type, LABEL_SECURITY_INFORMATION,
                nil, nil, nil, pSacl);
        Result := ERROR_SUCCESS = dwErr;
      end;
    end;

    LocalFree(Cardinal(pSD));
  end
  else
    Result := True;
end;


{ TSharedBufferManager }

constructor TSharedBufferManager.Create;
begin
  inherited Create;
  hMMF := CreateFileMapping(INVALID_HANDLE_VALUE, nil, PAGE_READWRITE or SEC_COMMIT, 0, sizeof(TSharedBuffer),
    PChar(GLOBAL_SHAREDBUFFER_FILE_MAPPING_NAME));
  if hMMF = 0 then
    RaiseLastOSError;

  SetObjectToLowIntegrity(hMMF);
//  TODO:     not GrantPermissionToAllApplicationPackages(m_hMMF, FILE_MAP_ALL_ACCESS) then

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
