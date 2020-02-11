unit Keyman.System.FrameworkInputPane;

interface

uses
  Winapi.Windows;

// See https://stackoverflow.com/a/55513524/1836776
// https://gist.github.com/DelphiWorlds/2098ebafd20aa43f6c5a69503b06c4ca
// https://docs.microsoft.com/en-us/windows/win32/api/shobjidl_core/nn-shobjidl_core-iframeworkinputpane

const
  CLSID_FrameworkInputPane: TGUID = '{D5120AA3-46BA-44C5-822D-CA8092C1FC72}';

type
  IFrameworkInputPaneHandler = interface
    ['{226C537B-1E76-4D9E-A760-33DB29922F18}']

    function Showing(
      var rcInputPaneScreenLocation: TRect;
      fEnsureFocusedElementInView: BOOL
    ): HResult; stdcall;

    function Hiding(
      fEnsureFocusedElementInView: BOOL
    ): HResult; stdcall;
  end;

  IFrameworkInputPane = interface
    ['{5752238B-24F0-495A-82F1-2FD593056796}']

    function Advise(
      pWindow: IUnknown;
      pHandler: IFrameworkInputPaneHandler;
      var pdwCookie: DWORD
    ): HRESULT; stdcall;

    function AdviseWithHWND(
      hwnd: HWND;
      pHandler: IFrameworkInputPaneHandler;
      var pdwCookie: DWORD
    ): HRESULT; stdcall;

    function Unadvise(
      dwCookie: DWORD
    ): HRESULT; stdcall;

    function Location(
      var prcInputPaneScreenLocation: TRect
    ): HRESULT; stdcall;
  end;

  TFrameworkInputPane = class
  private
    FInputPane: IFrameworkInputPane;
  public
    constructor Create;
    destructor Destroy; override;

    function GetLocation(var rt: TRECT): Boolean;
  end;

implementation

uses
  System.Types,
  Winapi.ActiveX,
  Keyman.System.DebugLogClient;

{ TFrameworkInputWrapper }

constructor TFrameworkInputPane.Create;
var
  hr: HRESULT;
begin
  inherited Create;

  FInputPane    := nil;

  hr := CoCreateInstance(
    CLSID_FrameworkInputPane, nil, CLSCTX_ALL {CLSCTX_INPROC_SERVER}, IFrameworkInputPane, FInputPane);
  if not Succeeded(hr) then
  begin
    // We'll silently fail; this may be an older OS
    FInputPane := nil;
    TDebugLogClient.Instance.WriteMessage('Unable to instantiate IFrameworkInputPane: %x', [hr]);
  end;
end;

destructor TFrameworkInputPane.Destroy;
begin
  FInputPane := nil;
  inherited Destroy;
end;

function TFrameworkInputPane.GetLocation(var rt: TRECT): Boolean;
begin
  Result := False;
  rt := rt.Empty;

  if Assigned(FInputPane) then
  begin
    Result := not FAILED(FInputPane.Location(rt));
  end;
end;

end.
