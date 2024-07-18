//
// Original code from:
// http://stackoverflow.com/questions/4354071/how-can-i-allow-a-form-to-accept-file-dropping-without-handling-windows-messages
//
unit DropTarget;

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  Winapi.ShellAPI,
  System.StrUtils,
  Vcl.Forms;

type
  IDragDrop = interface
    function DropAllowed(const FileNames: TArray<string>): Boolean;
    procedure Drop(const FileNames: TArray<string>);
  end;

  TDropTarget = class(TObject, IInterface, IDropTarget)
  private
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  private
    // IDropTarget
    FHandle: HWND;
    FDragDrop: IDragDrop;
    FDropAllowed: Boolean;
    procedure GetFileNames(const dataObj: IDataObject; var FileNames: TArray<string>);
    procedure SetEffect(var dwEffect: Integer);
    function DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult; stdcall;
    function DragOver(grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const dataObj: IDataObject; grfKeyState: Longint; pt: TPoint; var dwEffect: Longint): HResult; stdcall;
  public
    constructor Create(AHandle: HWND; const ADragDrop: IDragDrop);
    destructor Destroy; override;
  end;

implementation

{ TDropTarget }

constructor TDropTarget.Create(AHandle: HWND; const ADragDrop: IDragDrop);
begin
  inherited Create;
  FHandle := AHandle;
  FDragDrop := ADragDrop;
  RegisterDragDrop(FHandle, Self)
end;

destructor TDropTarget.Destroy;
begin
  RevokeDragDrop(FHandle);
  inherited;
end;

{ IUnknown }

function TDropTarget.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj)
    then Result := S_OK
    else Result := E_NOINTERFACE;
end;

function TDropTarget._AddRef: Integer;
begin
  Result := -1;
end;

function TDropTarget._Release: Integer;
begin
  Result := -1;
end;

{ Utility }

procedure TDropTarget.GetFileNames(const dataObj: IDataObject; var FileNames: TArray<string>);
var
  i: Integer;
  formatetcIn: TFormatEtc;
  medium: TStgMedium;
  dropHandle: HDROP;
begin
  FileNames := nil;
  formatetcIn.cfFormat := CF_HDROP;
  formatetcIn.ptd := nil;
  formatetcIn.dwAspect := DVASPECT_CONTENT;
  formatetcIn.lindex := -1;
  formatetcIn.tymed := TYMED_HGLOBAL;
  if dataObj.GetData(formatetcIn, medium)=S_OK then
  begin
    dropHandle := medium.hGlobal;
    SetLength(FileNames, DragQueryFile(dropHandle, $FFFFFFFF, nil, 0));
    for i := 0 to High(FileNames) do
    begin
      SetLength(FileNames[i], DragQueryFile(dropHandle, i, nil, 0));
      DragQueryFile(dropHandle, i, @FileNames[i][1], Length(FileNames[i])+1);
    end;
  end;
end;

procedure TDropTarget.SetEffect(var dwEffect: Integer);
begin
  if FDropAllowed
    then dwEffect := DROPEFFECT_COPY
    else dwEffect := DROPEFFECT_NONE;
end;

{ IDropTarget }

function TDropTarget.DragEnter(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  FileNames: TArray<string>;
begin
  Result := S_OK;
  try
    GetFileNames(dataObj, FileNames);
    FDropAllowed := (Length(FileNames)>0) and FDragDrop.DropAllowed(FileNames);
    SetEffect(dwEffect);
  except
    Result := E_UNEXPECTED;
  end;
end;

function TDropTarget.DragLeave: HResult;
begin
  Result := S_OK;
end;

function TDropTarget.DragOver(grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
begin
  Result := S_OK;
  try
    SetEffect(dwEffect);
  except
    Result := E_UNEXPECTED;
  end;
end;

function TDropTarget.Drop(const dataObj: IDataObject; grfKeyState: Integer; pt: TPoint; var dwEffect: Integer): HResult;
var
  FileNames: TArray<string>;
begin
  Result := S_OK;
  try
    GetFileNames(dataObj, FileNames);
    if Length(FileNames) > 0 then
      FDragDrop.Drop(FileNames);
  except
    Application.HandleException(Self);
  end;
end;

end.
