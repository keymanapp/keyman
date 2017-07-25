unit frmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ActiveX, ShlObj, ComObj, StdCtrls, OleCtrls,
  SHDocVw_EWB, EwbCore, EmbeddedWB;

type
  TForm1 = class(TForm, IDropTarget)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    EmbeddedWB1: TEmbeddedWB;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EmbeddedWB1DownloadComplete(Sender: TObject);
  private
    { Private declarations }
    function GetPageControlDropIndex(PageControl: TPageControl; P: TPoint): Integer;
    procedure DoDrop(data: string; pt: TPoint);
    function CreateNewTabBrowser(Url: string; idx: Integer): TTabSheet;
    // IDropTarget
    function DragEnter(const dataObj: IDataObject;
      grfKeyState: Longint;
      pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DragOver(grfKeyState: Longint;
      pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    function DragLeave: HRESULT; stdcall;
    function Drop(const dataObj: IDataObject;
      grfKeyState: Longint; pt: TPoint;
      var dwEffect: Longint): HRESULT; stdcall;
    // IUnknown
    // Ignore reference counting
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  NewTab: TTabSheet;
  DesignTimeWB: TEmbeddedWB;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  {Allow window to accept drop events}
  OleCheck(RegisterDragDrop(Handle, Form1));
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  {Finished accepting drops}
  RevokeDragDrop(Handle);
end;

function TForm1.GetPageControlDropIndex(PageControl: TPageControl; P: TPoint): Integer;
const
  TCM_GETITEMRECT = $130A;
var
  i: Integer;
  r: TRect;
begin
  Result := -1;
  with PageControl do
  begin
    p := ScreenToClient(p);
    for i := 0 to PageCount - 1 do
    begin
      Perform(TCM_GETITEMRECT, i, LPARAM(@r));
      if PtInRect(r, Point(P.X, P.Y)) then
      begin
        Result := i;
        Exit;
      end;
    end;
  end;
end;

procedure TForm1.EmbeddedWB1DownloadComplete(Sender: TObject);
var
  EWB: TEmbeddedWB;
begin
  EWB := (Sender as TEmbeddedWB);
  if EWB.LocationName <> '' then
    (EWB.Parent as TTabSheet).Caption := EWB.LocationName;
end;

function TForm1.CreateNewTabBrowser(Url: string; idx: Integer): TTabSheet;
begin
  NewTab := TTabSheet.Create(PageControl1);
  with NewTab do
  begin
    PageControl := PageControl1;
    Parent := PageControl1;
    PageIndex := idx;
    Caption := 'Loading...';
  end;
  DesignTimeWB := TEmbeddedWB.Create(NewTab);
  TControl(DesignTimeWB).Parent := NewTab;
  with DesignTimeWB do
  begin
    EnableMessageHandler := True;
    LoadSettings;
    OnDownloadComplete := EmbeddedWB1DownloadComplete;
    Align := alClient;
    if Trim(URL) <> '' then
      DesignTimeWB.NavigateWait(URL);
  end;
  PageControl1.ActivePage := NewTab;
  Result := NewTab;
end;

// IDropTarget

function TForm1.DragOver(grfKeyState: Longint;
  pt: TPoint;
  var dwEffect: Longint): HRESULT;
begin
  dwEffect := DROPEFFECT_LINK;
  if FindVCLWindow(pt) is TPageControl then
    Result := S_OK
  else
    Result := S_FALSE;
end;

function TForm1.DragEnter(const dataObj: IDataObject;
  grfKeyState: Longint;
  pt: TPoint;
  var dwEffect: Longint): HRESULT;
begin
  dwEffect := DROPEFFECT_LINK;
  Result := S_OK;
end;

procedure TForm1.DoDrop(data: string; pt: TPoint);
var
  idx: Integer;
//  TabSheet: TTabSheet;
begin
  idx := GetPageControlDropIndex(PageControl1, pt);
  if idx <> -1 then
  begin
    CreateNewTabBrowser(data, idx);
  end;
end;

function TForm1.DragLeave: HRESULT;
begin
  Result := S_OK;
end;

function TForm1._AddRef: Integer;
begin
  Result := 1;
end;

function TForm1._Release: Integer;
begin
  Result := 1;
end;

function TForm1.Drop(const dataObj: IDataObject;
  grfKeyState: Longint;
  pt: TPoint;
  var dwEffect: Longint): HRESULT;
var
  aFmtEtc: TFORMATETC;
  aStgMed: TSTGMEDIUM;
  pData: PChar;
begin
  {Make certain the data rendering is available}
  if (dataObj = nil) then
    raise Exception.Create('IDataObject-Pointer is not valid!');
  with aFmtEtc do
  begin
    cfFormat := CF_TEXT;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  {Get the data}
  OleCheck(dataObj.GetData(aFmtEtc, aStgMed));
  try
    {Lock the global memory handle to get a pointer to the data}
    pData := GlobalLock(aStgMed.hGlobal);
    DoDrop(pData, pt);
  finally
    {Finished with the pointer}
    GlobalUnlock(aStgMed.hGlobal);
    {Free the memory}
    ReleaseStgMedium(aStgMed);
  end;
  Result := S_OK;
end;



end.

