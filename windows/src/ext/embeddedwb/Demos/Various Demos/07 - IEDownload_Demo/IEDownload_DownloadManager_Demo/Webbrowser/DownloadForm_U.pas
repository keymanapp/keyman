//*************************************************************************
//                                                                        *
//                     IE Downloag Mgr                                    *
//                       For Delphi                                       *
//                                                                        *
//                     Freeware Demo                                      *
//  Developing Team:                                                      *
//  Eran Bodankin -bsalsa(bsalsa@bsalsa.com)                              *
//  Mathias Walter (mich@matze.tv)                                        *
//                                                                        *
//                                                                        *
//  Updated versions:                                                     *
//               http://www.bsalsa.com                                    *
//*************************************************************************
{LICENSE:
THIS SOFTWARE IS PROVIDED TO YOU "AS IS" WITHOUT WARRANTY OF ANY KIND,
EITHER EXPRESSED OR IMPLIED INCLUDING BUT NOT LIMITED TO THE APPLIED
WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
YOU ASSUME THE ENTIRE RISK AS TO THE ACCURACY AND THE USE OF THE SOFTWARE
AND ALL OTHER RISK ARISING OUT OF THE USE OR PERFORMANCE OF THIS SOFTWARE
AND DOCUMENTATION. [YOUR NAME] DOES NOT WARRANT THAT THE SOFTWARE IS ERROR-FREE
OR WILL OPERATE WITHOUT INTERRUPTION. THE SOFTWARE IS NOT DESIGNED, INTENDED
OR LICENSED FOR USE IN HAZARDOUS ENVIRONMENTS REQUIRING FAIL-SAFE CONTROLS,
INCLUDING WITHOUT LIMITATION, THE DESIGN, CONSTRUCTION, MAINTENANCE OR
OPERATION OF NUCLEAR FACILITIES, AIRCRAFT NAVIGATION OR COMMUNICATION SYSTEMS,
AIR TRAFFIC CONTROL, AND LIFE SUPPORT OR WEAPONS SYSTEMS. VSOFT SPECIFICALLY
DISCLAIMS ANY EXPRESS OR IMPLIED WARRANTY OF FITNESS FOR SUCH PURPOSE.

You may use, change or modify the component under 4 conditions:
1. In your website, add a link to "http://www.bsalsa.com"
2. In your application, add credits to "Embedded Web Browser"
3. Mail me  (bsalsa@bsalsa.com) any code change in the unit
   for the benefit of the other users.
4. Please, consider donation in our web site!
{*******************************************************************************}

unit DownloadForm_U;

interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Dialogs, StdCtrls,
  IEDownload, ComCtrls, UrlMon, ActiveX, DownloadRequestForm,
  ShellApi, Menus, ExtCtrls, IEDownloadTools, Graphics;

const
  SEE_MASK_NOZONECHECKS = $00800000;

type
  TDownloadForm = class(TForm)
    IEDownload: TIEDownload;
    pmDownloadItem: TPopupMenu;
    miCancel: TMenuItem;
    miOpen: TMenuItem;
    Panel1: TPanel;
    btnCancel: TButton;
    chkAutoClose: TCheckBox;

    ListView: TListView;
    cbRemoveComp: TCheckBox;
    cbExecute: TCheckBox;
    Memo1: TMemo;
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure miCancelClick(Sender: TObject);
    procedure ListViewContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure miOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure IEDownloadProgress(Sender: TBSCB; ulProgress, ulProgressMax,
      ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded, ElapsedTime,
      Speed, RemainingTime, Status, Percent: string);
    function IEDownloadBeginningTransaction(Sender: TBSCB; szURL,
      szHeaders: PWideChar; dwReserved: Cardinal;
      out szAdditionalHeaders: PWideChar): HRESULT;
    procedure cbRemoveCompClick(Sender: TObject);
    procedure cbExecuteClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure IEDownloadComplete(Sender: TCustomIEDownload; aFileNameAndPath,
      aFileName, aFolderName, aExtension: WideString;
      const ActiveConnections: Integer);
    procedure IEDownloadStartBinding(Sender: TBSCB; var Cancel: Boolean;
      pib: IBinding; const FileName: WideString; const FileSize: Integer);
    procedure IEDownloadBeforeDownload(Sender: TInfoData; const Url, FileName,
      FileExtension, Host, DownloadFolder: string; const FileSize: Integer;
      var Cancel: Boolean);
    procedure IEDownloadDataAvailable(Sender: TBSCB; var Buffer: PByte;
      var BufLength: Cardinal);
    procedure IEDownloadError(const ErrorCode: Integer; const stError: string);
  private
    { Private declarations }

    procedure Execute(FName: WideString);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
    procedure DoDownload(pmk: IMoniker; pbc: IBindCtx);
    procedure DoGo(inUrl: string);
  end;

var
  DownloadForm: TDownloadForm;

implementation

{$R *.dfm}

procedure TDownloadForm.cbExecuteClick(Sender: TObject);
var
  lvItem: TListItem;
begin
    if (cbExecute.Checked) and (ListView.Selected.Index > 0) and
    (ListView.Selected.SubItems[0]= 'Done.') then
    begin
      lvItem := ListView.Selected;
      Execute(IEDownload.DownloadFolder+ lvItem.Caption);
    end;
end;

procedure TDownloadForm.cbRemoveCompClick(Sender: TObject);
var
  idx: integer;
begin
  if (cbRemoveComp.checked) then
  begin
     for idx := 0 to ListView.Items.Count - 1 do
     begin
       if ListView.Items[idx].SubItems[0]= 'Done.' then
         begin
          TProgressBar(ListView.Items[idx].Data).Free;
          ListView.Items.Delete(idx);
         end;
     end;
  end;
end;

procedure TDownloadForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TDownloadForm.FormDestroy(Sender: TObject);
begin
  DownloadForm := nil;
end;

procedure TDownloadForm.btnCancelClick(Sender: TObject);
begin
  IEDownload.CancelAll;
  Close;
end;

procedure TDownloadForm.DoGo(inUrl: string);
begin
  if (not Visible) then
    Show
  else
    BringToFront;
  IEDownload.Go(inUrl);
end;

procedure TDownloadForm.DoDownload(pmk: IMoniker; pbc: IBindCtx);
begin
  if (not Visible) then
    Show
  else
    BringToFront;
  IEDownload.Download(pmk, pbc);
end;

procedure TDownloadForm.Execute(FName: WideString);
var
  lpExecInfo: TShellExecuteInfo;
begin
  ZeroMemory(@lpExecInfo, sizeof(lpExecInfo));
  with lpExecInfo do
  begin
   cbSize := sizeof(lpExecInfo);
   fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_NOZONECHECKS;
   Wnd := Self.Handle;
   lpVerb := 'open'; // use default handling
   lpFile := PWideChar(FName);
   nShow := SW_SHOWNORMAL;
  end;
  if (not ShellExecuteEx(@lpExecInfo)) then
    ShowMessage(SysErrorMessage(GetLastError()));
end;

function FindStringInLV(listItems: TListItems; inString: string): Integer;
var
  idx: Integer;
begin
  Result := -1;
  for idx := 0 to listItems.Count - 1 do
    if (listItems[idx].Caption = inString) then
    begin
      Result := idx;
      Break;
    end;
end;

function FindThreadIDInLV(listItem: TListItems; SubIdx, ID: integer): Integer;
var
  idx: Integer;
begin
  Result := -1;
  for idx := 0 to listItem.Count - 1 do
    if (listItem[idx].SubItems[SubIdx] = IntToStr(ID)) then
    begin
      Result := idx;
      Break;
    end;
end;

procedure TDownloadForm.IEDownloadBeforeDownload(Sender: TInfoData; const Url,
  FileName, FileExtension, Host, DownloadFolder: string;
  const FileSize: Integer; var Cancel: Boolean);
var
  sfi: TSHFileInfo;
  DownloadRequest: TDownloadRequest;
  ModRes: TModalResult;
begin  {New downlaod}
  DownloadRequest := TDownloadRequest.Create(nil);
  try
     with DownloadRequest do
     begin
       DontOpenThisExts.Add('msi');
       lbFromTxt.Caption := Host;
       lbNameTxt.Caption := FileName;
       lbSizeTxt.Caption := FormatSize(FileSize);
       if (FileSize >= 0) then
         lbSizeTxt.Caption := 'unknown';
        ZeroMemory(@sfi, sizeof(sfi));
       if (SHGetFileInfo(PChar(FileName),FILE_ATTRIBUTE_NORMAL, sfi,
         sizeof(sfi), SHGFI_ICON or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0) then
       begin
         if (sfi.hIcon <> 0) then
            imgIcon.Picture.Icon.Handle := sfi.hIcon;
         if (sfi.szTypeName[0] <> #0) then
            lbTypeTxt.Caption := sfi.szTypeName;
      end;
   end;

    modres := DownloadRequest.ShowModal;
    case ModRes of
      mrCancel:
      begin
        Cancel := True;
        DownloadRequest.Close;
      end;
      mrOpen:
        begin
          IEDownload.OpenDownloadFolder := True;
          btnCancel.Caption := 'Cancel All';
        end;
    mrSave:
      begin
        btnCancel.Caption := 'Cancel All';
        Sender.infDownloadFolder:= DownloadRequest.drDownloadFolder;
        Sender.infFileName:=DownloadRequest.drFileName;
      end;
    end;
    finally
      DownloadRequest.Free;
    end;
end;

function TDownloadForm.IEDownloadBeginningTransaction(Sender: TBSCB; szURL,
  szHeaders: PWideChar; dwReserved: Cardinal;
  out szAdditionalHeaders: PWideChar): HRESULT;
begin
   Result:= S_OK;
end;

procedure TDownloadForm.IEDownloadComplete(Sender: TCustomIEDownload;
  aFileNameAndPath, aFileName, aFolderName, aExtension: WideString;
  const ActiveConnections: Integer);
var
  lvItem: TListItem;
  idx: integer;
begin
    idx := FindStringInLV(Listview.Items, IEDownload.FileName);
    lvItem := ListView.Items[idx];
    if (Assigned(lvItem)) then
        lvItem.SubItems[6] := IntToStr(IEDownload.ActiveConnections);

  if cbExecute.Checked then
    Execute(IEDownload.DownloadedFile);
  lvItem := ListView.Selected;
  if (cbRemoveComp.checked) and (lvItem <> nil) then
  begin
    idx := lvItem.Index;
    TProgressBar(lvItem.Data).Free;
    ListView.Items.Delete(idx);
  end;
  if (IEDownload.ActiveConnections = 0) and (not IEDownload.Busy) then
  begin
    btnCancel.Caption := 'Close';
    if (chkAutoClose.Checked) then
      Close;
  end;
end;

procedure TDownloadForm.IEDownloadDataAvailable(Sender: TBSCB;
  var Buffer: PByte; var BufLength: Cardinal);
begin
   Memo1.Lines.Add(Char(Buffer));
end;

procedure TDownloadForm.IEDownloadError(const ErrorCode: Integer;
  const stError: string);
begin
  //Showmessage(stError);
end;

procedure TDownloadForm.IEDownloadProgress(Sender: TBSCB; ulProgress,
  ulProgressMax, ulStatusCode, FileSize: Cardinal; szStatusText: PWideChar; Downloaded,
  ElapsedTime, Speed, RemainingTime, Status, Percent: string);
var
  lvItem: TListItem;
  idx: integer;
  pb: TProgressBar;
begin
  if ListView.Items.Count = 0 then
    Exit;
  begin {New downlaod}
    idx := FindThreadIDInLV(Listview.Items, 5, Sender.ThreadID);
    lvItem := ListView.Items[idx];
    if (Assigned(lvItem)) then
    begin
      lvItem.SubItems[0] := Status;
      if ((ulStatusCode = BINDSTATUS_ENDDOWNLOADDATA) or
        (ulStatusCode = BINDSTATUS_DOWNLOADINGDATA)) then
      begin
        with lvItem do
        begin
          SubItems[1] := FormatSize(IEDownload.FileSize);
          SubItems[2] := Percent;
          SubItems[3] := Speed;
          SubItems[4] := RemainingTime;
          SubItems[6] := IntToStr(IEDownload.ActiveConnections);
        end;
        pb := TProgressBar(Listview.Items[idx].Data);
        pb.Max := ulProgressMax;
        pb.Position := ulProgress;
      end;
    end;
  end;
end;

procedure TDownloadForm.IEDownloadStartBinding(Sender: TBSCB;
  var Cancel: Boolean; pib: IBinding; const FileName: WideString;
  const FileSize: Integer);
var
  lvItem: TListItem;
  idx: Integer;
  pbRect: TRect;
  pb: TProgressBar;
begin {If the filename allready exists in our list view}
  idx := FindStringInLV(Listview.Items, IEDownload.FileName);
  if (idx > -1) then
  begin
    lvItem := Listview.Items[idx];
    with lvItem do
    begin
      SubItems[0] := 'Resuming...';
      SubItems.Add(''); // Size
      SubItems.Add(''); // Progress
      SubItems.Add(''); // Speed
      SubItems.Add('UnKnown'); // Remaining Time
      SubItems.Add(IntToStr(Sender.ThreadID));
      SubItems.Add(IntToStr(IEDownload.ActiveConnections));
      SubItems.Add(''); //ProgressBar
    end;
    pb := TProgressBar.Create(nil);
    pb.Parent := Listview;
    lvItem.Data := pb;
    pbRect := lvItem.DisplayRect(drBounds);
    pbRect.Left := 540 + pbRect.Left + ListView.Columns[7].Width;
    pbRect.Right := pbRect.Left+ Listview.Columns[7].Width+50;
    pb.BoundsRect := pbRect;
  end;


   idx := FindThreadIDInLV(Listview.Items, 5, Sender.ThreadID);
   if (idx > -1) then
     lvItem := ListView.Items[idx]
  else
    lvItem := Listview.Items.Add;
    with lvItem do
    begin
      Caption := IEDownload.FileName;
      SubItems.Add('Waiting'); // Status
      SubItems.Add(''); // Size
      SubItems.Add(''); // Progress
      SubItems.Add(''); // Speed
      SubItems.Add('UnKnown'); // Remaining Time
      SubItems.Add(IntToStr(Sender.ThreadID));
      SubItems.Add(IntToStr(IEDownload.ActiveConnections));
      SubItems.Add(''); //ProgressBar
    end;
    pb := TProgressBar.Create(nil);
    pb.Parent := Listview;
    lvItem.Data := pb;
    pbRect := lvItem.DisplayRect(drBounds);
    pbRect.Left := 540 + pbRect.Left + ListView.Columns[7].Width;
    pbRect.Right := pbRect.Left+ Listview.Columns[7].Width+50;
    pb.BoundsRect := pbRect;
  end;


procedure TDownloadForm.miCancelClick(Sender: TObject);
var
  lvItem: TListItem;
begin
  if (pmDownloadItem.Tag > -1) then
  begin
    lvItem := ListView.Items[miCancel.Tag];
    if (Assigned(lvItem) and Assigned(lvItem.Data)) then
    begin
      IEDownload.Cancel(TBSCB(lvItem.Data));
      lvItem.Data := nil;
    end;
    pmDownloadItem.Tag := -1;
  end;
end;

procedure TDownloadForm.miOpenClick(Sender: TObject);
var
  lvItem: TListItem;
begin
  if (pmDownloadItem.Tag > -1) then
  begin
    lvItem := ListView.Items[pmDownloadItem.Tag];
    if (Assigned(lvItem)) and (lvItem.SubItems[0]= 'Done.') then
        Execute(IEDownload.DownloadFolder+ lvItem.Caption);
  end;
end;

procedure TDownloadForm.ListViewContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  lvItem: TListItem;
  pt: TPoint;
begin
  // no item is selected but the context menu hotkey was pressed
  if ((MousePos.X = -1) and (MousePos.Y = -1)) then
  begin
    if (ListView.ItemIndex > -1) then
    begin
      lvItem := ListView.Items[ListView.ItemIndex];
      pmDownloadItem.Tag := ListView.ItemIndex;
      miCancel.Enabled := Assigned(lvItem.Data);
      miOpen.Enabled :=  (Assigned(lvItem)) and (lvItem.SubItems[0]= 'Done.');
        CanOpen(TBSCB(lvItem.Data).BscbInfo.infFileExt);
      pt := ListView.ClientToScreen(lvItem.GetPosition());
      pmDownloadItem.Popup(pt.X, pt.Y);
    end;
  end
  else
  begin
    lvItem := ListView.GetItemAt(MousePos.X, MousePos.Y);
    if (Assigned(lvItem)) then
    begin
      pmDownloadItem.Tag := lvItem.Index;
      lvItem := ListView.Items[pmDownloadItem.Tag];
      miCancel.Enabled := Assigned(lvItem.Data) and (lvItem.SubItems[0]<> 'Done.');
      miOpen.Enabled := (Assigned(lvItem)) and (lvItem.SubItems[0]= 'Done.');
      pt := ListView.ClientToScreen(MousePos);
      pmDownloadItem.Popup(pt.X, pt.Y);
    end;
  end;
end;

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
  btnCancel.Caption := 'Cancel All';
end;

procedure TDownloadForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if IEDownload.ActiveConnections > 0 then
    IEDownload.CancelAll;
end;

procedure TDownloadForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if IEDownload.ActiveConnections > 0 then
  begin
  if MessageDlg('There are active threads. Are you sure you want to quit?',
      mtWarning, [mbYes, mbNo], 0) = mrNo then
      CanClose:= False
      else
      IEDownload.CancelAll;
  end;
end;

end.
