unit DownloadForm_U;

interface

uses
{$IFDEF VER140}Variants, {$ENDIF}Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
   Dialogs, StdCtrls, IEDownload, ComCtrls, UrlMon, ActiveX, DownloadRequestForm,
   ShellApi, Menus, ExtCtrls;

const
   SEE_MASK_NOZONECHECKS = $00800000;

type
   TDownloadForm = class(TForm)
      IEDownload: TIEDownload;
      lvDownloads: TListView;
      pmDownloadItem: TPopupMenu;
      miCancel: TMenuItem;
      miOpen: TMenuItem;
    Panel1: TPanel;
    btnCancel: TButton;
    chkAutoClose: TCheckBox;
    ProgressBar1: TProgressBar;
      procedure FormDestroy(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
      procedure IEDownloadProgress(Sender: TBSCB; ulProgress, ulProgressMax,
         ulStatusCode: Cardinal; szStatusText: PWideChar; Downloaded, ElapsedTime,
         Speed, RemainingTime, Status: string);
      procedure IEDownloadComplete(Sender: TBSCB; Stream: TStream;
         Result: HRESULT);
      function IEDownloadBeginningTransaction(Sender: TBSCB; szURL,
         szHeaders: PWideChar; dwReserved: Cardinal;
         out szAdditionalHeaders: PWideChar): HRESULT;
      procedure IEDownloadBinding(var Sender: TBSCB; var Cancel: Boolean);
      procedure miCancelClick(Sender: TObject);
      procedure lvDownloadsContextPopup(Sender: TObject; MousePos: TPoint;
         var Handled: Boolean);
      procedure miOpenClick(Sender: TObject);
      procedure FormCreate(Sender: TObject);
      procedure FormClose(Sender: TObject; var Action: TCloseAction);
   private
    { Private declarations }
      finishedAll: Boolean;
      procedure Execute(Sender: TBSCB);
   protected
      procedure CreateParams(var Params: TCreateParams); override;
   public
    { Public declarations }
      procedure Download(Url: string); overload;
      procedure Download(
         pmk: IMoniker; // Identifies the object to be downloaded
         pbc: IBindCtx // Stores information used by the moniker to bind
         ); overload;
   end;

var
   DownloadForm: TDownloadForm;

implementation

{$R *.dfm}

uses
   Registry;

function GetTempDir: string;
var
   Path: array[0..MAX_PATH] of Char;
begin
   GetTempPath(MAX_PATH, Path);
   Result := Path;
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
   Close;
end;

procedure TDownloadForm.Download(Url: string);
begin
   if (not Visible) then
      Show
   else
      BringToFront;
   IEDownload.Go(Url);
end;

procedure TDownloadForm.Download(pmk: IMoniker; pbc: IBindCtx);
begin
   if (not Visible) then
      Show
   else
      BringToFront;
   IEDownload.Download(pmk, pbc);
end;

procedure TDownloadForm.Execute(Sender: TBSCB);
var
   lpExecInfo: TShellExecuteInfo;
begin
   ZeroMemory(@lpExecInfo, sizeof(lpExecInfo));
   lpExecInfo.cbSize := sizeof(lpExecInfo);
   lpExecInfo.fMask := SEE_MASK_NOCLOSEPROCESS or SEE_MASK_NOZONECHECKS;
   lpExecInfo.Wnd := Self.Handle;
   //lpExecInfo.lpVerb := 'open'; // use default handling
   lpExecInfo.lpFile := PChar(Sender.Info.DownloadDir + Sender.Info.FileName);
   lpExecInfo.nShow := SW_SHOWNORMAL;
   if (not ShellExecuteEx(@lpExecInfo)) then
      begin
         ShowMessage(SysErrorMessage(GetLastError()));
      end;
end;

procedure TDownloadForm.IEDownloadComplete(Sender: TBSCB; Stream: TStream;
   Result: HRESULT);
var
   i: Integer;
   lvItem: TListItem;
begin
   if ((Result = S_OK) and (Sender.Info.OpenAfterDownload)) then
      Execute(Sender);
   finishedAll := true;
   if (lvDownloads.Items.Count > 1) then
      for i := 0 to lvDownloads.Items.Count - 1 do
         if (i <> Sender.Info.Index) then
            begin
               lvItem := lvDownloads.Items[i];
               if Assigned(lvItem.Data) and (TBSCB(lvItem.Data).State < Canceled) then
                  begin
                     finishedAll := false;
                     Break;
                  end;
            end;
   if (finishedAll) then
      btnCancel.Caption := 'Close';
   if (chkAutoClose.Checked) then
      Close;
end;

function TDownloadForm.IEDownloadBeginningTransaction(Sender: TBSCB;
   szURL, szHeaders: PWideChar; dwReserved: Cardinal;
   out szAdditionalHeaders: PWideChar): HRESULT;
begin
   result := S_OK;
end;

procedure TDownloadForm.IEDownloadProgress(Sender: TBSCB; ulProgress,
   ulProgressMax, ulStatusCode: Cardinal; szStatusText: PWideChar; Downloaded,
   ElapsedTime, Speed, RemainingTime, Status: string);
var
   lvItem: TListItem;
begin
   ProgressBar1.Max := ulProgressMax;
   ProgressBar1.Position := ulProgress;
   lvItem := lvDownloads.Items[Sender.Info.Index];
   if (Assigned(lvItem)) then
      begin
         lvDownloads.Items.BeginUpdate;
         if ((ulStatusCode = BINDSTATUS_ENDDOWNLOADDATA) or
            (ulStatusCode = BINDSTATUS_DOWNLOADINGDATA)) then
            begin
               lvItem.SubItems[2] := Format('%.1f %%', [ulProgress / ulProgressMax * 100]);
               lvItem.SubItems[4] := RemainingTime;
               if (ulStatusCode <> BINDSTATUS_ENDDOWNLOADDATA) then
                  lvItem.SubItems[3] := Speed
               else
                  lvItem.SubItems[4] := '';
            end;

         lvItem.SubItems[0] := Status; 
         lvDownloads.Items.EndUpdate;
      end;
end;

function FindItem(listItems: TListItems; Caption: string): Integer;
var
   idx: Integer;
begin
   result := -1;
   for idx := 0 to listItems.Count - 1 do
      if (listItems[idx].Caption = Caption) then
         begin
            result := idx;
            break;
         end;
end;

procedure TDownloadForm.IEDownloadBinding(var Sender: TBSCB;
   var Cancel: Boolean);
var
   DownloadRequest: TDownloadRequest;
   ModRes: TModalResult;
   lvItem: TListItem;
   arrSize: array[0..255] of Char;
   idx: Integer;
begin
   idx := FindItem(lvDownloads.Items, Sender.Info.FileName);
   if (idx > -1) then
      begin
         lvItem := lvDownloads.Items[idx];
         lvItem.SubItems[0] := 'Resuming...';
      end
   else
      begin
         lvItem := lvDownloads.Items.Add;
         lvItem.Caption := Sender.Info.FileName;
         lvItem.SubItems.Add(''); // Status
         lvItem.SubItems.Add(StrFormatByteSize(Sender.Info.FileSize, arrSize, Length(arrSize) - 1)); // Size
         lvItem.SubItems.Add(Format('%.1f %%', [0.])); // Progress
         lvItem.SubItems.Add(''); // Speed
         lvItem.SubItems.Add(''); // Remaining Time
      end;
   lvItem.Data := Sender;
   Sender.Info.Index := lvItem.Index;

   DownloadRequest := TDownloadRequest.Create(nil);
   DontOpenThisExts.Add('msi');
   DownloadRequest.SetInfo(Sender);
   modres := DownloadRequest.ShowModal();
   case modres of
      mrCancel:
         Cancel := True;
      mrOpen:
         begin
            Sender.Info.OpenAfterDownload := True;
            Sender.Info.DownloadDir := GetTempDir;
            finishedAll := false;
            btnCancel.Caption := 'Cancel All';
         end;
      else
         begin
            finishedAll := false;
            btnCancel.Caption := 'Cancel All';
         end;
   end;
end;

procedure TDownloadForm.miCancelClick(Sender: TObject);
var
   lvItem: TListItem;
begin
   if (pmDownloadItem.Tag > -1) then
      begin
         lvItem := lvDownloads.Items[miCancel.Tag];
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
         lvItem := lvDownloads.Items[pmDownloadItem.Tag];
         if (Assigned(lvItem) and Assigned(lvItem.Data)) then
            Execute(TBSCB(lvItem.Data));
      end;
end;

procedure TDownloadForm.lvDownloadsContextPopup(Sender: TObject;
   MousePos: TPoint; var Handled: Boolean);
var
   lvItem: TListItem;
   pt: TPoint;
begin
   // no item is selected but the context menu hotkey was pressed
   if ((MousePos.X = -1) and (MousePos.Y = -1)) then
      begin
         if (lvDownloads.ItemIndex > -1) then
            begin
               lvItem := lvDownloads.Items[lvDownloads.ItemIndex];
               pmDownloadItem.Tag := lvDownloads.ItemIndex;
               miCancel.Enabled := Assigned(lvItem.Data) and (TBSCB(lvItem.Data).State = Finished);
               miOpen.Enabled := Assigned(lvItem.Data) and (TBSCB(lvItem.Data).State = Finished) and CanOpen(TBSCB(lvItem.Data).Info.FileExt);
               pt := lvDownloads.ClientToScreen(lvItem.GetPosition());
               pmDownloadItem.Popup(pt.X, pt.Y);
            end;
      end
   else
      begin
         lvItem := lvDownloads.GetItemAt(MousePos.X, MousePos.Y);
         if (Assigned(lvItem)) then
            begin
               pmDownloadItem.Tag := lvItem.Index;
               miCancel.Enabled := Assigned(lvItem.Data) and (TBSCB(lvItem.Data).State = Finished);
               miOpen.Enabled := Assigned(lvItem.Data) and (TBSCB(lvItem.Data).State = Finished) and CanOpen(TBSCB(lvItem.Data).Info.FileExt);
               pt := lvDownloads.ClientToScreen(MousePos);
               pmDownloadItem.Popup(pt.X, pt.Y);
            end;
      end;
end;

procedure TDownloadForm.FormCreate(Sender: TObject);
begin
   finishedAll := false;
   btnCancel.Caption := 'Cancel All';
end;

procedure TDownloadForm.FormClose(Sender: TObject;
   var Action: TCloseAction);
begin
   if (not finishedAll) then
      IEDownload.Cancel;
   lvDownloads.Clear;
end;

end.

