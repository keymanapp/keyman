{$WARN UNIT_PLATFORM OFF}
unit DownloadRequestForm;

interface

uses
   Windows, SysUtils, Classes, Graphics, Controls, Forms,
   Dialogs, StdCtrls, ExtCtrls;

const
   mrOpen = mrYesToAll + 1;
   mrSave = mrOpen + 1;

type
   TDownloadRequest = class(TForm)
      btnOpen: TButton;
      btnSave: TButton;
      btnCancel: TButton;
      lbQuestion: TLabel;
      lbName: TLabel;
      lbType: TLabel;
      lbFrom: TLabel;
      imgIcon: TImage;
      lbNameTxt: TLabel;
      lbTypeTxt: TLabel;
      lbFromTxt: TLabel;
      dlgSave: TSaveDialog;
      lbSize: TLabel;
      lbSizeTxt: TLabel;
      procedure btnCancelClick(Sender: TObject);
      procedure btnOpenClick(Sender: TObject);
      procedure btnSaveClick(Sender: TObject);
      procedure FormDestroy(Sender: TObject);
   private
  { Private-Deklarationen }
      FDownloadFolder: string;
      FFileName: string;
   public
  { Public-Deklarationen }
      property drDownloadFolder: string read FDownloadFolder;
      property drFileName: string read FFileName;
   end;

var
   DownloadRequest: TDownloadRequest;
   DontOpenThisExts, OpenOnlyThisExts: TStringList;

function CanOpen(FileExt: string): Boolean;

implementation

uses DownloadForm_U;

{$R *.DFM}

function CanOpen(FileExt: string): Boolean;
begin
   Result := ((DontOpenThisExts.IndexOf(FileExt) < 0) and
      ((OpenOnlyThisExts.Count = 0) or (OpenOnlyThisExts.IndexOf(FileExt) >= 0)));
end;

procedure TDownloadRequest.btnCancelClick(Sender: TObject);
begin
  // if FSender.State = sReady then
      DownloadForm.Close;
   Self.Close;
end;

procedure TDownloadRequest.btnOpenClick(Sender: TObject);
begin
   Self.ModalResult := mrOpen;
end;

procedure TDownloadRequest.btnSaveClick(Sender: TObject);
begin
   dlgSave.FileName := lbNameTxt.Caption;
   if (dlgSave.Execute) then
      begin
         FFileName := ExtractFileName(dlgSave.FileName);
         FDownloadFolder := ExtractFilePath(dlgSave.FileName);
         Self.ModalResult := mrSave;
      end;
end;

procedure TDownloadRequest.FormDestroy(Sender: TObject);
begin
   if (imgIcon.Picture.Icon.Handle <> 0) then
      DestroyIcon(imgIcon.Picture.Icon.Handle);
end;

initialization
   DontOpenThisExts := TStringList.Create();
   OpenOnlyThisExts := TStringList.Create();
finalization
   DontOpenThisExts.Free();
   OpenOnlyThisExts.Free();
end.

