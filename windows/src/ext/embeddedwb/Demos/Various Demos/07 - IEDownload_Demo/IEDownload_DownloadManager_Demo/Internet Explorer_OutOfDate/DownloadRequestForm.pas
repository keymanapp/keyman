{$WARN UNIT_PLATFORM OFF}
unit DownloadRequestForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	StdCtrls, ExtCtrls, IEDownload, ShellApi;

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
		FDestPath: String;
		FSender: TBSCB;
	public
		{ Public-Deklarationen }
		procedure SetInfo(Sender: TBSCB);
		property DestinationPath: String read FDestPath;
  end;

var
	DownloadRequest: TDownloadRequest;
  DontOpenThisExts, OpenOnlyThisExts: TStringList;


function CanOpen(FileExt: String): Boolean;
function StrFormatByteSize(dw: DWORD; szBuf: PChar; uiBufSize: UINT):
  PChar; stdcall; external 'shlwapi.dll' name 'StrFormatByteSizeA';
function StrFormatKBSize(qdw: LONGLONG; szBuf: PChar; uiBufSize: UINT):
  PChar; stdcall; external 'shlwapi.dll' name 'StrFormatKBSizeA';


implementation


uses DownloadForm_U;

{$R *.DFM}

function CanOpen(FileExt: String): Boolean;
begin
   result := ((DontOpenThisExts.IndexOf(FileExt) < 0) and
      ((OpenOnlyThisExts.Count = 0) or (OpenOnlyThisExts.IndexOf(FileExt) >= 0)));
end;

procedure TDownloadRequest.SetInfo(Sender: TBSCB);
var
	sfi: TSHFILEINFO;
   arrSize: array[0..255] of Char;
begin
	FSender := Sender;
	//lbFromTxt.Caption := Sender.UrlComponents.lpszHostName;
	lbNameTxt.Caption := Sender.Info.FileName;

   btnOpen.Enabled := CanOpen(Sender.Info.FileExt);

   if (Sender.Info.FileSize > 0) then begin
      StrFormatByteSize(Sender.Info.FileSize, arrSize, Length(arrSize)-1);
      lbSizeTxt.Caption := Trim(arrSize);
   end else
      lbSizeTxt.Caption := 'unknown';

	// get file information
	ZeroMemory(@sfi, sizeof(sfi));
	if (SHGetFileInfo(
		 PChar(Sender.Info.FileName),
		 FILE_ATTRIBUTE_NORMAL,
		 sfi,
		 sizeof(sfi),
		 SHGFI_ICON or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0) then
	begin
		if (sfi.hIcon <> 0) then
			imgIcon.Picture.Icon.Handle := sfi.hIcon;
		if (sfi.szTypeName[0] <> #0) then
			lbTypeTxt.Caption := sfi.szTypeName;
	end;
{	else
	begin
		ShowMessage(SysErrorMessage(GetLastError()));
	end;}
end;

procedure TDownloadRequest.btnCancelClick(Sender: TObject);
begin
   DownloadForm.Close;
   Close;
end;

procedure TDownloadRequest.btnOpenClick(Sender: TObject);
begin
	Self.ModalResult := mrOpen;
end;

procedure TDownloadRequest.btnSaveClick(Sender: TObject);
begin
	dlgSave.FileName := FSender.Info.FileName;
	if (dlgSave.Execute) then
	begin
		FSender.Info.FileName := ExtractFileName(dlgSave.FileName);
      FSender.Info.DownloadDir := ExtractFilePath(dlgSave.FileName);
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
