unit DownloadRequestForm;

interface

{$IFDEF DELPHI6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}

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
    FDestPath: string;
    FSender: TIEDownload;
  public
  { Public-Deklarationen }
    procedure SetInfo(Sender: TIEDownload);
    property DestinationPath: string read FDestPath;
  end;

var
  DownloadRequest: TDownloadRequest;
  DontOpenThisExts, OpenOnlyThisExts: TStringList;

function CanOpen(FileExt: string): Boolean;
function StrFormatByteSize(dw: DWORD; szBuf: PChar; uiBufSize: UINT):
  PChar; stdcall; external 'shlwapi.dll' name 'StrFormatByteSizeA';
function StrFormatKBSize(qdw: LONGLONG; szBuf: PChar; uiBufSize: UINT):
  PChar; stdcall; external 'shlwapi.dll' name 'StrFormatKBSizeA';

implementation

uses DownloadForm_U;

{$R *.DFM}

function CanOpen(FileExt: string): Boolean;
begin
  Result := ((DontOpenThisExts.IndexOf(FileExt) < 0) and
    ((OpenOnlyThisExts.Count = 0) or (OpenOnlyThisExts.IndexOf(FileExt) >= 0)));
end;

procedure TDownloadRequest.SetInfo(Sender: TIEDownload);
var
  sfi: TSHFileInfo;
  arrSize: array[0..255] of Char;
begin
  FSender := Sender;
  lbFromTxt.Caption := Sender.Url;
  lbNameTxt.Caption := Sender.FileName;
  btnOpen.Enabled := CanOpen(Sender.FileExtension);
  if (Sender.FileSize > 0) then
  begin
    StrFormatByteSize(Sender.FileSize, arrSize, Length(arrSize) - 1);
    lbSizeTxt.Caption := Trim(arrSize);
  end
  else
    lbSizeTxt.Caption := 'unknown';

 // get file information
  ZeroMemory(@sfi, SizeOf(sfi));
  if (SHGetFileInfo(
    PChar(Sender.FileName),
    FILE_ATTRIBUTE_NORMAL,
    sfi,
    SizeOf(sfi),
    SHGFI_ICON or SHGFI_TYPENAME or SHGFI_USEFILEATTRIBUTES) <> 0) then
  begin
    if (sfi.hIcon <> 0) then
      imgIcon.Picture.Icon.Handle := sfi.hIcon;
    if (sfi.szTypeName[0] <> #0) then
      lbTypeTxt.Caption := sfi.szTypeName;
  end;
end;

procedure TDownloadRequest.btnCancelClick(Sender: TObject);
begin
  if FSender.State = sReady then
    DownloadForm.Close;
  Self.Close;
end;

procedure TDownloadRequest.btnOpenClick(Sender: TObject);
begin
  Self.ModalResult := mrOpen;
end;

procedure TDownloadRequest.btnSaveClick(Sender: TObject);
begin
  dlgSave.FileName := FSender.FileName;
  if (dlgSave.Execute) then
  begin
    FSender.FileName := ExtractFileName(dlgSave.FileName);
    FSender.DownloadFolder := ExtractFilePath(dlgSave.FileName);
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

