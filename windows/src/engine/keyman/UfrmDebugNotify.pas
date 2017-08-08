(*
  Name:             UfrmDebugNotify
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 May 2010

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 May 2010 - mcdurdin - I2348 - Rework of debug log
                    04 May 2010 - mcdurdin - I2349 - Debug log capture hotkey
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
*)
unit UfrmDebugNotify;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, UserMessages;

type
  TfrmDebugNotify = class(TForm)
    memoNotes: TMemo;
    lblNotes: TLabel;
    editLogFileName: TEdit;
    chkNotepad: TCheckBox;
    cmdOK: TButton;
    lblLogFileName: TLabel;
    chkClipboard: TCheckBox;
    procedure FormShow(Sender: TObject);
    procedure cmdOKClick(Sender: TObject);
  private
    FLogFileHandle: THandle;
    FLogFileIndex: Integer;
    procedure SetLogFileIndex(const Value: Integer);
    procedure WMUserFormShown(var Message: TMessage); message WM_USER_FormShown;
    procedure SetClipboardToFilename(const Filename: string);
    { Private declarations }
  public
    { Public declarations }
    property LogFileHandle: THandle read FLogFileHandle write FLogFileHandle;
    property LogFileIndex: Integer read FLogFileIndex write SetLogFileIndex;
  end;

implementation

uses
  Clipbrd,
  DebugManager,
  ShellApi,
  ShlObj,
  utilexecute;

{$R *.dfm}

{ TfrmDebugNotify }

////////////////////////////////////////////////////////////////
// copies filenames from "Filenames" to the clipboard.
// "Filenames" can contain file- and directory names.
function Sto_CopyFilenamesToClipboard(Filenames: TStrings): Boolean;
var
  sFilenames: String;
  iIndex: Integer;
  hBuffer: HGLOBAL;
  pBuffer: PDropFiles;
begin
  // check entry conditions
  Result := (Filenames <> nil) and (Filenames.Count > 0);
  if (not Result) then Exit;
  // bring the filenames in a form,
  // separated by #0 and ending with a double #0#0
  sFilenames := '';
  for iIndex := 0 to Filenames.Count - 1 do
    sFilenames := sFilenames +
      ExcludeTrailingPathDelimiter(Filenames.Strings[iIndex]) + #0;
  sFilenames := sFilenames + #0;
  // allocate memory with the size of the "DropFiles" structure plus the
  // length of the filename buffer.
  hBuffer := GlobalAlloc(GMEM_MOVEABLE or GMEM_ZEROINIT,
    SizeOf(DROPFILES) + Length(sFilenames));
  try
  Result := (hBuffer <> 0);
  if (Result) then
  begin
    pBuffer := GlobalLock(hBuffer);
    try
    // prepare the "DROPFILES" structure
    pBuffer^.pFiles := SizeOf(DROPFILES);
    // behind the "DROPFILES" structure we place the filenames
    pBuffer := Pointer(Integer(pBuffer) + SizeOf(DROPFILES));
    CopyMemory(pBuffer, PChar(sFilenames), Length(sFilenames));
    finally
      GlobalUnlock(hBuffer);
    end;
    // copy buffer to the clipboard
    Clipboard.Open;
    try
    Clipboard.SetAsHandle(CF_HDROP, hBuffer);
    finally
      Clipboard.Close;
    end;
  end;
  except
    Result := False;
    // free only if handle could not be passed to the clipboard
    GlobalFree(hBuffer);
  end;
end;


procedure TfrmDebugNotify.SetClipboardToFilename(const Filename: string);
var
  s: TStrings;
begin
  s := TSTringList.Create;
  try
    s.Add(Filename);
    Sto_CopyFilenamesToClipboard(s);
  finally
    s.Free;
  end;
end;

type
  TDebugManagerEx = class(TDebugManager)
  end;

procedure TfrmDebugNotify.cmdOKClick(Sender: TObject);
begin
  TDebugManagerEx.WriteString(FLogFileHandle, memoNotes.Text);
  {
  buf := PChar(memoNotes.Text);
  buflen := Length(buf);
  WriteFile(FLogFileHandle, PChar(buf)^, buflen, n, nil);
  WriteFile(FLogFileHandle, #13#10, 2, n, nil);
  }
  TDebugManagerEx.CloseLogFile(FLogFileHandle);

  if chkNotepad.Checked then
    if not TUtilExecute.Shell(Handle, editLogFileName.Text, GetCurrentDir) then  // I3349
      ShowMessage(SysErrorMessage(GetLastError));

  if chkClipboard.Checked then
    SetClipboardToFilename(editLogFileName.Text);

  ModalResult := mrOk;
end;

procedure TfrmDebugNotify.FormShow(Sender: TObject);
begin
  PostMessage(Handle, WM_USER_FormShown, 0, 0);
end;

procedure TfrmDebugNotify.SetLogFileIndex(const Value: Integer);
begin
  FLogFileIndex := Value;
  editLogFileName.Text := TDebugManager.DebugLogFileName(Value);
end;

procedure TfrmDebugNotify.WMUserFormShown(var Message: TMessage);
var
  FThreadID: DWord;
begin
  FThreadID := GetWindowThreadProcessId(GetForegroundWindow);
  if FThreadID <> GetCurrentThreadId then
    AttachThreadInput(GetCurrentThreadId, FThreadID, True);
  BringToFront;
  SetFocus;
  if FThreadID <> GetCurrentThreadId then
    AttachThreadInput(GetCurrentThreadId, FThreadID, False);
end;

end.
