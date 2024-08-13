(*
  Name:             UfrmRegressionTests
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:
  Description:
  Create Date:      3 May 2011

  Modified Date:    22 Jun 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
                    22 Jun 2015 - mcdurdin - I4771 - Update regressiontest app for Engine and Developer 9.0
*)
{
  // *** TODO: need to add switching to correct underlying language.  -- SEARCH FOR "TODO"

  Test application for Keyman:

   1. Has a directory of keyboards, each of which starts with a comment explaining the testing procedure
   2. The comment has the following format:

       c RegressionTest: name of test
       c Encoding: ANSI|Unicode
       c Description: Information on the test,
       c Description: What we are testing for
       c Description: and so on...
       c Procedure: Press key x, key y, key z
       c Procedure: ..
       c FinalOutput: xxxxxx (extended string? -- possibly U+xxxx, ...)
       c [Language: x9, x1] -- syskbd that should be switched to before test runs [e.g. English, German]
                            -- switch to 0x0409 (US English) if not specified
       c [FinalPosition: n] -- if empty assumed last char
       c [FinalContext: xxxxxxx] -- if empty assumed same as output

       c .... other stuff after blank line
}

unit UfrmRegressionTests;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Menus, PlusMemo;

type
  TfrmRegressionTests = class(TForm)
    Splitter1: TSplitter;
    Panel1: TPanel;
    lbFiles: TListBox;
    Panel2: TPanel;
    lblTests: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    lbllDescript: TLabel;
    lblInstructions: TLabel;
    editTestName: TEdit;
    memoDescription: TMemo;
    memoInstructions: TMemo;
    Label3: TLabel;
    Label5: TLabel;
    editDesiredOutput: TPlusMemo;
    editStatus: TEdit;
    memo: TPlusMemo;
    Label6: TLabel;
    editEncoding: TEdit;
    Bevel1: TBevel;
    Label9: TLabel;
    editDesiredPosition: TEdit;
    Panel6: TPanel;
    Splitter2: TSplitter;
    memoLog: TMemo;
    tmrKey: TTimer;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    mnFileRunSelectedTest: TMenuItem;
    mnuFileRunAllTests: TMenuItem;
    N1: TMenuItem;
    mnuFileExit: TMenuItem;
    Cancel: TButton;
    cmdActivate: TButton;
    dlgOpen: TOpenDialog;
    mnuFileOpen: TMenuItem;
    N2: TMenuItem;
    Label2: TLabel;
    editFont: TEdit;
    cmdEdit: TButton;
    procedure FormCreate(Sender: TObject);
    procedure lbFilesClick(Sender: TObject);
    procedure memoANSIChange(Sender: TObject);
    procedure memoChange(Sender: TObject);
    procedure memoANSIExit(Sender: TObject);
    procedure memoExit(Sender: TObject);
    procedure tmrKeyTimer(Sender: TObject);
    procedure mnFileRunSelectedTestClick(Sender: TObject);
    procedure mnuFileRunAllTestsClick(Sender: TObject);
    procedure mnuFileExitClick(Sender: TObject);
    procedure CancelClick(Sender: TObject);
    procedure cmdActivateClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mnuFileOpenClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmdEditClick(Sender: TObject);
  private
    FAllTests: Boolean;
    FAutoProcList, FAutoProc, ActiveKeyboard, FilePath: string;
    FWideDesiredOutput: WideString;
    procedure RefreshFileList;
    function LoadFile(const fname: string): Boolean;
    function DecodeXString(s: string; var sout: widestring): string;
    procedure UpdateStatus;
    function ActivateKeyboard: Boolean;
    function CompileKeyboard: Boolean;
    function DeactivateKeyboard: Boolean;
    procedure Logmessage(const msg: string);
    procedure NextTest;
    procedure NextKey;
    function WaitForIdleKeyboard: Boolean;
  end;

var
  frmRegressionTests: TfrmRegressionTests;

implementation

uses
  System.Win.ComObj,
  Winapi.ActiveX,

  keymanapi_TLB,
  DebugUtils,
  ErrorControlledRegistry,
  keyman32_int,
  msctf,
  RegistryKeys,
  shellapi,
  UfrmRegressionTestsWaitForIdle,
  utilexecute,
  VKeys;

{$R *.DFM}

const
  SRegKey_RegressionTest = SRegKey_KeymanDeveloper + 'Regression Test';
  SRegValue_RegressionTestPath = 'test path';

procedure TfrmRegressionTests.FormCreate(Sender: TObject);
begin
  InitKeyman;
  FilePath := ExtractFilePath(ParamStr(0))+'\tests\';
  with TRegistryErrorControlled.Create do  // I2890
  try
    RootKey := HKEY_CURRENT_USER;
    if OpenKeyReadOnly(SRegKey_RegressionTest) then
      if ValueExists(SRegValue_RegressionTestPath) then
        FilePath := IncludeTrailingPathDelimiter(ReadString(SRegValue_RegressionTestPath));
  finally
    Free;
  end;

  RefreshFileList;
  if lbFiles.Items.Count > 0 then
  begin
    lbFiles.ItemIndex := 0;
    lbFilesClick(lbFiles);
  end;
end;

procedure TfrmRegressionTests.RefreshFileList;
var
  f: TSearchRec;
  n: Integer;
begin
  DeactivateKeyboard;
  lbFiles.Clear;
  n := FindFirst(FilePath+'*.kmn', 0, f);
  if n = 0 then
  begin
    while n = 0 do
    begin
      lbFiles.Items.Add(ChangeFileExt(f.Name, ''));
      n := FindNext(f);
    end;
    FindClose(f);
  end;
end;

procedure TfrmRegressionTests.lbFilesClick(Sender: TObject);
begin
  if lbFiles.ItemIndex < 0 then Exit;
  ActiveKeyboard := lbFiles.Items[lbFiles.ItemIndex];
  LoadFile(FilePath + ActiveKeyboard + '.kmn');
end;

type
  TFileEntryName = (fenName, fenFont, fenEncoding, fenDescription,
                    fenProcedure, fenAutoProc, fenFinalOutput, fenFinalContext, fenFinalPosition);
  TFileEntry = class
    Name: TFileEntryName;
    Value: string;
  end;

const
  FileEntryNames: array[TFileEntryName] of string = (
    'RegressionTest', 'Font', 'Encoding', 'Description',
    'Procedure', 'AutoProc', 'FinalOutput', 'FinalContext', 'FinalPosition'
    );

function TfrmRegressionTests.LoadFile(const fname: string): Boolean;
var
  Found: Boolean;
  f: TextFile;
  s, t: string;
  fileentries: TList;
  fe: TFileEntry;
  fen, fenp: TFileEntryName;
  n, i: Integer;
begin
  Result := False;
  fileentries := TList.Create;
  try
    AssignFile(f, fname);
    Reset(f);

    { Read the file entries from the file and abort if bad data found }

    try
      while not EOF(f) do
      begin
        readln(f, s); s := Trim(s);
        if s = '' then Break; // End of our introductory comments
        if LowerCase(Copy(s, 1, 2)) <> 'c ' then
        begin
          LogMessage('Invalid regression test file: '+fname+': Non-comment line found at start of file');
          Exit;
        end;
        Delete(s, 1, 2);
        n := Pos(':', s);
        if n = 0 then
        begin
          LogMessage('Invalid regression test file: '+fname+': Colon not found');
          Exit;
        end;

        Found := False;
        t := LowerCase(Copy(s, 1, n-1));
        Delete(s, 1, n); s := Trim(s);
        fenp := fenName;
        for fen := Low(TFileEntryName) to High(TFileEntryName) do
          if LowerCase(FileEntryNames[fen]) = t then
          begin
            Found := True;
            fenp := fen; Break;
          end;

        if not Found then
        begin
          LogMessage('Invalid regression test file: '+fname+': File entry "'+t+'" invalid.');
          Exit;
        end;

        Found := False;

        for i := 0 to fileentries.Count - 1 do
          if TFileEntry(fileentries[i]).Name = fenp then
          begin
            Found := True;
            with TFileEntry(fileentries[i]) do
              Value := Value + #13#10 + s;
            Break;
          end;
        if not Found then
        begin
          fe := TFileEntry.Create;
          fe.Name := fenp;
          fe.Value := s;
          fileentries.Add(fe);
        end;
      end;
    finally
      CloseFile(f);
    end;

    { Run through the fileentries and fill in the right hand page and stored values }

    editTestName.Text := '';
    memo.Clear;
    editEncoding.Text := '';
    editFont.Text := '';
    memoDescription.Text := '';
    memoInstructions.Text := '';
    editDesiredOutput.SetTextBuf('');// := '';
    editDesiredPosition.Text := '';

    for i := 0 to fileentries.Count - 1 do
    begin
      fe := TFileEntry(fileentries[i]);
      case fe.Name of
        fenName:
          editTestName.Text := fe.Value;
        fenFont:
          begin
            memo.Font.Name := fe.Value;
            memo.Font.Size := -40;
            editFont.Text := fe.Value;
            editDesiredOutput.Font.Name := fe.Value;
            editDesiredOutput.Font.Size := -20;
          end;
        fenEncoding:
          begin
//            fv := LowerCase(fe.Value) = 'unicode';
//            memo.AllowUnicodeInput := fv;
            editEncoding.Text := fe.Value;
          end;
        fenDescription:
          memoDescription.Text := fe.Value;
        fenProcedure:
          memoInstructions.Text := fe.Value;
        fenFinalOutput:
          begin
            DecodeXString(fe.Value, FWideDesiredOutput);
            editDesiredOutput.SetTextBuf(PWideChar(FWideDesiredOutput));
          end;
        //fenFinalContext:
        //  editDesiredContext.Text := DecodeXString(fe.Value, FWideDesiredContext);
        fenFinalPosition:
          editDesiredPosition.Text := fe.Value;
        fenAutoProc:
          begin FAutoProc := fe.Value; memoInstructions.Text := fe.Value; end;
      end;
    end;

    memo.Clear;

    UpdateStatus;

    Result := True;
  finally
    for i := 0 to fileentries.Count -1 do
      TFileEntry(fileentries[i]).Free;
    fileentries.Free;
  end;
end;

function CompilerMessage(line: Integer; msgcode: LongWord; text: PANsiChar): Integer; stdcall;
begin
  frmRegressionTests.LogMessage(string(AnsiString(text)));
  Result := 1;
end;

function TfrmRegressionTests.CompileKeyboard: Boolean;
var
  FCompilingFile, KMXFileName: String;
begin
  Result := False;

  FCompilingFile := FilePath + ActiveKeyboard + '.kmn';
  KMXFileName := ChangeFileExt(FCompilingFile, '.kmx');

  if CompileKeyboardFile(PChar(FCompilingFile), PChar(KMXFileName),
      True, CompilerMessage) <> 0 then
    Result := True
  else
    LogMessage('Unable to compile keyboard.');
end;

function TfrmRegressionTests.ActivateKeyboard: Boolean;
var
  kbd: IKeymanKeyboardInstalled;
begin
  kbd := TDebugUtils.GetDebugHostKeyboard;
  if not Assigned(kbd) then
  begin
    LogMessage('Debug Host keyboard not installed or configured.');
    Exit(False);
  end;

  if not SUCCEEDED(TDebugUtils.SelectTSFProfileForKeyboardLanguage((kbd as IKeymanKeyboardInstalled3).Languages.Items[1])) then
  begin
    LogMessage('Could not activate Debug Host keyboard');
    Exit(False);
  end;

  Result := Keyman_ForceKeyboard(FilePath + ActiveKeyboard + '.kmx');
//Result := False;
  if not Result then LogMessage('Unable to activate keyboard.');
end;

function TfrmRegressionTests.DeactivateKeyboard: Boolean;
var
  profile: TDebugUtilProfile;
begin
//  Result := False;
  profile.Profile.dwProfileType := TF_PROFILETYPE_KEYBOARDLAYOUT;
  profile.Profile.langid := $C09;
  profile.Profile.HKL := $4090C09;
  TDebugUtils.SetActiveTSFProfile(profile);

  Result := Keyman_StopForcingKeyboard;
end;

procedure TfrmRegressionTests.memoANSIChange(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmRegressionTests.memoChange(Sender: TObject);
begin
  UpdateStatus;
end;

procedure TfrmRegressionTests.UpdateStatus;
var
  FSuccess: Boolean;
begin
//  if memo.AllowUnicodeInput then
//  begin
    FSuccess :=
      (memo.Text = FWideDesiredOutput) and
      (memo.SelStart = StrToIntDef(editDesiredPosition.Text, memo.SelStart));
//  end
//  else
//  begin
//    FSuccess :=
//      (memo.Lines.Text = editDesiredOutput.WideText) and
//      (memo.SelStart = StrToIntDef(editDesiredPosition.Text, memo.SelStart));
//  end;

  if FSuccess then
  begin
    editStatus.Text := 'SUCCESSFUL!';
    editStatus.Color := clLime;
  end
  else
  begin
    editStatus.Text := 'NOT YET SUCCESSFUL';
    editStatus.Color := clRed;
  end;
end;

function wchr(n: Integer): WideChar;
begin
  Result := WideChar(n);
end;

function TfrmRegressionTests.DecodeXString(s: string; var sout: widestring): string;
var
  t, InQuotes: string;
  n: Integer;
begin
  Result := '';
  sout := '';
  InQuotes := '';
  while s <> '' do
  begin
    if InQuotes <> '' then
    begin
      if s[1] = InQuotes then
      begin
        InQuotes := '';
      end
      else
      begin
        Result := Result + s[1];
        sout := sout + s[1];
      end;
      Delete(s, 1, 1);
    end
    else if (s[1] = 'U') or (s[1] = 'u') then
    begin
      Result := Result + '?';
      sout := sout + wchr(StrToInt('$' + Copy(s, 3, 4)));
      delete(s, 1, 6);
    end
    else if (s[1] = 'D') or (s[1] = 'd') then
    begin
      n := Pos(' ', s);
      if n = 0 then n := Length(s);
      t := Copy(s, 2, n-2);
      Result := Result + Chr(StrToInt(t));
      sout := sout + WChr(StrToInt(t));
      Delete(s, 1, n);
    end
    else if (s[1] = 'X') or (s[1] = 'x') then
    begin
      n := Pos(' ', s);
      if n = 0 then n := Length(s);
      t := Copy(s, 2, n-2);
      Result := Result + Chr(StrToInt('$'+t));
      sout := sout + WChr(StrToInt('$'+t));
      Delete(s, 1, n);
    end
    else if CharInSet(s[1], ['"', '''']) then
    begin
      InQuotes := s[1]; Delete(s,1,1);
    end
    else if s[1] = ' ' then
    begin
      Delete(s,1,1);
    end
    else
      Exit;
  end;
end;

procedure TfrmRegressionTests.memoANSIExit(Sender: TObject);
begin
  DeactivateKeyboard;
end;

procedure TfrmRegressionTests.memoExit(Sender: TObject);
begin
  DeactivateKeyboard;
end;

procedure TfrmRegressionTests.Logmessage(const msg: string);
begin
  memoLog.Lines.Add(msg);
end;

procedure TfrmRegressionTests.tmrKeyTimer(Sender: TObject);
begin
  if FAutoProcList = ''
    then NextTest
    else NextKey;
end;

procedure TfrmRegressionTests.NextTest;
var
  keys: TKeyboardState;
  i: Integer;
  msg: TMsg;
begin
  for i := 0 to 255 do keys[i] := 0;
  SetKeyboardState(keys);

  if DeactivateKeyboard then Exit; // wait for next tick to start next keyboard

  if lbFiles.ItemIndex >= 0 then
  begin
    if editStatus.Text = 'SUCCESSFUL!'
      then LogMessage(lbFiles.Items[lbFiles.ItemIndex] + ' was successful.')
      else LogMessage(lbFiles.Items[lbFiles.ItemIndex] + ' FAILED.');
  end;

  tmrKey.Enabled := False;
  FAutoProcList := '';
  if not FAllTests then
  begin
    LogMessage('Test complete!');
    Exit;
  end;

  if lbFiles.ItemIndex = lbFiles.Items.Count - 1 then
  begin
    LogMessage('All tests complete');
    Exit;
  end;

  lbFiles.ItemIndex := lbFiles.ItemIndex + 1;
  lbFilesClick(lbFiles);

  if not CompileKeyboard then
  begin
    LogMessage('Unable to compile keyboard ' + lbFiles.Items[lbFiles.ItemIndex]);
    //tmrKey.Enabled := True;
    Exit;
  end;
  if not ActivateKeyboard then
  begin
    LogMessage('Unable to activate keyboard ' + lbFiles.Items[lbFiles.ItemIndex]);
    //tmrKey.Enabled := True;
    Exit;
  end;

  memo.Clear;
  memo.SetFocus;

  while PeekMessage(msg, 0, 0, 0, PM_NOREMOVE) do Application.ProcessMessages;

  FAutoProcList := FAutoProc;
  tmrKey.Enabled := True;
end;

procedure TfrmRegressionTests.NextKey;
var
  right: Boolean;
  s: string;
  i, n, sp, flags: Integer;
  sh: Char;
begin
  case UpCase(FAutoProcList[1]) of
    'S', 'C', 'A':
      begin
        sh := UpCase(FAutoProcList[1]);
        if sh = 'S' then sp := VK_SHIFT else if sh = 'C' then sp := VK_CONTROL else sp := VK_MENU;
        Delete(FAutoProcList, 1, 1);
        if FAutoProcList = '' then Exit;
        right := False;
        if UpperCase(FAutoProcList[1]) = 'R' then begin right := True; Delete(FAutoProcList, 1, 1); end
        else if UpperCase(FAutoProcList[1]) = 'L' then Delete(FAutoProcList, 1, 1);
        if FAutoProcList = '' then Exit;
        if FAutoProcList[1] = '+' then flags := 0 else flags := KEYEVENTF_KEYUP;
        Delete(FAutoProcList, 1, 1);
        if right then flags := flags or KEYEVENTF_EXTENDEDKEY;
        keybd_event(sp, 0, flags, 0);
      end;
    'U':
      begin
        i := StrToInt(Copy(FAutoProcList, 2, 3));
        Delete(FAutoProcList, 1, 4);
        keybd_event(i, 0, KEYEVENTF_KEYUP, 0);
      end;
    'K':
      begin
        n := Pos(' ', FAutoProcList);
        if n = 0 then n := Length(FAutoProcList)+1;
        s := Trim(UpperCase(Copy(FAutoProcList, 1, n-1)));
        Delete(FAutoProcList, 1, n);
        for i := 0 to 255 do
          if VKeyNames[i] = s then
          begin
            keybd_event(i, 0, 0, 0);
            FAutoProcList := Format('U%3.3d', [i]) + FAutoProcList;
            Exit;
          end;
      end;
    ' ':
      Delete(FAutoProcList, 1, 1);
  else
    NextTest;
  end;
end;

procedure TfrmRegressionTests.mnFileRunSelectedTestClick(Sender: TObject);
var
  msg: TMsg;
begin
  FAllTests := False;

  if not WaitForIdleKeyboard then Exit;

  if not CompileKeyboard then Exit;
  if not ActivateKeyboard then Exit;

  memo.Clear;
  memo.SetFocus;

  while PeekMessage(msg, 0, 0, 0, PM_NOREMOVE) do Application.ProcessMessages;
  FAutoProcList := FAutoProc;
  tmrKey.Enabled := True;
end;

procedure TfrmRegressionTests.mnuFileRunAllTestsClick(Sender: TObject);
begin
  lbFiles.ItemIndex := -1;
  FAllTests := True;
  memoLog.Clear;
  if not WaitForIdleKeyboard then Exit;
  NextTest;
end;

procedure TfrmRegressionTests.mnuFileExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmRegressionTests.CancelClick(Sender: TObject);
begin
  FAutoProcList := '';
  tmrKey.Enabled := False;
  DeactivateKeyboard;
end;

procedure TfrmRegressionTests.cmdActivateClick(Sender: TObject);
begin
  if not CompileKeyboard then Exit;

  memo.Clear;
  memo.SetFocus;

  if not ActivateKeyboard then Exit;
end;

procedure TfrmRegressionTests.FormDestroy(Sender: TObject);
begin
  DeactivateKeyboard;
//TODO:  Keyman_Exit;
end;

procedure TfrmRegressionTests.mnuFileOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    FilePath := ExtractFilePath(dlgOpen.FileName);
    with TRegistryErrorControlled.Create do  // I2890
    try
      RootKey := HKEY_CURRENT_USER;
      if not OpenKey(SRegKey_RegressionTest, True) then  // I2890
        RaiseLastRegistryError;
      WriteString(SRegValue_RegressionTestPath, FilePath);
    finally
      Free;
    end;
    RefreshFileList;
  end;
end;

function TfrmRegressionTests.WaitForIdleKeyboard: Boolean;
begin
  // Test to make sure that the keyboard state is:
  //   all keys up
  //   caps, num, scrl off
  Result := False;
  with TfrmRegressionTestsWaitForIdle.Create(Self) do
  try
    if ShouldShowModal then
      if ShowModal = mrCancel then Exit;
  finally
    Free;
  end;
  Result := True;
end;

procedure TfrmRegressionTests.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := WaitForIdleKeyboard;
end;

procedure TfrmRegressionTests.cmdEditClick(Sender: TObject);
var
  s, t: string;
begin
  if lbFiles.ItemIndex < 0 then Exit;
  s := lbFiles.Items[lbFiles.ItemIndex];
  if not TUtilExecute.Shell(Handle, '"'+FilePath + s + '.kmn"', FilePath) then
  begin
    with TRegistryErrorControlled.Create do // I2890
    try
      RootKey := HKEY_LOCAL_MACHINE;
      if OpenKeyReadOnly(SRegKey_KeymanDeveloper) then
        if ValueExists(SRegValue_RootPath) then
        begin
          t := IncludeTrailingPathDelimiter(ReadString(SRegValue_RootPath));

          if not TUtilExecute.Shell(Handle, t + 'tike.exe', FilePath, '"'+FilePath + s + '.kmn"') then
          begin
            ShowMessage('Could not find Keyman Developer to edit the keyboard.');
            Exit;
          end;
        end;
    finally
      Free;
    end;
  end;
end;

end.
