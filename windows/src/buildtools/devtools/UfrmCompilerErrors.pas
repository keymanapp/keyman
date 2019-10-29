(*
  Name:             frmCompilerErrors
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      6 Oct 2008

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          06 Oct 2008 - mcdurdin - Initial version
                    25 May 2012 - mcdurdin - I3339 - V9.0 - Add GUI compiler wrapper for quicker review of hints and warnings
                    26 Jun 2012 - mcdurdin - I3378 - KM9 - Delphi compiler wrapper needs quiet mode
*)
unit UfrmCompilerErrors;  // I3339

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Contnrs, Grids, ExtCtrls, StdCtrls;

type
  TCompilerError = class
  private
    FFileName: string;
    FLineNumber: Integer;
    FErrorType: string;
    FMessage: string;
  public
    constructor Create(AFileName: string; ALineNumber: Integer; AErrorType, AMessage: string);
    property FileName: string read FFileName;
    property LineNumber: Integer read FLineNumber;
    property ErrorType: string read FErrorType;
    property Message: string read FMessage;
  end;

  TCompilerErrors = class(TObjectList)
  private
    function GetItem(Index: Integer): TCompilerError;
    procedure SetItem(Index: Integer; const Value: TCompilerError);
  public
    property Items[Index: Integer]: TCompilerError read GetItem write SetItem; default;
  end;

  TOpenFileRetryRecord = record
    Retries: Integer;
    LineNumber: Integer;
    Filename: string;
  end;

  TCompilerErrorsForm = class(TForm)
    gridErrors: TStringGrid;
    chkContinueBuild: TCheckBox;
    cmdClose: TButton;
    cmdRebuild: TButton;
    cmdViewErrorSource: TButton;
    tmrOpenFileRetry: TTimer;
    cmdCancelFileOpenRetry: TButton;
    lblFileOpenRetry: TLabel;
    procedure cmdCloseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure gridErrorsDblClick(Sender: TObject);
    procedure gridErrorsDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cmdRebuildClick(Sender: TObject);
    procedure gridErrorsClick(Sender: TObject);
    procedure tmrOpenFileRetryTimer(Sender: TObject);
    procedure cmdCancelFileOpenRetryClick(Sender: TObject);
    procedure gridErrorsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    FOpenFile: TOpenFileRetryRecord;
    FErrors: TCompilerErrors;
    FProjectFileName: string;
    FCompileOptions: string;
    procedure SetCompilerErrors(const Value: TStrings);
    procedure SetProjectFileName(const Value: string);
    procedure FillTable;
    procedure EnableControls;
    procedure FinishFileOpenRetries;
  public
    property CompilerErrors: TStrings write SetCompilerErrors;
    property ProjectFileName: string read FProjectFileName write SetProjectFileName;
    property CompileOptions: string read FCompileOptions write FCompileOptions;
  end;

function DevParseDCC32Log(ec: Integer; const dprfilename, errlog, rebuildparams: string; Quiet: Boolean): Boolean;  // I3378

var
  GlobalHintsIgnored: Boolean = False;

implementation

uses
  Vcl.Clipbrd,
  DevDelphiCompileWrapper,
  DevIncludePaths,
  DevUtils,
  RegExpr,
  Registry,
  RegistryKeys,
  shellapi;

{$IFDEF VER310}
const BDSPath = 'C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\bds.EXE';
{$ELSE}
{$IFDEF VER320}
const BDSPath = 'C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\bds.EXE';
{$ELSE}
{$IFDEF VER330}
const BDSPath = 'C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\bds.EXE';
{$ELSE}
ERROR: BDSPath is not defined
{$ENDIF}
{$ENDIF}
{$ENDIF}


{$R *.DFM}

function DevParseDCC32Log(ec: Integer; const dprfilename, errlog, rebuildparams: string; Quiet: Boolean): Boolean;  // I3378
var
  FErrLogText: TStringList;
  nFatal: Integer;
  nErrors: Integer;
  nWarnings: Integer;
  nHints: Integer;
begin
  Result := True;
  FErrLogText := TStringList.Create;
  try
    if FileExists(errlog) then
      FErrLogText.LoadFromFile(errlog);

    if Quiet then  // I3378
    begin
      nHints := 0;
      nWarnings := 0;
    end
    else
    begin
      nHints := Pos('Hint:', FErrLogText.Text);
      nWarnings := Pos('Warning:', FErrLogText.Text);
    end;
    nErrors := Pos('Error:', FErrLogText.Text);
    nFatal := Pos('Fatal:', FErrLogText.Text);

    if (nHints > 0) or (nWarnings > 0) or (nErrors > 0) or (nFatal > 0) or (ec <> 0) then
    begin
      with TCompilerErrorsForm.Create(Screen.ActiveForm) do
      try
        ProjectFileName := dprfilename;
        CompileOptions := rebuildparams;
        CompilerErrors := FErrLogText;
        if not GlobalHintsIgnored or not chkContinueBuild.Enabled then
          Result := ShowModal = mrOk;
      finally
        Free;
      end;
    end;
  finally
    FErrLogText.Free;
  end;
end;

{ TCompilerErrorsForm }

procedure TCompilerErrorsForm.SetCompilerErrors(const Value: TStrings);
var
  i: Integer;
  rError, rFatalError, rIgnore: TRegExpr;
begin
  FErrors.Clear;
  FillTable;

  rError := TRegExpr.Create;
  rFatalError := TRegExpr.Create;
  rIgnore := TRegExpr.Create;
  try
    rError.Expression := '^(.*)\((\d+)\) (\w+): (.+)$';
    rFatalError.Expression := '^Error: (.+)$';
    rIgnore.Expression :=
      '(^\s*$)|'+                     // blank line
      '(^DCC32)|'+                    // DCC32 command line
      '(^Borland Delphi Version)|'+   // Delphi version line
      '(^Copyright)|'+                // Delphi copyright line
      '(\s*exit code)';               // Exit response line

    for i := 0 to Value.Count - 1 do
    begin
      if rError.Exec(Value[i]) then
        FErrors.Add(TCompilerError.Create(rError.Match[1], StrToInt(rError.Match[2]), rError.Match[3], rError.Match[4]))
      else if rFatalError.Exec(Value[i]) then
        FErrors.Add(TCompilerError.Create(ExtractFileName(FProjectFileName), 0, 'Error', rFatalError.Match[1]))
      else if not rIgnore.Exec(Value[i]) then
        FErrors.Add(TCompilerError.Create('', 0, '', Value[i]));
    end;
  finally
    rIgnore.Free;
    rFatalError.Free;
    rError.Free;
  end;

  FillTable;
end;

procedure TCompilerErrorsForm.FillTable;
var
  i: Integer;
begin
  chkContinueBuild.Enabled := True;

  gridErrors.RowCount := FErrors.Count + 1;
  if FErrors.Count = 0 then Exit;
  gridErrors.FixedRows := 1;

  gridErrors.Cells[0, 0] := 'Type';
  gridErrors.Cells[1, 0] := 'File';
  gridErrors.Cells[2, 0] := 'Line';
  gridErrors.Cells[3, 0] := 'Message';

  gridErrors.ColWidths[0] := 100;
  gridErrors.ColWidths[1] := 150;
  gridErrors.ColWidths[2] := 60;
  gridErrors.ColWidths[3] := gridErrors.ClientWidth - 315;

  for i := 0 to FErrors.Count - 1 do
  begin
    if (FErrors[i].FErrorType = 'Error') or (FErrors[i].FErrorType = 'Fatal') or (Copy(FErrors[i].Message, 1, 6) = 'Fatal:') then chkContinueBuild.Enabled := False;
    gridErrors.Objects[0, i+1] := FErrors[i];
  end;
end;

procedure TCompilerErrorsForm.SetProjectFileName(const Value: string);
begin
  FProjectFileName := Value;
  Caption := 'Compile Errors - '+ExtractFileName(Value);
end;

{ TCompilerError }

constructor TCompilerError.Create(AFileName: string; ALineNumber: Integer;
  AErrorType, AMessage: string);
begin
  inherited Create;
  FFileName := AFileName;
  FLineNumber := ALineNumber;
  FErrorType := AErrorType;
  FMessage := AMessage;
end;

{ TCompilerErrors }

function TCompilerErrors.GetItem(Index: Integer): TCompilerError;
begin
  Result := inherited GetItem(Index) as TCompilerError;
end;

procedure TCompilerErrors.SetItem(Index: Integer; const Value: TCompilerError);
begin
  inherited SetItem(Index, Value);
end;

procedure TCompilerErrorsForm.cmdCloseClick(Sender: TObject);
begin
  if chkContinueBuild.Checked then ModalResult := mrOk else ModalResult := mrCancel;
end;

procedure TCompilerErrorsForm.FormCreate(Sender: TObject);
begin
  inherited;
  FErrors := TCompilerErrors.Create;
end;

procedure TCompilerErrorsForm.FormDestroy(Sender: TObject);
begin
  inherited;
  FErrors.Free;
end;

procedure TCompilerErrorsForm.gridErrorsDblClick(Sender: TObject);
var
  h: THandle;
  e: TCompilerError;
  CommandLine: string;
  n: Cardinal;
begin
  if gridErrors.Objects[0, gridErrors.Row] = nil then Exit;
  e := gridErrors.Objects[0, gridErrors.Row] as TCompilerError;
  if e.FileName = '' then Exit;

{$IFDEF VER320}
  CommandLine := ' -np -ns -ni -pDelphi "'+FProjectFileName+'"';
{$ELSE}
  // Assume this works for Delphi compiler version 330 (10.3.2 Rio / 20.0) and above
  CommandLine := ' -np -ns -p Delphi "'+FProjectFileName+'"';
{$ENDIF}

  with TRegistry.Create do
  try
    if OpenKey(SRegKey_DelphiProjectManager_CU, True) and ValueExists(SRegValue_CallbackWindow)
      then h := ReadInteger(SRegValue_CallbackWindow)
      else h := 0;
  finally
    Free;
  end;

  if h = 0 then
  begin
    n := ShellExecute(Handle, 'open', PChar(BDSPath), PChar(CommandLine), PChar(ExtractFileDir(FProjectFileName)), SW_SHOWNORMAL);
    if n < 32 then
    begin
      ShowMessage(SysErrorMessage(n));
      Exit;
    end;
  end;

  FOpenFile.Retries := 0;
  FOpenFile.FileName := e.FileName;
  FOpenFile.LineNumber := e.LineNumber;
  tmrOpenFileRetry.Enabled := True;
  cmdCancelFileOpenRetry.Visible := True;
  lblFileOpenRetry.Caption := 'Please wait while connecting to Delphi: '+IntToStr(30 - FOpenFile.Retries);

  tmrOpenFileRetryTimer(nil);
end;

procedure TCompilerErrorsForm.tmrOpenFileRetryTimer(Sender: TObject);
var
  h: THandle;
  cds: TCopyDataStruct;
  s: string;
  dw: DWord;
begin
  with TRegistry.Create do
  try
    if OpenKey(SRegKey_DelphiProjectManager_CU, True) and ValueExists(SRegValue_CallbackWindow)
      then h := ReadInteger(SRegValue_CallbackWindow)
      else h := 0;
  finally
    Free;
  end;

  if h <> 0 then
  begin
    cds.dwData := $1234;
    s := IntToStr(FOpenFile.LineNumber) + ';' + FOpenFile.FileName + ';' + FProjectFileName;
    cds.cbData := Length(s)*2+2;
    cds.lpData := PChar(s);
    if (SendMessageTimeout(h, WM_COPYDATA, Handle, Integer(@cds), 0, 5000, @dw) = 0) or (dw = 0) then
    begin
      ShowMessage('Failed to open file: '+SysErrorMessage(GetLastError));
    end;
    FinishFileOpenRetries;
  end
  else
  begin
    Inc(FOpenFile.Retries);
    lblFileOpenRetry.Caption := 'Please wait while connecting to Delphi: '+IntToStr(30 - FOpenFile.Retries);
    if FOpenFile.Retries = 30 then FinishFileOpenRetries;
  end;
end;

procedure TCompilerErrorsForm.FinishFileOpenRetries;
begin
  tmrOpenFileRetry.Enabled := False;
  cmdCancelFileOpenRetry.Visible := False;
  lblFileOpenRetry.Caption := '';
end;

(*  else
  begin
    cds.dwData := $1234;
    cds.cbData := Length(s)+1;
    cds.lpData := PChar(s);
    if (SendMessageTimeout(h, WM_COPYDATA, Handle, Integer(@cds), 0, 5000, dw) = 0) or (dw = 0) then
    begin
      n := ShellExecute(Handle, 'open', PChar(BDSPath), PChar(CommandLine), PChar(ExtractFileDir(FProjectFileName)), SW_SHOWNORMAL);
      if n < 32 then
        S4SShowMessage(SysErrorMessage(n));
    end;
  end;
end;*)

procedure TCompilerErrorsForm.gridErrorsDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  e: TCompilerError;
  ARect: TRect;
begin
  if ARow = 0 then
  begin
    gridErrors.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top + 2, gridErrors.Cells[ACol, ARow]);
    Exit;
  end;

  e := gridErrors.Objects[0, ARow] as TCompilerError;

  if e.FileName = '' then
  begin
    ARect := gridErrors.CellRect(0, ARow);
    ARect.Right := gridErrors.CellRect(3, ARow).Right;
    if gdSelected in State
      then gridErrors.Canvas.Font.Color := clWhite
      else gridErrors.Canvas.Font.Color := clBlack;
    gridErrors.Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top + 2, e.Message);
  end
  else
  begin
    if (e.ErrorType = 'Error') or (e.ErrorType = 'Fatal') then
    begin
      gridErrors.Canvas.Font.Color := clRed;
      if gdSelected in State then gridErrors.Canvas.Brush.Color := $a0a0ff;
    end
    else if e.ErrorType = 'Warning' then
    begin
      gridErrors.Canvas.Font.Color := $0078AA;
      if gdSelected in State then gridErrors.Canvas.Brush.Color := $a0ffff;
    end
    else if e.ErrorType = 'Hint' then
    begin
      gridErrors.Canvas.Font.Color := clGreen;
      if gdSelected in State then gridErrors.Canvas.Brush.Color := $a0ffa0;
    end
    else gridErrors.Canvas.Font.Color := clWhite;

    case ACol of
      0: gridErrors.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, e.ErrorType);
      1: gridErrors.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, ExtractFileName(e.FileName));
      2: gridErrors.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, IntToStr(e.LineNumber));
      3: gridErrors.Canvas.TextRect(Rect, Rect.Left+2, Rect.Top+2, e.Message);
    end;
  end;
end;

procedure TCompilerErrorsForm.gridErrorsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  e: TCompilerError;
begin
  if (Key = Ord('C')) and (shift = [ssCtrl]) then
  begin
    Key := 0;
    if gridErrors.Row > 0 then
    begin
      e := gridErrors.Objects[0, gridErrors.Row] as TCompilerError;
      Clipboard.AsText := e.Message;
    end
    else
      Clipboard.AsText := gridErrors.Cells[0, 0];
  end;
end;

procedure TCompilerErrorsForm.cmdCancelFileOpenRetryClick(Sender: TObject);
begin
  FinishFileOpenRetries;
end;

procedure TCompilerErrorsForm.cmdRebuildClick(Sender: TObject);
var
  n, ec: Integer;
  errlog: string;
  FErrLogText: TStrings;
begin
  gridErrors.RowCount := 1;
  gridErrors.Update;
  cmdRebuild.Update;

  Screen.Cursor := crHourglass;
  try
    errlog := ExtractFilePath(FProjectFileName)+'error.log';

    FErrLogText := TStringList.Create;
    try
      if not DevUtils.CommandExecute(dcc32+' '+FCompileOptions+' '+FProjectFileName,errlog,ExtractFilePath(FProjectFileName),SW_HIDE,ec) then
        FErrLogText.Text := 'Unable to start compiler'
      else
      begin
        if FileExists(errlog) then
          FErrLogText.LoadFromFile(errlog);

        n := Pos('Hint:', FErrLogText.Text);
        if n = 0 then n := Pos('Warning:', FErrLogText.Text);
        if n = 0 then n := Pos('Error:', FErrLogText.Text);
        if n = 0 then n := Pos('Fatal:', FErrLogText.Text);

        if (n = 0) and (ec = 0) then
        begin
          ModalResult := mrOk;
          Exit;
        end;
      end;

      SetCompilerErrors(FErrLogText)
    finally
      FErrLogText.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TCompilerErrorsForm.gridErrorsClick(Sender: TObject);
begin
  EnableControls;
end;

procedure TCompilerErrorsForm.EnableControls;
var
  e: Boolean;
begin
  e := (gridErrors.Objects[0, gridErrors.Row] <> nil) and ((gridErrors.Objects[0, gridErrors.Row] as TCompilerError).FileName <> '');

  cmdViewErrorSource.Enabled := e;
end;

end.
