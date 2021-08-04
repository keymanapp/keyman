(*
  Name:             UfrmDebugStatus_RegTest
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    03 Aug 2015 - mcdurdin - I4817 - Regression test buttons have incorrect labels
                    24 Aug 2015 - mcdurdin - I4839 - Keyman Developer crashes when starting regression test [CrashID:tike.exe_9.0.511.0_0058BC63_EInvalidOperation
                    ]
*)
unit UfrmDebugStatus_RegTest;  // I3306

interface

uses
  System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DebugListBox, Buttons, DebugBitBtn, ExtCtrls, Menus,
  regressiontest, UfrmDebugStatus_Child, debugdeadkeys,
  Keyman.System.Debug.DebugEvent;

type
  TfrmDebugStatus_RegTest = class(TfrmDebugStatus_Child)
    panRegTestCommands: TPanel;
    cmdRegTestStartStopLog: TDebugBitBtn;
    cmdRegTestStartStopTest: TDebugBitBtn;
    cmdRegTestOptions: TDebugBitBtn;
    lbRegTestLog: TDebugListBox;
    mnuRegTest: TPopupMenu;
    mnuDebugRegTestClearLog: TMenuItem;
    N3: TMenuItem;
    mnuDebugRegTestEdit: TMenuItem;
    N2: TMenuItem;
    mnuDebugRegTestOpen: TMenuItem;
    mnuDebugRegTestSave: TMenuItem;
    N1: TMenuItem;
    mnuDebugRegTestBatch: TMenuItem;
    dlgOpenRegtestBatch: TOpenDialog;
    dlgOpenRegTest: TOpenDialog;
    dlgSaveRegTest: TSaveDialog;
    procedure cmdRegTestStartStopLogClick(Sender: TObject);
    procedure cmdRegTestStartStopTestClick(Sender: TObject);
    procedure cmdRegTestOptionsClick(Sender: TObject);
    procedure mnuRegTestPopup(Sender: TObject);
    procedure mnuDebugRegTestClearLogClick(Sender: TObject);
    procedure mnuDebugRegTestEditClick(Sender: TObject);
    procedure mnuDebugRegTestOpenClick(Sender: TObject);
    procedure mnuDebugRegTestSaveClick(Sender: TObject);
    procedure mnuDebugRegTestBatchClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Regression testing }

    FRegTestLogging: Boolean;
    FRegTestRunning: Boolean;
    FRegTest: TRegressionTest;
    FRegTestNum: Integer;
    FDeadkeys: TDebugDeadkeyInfoList;

    procedure SetRegTestLogging(const Value: Boolean);
    procedure EnableRegTestControls;
    procedure SetRegTestRunning(const Value: Boolean);
    procedure StartRegTest;
    function RegTestValid: Boolean;
    procedure RegTestAddLog(rte: TRegressionTestEvent);
    procedure BatchRegTest;
    procedure SetDeadkeys(const Value: TDebugDeadkeyInfoList);

  protected
    function GetHelpTopic: string; override;
    procedure DebugKeyboardChanged; override;

  public
    procedure RegTestNextKey;
    procedure RegTestLogContext;
    procedure RegTestLogKey(key: PAIDebugKeyInfo);

    procedure RegTestSetup(SystemKeyboardName, FileName: string; IsANSITest: Boolean);

    property RegTestLogging: Boolean read FRegTestLogging write SetRegTestLogging;
    property RegTestRunning: Boolean read FRegTestRunning write SetRegTestRunning;
    property RegTestNum: Integer read FRegTestNum write FRegTestNum;

    property Deadkeys: TDebugDeadkeyInfoList read FDeadkeys write SetDeadkeys;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  KeyNames,
  TextFileFormat,
  UfrmEditor,
  UfrmRegressionTestFailure, UfrmMain, UfrmDebugStatus;

{$R *.dfm}

procedure TfrmDebugStatus_RegTest.BatchRegTest;
var
  i: Integer;
begin
  if RegTestNum < dlgOpenRegtestBatch.Files.Count then
  begin
    FRegTest.Clear;
    lbRegTestLog.Clear;
    try
      FRegTest.Load(dlgOpenRegtestBatch.Files[RegTestNum]);
      memoDebug.SetFocus;
      for i := 0 to FRegTest.Events.Count - 1 do
        RegTestAddLog(FRegTest.Events[i]);
      lbRegTestLog.ItemIndex := lbRegTestLog.Items.Count - 1;
    except
      on E:ERegressionTestFile do
      begin
        ShowMessage('The file '''+ExtractFileName(dlgOpenRegtestBatch.Files[RegTestNum])+
          ''' was not recognised as a valid regression test file: '#13#10+E.Message);
      end;
    end;
    EnableRegTestControls;

    RegTestRunning := True;
    if RegTestRunning then StartRegTest;
    RegTestNum := RegTestNum + 1;
  end
  else
  begin
    ShowMessage('All regression tests completed successfully.');
    RegTestNum := -1;
  end;
end;

{ Regression testing interfaces }

procedure TfrmDebugStatus_RegTest.mnuDebugRegTestBatchClick(Sender: TObject);
begin
  if dlgOpenRegtestBatch.Execute then
    if dlgOpenRegtestBatch.Files.Count > 0 then
    begin
      RegTestNum := 0;
      BatchRegTest;
    end;
end;

procedure TfrmDebugStatus_RegTest.mnuDebugRegTestClearLogClick(Sender: TObject);
begin
  FRegTest.Clear;
  lbRegTestLog.Clear;
  memoDebug.SetFocus;
  EnableRegTestControls;
end;

procedure TfrmDebugStatus_RegTest.mnuDebugRegTestEditClick(Sender: TObject);
begin
  with TfrmEditor.Create(frmKeymanDeveloper) do
  begin
    TextFileFormat := tffUTF16;
    SetEditorText(FRegTest.XMLWideString);
    //EditorFrame.Memo.SetTextBuf(PWideChar());
    EditorFormat := efXML;
  end;
end;

procedure TfrmDebugStatus_RegTest.mnuDebugRegTestOpenClick(Sender: TObject);
var
  i: Integer;
begin
  if dlgOpenRegTest.Execute then
  begin
    FRegTest.Clear;
    lbRegTestLog.Clear;
    try
      FRegTest.Load(dlgOpenRegTest.FileName);
      memoDebug.SetFocus;
      for i := 0 to FRegTest.Events.Count - 1 do
        RegTestAddLog(FRegTest.Events[i]);
      lbRegTestLog.ItemIndex := lbRegTestLog.Items.Count - 1;
    except
      on E:ERegressionTestFile do
      begin
        ShowMessage('The file '''+ExtractFileName(dlgOpenRegTest.FileName)+
          ''' was not recognised as a valid regression test file: '#13#10+E.Message);
      end;
    end;
    EnableRegTestControls;
  end;
end;

procedure TfrmDebugStatus_RegTest.mnuDebugRegTestSaveClick(Sender: TObject);
begin
  if dlgSaveRegTest.Execute then
  begin
    FRegTest.Save(dlgSaveRegTest.FileName);
    memoDebug.SetFocus;
  end;
end;

procedure TfrmDebugStatus_RegTest.mnuRegTestPopup(Sender: TObject);
begin
  mnuDebugRegTestClearLog.Enabled := not RegTestRunning and not RegTestLogging and (FRegTest.Events.Count > 0);
  mnuDebugRegTestEdit.Enabled := not RegTestRunning and not RegTestLogging and (FRegTest.Events.Count > 0);
  mnuDebugRegTestSave.Enabled := not RegTestRunning and not RegTestLogging and (FRegTest.Events.Count > 0);
  mnuDebugRegTestOpen.Enabled := not RegTestRunning and not RegTestLogging;
end;

procedure TfrmDebugStatus_RegTest.SetDeadkeys(
  const Value: TDebugDeadkeyInfoList);
begin
  FDeadkeys := Value;
end;

procedure TfrmDebugStatus_RegTest.SetRegTestLogging(const Value: Boolean);
begin
  FRegTestLogging := Value;
  if FRegTestLogging   // I4817
    then cmdRegTestStartStopLog.Caption := 'Stop &Log'
    else cmdRegTestStartStopLog.Caption := 'Start &Log';
  EnableRegTestControls;
end;

procedure TfrmDebugStatus_RegTest.SetRegTestRunning(const Value: Boolean);
begin
  FRegTestRunning := Value;
  if FRegTestRunning   // I4817
    then cmdRegTestStartStopTest.Caption := 'Stop Test'
    else cmdRegTestStartStopTest.Caption := 'Start Test';
  EnableRegTestControls;
end;

procedure TfrmDebugStatus_RegTest.StartRegTest;
begin
  FRegTest.CurrentEvent := 0;
  lbRegTestLog.ItemIndex := 0;
  memoDebug.Clear;
  memoDebug.SetFocus;
  Application.ProcessMessages;
  FRegTest.ExecuteCurrentEvent;
end;

function TfrmDebugStatus_RegTest.RegTestValid: Boolean;
var
  ws: WideString;
begin
  ws := memoDebug.Text;
  if Assigned(FDeadkeys) then
    FDeadkeys.FillDeadkeys(0, ws);
  Result := FRegTest.Events[FRegTest.CurrentEvent].PostContext = ws;
end;

procedure TfrmDebugStatus_RegTest.RegTestLogKey(key: PAIDebugKeyInfo);
var
  rte: TRegressionTestEvent;
begin
  if not RegTestLogging then Exit;
  rte := TRegressionTestEvent.Create(FRegTest);
  rte.VKey := key.VirtualKey;
  rte.ShiftState := key.Modifiers; // Core modifiers are compatible with Keyman32 shiftstates
  FRegTest.Events.Add(rte);
  RegTestAddLog(rte);
  lbRegTestLog.ItemIndex := lbRegTestLog.Items.Count - 1;
  EnableRegTestControls;
end;

procedure TfrmDebugStatus_RegTest.RegTestLogContext;
var
  ws: WideString;
  rte: TRegressionTestEvent;
begin
  if not RegTestLogging then Exit;
  if FRegTest.Events.Count = 0 then Exit; // Don't record the context when logging turned on half way through an event
  rte := FRegTest.Events[FRegTest.Events.Count - 1];

  ws := memoDebug.Text;
  if Assigned(FDeadkeys) then
    FDeadkeys.FillDeadkeys(0, ws);

  rte.PostContext := ws;
end;

procedure TfrmDebugStatus_RegTest.RegTestNextKey;
begin
  if not RegTestRunning then Exit;
  if not RegTestValid then
  begin
    RegTestRunning := False;
    lbRegTestLog.ItemIndex := FRegTest.CurrentEvent;
    with TfrmRegressionTestFailure.Create(Application.MainForm) do
    try
      memoExpected.Font := memoDebug.Font;
      memoActual.Font := memoDebug.Font;
      //if RegTestNum > 0
        //then
      //RegTestName := ExtractFileName(dlgOpenRegTestBatch.Files[RegTestNum-1]);
      TestName := ExtractFileName(FRegTest.TestFileName);

      ExpectedString := FRegTest.Events[FRegTest.CurrentEvent].PostContext;
      ActualString := memoDebug.Text;
      CurrentEvent := FRegTest.Events[FRegTest.CurrentEvent].ShiftStateAsString +
        SKeyNames[FRegTest.Events[FRegTest.CurrentEvent].VKey];
      ShowModal;
    finally
      Free;
    end;
  end
  else if FRegTest.CurrentEvent = FRegTest.Events.Count - 1 then
  begin
    FRegTest.CurrentEvent := 0;
    lbRegTestLog.ItemIndex := -1;
    RegTestRunning := False;
    if RegTestNum > 0
      then BatchRegTest
      else ShowMessage('Regression test was successful.');
  end
  else
  begin
    FRegTest.CurrentEvent := FRegTest.CurrentEvent + 1;
    //lbRegTestLog.ItemIndex := FRegTest.CurrentEvent;
    FRegTest.ExecuteCurrentEvent;
  end;
end;

procedure TfrmDebugStatus_RegTest.RegTestSetup(SystemKeyboardName,
  FileName: string; IsANSITest: Boolean);
begin
  FRegTest.SystemKeyboard := SystemKeyboardName;
  FRegTest.FileName := FileName;
  if IsANSITest
    then FRegTest.BeginMode := rtbmANSI
    else FRegTest.BeginMode := rtbmUnicode;
end;

procedure TfrmDebugStatus_RegTest.RegTestAddLog(rte: TRegressionTestEvent);
begin
  lbRegTestLog.Items.AddObject(rte.ShiftStateAsString + SKeyNames[rte.VKey], rte);
end;

procedure TfrmDebugStatus_RegTest.cmdRegTestOptionsClick(Sender: TObject);
var
  pt: TPoint;
  pmparams: TTPMParams;
begin
  mnuRegTestPopup(mnuRegTest);  // Must call explicitly because using TrackPopupMenuEx instead of .Popup
  pt := cmdRegTestOptions.ClientToScreen(Point(0, 0));
  pmparams.cbSize := SizeOf(TTPMParams);
  pmparams.rcExclude := Rect(pt.x, pt.y, pt.x+cmdRegTestOptions.Width, pt.y+cmdRegTestOptions.Height);
  TrackPopupMenuEx(mnuRegTest.Items.Handle, TPM_VERTICAL or TPM_LEFTBUTTON or TPM_LEFTALIGN,
    pt.x, pt.y, PopupList.Window, @pmparams);
end;

procedure TfrmDebugStatus_RegTest.cmdRegTestStartStopLogClick(Sender: TObject);
begin
  RegTestLogging := not RegTestLogging;
  try   // I4839
    memoDebug.SetFocus;
  except
    on E:EInvalidOperation do ;  // I4839
  end;
end;

procedure TfrmDebugStatus_RegTest.cmdRegTestStartStopTestClick(Sender: TObject);
begin
  RegTestRunning := not RegTestRunning;
  if RegTestRunning then StartRegTest;
end;

procedure TfrmDebugStatus_RegTest.DebugKeyboardChanged;
begin
  inherited;
  FRegTest.DebugKeyboard := DebugKeyboard;
end;

procedure TfrmDebugStatus_RegTest.EnableRegTestControls;
begin
  cmdRegTestStartStopTest.Enabled := not FRegTestLogging and (FRegTest.Events.Count > 0);
  cmdRegTestOptions.Enabled := not FRegTestLogging and not FRegTestRunning;
  cmdRegTestStartStopLog.Enabled := not FRegTestRunning;
end;

procedure TfrmDebugStatus_RegTest.FormCreate(Sender: TObject);
begin
  inherited;
  FRegTest := TRegressionTest.Create;
  RegTestLogging := False;
  RegTestRunning := False;
end;

procedure TfrmDebugStatus_RegTest.FormDestroy(Sender: TObject);
begin
  inherited;
  FreeAndNil(FRegTest);
end;

function TfrmDebugStatus_RegTest.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_DebugStatus_RegTest;
end;

end.
