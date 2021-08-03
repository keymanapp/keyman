(*
  Name:             UfrmDebugStatus_Key
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      14 Sep 2006

  Modified Date:    3 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          14 Sep 2006 - mcdurdin - Initial version
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    03 Aug 2015 - mcdurdin - I4809 - Track keystrokes in debug status form
                    03 Aug 2015 - mcdurdin - I4815 - Add Restart Debugger button to debug status window
                    
                    03 Aug 2015 - mcdurdin - I4816 - Tidy up display of key in debug status window
                    
*)
unit UfrmDebugStatus_Key;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, PaintPanel, UfrmDebugStatus_Child, Vcl.StdCtrls,
  regressiontest,
  Keyman.System.Debug.DebugEvent,
  Keyman.System.Debug.DebugUIStatus;

type
  TfrmDebugStatus_Key = class(TfrmDebugStatus_Child)
    imgKeyCap: TImage;
    panKey: TPaintPanel;
    lblCurrentKeystroke: TLabel;   // I4809
    lbKeystrokeLog: TListBox;   // I4809
    lblKeystrokeLog: TLabel;   // I4809
    cmdRestartDebugger: TButton;   // I4815
    procedure panKeyPaint(Sender: TObject);
    procedure cmdRestartDebuggerClick(Sender: TObject);   // I4815
    procedure FormCreate(Sender: TObject);   // I4809
    procedure FormDestroy(Sender: TObject);   // I4809
  private
    KeyBitmap: TBitmap;
    FRegTest: TRegressionTest;   // I4809
    FUIStatus: TDebugUIStatus;   // I4809
    procedure SetUIStatus(const Value: TDebugUIStatus);   // I4809
  protected
    function GetHelpTopic: string; override;
  public
    procedure ClearLog;   // I4809
    procedure ShowKey(key: PAIDebugKeyInfo);
    property UIStatus: TDebugUIStatus read FUIStatus write SetUIStatus;   // I4809
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,

  KeyNames,
  UKeyBitmap;

{$R *.dfm}

procedure TfrmDebugStatus_Key.cmdRestartDebuggerClick(Sender: TObject);   // I4815
begin
  memoDebug.Clear;
  ClearLog;
  memoDebug.SetFocus;
end;

procedure TfrmDebugStatus_Key.FormCreate(Sender: TObject);
begin
  inherited;
  FRegTest := TRegressionTest.Create;   // I4809
end;

procedure TfrmDebugStatus_Key.FormDestroy(Sender: TObject);
begin
  inherited;
  FRegTest.Free;   // I4809
end;

function TfrmDebugStatus_Key.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_DebugStatus_Key
end;

procedure TfrmDebugStatus_Key.panKeyPaint(Sender: TObject);
begin
  panKey.Canvas.Brush.Color := clWindow;   // I4816
  panKey.Canvas.Pen.Color := clBlack;   // I4816
  panKey.Canvas.Rectangle(panKey.ClientRect);   // I4816
  if Assigned(KeyBitmap) then
    panKey.Canvas.Draw(2, 2, KeyBitmap);   // I4816
end;

procedure TfrmDebugStatus_Key.ClearLog;   // I4809
begin
  lbKeystrokeLog.Clear;
  FRegTest.Clear;
end;

procedure TfrmDebugStatus_Key.SetUIStatus(const Value: TDebugUIStatus);   // I4809
begin
  FUIStatus := Value;
  cmdRestartDebugger.Enabled := FUIStatus in [duiPaused, duiFocusedForInput, duiReadyForInput, duiTest];
  if cmdRestartDebugger.Enabled and (memoDebug <> nil) and (memoDebug.Text = '') then
    ClearLog;
end;

procedure TfrmDebugStatus_Key.ShowKey(key: PAIDebugKeyInfo);   // I4816
var
  rte: TRegressionTestEvent;
begin
  if not assigned(key) then
  begin
    FreeAndNil(KeyBitmap);
  end
  else
  begin
    CreateKeyBitmap(imgKeyCap.Picture.Bitmap, panKey.Color, key.Modifiers, key.VirtualKey, #0, KeyBitmap);

    rte := TRegressionTestEvent.Create(FRegTest);   // I4809
    rte.VKey := key.VirtualKey;
    rte.ShiftState := key.Modifiers; // these bitmasks are compatible
    FRegTest.Events.Add(rte);

    lbKeystrokeLog.ItemIndex := lbKeystrokeLog.Items.AddObject(rte.ShiftStateAsString + SKeyNames[rte.VKey], rte);   // I4809
  end;

  panKey.Invalidate;
end;

end.
