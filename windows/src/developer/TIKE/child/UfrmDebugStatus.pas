(*
  Name:             UfrmDebugStatus
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      14 Sep 2006

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          14 Sep 2006 - mcdurdin - Initial version
                    04 Dec 2006 - mcdurdin - Tweak display of debug pages, to Unicode
                    04 Jan 2007 - mcdurdin - Remove old code
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit UfrmDebugStatus;  // I3306

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Buttons,
  Vcl.ComCtrls,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.ExtCtrls,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,
  Winapi.Messages,
  Winapi.Windows,

  DebugBitBtn,
  DebugDeadkeys,
  debugkeyboard,
  DebugListBox,
  DebugListView,
  KeymanDeveloperDebuggerMemo,
  Keyman.System.Debug.DebugCore,
  Keyman.System.Debug.DebugEvent,
  PaintPanel,
  RegressionTest,
  UfrmDebugStatus_CallStack,
  UfrmDebugStatus_Child,
  UfrmDebugStatus_Deadkeys,
  UfrmDebugStatus_Elements,
  UfrmDebugStatus_Events,
  UfrmDebugStatus_Key,
  UfrmDebugStatus_RegTest,
  UfrmTike;

const
  DebugTab_Status            = 0;
  DebugTab_Stores            = 1;
  DebugTab_CallStack         = 2;
  DebugTab_Deadkeys          = 3;
  DebugTab_RegressionTesting = 4;

type
  TfrmDebugStatus = class(TTikeForm)
    pagesDebug: TPageControl;
    tabDebugStores: TTabSheet;
    tabDebugCallStack: TTabSheet;
    tabDebugDeadkeys: TTabSheet;
    tabDebugRegressionTesting: TTabSheet;
    tabDebugKey: TTabSheet;
    tabDebugEvents: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FChildren: TArray<TfrmDebugStatus_Child>;
    FCallStack: TfrmDebugStatus_CallStack;
    FDeadKeys: TfrmDebugStatus_DeadKeys;
    FElements: TfrmDebugStatus_Elements;
    FRegTest: TfrmDebugStatus_RegTest;
    FKey: TfrmDebugStatus_Key;
    FEvents: TfrmDebugStatus_Events;
    FCurrentEvent: TDebugEvent;

    procedure SetDisplayFont(const Value: TFont);
    procedure SetCurrentEvent(const Value: TDebugEvent);

  protected
    function GetHelpTopic: string; override;
  public
    property DisplayFont: TFont write SetDisplayFont;
    property CurrentEvent: TDebugEvent read FCurrentEvent write SetCurrentEvent;
    procedure SetDebugCore(const Value: TDebugCore);
    procedure SetDebugKeyboard(const Value: TDebugKeyboard);
    procedure SetDebugMemo(const Value: TKeymanDeveloperDebuggerMemo);

    property Key: TfrmDebugStatus_Key read FKey;
    property Elements: TfrmDebugStatus_Elements read FElements;
    property CallStack: TfrmDebugStatus_CallStack read FCallStack;
    property DeadKeys: TfrmDebugStatus_DeadKeys read FDeadKeys;
    property RegTest: TfrmDebugStatus_RegTest read FRegTest;
    property Events: TfrmDebugStatus_Events read FEvents;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics,
  UfrmMain;

{$R *.dfm}

procedure TfrmDebugStatus.FormCreate(Sender: TObject);
begin
  pagesDebug.ActivePage := tabDebugKey;
  inherited;

  SetLength(FChildren, 6);
  FKey := TfrmDebugStatus_Key.Create(Self);
  FKey.Parent := tabDebugKey;
  FKey.Visible := True;
  FChildren[0] := FKey;

  FElements := TfrmDebugStatus_Elements.Create(Self);
  FElements.Parent := tabDebugStores;
  FElements.Visible := True;
  FChildren[1] := FElements;

  FCallStack := TfrmDebugStatus_CallStack.Create(Self);
  FCallStack.Parent := tabDebugCallStack;
  FCallStack.Visible := True;
  FChildren[2] := FCallstack;

  FDeadKeys := TfrmDebugStatus_DeadKeys.Create(Self);
  FDeadKeys.Parent := tabDebugDeadkeys;
  FDeadKeys.Visible := True;
  FChildren[3] := FDeadKeys;

  FRegTest := TfrmDebugStatus_RegTest.Create(Self);
  FRegTest.Parent := tabDebugRegressionTesting;
  FRegTest.Visible := True;
  FChildren[4] := FRegTest;

  FEvents := TfrmDebugStatus_Events.Create(Self);
  FEvents.Parent := tabDebugEvents;
  FEvents.Visible := True;
  FChildren[5] := FEvents;
end;

function TfrmDebugStatus.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_DebugStatus;
end;

procedure TfrmDebugStatus.SetCurrentEvent(const Value: TDebugEvent);
var
  child: TfrmDebugStatus_Child;
begin
  FCurrentEvent := Value;
  for child in FChildren do
    child.SetCurrentEvent(Value);
end;

procedure TfrmDebugStatus.SetDebugCore(const Value: TDebugCore);
var
  child: TfrmDebugStatus_Child;
begin
  for child in FChildren do
  begin
    child.SetDebugCore(Value);
  end;
end;

procedure TfrmDebugStatus.SetDebugKeyboard(const Value: TDebugKeyboard);
var
  child: TfrmDebugStatus_Child;
begin
  for child in FChildren do
  begin
    child.SetDebugKeyboard(Value);
  end;
end;

procedure TfrmDebugStatus.SetDebugMemo(const Value: TKeymanDeveloperDebuggerMemo);
var
  child: TfrmDebugStatus_Child;
begin
  for child in FChildren do
  begin
    child.SetDebugMemo(Value);
  end;
end;

procedure TfrmDebugStatus.SetDisplayFont(const Value: TFont);
var
  child: TfrmDebugStatus_Child;
begin
  for child in FChildren do
  begin
    child.SetDisplayFont(Value);
  end;
end;

end.
