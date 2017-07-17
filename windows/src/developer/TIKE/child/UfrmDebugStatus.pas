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
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, DebugBitBtn, ExtCtrls, DebugListBox, ComCtrls,
  DebugListView, PaintPanel, DebugDeadkeys, UfrmDebug,
  debugging, debugkeyboard, RegressionTest,
  UfrmDebugStatus_Key,
  UfrmDebugStatus_Elements,
  UfrmDebugStatus_CallStack,
  UfrmDebugStatus_Deadkeys,
  UfrmDebugStatus_RegTest, UfrmTike;

const
  DebugTab_Status            = 0;
  DebugTab_Stores            = 1;
  DebugTab_CallStack         = 2;
  DebugTab_Deadkeys          = 3;
  DebugTab_RegressionTesting = 4;

type
  TfrmDebugStatus = class(TTIKEForm)
    pagesDebug: TPageControl;
    tabDebugStores: TTabSheet;
    tabDebugCallStack: TTabSheet;
    tabDebugDeadkeys: TTabSheet;
    tabDebugRegressionTesting: TTabSheet;
    tabDebugKey: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    FDebugForm: TfrmDebug;
    FCallStack: TfrmDebugStatus_CallStack;
    FDeadKeys: TfrmDebugStatus_DeadKeys;
    FElements: TfrmDebugStatus_Elements;
    FRegTest: TfrmDebugStatus_RegTest;
    FKey: TfrmDebugStatus_Key;

    procedure SetDebugForm(const Value: TfrmDebug);
    procedure SetDisplayFont(const Value: TFont);

  public
    property DebugForm: TfrmDebug read FDebugForm write SetDebugForm;

    property DisplayFont: TFont write SetDisplayFont;

    property Key: TfrmDebugStatus_Key read FKey;
    property Elements: TfrmDebugStatus_Elements read FElements;
    property CallStack: TfrmDebugStatus_CallStack read FCallStack;
    property DeadKeys: TfrmDebugStatus_DeadKeys read FDeadKeys;
    property RegTest: TfrmDebugStatus_RegTest read FRegTest;
  end;

//var
//  frmDebugStatus: TfrmDebugStatus;

implementation

uses
  UfrmMain;

{$R *.dfm}

procedure TfrmDebugStatus.FormCreate(Sender: TObject);
begin
  //pagesDebug.ActivePage := tabDebugStores;
  inherited;
  
  FKey := TfrmDebugStatus_Key.Create(Self);
  FKey.Parent := tabDebugKey;
  FKey.Visible := True;

  FElements := TfrmDebugStatus_Elements.Create(Self);
  FElements.Parent := tabDebugStores;
  FElements.Visible := True;

  FCallStack := TfrmDebugStatus_CallStack.Create(Self);
  FCallStack.Parent := tabDebugCallStack;
  FCallStack.Visible := True;

  FDeadKeys := TfrmDebugStatus_DeadKeys.Create(Self);
  FDeadKeys.Parent := tabDebugDeadkeys;
  FDeadKeys.Visible := True;

  FRegTest := TfrmDebugStatus_RegTest.Create(Self);
  FRegTest.Parent := tabDebugRegressionTesting;
  FRegTest.Visible := True;
end;

procedure TfrmDebugStatus.SetDebugForm(const Value: TfrmDebug);
var
  FDebugKeyboard: TDebugKeyboard;
begin
  FDebugForm := Value;
  Visible := Assigned(Value);

  if Assigned(Value) then FDebugKeyboard := Value.GetDebugKeyboard
  else FDebugKeyboard := nil;

  FKey.SetDebugKeyboard(FDebugKeyboard);
  FElements.SetDebugKeyboard(FDebugKeyboard);
  FCallStack.SetDebugKeyboard(FDebugKeyboard);
  FDeadKeys.SetDebugKeyboard(FDebugKeyboard);
  FRegTest.SetDebugKeyboard(FDebugKeyboard);
end;

procedure TfrmDebugStatus.SetDisplayFont(const Value: TFont);
begin
  //FDisplayFont.Assign(Value);

  FKey.SetDisplayFont(Value);
  FElements.SetDisplayFont(Value);
  FCallStack.SetDisplayFont(Value);
  FDeadKeys.SetDisplayFont(Value);
  FRegTest.SetDisplayFont(Value);
end;

end.
