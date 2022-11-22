(*
  Name:             UfrmRegressionTestFailure
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      19 Nov 2007

  Modified Date:    18 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          19 Nov 2007 - mcdurdin - I1157 - const string parameters
                    18 May 2012 - mcdurdin - I3323 - V9.0 - Change from Plus-MemoU to Plus-Memo
*)
unit UfrmRegressionTestFailure;  // I3323

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, UfrmTike;

type
  TfrmRegressionTestFailure = class(TTIKEForm)
    Label1: TLabel;
    Label2: TLabel;
    lblEvent: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    memoExpected: TMemo;
    memoActual: TMemo;
    cmdOk: TButton;
    Label3: TLabel;
    lblTestName: TLabel;
  private
    FCurrentEvent: string;
    FActualString: WideString;
    FExpectedString: WideString;
    FTestName: string;
    procedure SetCurrentEvent(Value: string);
    procedure SetActualString(Value: WideString);
    procedure SetExpectedString(Value: WideString);
    procedure SetTestName(Value: string);
  protected
    function GetHelpTopic: string; override;
  public
    { Public declarations }
    property TestName: string read FTestName write SetTestName;
    property CurrentEvent: string read FCurrentEvent write SetCurrentEvent;
    property ExpectedString: WideString read FExpectedString write SetExpectedString;
    property ActualString: WideString read FActualString write SetActualString;
  end;

implementation

uses
  Keyman.Developer.System.HelpTopics;

{$R *.DFM}

{ TfrmRegressionTestFailure }

function TfrmRegressionTestFailure.GetHelpTopic: string;
begin
  Result := SHelpTopic_Context_RegressionTestFailure;
end;

procedure TfrmRegressionTestFailure.SetActualString(Value: WideString);
begin
  FActualString := Value;
  memoActual.SetTextBuf(PWideChar(Value));
end;

procedure TfrmRegressionTestFailure.SetCurrentEvent(Value: string);
begin
  FCurrentEvent := Value;
  lblEvent.Caption := Value;
end;

procedure TfrmRegressionTestFailure.SetExpectedString(Value: WideString);
begin
  FExpectedString := Value;
  memoExpected.SetTextBuf(PWideChar(Value));
end;

procedure TfrmRegressionTestFailure.SetTestName(Value: string);
begin
  FTestName := Value;
  lblTestName.Caption := Value;
end;

end.
