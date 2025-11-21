(*
  Name:             KeymanControlRestart
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Jun 2007

  Modified Date:    19 Jun 2007
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Jun 2007 - mcdurdin - Initial version
*)
unit KeymanEngineControl;

interface

type
  IKeymanEngineControl = interface
    ['{5625CA69-3064-4F4E-8EAE-DB82B5BFD7F4}']
    procedure RestartEngine; safecall;
    procedure ShutdownKeyman32Engine; safecall;
    procedure StartKeyman32Engine; safecall;   // I5133
    procedure StopKeyman32Engine; safecall;   // I5133
    procedure ResetKeyman32Engine; safecall;   // I5133
    procedure RegisterControllerWindow(Value: LongWord); safecall; // deprecated in #5060; remains for interface stability
    procedure UnregisterControllerWindow(Value: LongWord); safecall; // deprecated in #5060; remains for interface stability
    procedure DisableUserInterface; safecall;
    procedure EnableUserInterface; safecall;
    procedure UpdateTouchPanelVisibility(Value: Boolean); safecall;

    procedure DiagnosticTestException; safecall;

    function LastRefreshToken: IntPtr; safecall;

    // New in #5060:

    procedure RegisterMasterController(Value: LongWord); safecall;
    procedure UnregisterMasterController(Value: LongWord); safecall;
    procedure RegisterControllerThread(Value: LongWord); safecall;
    procedure UnregisterControllerThread(Value: LongWord); safecall;

    // New in 19.0

    procedure WatchDogKeyEvent; safecall;    // 32 bit only
  end;

implementation

end.
