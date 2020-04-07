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
    procedure RegisterControllerWindow(Value: LongWord); safecall;
    procedure UnregisterControllerWindow(Value: LongWord); safecall;
    procedure DisableUserInterface; safecall;
    procedure EnableUserInterface; safecall;
    procedure UpdateTouchPanelVisibility(Value: Boolean); safecall;

    procedure DiagnosticTestException; safecall;
  end;

implementation

end.
