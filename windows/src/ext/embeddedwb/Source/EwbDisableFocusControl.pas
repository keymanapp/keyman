(*

This unit's purpose is to disable the FocusControl even if Enable_AutoFocusControl
is defined in EWB.inc.

EwbControl.FocusControl is activated in the initialization section of the
EmbeddedWB unit.
To disable it, put this unit before the first form
unit in your DPR 'uses' clause.

Example:

program Project1;

uses
  //..
  EWBDisableFocusControl,
  Forms,
  Unit1 in 'Unit1.pas' {Form1};
  //..

*)

unit EWBDisableFocusControl;

interface

uses
  EwbFocusControl;

implementation

initialization
  EWBEnableFocusControl := False;

end.
