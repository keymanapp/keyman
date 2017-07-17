(*

This unit's purpose is to disable the MouseWheelFix even if Enable_AutoMouseWheelFix
is defined in EWB.inc.

EwbControl.MouseWheelFix is activated in the initialization section of the
EmbeddedWB unit.
To disable it, put this unit before the first form
unit in your DPR 'uses' clause.

Example:

program Project1;

uses
  //..
  EwbDisableMouseWheelFix,
  Forms,
  Unit1 in 'Unit1.pas' {Form1};
  //..

*)

unit EwbDisableMouseWheelFix;

interface

uses
  EWBMouseHook;

implementation

initialization
  EWBEnableMouseWheelFix := False;

end.
