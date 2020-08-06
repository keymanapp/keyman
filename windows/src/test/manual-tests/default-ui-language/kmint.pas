// This is a clean reimplementation of kmint.pas from kmshell which has a bunch of
// legacy dependency mess
unit kmint;

interface

uses
  custinterfaces,
  keymanapi_TLB;

var
  kmcom: IKeyman = nil;

function KeymanCustomisation: IKeymanCustomisation;
function LoadKMCOM: Boolean;

implementation

uses
  Winapi.ActiveX,
  Winapi.Windows;

function LoadKMCOM: Boolean;
begin
  kmcom := CoKeyman.Create;
  Exit(True);
end;

function KeymanCustomisation: IKeymanCustomisation;
begin
  Result := (kmcom.Control as IKeymanCustomisationAccess).KeymanCustomisation;
end;

initialization
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
finalization
  CoUninitialize;
end.

