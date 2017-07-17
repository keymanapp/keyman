(*
  Name:             VistaMessages
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Dec 2006

  Modified Date:    4 Dec 2006
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Dec 2006 - mcdurdin - Initial version
*)
unit VistaMessages;

interface

uses
  Windows;

function ChangeWindowMessageFilter(message: UINT; dwFlag: DWORD): BOOL; stdcall;

implementation

type
  TChangeWindowMessageFilterFunc = function(message: UINT; dwFlag: DWORD): BOOL; stdcall;

var
  FChangeWindowMessageFilter: TChangeWindowMessageFilterFunc = nil;
  FInit: Boolean = False;

function ChangeWindowMessageFilter(message: UINT; dwFlag: DWORD): BOOL; stdcall;
begin
  if not FInit then
  begin
    @FChangeWindowMessageFilter := GetProcAddress(GetModuleHandle('user32.dll'), 'ChangeWindowMessageFilter');
    FInit := True;
  end;

  if not Assigned(FChangeWindowMessageFilter)
    then Result := TRUE
    else Result := FChangeWindowMessageFilter(message, dwFlag);
end;


end.
