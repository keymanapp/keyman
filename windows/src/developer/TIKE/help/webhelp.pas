(*
  Name:             webhelp
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      30 Apr 2015

  Modified Date:    30 Apr 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          30 Apr 2015 - mcdurdin - I4677 - V9.0 - Move Developer help to online only
                    
*)
unit webhelp;   // I4677

interface

uses
  Winapi.Windows;

type
  TWebHookHelpSystem = class(TObject)
  private
    FRootWebPath: String;
  public
    constructor Create(aRootWebPath: String);

    function HelpContext(aContextId: DWord): Boolean;
    function HelpTopic(aTopic: String): Boolean;
  end;

implementation

uses
  System.StrUtils,
  System.SysUtils,

  utilexecute;

constructor TWebHookHelpSystem.Create(aRootWebPath: string);
begin
  inherited Create;
  FRootWebPath := aRootWebPath;
  if (FRootWebPath <> '') and (FRootWebPath[Length(FRootWebPath)] <> '/') then
    FRootWebPath := FRootWebPath + '/';
end;

function TWebHookHelpSystem.HelpContext(aContextId: DWord): Boolean;
begin
  TUtilExecute.URL(FRootWebPath + 'context?'+IntToStr(aContextId));
  Result := True;
end;

function TWebHookHelpSystem.HelpTopic(aTopic: String): Boolean;
var
  i: Integer;
  s: string;
begin
  Result := False;

  if aTopic <> '' then
  begin
    for i := 1 to Length(aTopic) do if aTopic[i] = '\' then aTopic[i] := '/';
    if aTopic[1] = '/' then Delete(aTopic, 1, 1);
    s := FRootWebPath + aTopic;
  end
  else
    s := FRootWebPath;

  TUtilExecute.URL(s);
end;

end.


