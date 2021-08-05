(*
  Name:             help
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      4 Jan 2007

  Modified Date:    24 Apr 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          04 Jan 2007 - mcdurdin - Initial version
                    15 Jan 2007 - mcdurdin - Call topicname.htm instead of topicname.html
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    08 Jun 2012 - mcdurdin - I3349 - V9.0 - Consolidate all process creation into TUtilExecute
                    24 Apr 2014 - mcdurdin - I3993 - V9.0 - Help Contents link does not work from Keyman menu
                    24 Apr 2014 - mcdurdin - I3676 - V9.0 - Help window Help and Help on Keyboard links don't work
                    
*)
unit help;  // I3306

interface

procedure OpenHelp(topicname: WideString);
function GetCHMPath: WideString;

implementation

uses
  System.SysUtils,
  Vcl.Dialogs,
  Winapi.Windows,

  custinterfaces,
  KeymanPaths,
  kmint,
  MessageIdentifiers,
  MessageIdentifierConsts,
  utilexecute;

function GetCHMPath: WideString;
var
  FFileName: WideString;
begin
  FFileName := MsgFromId(SKOnlineHelpFile);

  with kmint.KeymanCustomisation.CustMessages do
    Result := GetLocalePath + LanguageCode + '\' + FFileName;

  if not FileExists(Result) then
    Result := TKeymanPaths.KeymanHelpPath(FFileName);

  if not FileExists(Result) then
    Result := '';
end;

procedure OpenHelp(topicname: WideString);
var
  s: string;
begin
  s := GetCHMPath;
  if s = '' then
  begin
    ShowMessage(MsgFromId(SKCouldNotFindHelp));
    Exit;
  end;

  if topicname = '' then topicname := 'index';

  // We don't use Application.HelpJump here, because we need to load the topic
  // and leave the help file open after the application closes
  TUtilExecute.Shell(0, 'hh.exe', '', '"mk:@MSITStore:'+s+'::'+topicname+'.htm"');  // I3349   // I3993   // I3676
end;

end.
