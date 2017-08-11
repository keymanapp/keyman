(*
  Name:             buildtools/devtools/DevCheckGitStatus
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      13 Sep 2016

  Modified Date:    13 Sep 2016
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          13 Sep 2016 - mcdurdin - I5087 - Move from SVN to GIT for Keyman source
*)
unit DevCheckGitStatus;

interface

type
  TCheckGitStatus = class
    class function Run: Boolean;
  end;

implementation

uses
  DevUtils,
  System.Classes,
  System.SysUtils,
  SourceRootPath;

{ TCheckSvnStatus }

class function TCheckGitStatus.Run: Boolean;
var
  ec, i: Integer;
  Empty: Boolean;
begin
  if not DevUtils.CommandExecute('git status --porcelain', 'c:\keyman\gitstatus.txt', CSourceRootPath, 0, ec, False, True) or (ec <> 0) then
    Exit(False);
  with TStringList.Create do
  try
    LoadFromFile('c:\keyman\gitstatus.txt');
    Empty := True;

    for i := 0 to Count - 1 do
      if Trim(Strings[i]) <> '' then
      begin
        Empty := False;
        Break;
      end;

    if Empty then
      Exit(True);

    writeln('There are uncommitted files:');
    for i := 0 to Count - 1 do
      writeln('  '+Strings[i]);
  finally
    Free;
  end;

  Result := False;
end;

end.
