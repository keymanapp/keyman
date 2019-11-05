(*
  Name:             DevDelphiCompileWrapper
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      25 May 2012

  Modified Date:    26 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 May 2012 - mcdurdin - I3339 - V9.0 - Add GUI compiler wrapper for quicker review of hints and warnings
                    26 Jun 2012 - mcdurdin - I3378 - KM9 - Delphi compiler wrapper needs quiet mode
*)
unit DevDelphiCompileWrapper;  // I3339

interface

type
  TDelphiCompileWrapper = class
  public
    class function Run(Quiet: Boolean = False; Silent: Boolean = False): Boolean;  // I3378
  end;

{$IFDEF VER310}
const dcc32 = 'C:\Program Files (x86)\Embarcadero\Studio\18.0\bin\DCC32.EXE';
{$ELSE}
{$IFDEF VER320}
const dcc32 = 'C:\Program Files (x86)\Embarcadero\Studio\19.0\bin\DCC32.EXE';
{$ELSE}
{$IFDEF VER330}
const dcc32 = 'C:\Program Files (x86)\Embarcadero\Studio\20.0\bin\DCC32.EXE';
{$ENDIF}
{$ENDIF}
{$ENDIF}

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  DevUtils,
  UfrmCompilerErrors;

class function TDelphiCompileWrapper.Run(Quiet, Silent: Boolean): Boolean;  // I3378
var
  ParamText: string;
  DprFilename: string;
  ec: Integer;
  I: Integer;
begin
  Result := False;

  ParamText := '';
  DprFilename := ExpandFileName(ParamStr(ParamCount));

  for I := 2 to ParamCount - 1 do
  begin
    ParamText := ParamText + ' "'+ParamStr(I)+'"';
  end;

  DevLog('DCC32 '+DprFilename,True);

  if not DevUtils.CommandExecute(DCC32+ParamText+' '+DprFilename,ExtractFilePath(dprfilename)+'error.log',
    ExtractFilePath(dprfilename),SW_HIDE,ec,True) or
    (ec<>0) then
  begin
    DevLog('ERROR: Unable to build '+dprfilename+#13#10+SysErrorMessage(GetLastError),True);
    //WinExecuteFunction('notepad',ExtractFilePath(dprfilename)+'error.log');
    if Silent then Result := False;
  end
  else if Silent then
    Result := True;

  if not Silent then
    Result := DevParseDCC32Log(ec, dprfilename, ExtractFilePath(dprfilename)+'error.log', ParamText, Quiet);  // I3378
end;

end.
