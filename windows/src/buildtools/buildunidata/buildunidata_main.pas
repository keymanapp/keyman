(*
  Name:             buildunidata_main
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:
  Create Date:      28 Sep 2006

  Modified Date:    25 Mar 2011
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          28 Sep 2006 - mcdurdin - Initial version
                    25 Mar 2011 - mcdurdin - I2845 - After fixing Developer Unicode data references, BuildUniData fails due to path overrides
*)
unit buildunidata_main;

interface

procedure Run;

implementation

uses
  Classes,
  JRO_TLB,
  SysUtils,
  UnicodeData,
  utildir,
  Windows;

type
  TUIManager = class(TInterfacedObject, IUnicodeDataUIManager)

  public
    procedure UDUI_Error(Sender: TUnicodeData; Error: TUnicodeDataError;
      const Details: WideString);
    function UDUI_ShouldStartRebuildOnError(const Msg: WideString): Boolean;
    function UDUI_StartRebuild(Callback: TNotifyEvent; AskFirst: Boolean): Boolean;
    procedure UDUI_UpdateStatus(const Msg: WideString; Pos: Integer;
      Max: Integer);
  end;

procedure CompactUnicodeDatabase(const DatabaseName: string); forward;

procedure Run;
var
  FUnicodeDataUIManager: IUnicodeDataUIManager;
begin
  try
    if ParamCount < 1 then
    begin
      writeln('Usage: buildunidata <sourcepath> <unicodedata.mdb (path and file)>');
      Exit;
    end;

    writeln('Building Unicode Database');
    FUnicodeDataUIManager := TUIManager.Create;
    with TUnicodeData.Create(ParamStr(1), FUnicodeDataUIManager, ExtractFilePath(ParamStr(2)), False) do  // I2845
    try
      BuildDatabase;
    finally
      Free;
      FUnicodeDataUIManager := nil;
    end;
    write(#13#10+'Unicode Database build completed');

    if ExitCode > 0
      then writeln(' with errors.')
      else writeln(' successfully.');

    if ExitCode = 0 then
    begin
      writeln('Compacting Unicode Database');
      CompactUnicodeDatabase(ParamStr(2));
      writeln('Compacting Unicode Database completed successfully.');
    end;
  except
    on E:Exception do
    begin
      writeln('FATAL ERROR '+E.ClassName+': '+E.Message);
      ExitCode := 2;
    end;
  end;
end;

{ TUIManager }

procedure TUIManager.UDUI_Error(Sender: TUnicodeData; Error: TUnicodeDataError; const Details: WideString);
begin
  writeln('ERROR: '+Details);
  ExitCode := 1;
end;

function TUIManager.UDUI_ShouldStartRebuildOnError(const Msg: WideString): Boolean;
begin
  Result := True;
end;

function TUIManager.UDUI_StartRebuild(Callback: TNotifyEvent; AskFirst: Boolean): Boolean;
begin
  Callback(Self);
  Result := TRue;
end;

procedure TUIManager.UDUI_UpdateStatus(const Msg: WideString; Pos, Max: Integer);
begin
  write(#13 + Msg);
end;

procedure CompactUnicodeDatabase(const DatabaseName: string);
var
  je: IJetEngine;
  connsrc, conndst: WideString;
  tempdbpath: string;
const
  tempdb = 'tempdb.mdb';
begin
  tempdbpath := KGetTempPath + tempdb;
  connsrc := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+DatabaseName+';Jet OLEDB:Engine Type=5';
  conndst := 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source='+tempdbpath+';Jet OLEDB:Engine Type=5';
  je := JRO_TLB.CoJetEngine.Create;
  je.CompactDatabase(connsrc, conndst);
  je := nil;

  SysUtils.DeleteFile(DatabaseName);
  MoveFile(PChar(tempdbpath), PChar(DatabaseName));
end;

end.
