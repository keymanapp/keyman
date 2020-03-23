(*
  Name:             utilhandleexception
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      19 Mar 2007

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          19 Mar 2007 - mcdurdin - Remove forms.pas dependency
                    28 Jul 2008 - mcdurdin - I1574 - Use new global reporting of exceptions
                    18 Mar 2011 - mcdurdin - I2824 - Consolidate logging in Diag folder
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
*)
unit utilhandleexception;

interface

uses
  System.SysUtils;

procedure LogException(const SourceClassName: string; E: Exception; ExceptAddr: Pointer); overload;
procedure LogException(E: Exception); overload;

implementation

uses
  System.Classes,
  Winapi.Windows,

  ErrLogPath,
  KLog,
  utildir,
  VersionInfo;

function ConvertedExceptAddr(ExceptAddr: Pointer): Pointer;

  function GetLogicalAddr( Address: Pointer ): Pointer;
  const
    CODE_OFFSET = $1000;
  begin
  {
    hard-coded $1000 instead of more correct FPImgHdr^.OptionalHeader.BaseOfCode
    because there are problems with corrupted header in packed EXEs
    BTW Inprise linkers always set code base = $1000 :)
  }
    if Address <> nil then Result := Pointer(Cardinal(Address)-CODE_OFFSET)
                      else Result := nil;
  end;

var
  Info: TMemoryBasicInformation;
begin
  VirtualQuery(ExceptAddr, Info, sizeof(Info));
  if Info.State <> MEM_COMMIT then
    Result := GetLogicalAddr(ExceptAddr)
  else
    Result := GetLogicalAddr( Pointer(Integer(ExceptAddr)-Integer(Info.AllocationBase)) );
end;

procedure LogException(const SourceClassName: string; E: Exception; ExceptAddr: Pointer);
{$IFDEF CPUX64}
var
  msg, errlogfile: string;
  errlog: TStringList;
begin
  try
    if E = nil
      then msg := Format('Exception in %s at %p', [SourceClassName, ConvertedExceptAddr(ExceptAddr)])
      else msg := Format('Exception in %s at %p (%s): %s', [SourceClassName, ConvertedExceptAddr(ExceptAddr), E.ClassName, (E as Exception).Message]);
    KL.LogError(msg);

    errlog := TStringList.Create;
    try
      errlogfile := GetErrLogFileName('kmcomapi');  // I2824

      errlog.Text :=
        'Crash Identifier: kmcomapi.dll_'+GetVersionString+'_'+IntToHex(Integer(ExceptAddr),8)+#13#10#13#10+
        'KMCOMAPI EXCEPTION AT '+FormatDateTime('yyyy-mm-dd hh:nn:ss', Now) + #13#10 +
        'kmcomapi.dll version ' + GetVersionString + #13#10 +
        msg + #13#10#13#10;

      if FileExists(errlogfile) then
        with TStringList.Create do
        try
          LoadFromFile(errlogfile);  // use prolog encoding
          errlog.Text := Text + errlog.Text;
        finally
          Free;
        end;
      errlog.SaveToFile(errlogfile, TEncoding.UTF8);  // I3337
    finally
      errlog.Free;
    end;
  except
    ;
  end;
{$ELSE}
begin
  {$MESSAGE HINT 'TODO: Write a raw call stack to diag folder which can then be sucked in by client app and reported'}
{$ENDIF}
end;

procedure LogException(E: Exception);
begin
  LogException('', E, ExceptAddr);
end;

end.

