(*
  Name:             klog
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      1 Aug 2006

  Modified Date:    8 Jun 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          01 Aug 2006 - mcdurdin - Disable logging with KLOGGING define
                    14 Sep 2006 - mcdurdin - Add Current Tick Count to logs (for correlation with system.log)
                    04 May 2012 - mcdurdin - I3309 - V9.0 - Migrate to Delphi XE2, VS2010, svn 1.7
                    04 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
*)
unit klog;  // I3309

interface

{TNT-IGNORE-UNIT}

{DEFINE KLOGGING}

{$IFDEF KLOGGING}
uses
  Classes;
{$ENDIF}

type
  TKLog = class
  private
{$IFDEF KLOGGING}
    FAppName: string;
    FLogFileName: string;
    FLogFile: TextFile;
    FMethodStack: TStringList;
    FFileOpen: Boolean;
{$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const msg: string); overload;
    procedure Log(const msg: string; const fmt: array of const); overload;
    procedure LogError(const msg: string); overload;
    procedure LogError(const msg: string; const fmt: array of const); overload;
    procedure MethodEnter(instance: TObject; const name: string; const parameters: array of const);
    procedure MethodExit(instance: TObject; const name: string);
  end;

function KL: TKLog;
function KLEnabled: Boolean;

implementation

{$IFDEF KLOGGING}
uses
  Variants, Windows, SysUtils, ErrorControlledRegistry, ErrLogPath, VersionInfo, Unicode;

{$ENDIF}

var
  FKLog: TKLog = nil;

function KL: TKLog;
begin
  if not Assigned(FKLog) then FKlog := TKLog.Create;
  Result := FKlog;
end;

{ TKLog }

constructor TKLog.Create;
{$IFDEF KLOGGING}

  function DirectoryExists(const Name: string): Boolean;
  var
    Code: Dword;
  begin
    Code := GetFileAttributes(PChar(Name));
    Result := (Code <> $FFFFFFFF) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
  end;

var
  buf: array[0..260] of char;
  FRootPath: string;
begin
  inherited Create;
  FRootPath := GetErrLogPath;

  FMethodStack := TStringList.Create;
  GetModuleFileName(hInstance, buf, 260); FAppName := buf;
  FLogFileName := FRootPath+ChangeFileExt(ExtractFileName(FAppName), '') + IntToStr(GetCurrentProcessId) + '.log';
  Log(StringOfChar('=', 160));
  Log('Starting application '+FAppName+', version '+GetFileVersionString(FAppName));
{$ELSE}
begin
  inherited Create;
{$ENDIF}
end;

destructor TKLog.Destroy;
{$IFDEF KLOGGING}
var
  i: Integer;
begin
  for i := 0 to FMethodStack.Count - 1 do
    Log('Unterminated method '+FMethodStack[i]);
  FMethodStack.Clear;
  Log('Stopping application '+FAppName);
  Log(StringOfChar('=', 160));
  FMethodStack.Free;
  if FFileOpen then CloseFile(FLogFile);
{$ELSE}
begin
{$ENDIF}
  inherited Destroy;
end;

procedure TKLog.Log(const msg: string);
begin
{$IFDEF KLOGGING}
  if not FFileOpen then
  begin
    AssignFile(FLogFile, FLogFileName);
    if FileExists(FLogFileName) then Append(FLogFile) else Rewrite(FLogFile);
    FFileOpen := True;
  end;
  writeln(FLogFile, Format('%12.12d ', [GetTickCount()])+FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ': ' + StringOfChar(' ', FMethodStack.Count*2) + msg);
  flush(FLogFile);
  OutputDebugString(PChar('KLog:' + msg + #13#10));
{$ENDIF}
end;

procedure TKLog.LogError(const msg: string);
begin
{$IFDEF KLOGGING}
  Log('###ERROR###: '+msg);
{$ENDIF}
end;

procedure TKLog.LogError(const msg: string; const fmt: array of const);
begin
{$IFDEF KLOGGING}
  LogError(Format(msg, fmt));
{$ENDIF}
end;

procedure TKLog.MethodEnter(instance: TObject; const name: string; const parameters: array of const);
{$IFDEF KLOGGING}
var
  s: string;
  vr: TVarRec;
  i: Integer;
begin
  s := '';
  for i := Low(parameters) to High(parameters) do
  begin
    vr := TVarRec(parameters[i]);
    if i > Low(parameters) then s := s + ', ';
    case vr.VType of
      vtInteger:    s := s + IntToStr(vr.VInteger);
      vtBoolean:    if vr.VBoolean then s := s + 'True' else s := s + 'False';
      vtChar:       s := s + ''''+Char(vr.VChar)+'''';  // I3310
      vtExtended:   s := s + FloatToStr(vr.VExtended^);
      vtString:     s := s + ''''+String_AtoU(vr.VString^)+'''';  // I3310  // I3310
      vtPointer:    s := s + '$'+IntToHex(Integer(vr.VPointer), 8);
      vtPChar:      s := s + ''''+String_AtoU(vr.VPChar)+'''';  // I3310
      vtObject:     if vr.VObject = nil then s := s + 'object <nil>' else s := s + 'object '+vr.VObject.ClassName+' [$'+IntToHex(Integer(Pointer(vr.VObject)), 8)+']';
      vtClass:      if vr.VClass = nil then s := s + 'class <nil>' else s := s + 'class '+vr.VClass.ClassName;
      vtWideChar:   s := s + ''''+vr.VWideChar+'''';
      vtPWideChar:  s := s + ''''+vr.VPWideChar+'''';
      vtAnsiString: s := s + ''''+string(vr.VAnsiString)+'''';
      vtCurrency:   s := s + CurrToStr(vr.VCurrency^);
      vtVariant:    try s := s + VarToStr(vr.VVariant^); except s := s + '<error reading variant>'; end;
      vtInterface:  s := s + 'interface $'+IntToHex(Integer(vr.VInterface), 8);
      vtWideString: s := s + ''''+widestring(vr.VWideString)+'''';
      vtInt64:      s := s + IntToStr(vr.VInt64^);
      vtUnicodeString: s := s + ''''+WideString(vr.VUnicodeString)+'''';  // I3309
    end;
  end;
  if Assigned(instance) then
  begin
    Log(instance.ClassName+'.'+name+'('+s+') enter');
    FMethodStack.Add(instance.ClassName+'.'+name);
  end
  else
  begin
    Log(name+'('+s+') enter');
    FMethodStack.Add(name);
  end;
{$ELSE}
begin
{$ENDIF}
end;

procedure TKLog.MethodExit(instance: TObject; const name: string);
{$IFDEF KLOGGING}
var
  s: string;
begin
  if Assigned(instance)
    then s := instance.ClassName+'.'+name
    else s := name;
  while (FMethodStack.Count > 0) and (FMethodStack[FMethodStack.Count-1] <> s) do
  begin
    Log(FMethodStack[FMethodStack.Count-1] + ' not exiting properly!');
    FMethodStack.Delete(FMethodStack.Count-1);
  end;
  if FMethodStack.Count > 0 then FMethodStack.Delete(FMethodStack.Count-1);
  Log(s+'() exit');
{$ELSE}
begin
{$ENDIF}
end;

procedure TKLog.Log(const msg: string; const fmt: array of const);
begin
{$IFDEF KLOGGING}
  Log(Format(msg, fmt));
{$ENDIF}
end;

function KLEnabled: Boolean;
begin
{$IFDEF KLOGGING}
  Result := True;
{$ELSE}
  Result := False;
{$ENDIF}
end;

initialization
  KL;
finalization
  FKLog.Free;
  FKLog := nil;
end.
