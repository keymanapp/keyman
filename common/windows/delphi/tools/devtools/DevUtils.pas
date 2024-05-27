(*
  Name:             DevUtils
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      4 May 2012

  Modified Date:    13 Sep 2016
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          04 May 2012 - mcdurdin - I3307 - V9.0 - Delphi XE2 path and package manager
                    25 May 2012 - mcdurdin - I3339 - V9.0 - Add GUI compiler wrapper for quicker review of hints and warnings
                    26 Jun 2012 - mcdurdin - I3378 - KM9 - Delphi compiler wrapper needs quiet mode
                    02 Jan 2013 - mcdurdin - I3726 - V9.0 - Release build should check Debug key and SVN state
                    02 Jan 2013 - mcdurdin - I3726 - V9.0 - Release build should check Debug key and SVN state
                    13 Sep 2016 - mcdurdin - I5087 - Move from SVN to GIT for Keyman source
*)
unit DevUtils;  // I3307  // I3339

interface

procedure DevLog(const s: string; nl: Boolean);
procedure Run;
function CommandExecute(const cmdline, logfname, curdir: string; sw: Integer; var ecode : integer; echolog: Boolean = False; cleanlog: boolean = False): Boolean;   // I8935

implementation

uses
  System.Classes,
  System.SysUtils,
  Winapi.Windows,
  Winapi.ActiveX,
  DevCheckGitStatus,   // I5087
  DevInstallPackages,
  DevIncludePaths,
  DevReleaseBuildCheck,
  Keyman.System.DevTools.BuildMessageConstants,
  Keyman.System.DevTools.BuildSetupStringTranslations,
  RegistryKeys;

procedure DevLog(const s: string; nl: Boolean);
begin
  if nl
    then writeln(s)
    else write(s);
end;

procedure Run;
var
  Success: Boolean;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  try
    // Parse Command Line
    //
    // -ip package.bpl      : install package
    // -rp                  : reset packages
    // -ai path[;path...]   : add include path(s)
    // -ri                  : reset include paths

    if ParamCount < 1 then
    begin
      writeln('Usage: devutils <command>');    // I3339  // I3378
      writeln('  -ip package.bpl      : install package');
      writeln('  -rp                  : reset packages');
      writeln('  -ai path[;path...]   : add include path(s)');
      writeln('  -ri                  : reset include paths');
      writeln('  -ti                  : touch PathDefines.mak (create if not present)');  //TODO: remove as unused
      writeln('  -rt                  : check release build prereqs, e.g. HKCU\'+SRegKey_KeymanDebug_CU);   // I3726
      writeln('  -git                 : check git repository commit/update status');   // I3726   // I5087   //TODO: remove as unused
      writeln('  -buildmessageconstants <strings.xml> <messageidentifierconsts.pas>: build MessageIdentifierConsts.pas from current strings.xml');
      writeln('  -buildsetupstrings <sourcepath> <destinationpath>: build Keyman.Setup.System.Locale.*.pas from strings.xml found in sourcepath');
      ExitCode := 1;
      Exit;
    end;

    if (ParamStr(1) = '-ip') and (ParamCount = 2) then
      Success := TInstallPackages.InstallPackage(ParamStr(2))
    else if (ParamStr(1) = '-rp') and (ParamCount = 1) then
      Success := TInstallPackages.Reset
    else if (ParamStr(1) = '-ai') and (ParamCount = 2) then
      Success := TIncludePaths.Add(ParamStr(2))
    else if (ParamStr(1) = '-ri') and (ParamCount = 1) then
      Success := TIncludePaths.Reset
    else if (ParamStr(1) = '-ti') and (ParamCount = 1) then
      Success := TIncludePaths.Touch
    else if (ParamStr(1) = '-rt') then   // I3726
      Success := TReleaseBuildCheck.Run
    else if (ParamStr(1) = '-git') then   // I3726   // I5087
      Success := TCheckGitStatus.Run
    else if (ParamStr(1) = '-buildmessageconstants') and (ParamCount = 3) then
      Success := TBuildMessageConstants.Run(ParamStr(2), ParamStr(3))
    else if (ParamStr(1) = '-buildsetupstrings') and (ParamCount = 3) then
      Success := TBuildSetupStringTranslations.Run(ParamStr(2), ParamStr(3))
    else
    begin
      writeln('Invalid parameters');
      ExitCode := 2;
      Exit;
    end;

    if Success
      then ExitCode := 0
      else ExitCode := 3;
  finally
    CoUninitialize;
  end;
end;

function CommandExecute(const cmdline, logfname, curdir: string; sw: Integer; var ecode : integer; echolog: Boolean = False; cleanlog: boolean = False): Boolean;   // I8935
var
  si: TStartupInfo;
  b, ec: DWord;
  buf: array[0..512] of ansichar;
  SecAttrs: TSecurityAttributes;
  hsoutread, hsoutwrite: THandle;
  hsinread, hsinwrite: THandle;
  pi: TProcessInformation;
  //flog : TextFile;
  FLogText: string;
  locallog : boolean;
  s, str: string;
  loglist: TStringList;
  n: Integer;
  vcmdline: PChar;
begin
  //locallog := False;
  Result := False;
  ecode := -1;
  FillChar(SecAttrs, SizeOf(SecAttrs), #0);
  SecAttrs.nLength              := SizeOf(SecAttrs);
  SecAttrs.lpSecurityDescriptor := nil;
  SecAttrs.bInheritHandle       := TRUE;
  if not CreatePipe(hsoutread, hsoutwrite, @SecAttrs, 0) then Exit;

  FillChar(SecAttrs, SizeOf(SecAttrs), #0);
  SecAttrs.nLength              := SizeOf(SecAttrs);
  SecAttrs.lpSecurityDescriptor := nil;
  SecAttrs.bInheritHandle       := TRUE;
  if not CreatePipe(hsinread, hsinwrite, @SecAttrs, 0) then
  begin
    CloseHandle(hsoutread);
    CloseHandle(hsoutwrite);
    Exit;
  end;

  { See support.microsoft.com kb 190351 }

  n := 0;
  FLogText := '';

  try
    locallog := (logfname<>'');
    if locallog then
    begin
      loglist := TStringList.Create;
        //AssignFile(flog,logfname);
        //Rewrite(flog);
      if not cleanlog then loglist.Add(cmdline);   // I8935
    end
    else
      loglist := nil;

    vcmdline := AllocMem((Length(cmdline)+1)*sizeof(char));
    try
      si.cb := SizeOf(TStartupInfo);
      si.lpReserved := nil;
      si.lpDesktop := nil;
      si.lpTitle := nil;
      si.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      si.wShowWindow := sw;
      si.cbReserved2 := 0;
      si.lpReserved2 := nil;
      si.hStdInput := hsinread;
      si.hStdOutput := hsoutwrite;
      si.hStdError := hsoutwrite;

      StrCopy(vcmdline, PWideChar(cmdline));
      //vcmdline := cmdline;

      if CreateProcess(nil, PChar(vcmdline), nil, nil, True, HIGH_PRIORITY_CLASS, nil, PChar(curdir), si, pi) then
      begin
        if GetExitCodeProcess(pi.hProcess, ec) then
        begin
          while ec = STILL_ACTIVE do
          begin
            Sleep(20);
            Inc(n);
            if n = 30 then
            begin
              {Application.ProcessMessages;
              if FCancelCommand then
              begin
                TerminateProcess(pi.hProcess, 0);
                ec := 0;
                Exit;
              end;}
              n := 0;
            end;
            PeekNamedPipe(hsoutread, nil, 0, nil, @b, nil);
            if b > 0 then
            begin
              ReadFile(hsoutread, buf, High(buf), b, nil);
              s := String(Copy(buf, 1, b));
              if locallog then str := str + s;
              if echolog then DevLog(s, False);
            end;
            if not GetExitCodeProcess(pi.hProcess, ec) then ec := 0;
          end;
        end;

        repeat
          PeekNamedPipe(hsoutread, nil, 0, nil, @b, nil);
          if b > 0 then
          begin
            ReadFile(hsoutread, buf, High(buf), b, nil);
            s := String(Copy(buf, 1, b));
            if locallog then str := str + s;
            if echolog then DevLog(s, False);
          end;
        until b = 0;

        if echolog then DevLog('', True);

        CloseHandle(pi.hProcess);
        CloseHandle(pi.hThread);
      end
      else
        Exit;

      if locallog then
      begin
        loglist.Add(str);
        if not cleanlog then loglist.Add(#13#10'exit code = ' + IntToStr(ec));   // I8935
        loglist.SaveToFile(logfname, TEncoding.Default);
      end;
    finally
      loglist.Free;
      FreeMem(vcmdline);
    end;

    Result := True;
  finally
    if echolog then DevLog('exit code ='+IntToStr(ec),True);
    ecode := ec;
    //if locallog then CloseFile(flog);
    CloseHandle(hsoutread);
    CloseHandle(hsoutwrite);
    CloseHandle(hsinread);
    CloseHandle(hsinwrite);
  end;
end;

end.
