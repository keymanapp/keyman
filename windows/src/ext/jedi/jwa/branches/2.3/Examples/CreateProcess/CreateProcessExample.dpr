{******************************************************************************}
{ JEDI CreateProcess Example Project											   }
{ http://jedi-apilib.sourceforge.net										   }
{ 																			   }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 																			   }
{ Author(s): Christian Wimmer												   }
{ Creation date: 24th October 2008 					   				   			   }
{ Last modification date: 24th October 2008										   }
{ 																			   }
{ Description: Shows how to use the CreateProcess function.  		   }
{ 																			   }
{ 																			   }
{ Preparations: JWA must be ready to use.       							   }
{ 																			   }
{ 																			   }
{ Version history: 24th October 2008 initial release				     		   }
{ 																			   }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 		   }
{ productive environments.													   }
{ The code has surely some errors that need to be fixed. In such a case	   	   }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.													   }
{ 																			   }	 	 		
{******************************************************************************}
program CreateProcessExample;

{$APPTYPE CONSOLE}

uses
  JwaWinNt,
  JwaWinType,
  JwaWinError,
  JwaWinBase,
  JwaUserEnv,
  JwaWinUser,

  SysUtils;

function WindowsDirectory : String;
var
  WinPath : array[0..MAX_PATH] of Char;
begin
  ZeroMemory(@WinPath, sizeof(WinPath));
  GetWindowsDirectory(WinPath, MAX_PATH);
  result := WinPath;
end;

procedure StartApp(const App, Parameters, CurDir : String);
var
  StartupInfo: TStartupInfo;
  ProcInfo : TProcessInformation;
  pEnv : Pointer; 

  pCurDir,
  pCmdLine : PChar;
begin
  ZeroMemory(@StartupInfo, sizeof(StartupInfo));
  StartupInfo.cb          := SizeOf(StartupInfo);
  StartupInfo.lpDesktop   := 'winsta0\default';

  CreateEnvironmentBlock(@pEnv, 0, true);

  try
    if Length(Parameters) > 0 then
      pCmdLine := PChar('"'+App+'" ' + Parameters)
    else
      pCmdLine := PChar('"'+App+'" ');

    pCurDir := Nil;
    if Length(CurDir) > 0 then
      pCurDir := PChar(CurDir);

   if not
    CreateProcess(
    PChar(App),//__in_opt     LPCTSTR lpApplicationName,
    pCmdLine, //__inout_opt  LPTSTR lpCommandLine,
    nil,//__in_opt     LPSECURITY_ATTRIBUTES lpProcessAttributes,
    nil,//__in_opt     LPSECURITY_ATTRIBUTES lpThreadAttributes,
    true,//__in         BOOL bInheritHandles,
    CREATE_NEW_CONSOLE or CREATE_UNICODE_ENVIRONMENT,//__in         DWORD dwCreationFlags,
    pEnv,//__in_opt     LPVOID lpEnvironment,
    pCurDir,//__in_opt     LPCTSTR lpCurrentDirectory,
    StartupInfo,//__in         LPSTARTUPINFO lpStartupInfo,
    ProcInfo,//__out        LPPROCESS_INFORMATION lpProcessInformation
  ) then
   raiseLastOsError;
  finally
    DestroyEnvironmentBlock(pEnv);
  end;

  //wait for 60secs
  WaitForSingleObject(ProcInfo.hProcess, 60*1000);

  //wait until process can receive user input
  //WaitForInputIdle(ProcInfo.hProcess, 60*1000);

  CloseHandle(ProcInfo.hProcess);
  CloseHandle(ProcInfo.hThread);
end;

begin
  StartApp(WindowsDirectory+'\explorer.exe','/separate','');
end.
 