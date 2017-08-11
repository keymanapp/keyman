{******************************************************************************}
{ JEDI "Set Parent Process" Example Project											   }
{ http://jedi-apilib.sourceforge.net										   }
{ 																			   }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 																			   }
{ Author(s): Christian Wimmer												   }
{ Creation date: 24th October 2008 					   				   			   }
{ Last modification date: 24th October 2008										   }
{ 																			   }
{ Description: This examples demonstrates how to use CreateProcess and  		   }
{ 	reset the parent process of the new process.													   }
{ 	The new process will have the given process as its parent. 	    		   }
{ 	The parent process ID is read from command line.						   }
{ 																			   }
{ 	The following necessary functions	are used:																	   }
{ 	* InitializeProcThreadAttributeList																		   }
{ 	* UpdateProcThreadAttribute																		   }
{ 	* DeleteProcThreadAttributeList																		   }
{ 	* CreateProcess																		   }

{ 																			   }
{ Preparations: JWA must be ready to use.       							   }
{ 	It only works on Windows VISTA.																	   }
{ 																			   }
{ Todo: Not written for unicode! Needs some $IFDEF UNICODE					   }
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
program CreateProcessParent;

{$APPTYPE CONSOLE}

uses
  jwaWinNt,
  jwaWinType,
  JwaWinError,
  jwaWinBase,
  SysUtils;

type
  PAttributesList = ^TAttributesList;
  //just a return container for SetParentProcess
  TAttributesList = record
    //Attribute count of List
	  Count   : DWORD;
    //List of attributes
	  List    : PProcThreadAttributeList;
    //pointer to process handle stored in List
	  Process : PHANDLE;
  end;

function SetParentProcess(ProcessID : DWORD; AddAttribute : DWORD = 0) : PAttributesList;
var
  Size     : SIZE_T;
  List     : PProcThreadAttributeList;
  hProcess : PHANDLE;
begin
  result := nil;

  //get size of necessary memory
  if (not InitializeProcThreadAttributeList(
    nil,//__out_opt   LPPROC_THREAD_ATTRIBUTE_LIST lpAttributesList,
		1 + AddAttribute,//__in        DWORD dwAttributeCount,
		0,//__reserved  DWORD dwFlags,
		Size//__inout     PSIZE_T lpSize
  	)) then
  begin
    if (GetLastError() = ERROR_INSUFFICIENT_BUFFER) then
		begin

      GetMem(List, Size);

      //Create the attribute list
      if (not InitializeProcThreadAttributeList(
        List,//__out_opt   LPPROC_THREAD_ATTRIBUTE_LIST lpAttributesList,
        1 + AddAttribute,//__in        DWORD dwAttributeCount,
        0,//__reserved  DWORD dwFlags,
        Size//__inout     PSIZE_T lpSize
        )) then
      begin
        FreeMem(List);
        exit;
      end;

      //get handle to process we want as a parent process
      new(hProcess);
      hProcess^ := OpenProcess(PROCESS_CREATE_PROCESS, false, ProcessID);
      //hProcess must be kept alive until the list is destroyed!

      if (hProcess^ = 0) then
      begin
        FreeMem(List);
        Dispose(hProcess);
        exit;
      end;

      //set the parent process attribute
      if (not UpdateProcThreadAttribute(
			  List,//__inout    LPPROC_THREAD_ATTRIBUTE_LIST lpAttributesList,
			  0,//__in       DWORD dwFlags,
			  PROC_THREAD_ATTRIBUTE_PARENT_PROCESS,//__in       DWORD_PTR Attribute,
			  hProcess,//__in       PVOID lpValue,
			  sizeof(hProcess^),//__in       SIZE_T cbSize,
			  nil,//__out_opt  PVOID lpPreviousValue,
			  nil//__in_opt   PSIZE_T lpReturnSize
			)) then
			begin
		    FreeMem(List);
        Dispose(hProcess);
        exit;
      end;

      //create the return values
      new(result);
      result^.Count := 1 + AddAttribute;
			result^.Process := hProcess;
			result^.List := List;
    end;
  end;
end;

procedure FreeAttributesList(var AttributesList : PAttributesList);
begin
	if (AttributesList = nil) then
		exit;

	if (AttributesList^.List <> nil) then
  begin
		DeleteProcThreadAttributeList(AttributesList^.List);
    FreeMem(AttributesList^.List);
  end;

	if (AttributesList^.Process <> nil) then
	begin
		CloseHandle(AttributesList^.Process^);
    Dispose(AttributesList^.Process);
  end;

	FreeMem(AttributesList);
	AttributesList := nil;
end;


var
  AttributesList : PAttributesList;
	pi : PROCESS_INFORMATION;
	si : STARTUPINFOEX;
  CommandLine : String;
  WinPath : array[0..MAX_PATH] of Char;
  PID : DWORD;
begin
  Write('Enter a PID from any process in your session: ');
  Readln(PID);

  try
    AttributesList := setParentProcess(PID);
    if (AttributesList = nil) then
      exit;


    ZeroMemory(@si, sizeof(si));
    si.lpAttributeList := AttributesList^.List;
    si.StartupInfo.cb := sizeof(si); //Ex size!

    ZeroMemory(@WinPath, sizeof(WinPath));
    GetWindowsDirectory(WinPath, MAX_PATH);

    CommandLine := (WinPath) + '\explorer.exe /separate';

    try
      if (not CreateProcess(
          nil,//__in_opt    LPCWSTR lpApplicationName,
          PChar(CommandLine),//__inout_opt LPWSTR lpCommandLine,
          nil,//__in_opt    LPSECURITY_ATTRIBUTES lpProcessAttributes,
          nil,//__in_opt    LPSECURITY_ATTRIBUTES lpThreadAttributes,
          FALSE, //__in        BOOL bInheritHandles,
          CREATE_DEFAULT_ERROR_MODE or
          EXTENDED_STARTUPINFO_PRESENT,//__in        DWORD dwCreationFlags,
          nil,//__in_opt    LPVOID lpEnvironment,
          nil,//__in_opt    LPCWSTR lpCurrentDirectory,
          TStartupInfo(PStartupInfo(@si)^), //__in        LPSTARTUPINFOW lpStartupInfo,
          pi//__out       LPPROCESS_INFORMATION lpProcessInformation
          )) then
      begin
        exit;
      end;

      SetLastError(0);

      CloseHandle(pi.hProcess);
      CloseHandle(pi.hThread);
    finally
      FreeAttributesList(AttributesList);
      CommandLine := '';
    end;

  finally
    if GetLastError() <> 0 then
    begin
      Writeln('LastError: ', GetLastError());
      readln;
    end;
  end;
end.
