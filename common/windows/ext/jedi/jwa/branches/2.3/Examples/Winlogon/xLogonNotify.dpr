{******************************************************************************}
{ JEDI API example WinLogon Notification Package  			                       }
{ http://jedi-apilib.sourceforge.net					                                 }
{ 									                                                           }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 									                                                           }
{ Author(s): stOrM!, Christian Wimmer   			                                 }
{ Creation date: 23th May 2008 					 	                                     }
{ Last modification date:	26th May 2008				                                 }
{ 									                                                           }
{ Description: Demonstrates how to create a Winlogon Notification Package and  }
{    draws a transparent window inside Winlogon Process	containing a 	         }
{ 	 PNG Image							                                                   }
{ Preparations: JwaWindows, any layer based graphic apllication e.g. Gimp or   }
{ 				Adobe Photoshop				                                               }
{ Article link:   							                                               }
{ http://blog.delphi-jedi.net/2008/05/27/                                      }
{      winlogon-notification-packagewinlogon-notification-package/	           }
{ Version history: 23/05/2008 first release        			                       }
{ 									                                                           }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 	       }
{ productive environments.						                                         }
{ The code has surely some errors that need to be fixed. In such a case	       }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link.						                                         }
{ 									                                                           }
{ Be aware that third party components used by this example may have other     }
{ licenses. Please see the corresponding component what they are.              }
{ You have to download them manually.                                          }
{ 									                                                           }
{ The JEDI API Logo is copyrighted and must not be used without permission     }
{ 									                                                           }
{ External components:                                                         }
{   PNG components: http://www.thany.org/article/18/Delphi_components          }
{ 	Graphics32 : http://sourceforge.net/projects/graphics32/                   }
{******************************************************************************}


library xLogonNotify;

uses
  SysUtils,
  Classes,
  Windows,
  Forms,
  LogoAppThread in 'LogoAppThread.pas',
  Unit1 in 'Unit1.pas' {Form1};

type
  TFnMsgeCallback = function (bVerbose: Boolean; lpMessage: PWideChar): Cardinal; stdcall;

TWlxNotificationInfo = record
    Size: Cardinal;
    Flags: Cardinal;
    UserName: PWideChar;
    Domain: PWideChar;
    WindowStation: PWideChar;
    Token: Cardinal;
    Desktop: Cardinal;
    StatusCallback: TFnMsgeCallback;
  end;
  PWlxNotificationInfo = ^TWlxNotificationInfo;

{$R *.res}

var Thread : TLogoThread;

procedure StartupHandler(Info: PWlxNotificationInfo); stdcall;

begin
try
  //don't show logo on Failsafe
  if GetSystemMetrics(SM_CLEANBOOT) <> 0 then exit;
   Thread := TLogoThread.Create(true);
   Thread.FreeOnTerminate := true;
   Thread.Resume;
 except
  end;
end;

procedure LogonHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure StartShellHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure LockHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure UnLockHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure StartScreenSaverHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure StopScreenSaverHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure LogoffHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure ShutdownHandler(Info: PWlxNotificationInfo); stdcall;
begin
  try
   Thread.Terminate;
  if WaitForSingleObject(Thread.Handle, 3* 1000{sec}) = WAIT_TIMEOUT then
   TerminateThread(Thread.Handle, 0);
 except
end;
end;

procedure DisconnectHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure ReconnectHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure PostShellHandler(Info: PWlxNotificationInfo); stdcall;
begin

end;

procedure EntryPointProc(reason: integer);
begin
  case reason of
    DLL_PROCESS_ATTACH:  //1
      begin
        DisableThreadLibraryCalls(hInstance);

      end;
    DLL_THREAD_ATTACH:   //2
      begin

      end;
    DLL_PROCESS_DETACH:  //3
      begin

      end;
    DLL_THREAD_DETACH:   //0
      begin

      end;
  end;

end;

exports
  StartupHandler,
  LogonHandler,
  StartShellHandler,
  LockHandler,
  UnLockHandler,
  StartScreenSaverHandler,
  StopScreenSaverHandler,
  LogoffHandler,
  ShutdownHandler,
  DisconnectHandler,
  ReconnectHandler,
  PostShellHandler;

begin
  DllProc := @EntryPointProc;
  DllProc(DLL_PROCESS_ATTACH);
end.

 