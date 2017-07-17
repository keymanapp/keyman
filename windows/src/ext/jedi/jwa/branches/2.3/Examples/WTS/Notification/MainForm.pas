{******************************************************************************}
{ JEDI Windows Terminal Server Session Notification Example                    }
{ http://jedi-apilib.sourceforge.net										   }
{ 																			   }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{ 																			   }
{ Author(s): Christian Wimmer												   }
{ Creation date: 24th September 2008 					   				   	   }
{ Last modification date: 24th September 2008								   }
{ 																			   }
{ Description: Shows how to use the session notification messages 		       }
{   This technique cannot be used for services.                                }
{    Instead use HandlerEx mechanism (MSDN).                                   }
{ 																			   }
{ Tested on: Windows Vista SP1                                                 }
{ 																			   }
{ Requires: Windows XP or newer                                                }
{ 																			   }
{ Delphi: Delphi 7                                                             }
{ 																			   }
{ Preparations: JWA must be ready to use.       							   }
{ 																			   }
{ 																			   }
{ Version history: 24th Sept 2008 initial release				     	       }
{ 																			   }
{ No license. Use this example with no warranty at all and on your own risk.   }
{ This example is just for learning purposes and should not be used in 		   }
{ productive environments.													   }
{ The code has surely some errors that need to be fixed. In such a case	   	   }
{ you can contact the author(s) through the JEDI API hompage, the mailinglist  }
{ or via the article link (if available).									   }
{ 																			   }
{******************************************************************************}
unit MainForm;

interface

uses
  JwaWindows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TNotificiationForm = class(TForm)
    Memo1: TMemo;
    RegisterButton: TButton;
    CheckBoxAllSessions: TCheckBox;
    procedure RegisterButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
    fRegState : Boolean;
  public
    { Public-Deklarationen }
    procedure OnWTSSessionChange(var Message : TMessage); message WM_WTSSESSION_CHANGE;

  end;

var
  NotificiationForm: TNotificiationForm;

implementation

{$R *.dfm}

procedure TNotificiationForm.RegisterButtonClick(Sender: TObject);
var Flags : DWORD;
begin
  try
    if fRegState then
    begin
      WTSUnRegisterSessionNotification(Handle);
      RegisterButton.Caption := 'Register for Notifications';
    end
    else
    begin
      if CheckBoxAllSessions.Checked then
        Flags := NOTIFY_FOR_ALL_SESSIONS //needs admin rights to work correctly
      else
        Flags := NOTIFY_FOR_THIS_SESSION;

      RegisterButton.Caption := 'Unregister for Notifications';
      Win32Check(WTSRegisterSessionNotification(Handle, Flags));
    end;
  finally
    fRegState := not fRegState;  
  end;
end;

procedure TNotificiationForm.FormDestroy(Sender: TObject);
begin
  if fRegState then
    WTSUnRegisterSessionNotification(Handle);
end;

procedure TNotificiationForm.OnWTSSessionChange(var Message: TMessage);
var S : String;
begin
  case Message.WParam of
    WTS_CONSOLE_CONNECT: S := 'A new session #%d was connected to the local console.';
    WTS_CONSOLE_DISCONNECT: S := 'The session #%d was removed from the local console.';

    WTS_REMOTE_CONNECT : S := 'A new session #%d was connected to the remote console.';
    WTS_REMOTE_DISCONNECT: S := 'The session #%d was removed from the remote console.';
    WTS_SESSION_LOGON: S := 'A user has logged on to the session #%d.';
    WTS_SESSION_LOGOFF: S := 'A user logged off. Session #%d';

    //These messages are not send, if the winlogon desktop is shown without
    //password input
    WTS_SESSION_LOCK: S := 'The session #%d is locked.';
    WTS_SESSION_UNLOCK: S := 'The session #%d is unlocked.';

    WTS_SESSION_REMOTE_CONTROL:
      begin
        S := 'The session #%d changed its remote status. New status is: ';
        if GetSystemMetrics(SM_REMOTECONTROL) = 0 then
          S := S + 'Session is locally controlled.'
        else
          S := S + 'Session is remotely controlled.';
      end;
  else
    S := '';
  end;
  if Length(S) > 0 then
    Memo1.Lines.Add(Format(S,[Message.LParam]));
end;

procedure TNotificiationForm.FormCreate(Sender: TObject);
begin
  fRegState := false;
end;

end.
