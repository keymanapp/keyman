unit Keyman.Developer.UI.ServerUI;

interface

type
  TServerUI = class sealed
    class function VerifyServerRunning: Boolean; static;
  end;

implementation

uses
  System.UITypes,
  Vcl.Controls,
  Vcl.Dialogs,
  Winapi.Windows,

  Keyman.Developer.System.ServerAPI;

class function TServerUI.VerifyServerRunning: Boolean;
var
  t: UInt64;
begin
  if TServerDebugAPI.Running then
    Exit(True);

  case MessageDlg('Keyman Developer Server does not appear to be running. '+
     'Try and start Keyman Developer Server now?',
     mtConfirmation, mbYesNoCancel, 0) of
    mrYes:
      // Trying to start Server
      begin
        TServerDebugAPI.StartServer;
        repeat
          t := GetTickCount64;
          while GetTickCount64 - t < 3000 do
          begin
            Sleep(500);
            if TServerDebugAPI.Running then Exit(True);
          end;
          case MessageDlg('Keyman Developer Server has not yet started successfully. Keep waiting?',
              mtConfirmation, mbOkCancel, 0) of
            mrOk: ;
            mrCancel: Exit(False);
          end;
        until False;
      end;
    mrNo:
      // Try and continue even though Server does not appear to be running...
      Exit(True);
    mrCancel:
      ;
  end;

  Result := False;
end;

end.
