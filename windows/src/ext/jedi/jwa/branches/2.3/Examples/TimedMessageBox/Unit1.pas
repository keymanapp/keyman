unit Unit1;

//JwaWindows or only JwaShlwAPI ?
{.$DEFINE JWAWINDOWS}
interface

uses
  Messages, Variants, Classes, Controls, Forms,
  Dialogs, StdCtrls, Windows,
{$IFDEF JWAWINDOWS}
  JwaWindows; 
{$ELSE}
  JwaWinUser, JwaShLWAPI;
{$ENDIF}

type
  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  iResult: Integer;
  iFlags : Integer;
begin
  iFlags  := MB_YESNO or MB_SETFOREGROUND or MB_SYSTEMMODAL or MB_ICONINFORMATION;

  iResult := MessageBoxTimeout(
    Application.Handle,
    'Test MessageBoxTimeOut countdown 5 seconds...',
    'MessageBoxTimeout for Jedi by stOrM!', iFlags, 0, 5000);

  // iResult = MB_TIMEDOUT if no buttons clicked, otherwise
  // iResult will return the value of the button clicked

  case iResult of
  IDYES:  // Pressed Yes button
    ShowMessage('Yes');

  IDNO:  // Pressed the No button
    ShowMessage('No');

  MB_TIMEDOUT: // MessageBox timed out
    ShowMessage('TimedOut');
  end;
end;

end.
