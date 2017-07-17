unit Unit1;

interface


{.$DEFINE JWA_WINDOWS}
uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
{$IFDEF JWA_WINDOWS}
  JwaWindows;  //first create the jwaWindows.dcu
{$ELSE}
  JwaShlWAPI;  //set search path to jwapi/Win32API and jwapi/Common
{$ENDIF JWA_WINDOWS}

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
begin
  MessageBoxCheck(Handle, 'Dies ist nur ein Testtext.',
    'MessageBoxCheck Example for Jedi by stOrM!', MB_OK or MB_ICONINFORMATION, 0,
    '2A622957-D0A5-41CA-8571-898247531A2C');


{ if you click the checkbox, the value will be stored in the registry under:
HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\
DontShowMeThisDialogAgain }

end;

end.
