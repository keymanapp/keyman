unit test_i3723;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  keymanapi_TLB,
  System.Win.Registry;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  kmcom: ITavultesoftKeyman;
  keyboard: IKeymanKeyboardInstalled;
  n: Integer;
begin
  kmcom := CoTavultesoftKeyman.Create;
  n := kmcom.Keyboards.IndexOf('i3723');
  if n = 0 then
  begin
    ShowMessage('Please install keyboard i3723.kmx before continuing.');
    Exit;
  end;

  keyboard := kmcom.Keyboards[n];

  (keyboard as IKeymanKeyboardInstalled3).Languages.Install(0, 'en-NZ');
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  kmcom: ITavultesoftKeyman;
  keyboard: IKeymanKeyboardInstalled;
  n: Integer;
begin
  kmcom := CoTavultesoftKeyman.Create;
  n := kmcom.Keyboards.IndexOf('i3723');
  if n = 0 then
  begin
    ShowMessage('Please install keyboard i3723.kmx before continuing.');
    Exit;
  end;

  keyboard := kmcom.Keyboards[n];

  if (keyboard as IKeymanKeyboardInstalled3).Languages.Count = 0 then
  begin
    ShowMessage('Please add a language profile for keyboard i3723.kmx before continuing.');
    Exit;
  end;

  (keyboard as IKeymanKeyboardInstalled3).Languages[1].Uninstall;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey('Software\Tavultesoft', True)
      then Label1.Caption := 'Process is elevated'
      else Label1.Caption := 'Process is not elevated';
  finally
    Free;
  end;
end;

end.
