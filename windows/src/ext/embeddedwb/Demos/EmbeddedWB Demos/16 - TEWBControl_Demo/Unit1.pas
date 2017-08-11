unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, StdCtrls, EwbControlComponent, OleCtrls, SHDocVw_EWB, EwbCore, EmbeddedWB,
  ExtCtrls, Graphics;

type
  TForm1 = class(TForm)
    EmbeddedWB1: TEmbeddedWB;
    EmbeddedWB2: TEmbeddedWB;
    EwbControl1: TEwbControl;
    ListBox1: TListBox;
    Timer1: TTimer;
    Button2: TButton;
    Panel1: TPanel;
    chkFocusControl: TCheckBox;
    ChkMouseWheelFix: TCheckBox;
    chkActiveFormOnly: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure ChkMouseWheelFixClick(Sender: TObject);
    procedure chkFocusControlClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EwbControl1MouseWheel(p: TPoint; hwndFromPoint: HWND;
      lp: Integer; var Handled: Boolean);
    procedure Button2Click(Sender: TObject);
    procedure chkActiveFormOnlyClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  EwbCoreTools, Unit2;

procedure TForm1.ChkMouseWheelFixClick(Sender: TObject);
begin
  EwbControl1.MouseWheelFix.Active := ChkMouseWheelFix.Checked;
end;

procedure TForm1.chkFocusControlClick(Sender: TObject);
begin
  EwbControl1.FocusControl.Active := ChkFocusControl.Checked;
end;

procedure TForm1.chkActiveFormOnlyClick(Sender: TObject);
begin
  EwbControl1.MouseWheelFix.ActiveFormOnly := chkActiveFormOnly.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: ShortInt;
begin
  for i := 0 to 50 do Listbox1.Items.Add(Inttostr(i));
  ChkMouseWheelFix.Checked := EwbControl1.MouseWheelFix.Active;
  ChkFocusControl.Checked := EwbControl1.FocusControl.Active;
end;

procedure TForm1.EwbControl1MouseWheel(p: TPoint; hwndFromPoint: HWND;
  lp: Integer; var Handled: Boolean);
begin
  if GetWinClass(hwndFromPoint) = 'TListBox' then
  begin
    Windows.SetFocus(hwndFromPoint);
    Handled := True;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  try
    Label3.caption := Screen.ActiveControl.Name;
  except
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Form2.Show;
end;


end.

