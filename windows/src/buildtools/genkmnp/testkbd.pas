unit testkbd;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TForm2 = class(TForm)
    Label1: TLabel;
  private
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

procedure TForm2.WMKeyDown(var Message: TMessage);
begin
    Label1.Caption := IntToHex(Message.LParam, 8);
end;

{$R *.DFM}

end.
