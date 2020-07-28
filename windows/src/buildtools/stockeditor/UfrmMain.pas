unit UfrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  Tmd = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  md: Tmd;

implementation

uses COMMessageEditor;

{$R *.DFM}

procedure Tmd.Button1Click(Sender: TObject);
begin
  with TfrmCOMMessages.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

end.
