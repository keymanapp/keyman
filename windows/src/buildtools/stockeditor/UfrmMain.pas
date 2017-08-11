unit UfrmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  Tmd = class(TForm)
    cmdMessages: TButton;
    Button1: TButton;
    procedure cmdMessagesClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  md: Tmd;

implementation

uses StockMessageEditor, COMMessageEditor;

{$R *.DFM}

procedure Tmd.cmdMessagesClick(Sender: TObject);
begin
  with TfrmStockMessageEditor.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

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
