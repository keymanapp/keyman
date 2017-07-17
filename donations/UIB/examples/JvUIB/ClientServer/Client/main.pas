unit main;

interface

uses
{$IFDEF LINUX}
  libc, QForms, QStdCtrls, QControls, QGraphics, QDialogs, QExtCtrls, QGrids,
{$ELSE}
  Windows, Graphics, Controls, Forms, Messages, Dialogs, StdCtrls, Grids,
{$ENDIF}
  SysUtils, Classes, JvUIB, RemoteObject_UIB;

type
  TMainForm = class(TForm)
    Button1: TButton;
    StringGrid: TStringGrid;
    Button2: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { D�clarations priv�es }
  public
    { D�clarations publiques }
  end;

var
  MainForm: TMainForm;

implementation
uses JvUIBLib;

{$R *.dfm}
var ClientObj: TProxyRemoteObject;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ClientObj := TProxyRemoteObject.Create;
  ClientObj.Host := 'localhost';
  ClientObj.Port := 9545;
  ClientObj.Active := true;
  ClientObj.ClassID := CLSID_RemoteObject
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  ClientObj.Free;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  Stream: TMemoryStream;
  SQLResult: TSQLResult;
  i, j: Integer;
begin
  Stream := TMemoryStream.Create;
  SQLResult := TSQLResult.Create;
  try
    ClientObj.OpenQuery('select * from project', Stream);
    SQLResult.LoadFromStream(Stream);

    StringGrid.ColCount := SQLResult.FieldCount  + 1;
    StringGrid.RowCount := SQLResult.RecordCount + 1;

    for i := 1 to SQLResult.FieldCount do
      StringGrid.Cells[i, 0] := SQLResult.AliasName[i-1];

    for i := 1 to SQLResult.RecordCount do
    begin
      SQLResult.GetRecord(i-1);
      StringGrid.Cells[0, i] := Inttostr(i);
      for j := 1 to SQLResult.FieldCount do
        StringGrid.Cells[j, i] := SQLResult.AsString[j-1];
    end;
  finally
     Stream.Free;
     SQLResult.Free;
  end;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var Count: Integer;
begin
  ClientObj.GetEmployeeCount(Count);
  Label1.Caption := IntToStr(Count);
end;

end.
