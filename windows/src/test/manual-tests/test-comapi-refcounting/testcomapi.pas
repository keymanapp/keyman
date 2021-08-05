unit testcomapi;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, keymanapi_TLB, StdCtrls;

type
  TProc = procedure of object;

  TForm1 = class(TForm)
    cmdStartTest: TButton;
    memoLog: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure cmdStartTestClick(Sender: TObject);
  private
    kmcom: ITavultesoftKeyman;
    procedure TestKeyboards;
    procedure Test(Name: string; Func: TProc);
    procedure TestPackages;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.cmdStartTestClick(Sender: TObject);
begin
  Test('Keyboards', TestKeyboards);
  Test('Packages', TestPackages);
end;

procedure TForm1.Test(Name: string; Func: TProc);
begin
  try
    Func;
  except
    on E:Exception do
      memoLog.Lines.Add('FAIL: '+Name+' on _Release: ' + E.ClassName + ' - ' + E.Message);
  end;
end;

procedure TForm1.TestKeyboards;
var
  k: IKeymanKeyboardInstalled;
  s, t: WideString;
begin
  k := kmcom.Keyboards.Items[1];
  s := k.Name;
  kmcom.Keyboards.Refresh;
  try
    t := k.Name;
  except
    on E:Exception do s := E.ClassName + ' - ' + E.Message;
  end;

  if t <> s
    then memoLog.Lines.Add('FAIL: Keyboards: '+t)
    else memoLog.Lines.Add('PASS: Keyboards: '+s+'='+t);
end;

procedure TForm1.TestPackages;
var
  k: IKeymanPackageInstalled;
  s, t: WideString;
begin
  k := kmcom.Packages.Items[1];
  s := k.Name;
  kmcom.Refresh;
//  kmcom.Packages.Refresh;
  try
    t := k.Name;
  except
    on E:Exception do s := E.ClassName + ' - ' + E.Message;
  end;

  if t <> s
    then memoLog.Lines.Add('FAIL: Packages: '+t)
    else memoLog.Lines.Add('PASS: Packages: '+s+'='+t);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  kmcom := CoTavultesoftKeyman.Create;
end;

end.
