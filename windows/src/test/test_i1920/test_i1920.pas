(*
  Name:             test_i1920
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      23 Apr 2009

  Modified Date:    23 Apr 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          23 Apr 2009 - mcdurdin - I1920 - Test I1920
*)
unit test_i1920;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    f1,f2:TFileStream;
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
  f1 := TFileStream.Create('c:\program files\tavultesoft\keyman desktop professional 7.0\desktop_pro.pxx', fmOpenRead or fmShareDenyWrite);
  label1.Caption := 'F1 open';
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  f2 := TFileStream.Create('c:\program files\tavultesoft\keyman desktop professional 7.0\desktop_pro.pxx', fmOpenRead or fmShareDenyWrite);
  label2.Caption := 'F2 open';
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FreeAndNil(f1);
  label1.Caption := 'F1 close';
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FreeAndNil(f2);
  label2.Caption := 'F2 close';
end;

end.
