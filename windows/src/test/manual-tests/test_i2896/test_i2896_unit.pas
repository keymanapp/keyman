(*
  Name:             test_i2896_unit
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      28 Jun 2011

  Modified Date:    28 Jun 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          28 Jun 2011 - mcdurdin - I2896 - Test root certificate availability
*)
unit test_i2896_unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  certificate_check;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  buf: array[0..260] of char;
  AppName: string;
  cert_p: PWinCertificate;

    function Check(s: string; v: Integer): Boolean;
    begin
      Result := v = ERROR_CS_NONE;
      if not Result then
      begin
        memo1.lines.add('Failed to verify, '+s+': '+IntToStr(v));
      end;
    end;
begin
  if not Check('CCAvailable', CCAvailable) then Exit;

  GetModuleFileName(hInstance, buf, 260); AppName := buf;

  {Check('CCIsFileSigned', CCIsFileSigned(AppName));
  if not Check('CCRetrieveSignature', CCRetrieveSignature(AppName, cert_p)) then Exit;
  FreeMem(cert_p);}

  if Check('CCRetrieveSignature', CCRetrieveSignature(AppName, cert_p)) then
  begin
    if not Check('CCIsFileSigned', CCIsFileSigned(AppName)) then
    begin
      ShowMessage('Root certificates are out of date -- get the latest matest');
    end;
    FreeMem(cert_p);
  end;
end;

end.
