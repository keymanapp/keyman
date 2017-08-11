(*
  Name:             test_load_user_profile
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      22 Feb 2011

  Modified Date:    3 May 2011
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          22 Feb 2011 - mcdurdin - I2651 - Install does not set desired default options
                    03 May 2011 - mcdurdin - I2890 - Record diagnostic data when encountering registry errors
*)
unit test_load_user_profile;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
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
  SysConst,
  ShellUserRegistry,
  ErrorControlledRegistry;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with TRegistryErrorControlled.Create do  // I2890
  try
    if OpenKeyReadOnly('Software\Tavultesoft\Test') then
      ShowMessage(ReadString('Test'));
  finally
    Free;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  with TShellUserRegistry.Create do
  try
    if OpenKeyReadOnly('Software\Tavultesoft\Test') then
      ShowMessage(ReadString('Test'));
  finally
    Free;
  end;
end;


end.
