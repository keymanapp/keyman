(*
  Name:             vafuForm2
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      2 Feb 2012

  Modified Date:    2 Feb 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          02 Feb 2012 - mcdurdin - I2975 - VistaAltFixUnit causes crash on shutdown
*)
unit vafuForm2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs;

type
  TForm2 = class(TForm)
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
//  ShowMessage('!');
//  SendMessage(Handle, WM_USER, 0, 0);
end;

end.
