(*
  Name:             testvafu
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
unit testvafu;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, VistaAltFixUnit2, StdCtrls, ExtCtrls, XPMan;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Panel1: TPanel;
    Button3: TButton;
    CheckBox3: TCheckBox;
    XPManifest1: TXPManifest;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses vafuForm2;

procedure TForm1.Button1Click(Sender: TObject);
begin
  TForm1.Create(Self).Show;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TVistaAltFix2.Create(nil);
end;

end.
