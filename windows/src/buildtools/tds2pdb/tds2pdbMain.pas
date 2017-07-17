(*
  Name:             tds2pdbMain
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      17 May 2012

  Modified Date:    17 May 2012
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          17 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
*)
unit tds2pdbMain;  // I3306

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    dlgOpen: TOpenDialog;
    editTDSFileName: TEdit;
    cmdBrowseTDS: TButton;
    cmdAnalyze: TButton;
    memoData: TMemo;
    procedure cmdBrowseTDSClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.cmdBrowseTDSClick(Sender: TObject);
begin
  if dlgOpen.Execute then
  begin
    editTDSFileName.Text := dlgOpen.FileName;
  end;
end;

end.
