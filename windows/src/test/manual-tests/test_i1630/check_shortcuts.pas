(*
  Name:             check_shortcuts
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      16 Jan 2009

  Modified Date:    16 Jan 2009
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          16 Jan 2009 - mcdurdin - Initial version
*)
unit check_shortcuts;

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
  Menus, KeyNames, utilhotkey;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Integer;
  sc: TShortCut;
begin
  for i := Low(SKeyNames) to High(SKeyNames) do
  begin
    if Copy(SKeyNames[i], 1, 1) = '?' then Continue;
    sc := HotkeyToShortcut(i);
    memo1.Lines.Add(IntToStr(i) + #9 + SKeyNames[i] + #9 + IntToStr(Lo(sc)) + #9 + ShortCutToTextEx(sc));
  end;
end;

end.
