unit wm_unichar_form;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, AppEvnts, ComCtrls, StdCtrls, TntComCtrls, TntStdCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    RichEdit1: TRichEdit;
    TntMemo1: TTntMemo;
    TntRichEdit1: TTntRichEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
