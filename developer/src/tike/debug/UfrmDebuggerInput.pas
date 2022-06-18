unit UfrmDebuggerInput;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, UfrmTike;

type
  TfrmDebuggerInput = class(TTIKEForm)
    panInput: TPanel;
    Label1: TLabel;
    cmdCancel: TButton;
    Label2: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

end.
