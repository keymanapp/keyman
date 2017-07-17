program PrivateSD;

uses
{$IFDEF FASTMM}
  FastMM4,   
{$ENDIF}  
  Forms,
  MainForm in 'MainForm.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
