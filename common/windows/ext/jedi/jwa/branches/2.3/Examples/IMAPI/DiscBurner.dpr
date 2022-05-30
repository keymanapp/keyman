{Demonstrates how to use the unit uDiscBurner.pas
Written in Delphi 2009.
}
program DiscBurner;

uses
  Forms,
  uFormMain in 'uFormMain.pas' {form_Main},
  uDiscBurner in 'uDiscBurner.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(Tform_Main, form_Main);
  Application.Run;
end.
