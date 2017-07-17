unit UfrmMainBrowserRendererSettings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    cmdCorrect: TButton;
    Label1: TLabel;
    procedure cmdCorrectClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Win.Registry,

  KeymanPaths;

{$R *.dfm}

procedure TForm1.cmdCorrectClick(Sender: TObject);
var
  res: Integer;
begin
  res := 0;

  with TRegistry.Create do
  try
    if OpenKey('Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION', False) then
    begin
      if ValueExists(TKeymanPaths.S_KMShell) then
      begin
        DeleteValue(TKeymanPaths.S_KMShell);
        Inc(res);
      end;
      if ValueExists(TKeymanPaths.S_KeymanExe) then
      begin
        DeleteValue(TKeymanPaths.S_KeymanExe);
        Inc(res);
      end;
    end;

    if OpenKey('\Software\Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_NATIVE_DOCUMENT_MODE', False) then
    begin
      if ValueExists(TKeymanPaths.S_KMShell) then
      begin
        DeleteValue(TKeymanPaths.S_KMShell);
        Inc(res);
      end;

      if ValueExists(TKeymanPaths.S_KeymanExe) then
      begin
        DeleteValue(TKeymanPaths.S_KeymanExe);
        Inc(res);
      end;
    end;

  finally
    Free;
  end;

  if res > 0 then
    ShowMessage('The browser renderer settings were corrected.')
  else
    ShowMessage('No settings were changed: the browser renderer settings appear to be correct.');
end;

end.
