program DefaultUILanguage;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Keyman.Configuration.System.InstallDefaultUILanguage in '..\..\..\desktop\kmshell\main\Keyman.Configuration.System.InstallDefaultUILanguage.pas',
  kmint in 'kmint.pas',
  custinterfaces in '..\..\..\global\delphi\cust\custinterfaces.pas',
  keymanapi_TLB in '..\..\..\engine\kmcomapi\keymanapi_TLB.pas';

var
  usertags, tags: TStrings;
begin
  try
    LoadKMCOM;

    tags := TInstallDefaultUILanguage.GetKeymanUILanguages;
    usertags := TInstallDefaultUILanguage.GetUserUILanguages;
    try
      writeln('Keyman UI Languages:'#13#10+tags.Text);
      writeln('System UI Languages:'#13#10+usertags.Text);
      writeln('Matching System UI Language: '+TInstallDefaultUILanguage.Find(tags, usertags));
    finally
      tags.Free;
      usertags.Free;
    end;


  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
