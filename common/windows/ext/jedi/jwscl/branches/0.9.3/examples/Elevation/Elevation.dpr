{
This demo shows how JwElevateProcess from unit JwsclElevation works.

Note:
 Using Vista or Win7 radiobox on a pre Vista OS does not work
 and will fail with invalid parameter exception.

Original author is: Christian Wimmer
This application is part of the JEDI API Project.
Visit at http://blog.delphi-jedi.net/

Version 1.0
Creation date: 17. January 2009

}
program Elevation;

uses
  Forms,
  ElevationForm in 'ElevationForm.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
