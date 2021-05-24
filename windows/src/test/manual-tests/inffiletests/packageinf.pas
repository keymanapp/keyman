unit packageinf;

interface

procedure run;

implementation

uses
  System.Win.ComObj,
  Winapi.ActiveX,
  PackageInfo,
  kmpinffile;

procedure run;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);
  with TKMPInfFile.Create do
  try
    FileName := 'c:\temp\kmp.inf';
    LoadIni;
    FileName := 'c:\temp\kmp.xml';
    SaveXML;
    FileName := 'c:\temp\kmp.json';
    SaveJSON;
  finally
    Free;
  end;
end;

end.

