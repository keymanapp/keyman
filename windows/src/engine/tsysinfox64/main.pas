unit main;

interface

procedure Run;

implementation

uses
  ActiveX, Windows, si_base;

procedure Run;
begin
  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  if ParamCount = 0 then
  begin
    Windows.MessageBox(0, 'Usage: tsysinfox64.exe <output.xml>',
      'Keyman System Information (X64)', MB_OK or MB_ICONERROR);
    Exit;
  end;

  with TSIList.Create do
  try
    Collect;
    Save(ParamStr(1));
  finally
    Free;
  end;

  CoUninitialize;
end;

end.
