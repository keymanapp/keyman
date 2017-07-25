unit InternetState;

interface

uses Windows, wininet;

function IsKeymanWebsiteAvailable: Boolean;

implementation

function IsKeymanWebsiteAvailable: Boolean;
var
  dw: DWord;
  hInt, hURL: HINTERNET;
begin
  Result := False;

  if InternetGetConnectedState(@dw, 0) then
  begin
    // We can test whether the website is alive now
    hInt := InternetOpen(PChar(API_UserAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
    if hInt <> nil then
    begin
      hURL := InternetOpenUrl(hInt, URL_KeymanHome, nil, 0, 0, 0);
      if hURL <> nil then
      begin
        InternetCloseHandle(hURL);
        Result := True;
      end;
      InternetCloseHandle(hInt);
    end;
  end;
end;

end.
