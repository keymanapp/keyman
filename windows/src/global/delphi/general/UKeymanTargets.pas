unit UKeymanTargets;

interface

type
  TKeymanTarget = (
    ktAny,
    ktWindows, ktMacosx, ktLinux,
    ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet,
    ktMobile, ktDesktop, ktTablet);

  TKeymanTargets = set of TKeymanTarget;

const
  // Note: if these change, update the copied values in KeyboardParser.pas accordingly

  AllKeymanTargets: TKeymanTargets = [
    ktAny,
    ktWindows, ktMacosx, ktLinux,
    ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet,
    ktMobile, ktDesktop, ktTablet
  ];

  TouchKeymanTargets: TKeymanTargets = [
    ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet,
    ktMobile, ktTablet
  ];

  // Compile to .kmx
  KMXKeymanTargets: TKeymanTargets = [
    ktWindows, ktMacosx, ktDesktop
  ];

  // Compile to .js
  KMWKeymanTargets: TKeymanTargets = [
    ktWeb, ktIphone, ktIpad, ktAndroidphone, ktAndroidtablet,
    ktMobile, ktTablet
  ];

  // Other targets?
  //

  SKeymanTargets: array[TKeymanTarget] of string = (
    'any',
    'windows', 'macosx', 'linux',
    'web', 'iphone', 'ipad', 'androidphone', 'androidtablet',
    'mobile', 'desktop', 'tablet');

function KeymanTargetsToString(ATargets: TKeymanTargets): string;
function StringToKeymanTargets(ATargets: string): TKeymanTargets;

implementation

uses
  System.SysUtils,
  utilstr;

function StringToKeymanTargets(ATargets: string): TKeymanTargets;
var
  s: string;
  i: TKeymanTarget;
begin
  Result := [];
  s := StrToken(ATargets, ' ');
  while s <> '' do
  begin
    for i := Low(TKeymanTarget) to High(TKeymanTarget) do
      if SameText(SKeymanTargets[i], s) then
      begin
        Include(Result, i);
        Break;
      end;
    s := StrToken(ATargets, ' ');
  end;
end;

function KeymanTargetsToString(ATargets: TKeymanTargets): string;
var
  i: TKeymanTarget;
begin
  Result := '';
  for i := Low(TKeymanTarget) to High(TKeymanTarget) do
    if i in ATargets then
      Result := Result + SKeymanTargets[i] + ' ';
  Result := Trim(Result);
end;

end.
