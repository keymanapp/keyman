unit KeymanOptionNames;

interface

type
  TUtilKeymanOption = (
    // General options
    koKeyboardHotkeysAreToggle, koAltGrCtrlAlt, koReleaseShiftKeysAfterKeyPress,
    koShowHints,   // I1256
    // Startup options
    koTestKeymanFunctioning,
    koStartWithWindows,
    koShowStartup,
    koCheckForUpdates,
    // Windows options
    koDebugging,
    koSwitchLanguageForAllApplications, // I1089
    koDeadkeyConversion,   // I4552
    // On Screen Keyboard Options
    koAutoOpenOSK,          // I1288
    koAutoSwitchOSKPages,  // I1375
    koUseTouchLayout,
    koBaseLayout
    );

function KeymanOptionName(Option: TUtilKeymanOption): string;

implementation

uses
  System.TypInfo;

function KeymanOptionName(Option: TUtilKeymanOption): string;
begin
  Result := GetEnumName(TypeInfo(TUtilKeymanOption), Ord(Option));
end;

end.
