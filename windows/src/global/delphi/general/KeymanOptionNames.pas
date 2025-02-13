unit KeymanOptionNames;

interface

type
  TUtilKeymanOption = (
    // General options
    koKeyboardHotkeysAreToggle,
    koAltGrCtrlAlt,
    koRightModifierHK,
    koReleaseShiftKeysAfterKeyPress,
    koShowHints,   // I1256
    koCheckForUpdates,
    // Startup options
    koTestKeymanFunctioning,
    koStartWithWindows,
    koShowStartup,
    // Windows options
    koDebugging,
    koSwitchLanguageForAllApplications, // I1089
    koDeadkeyConversion,   // I4552
    // On Screen Keyboard Options
    koAutoOpenOSK,          // I1288
    koAutoSwitchOSKPages,  // I1375
    koUseTouchLayout,
    koBaseLayout,
    koAutomaticallyReportErrors,
    koAutomaticallyReportUsage
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
