unit Keyman.System.BaseKeyboard;

interface

uses
  Winapi.Windows;

type
  TBaseKeyboard = class sealed
  private
    class function IsLatinScriptLayout(Layout: DWORD): Boolean; static;
  public
    class function GetDefaultBaseLayoutID: DWORD; static;
  end;

implementation

uses
  System.SysUtils,
  System.Win.Registry,

  Glossary,
  RegistryKeys,
  utilkeyman;

const
  BaseKeyboardID_USEnglish: Integer = $00000409;

  // List constructed from Win10 20H2, Mar 2021
  KnownLatinScriptLayouts: array[0..93] of DWORD = (
    $00000405, // Czech
    $00000406, // Danish
    $00000407, // German
    $00000409, // US
    $0000040a, // Spanish
    $0000040b, // Finnish
    $0000040c, // French
    $0000040e, // Hungarian
    $0000040f, // Icelandic
    $00000410, // Italian
    $00000413, // Dutch
    $00000414, // Norwegian
    $00000415, // Polish (Programmers)
    $00000416, // Portuguese (Brazilian ABNT)
    $00000418, // Romanian (Legacy)
    $0000041a, // Standard
    $0000041b, // Slovak
    $0000041c, // Albanian
    $0000041d, // Swedish
    $0000041f, // Turkish Q
    $00000424, // Slovenian
    $00000425, // Estonian
    $00000426, // Latvian
    $00000427, // Lithuanian IBM
    $0000042a, // Vietnamese
    $0000042c, // Azeri Latin
    $0000042e, // Sorbian Standard (Legacy)
    $00000432, // Setswana
    $00000438, // Faeroese
    $0000043a, // Maltese 47-Key
    $0000043b, // Norwegian with Sami
    $00000442, // Turkmen
    $00000452, // United Kingdom Extended
    $00000468, // Hausa
    $0000046a, // Yoruba
    $0000046c, // Sesotho sa Leboa
    $0000046e, // Luxembourgish
    $0000046f, // Greenlandic
    $00000470, // Igbo
    $00000474, // Guarani
    $00000475, // Hawaiian
    $00000481, // Maori
    $00000488, // Wolof
    $00000807, // Swiss German
    $00000809, // United Kingdom
    $0000080a, // Latin American
    $0000080c, // Belgian French
    $00000813, // Belgian (Period)
    $00000816, // Portuguese
    $0000081a, // Serbian (Latin)
    $0000083b, // Swedish with Sami
    $0000085d, // Inuktitut - Latin
    $0000085f, // Central Atlas Tamazight
    $00000c0c, // Canadian French (Legacy)
    $00001009, // Canadian French
    $0000100c, // Swiss French
    $00001809, // Irish
    $00004009, // India
    $00010402, // Bulgarian (Latin)
    $00010405, // Czech (QWERTY)
    $00010407, // German (IBM)
    $00010409, // United States-Dvorak
    $0001040a, // Spanish Variation
    $0001040e, // Hungarian 101-key
    $00010410, // Italian (142)
    $00010415, // Polish (214)
    $00010416, // Portuguese (Brazilian ABNT2)
    $00010418, // Romanian (Standard)
    $0001041b, // Slovak (QWERTY)
    $0001041f, // Turkish F
    $00010426, // Latvian (QWERTY)
    $00010427, // Lithuanian
    $0001042c, // Azerbaijani (Standard)
    $0001042e, // Sorbian Extended
    $0001043a, // Maltese 48-Key
    $0001043b, // Sami Extended Norway
    $0001045d, // Inuktitut - Naqittaut
    $0001080c, // Belgian (Comma)
    $0001083b, // Finnish with Sami
    $00011009, // Canadian Multilingual Standard
    $00011809, // Gaelic
    $00020405, // Czech Programmers
    $00020409, // United States-International
    $00020418, // Romanian (Programmers)
    $00020426, // Latvian (Standard)
    $00020427, // Lithuanian Standard
    $0002042e, // Sorbian Standard
    $0002083b, // Sami Extended Finland-Sweden
    $00030408, // Greek (220) Latin
    $00030409, // United States-Dvorak for left hand
    $00040408, // Greek (319) Latin
    $00040409, // United States-Dvorak for right hand
    $00050408, // Greek Latin
    $00050409  // US English Table for IBM Arabic 238_L
  );

// Tests if the reported base layout is known to be Latin script or not
//
// We work off a database of known layouts, then fallback to our cache. If not
// found in the cache, or the cache has not yet been built, then we'll assume it
// isn't a Latin script layout.
//
// The reason we do this is while we can determine algorithmically that a given
// layout is Latin-script based, this involves a fairly costly load of the
// layout DLL and enumeration of every key on the keyboard. We don't want to be
// loading the layout DLL every time that the COM objects are instantiated; both
// because of the cost and also to avoid any potential side-effects.
//
// If the user has chosen a layout that is not present in our known layouts
// list, through the Base Keyboard dialog, we can safely assume the cache has
// been built -- because the cache has to be populated in order to present the
// list of available layouts in kmshell's BaseKeyboards classes.
//
class function TBaseKeyboard.IsLatinScriptLayout(Layout: DWORD): Boolean;
var
  i: DWord;
  r: TRegistry;
begin
  // Check our known Latin script layouts first, because we may not yet
  // have built our local cache
  for i in KnownLatinScriptLayouts do
    if i = Layout then
      Exit(True);

  // Otherwise, our Latin script keyboard cache in the registry tells us
  // if this is a known Latin-script layout
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    Result :=
      r.OpenKeyReadOnly(SRegKey_LatinKeyboardCache_LM) and
      r.ValueExists(IntToHex(Layout, 8)) and
      (r.ReadString(SRegKey_LatinKeyboardCache_LM) = '1');
  finally
    r.Free;
  end;
end;

//
// Return only a valid Latin script base layout keyboard id
//
class function TBaseKeyboard.GetDefaultBaseLayoutID: DWORD;   // I4169
begin
  Result := GetDefaultHKL;

  if Result <> 0 then
    Result := HKLToKeyboardID(Result);

  if (Result = 0) or not IsLatinScriptLayout(Result) then
    Result := BaseKeyboardID_USEnglish;
end;

end.
