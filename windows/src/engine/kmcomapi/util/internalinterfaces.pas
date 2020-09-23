(*
  Name:             internalinterfaces
  Copyright:        Copyright (C) 2003-2017 SIL International.
  Documentation:    
  Description:      
  Create Date:      25 Jan 2011

  Modified Date:    17 Aug 2014
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          25 Jan 2011 - mcdurdin - I2569 - Keyboard welcome should always shown from kmshell
                    01 Jan 2013 - mcdurdin - I3717 - V9.0 - Need ability to select base keyboard in Keyman Configuration
                    16 Apr 2014 - mcdurdin - I4169 - V9.0 - Mnemonic layouts should be recompiled to positional based on user-selected base keyboard
                    17 Aug 2014 - mcdurdin - I4376 - V9.0 - Unticked keyboards in configuration should be removed from language profile
                    17 Aug 2014 - mcdurdin - I4381 - V9.0 - Keyman keyboards should be removed from language bar when Keyman exits
                    
*)
unit internalinterfaces;

interface

uses
  ActiveX,
  Classes,
  msctf,
  keymanapi_tlb,
  RegKeyboards,
  custinterfaces,
  Variants;

type
  IIntKeyman = interface
    ['{F6CC9DBA-7267-48FF-8E7A-DE0CA0274078}']
  end;

  IIntKeymanInterface = interface
    ['{D1EBBED5-B9E3-4807-969D-DCF9E1FFB287}']
    function XMLClassName: WideString;
    function Serialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString; 
    function DoSerialize(Flags: TOleEnum; const ImagePath: WideString; References: TStrings): WideString; // Wraps serialize with tag
  end;

  IIntKeymanCollection = interface(IIntKeymanInterface)
    ['{6C8DC6FD-C3F0-4A10-A33D-D429FF86F9D2}']
    function Count: Integer;
    function GetIntItem(Index:Integer): IIntKeymanInterface;
    property Items[Index: Integer]: IIntKeymanInterface read GetIntItem;
  end;

  IIntKeymanErrors = interface(IIntKeymanCollection)
    ['{64829021-64EB-4205-BF83-2ADCA4CD27C3}']
    procedure DoClear;
    procedure Add(ErrorCode: Cardinal; Severity: KeymanErrorSeverity); safecall;
    procedure AddFmt(ErrorCode: Cardinal; Parameters: OleVariant; Severity: KeymanErrorSeverity); safecall;
  end;

  IIntKeymanError = IIntKeymanInterface;

  IIntKeymanLanguages = IIntKeymanCollection;

  IIntKeymanKeyboardInstalled = interface(IIntKeymanInterface)
    ['{4876E6DF-C557-46E2-84F4-787BE5F55DDA}']
    function RegKeyboard: TRegKeyboard;
    procedure ClearVisualKeyboard;
    procedure UpdateBaseLayout;   // I4169
    procedure RefreshInstallation;
  end;

  IIntKeymanKeyboardLanguage = interface   // I4376
    ['{81C18942-3C81-4F50-A64A-35A4FE863177}']
    procedure Disable;
  end;

  IIntKeymanKeyboardsInstalled = interface(IIntKeymanCollection)   // I4381
    ['{B03FF25E-E3D8-4D0C-AC7D-68717FFD4473}']
  end;

  IIntKeymanPackageInstalled = interface(IIntKeymanInterface)
    ['{1B0AEAAF-5638-4FF3-BC02-308D42574CAF}']
    procedure RefreshKeyboards;
    function GetKeyboardLanguageList(const KeyboardID: string): TObject;
  end;

  IIntKeymanPackagesInstalled = IIntKeymanCollection;

  IIntKeymanSystemInfo = interface(IIntKeymanInterface)
    ['{90E3F800-E232-4C12-B7B0-7EFE43B71585}']
    procedure SetReboot; safecall;
  end;

  IIntKeymanOptions = IIntKeymanCollection;

  IIntKeymanControl = interface(IIntKeymanInterface)
    ['{1C679B76-D7FE-48C2-AF7A-484EFD0A5382}']
    function CurrentUILanguage: string;
    function IsKeymanRunning: WordBool; safecall;
    procedure AutoApplyKeyman;
    procedure ApplyKeyman;
    function GetAutoApply: Boolean;
    procedure SetAutoApply(Value: Boolean);
    property AutoApply: Boolean read GetAutoApply write SetAutoApply;
    procedure Refresh;
    function KeymanCustomisation: IKeymanCustomisation;
  end;

  IIntKeymanKeyboardsPackageInstalled = interface(IIntKeymanInterface)
    ['{D2A1781B-31E1-4639-AF2B-2FCA75DC4C86}']
    procedure RefreshSource;
  end;

  IIntKeymanHotkeys = interface(IIntKeymanInterface)
    ['{48971D57-F941-467D-ABB6-FD6282A41FA2}']
  end;

implementation

end.
