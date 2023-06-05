(*
  Name:             compile
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      20 Jun 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          20 Jun 2006 - mcdurdin - Initial version
                    06 Oct 2006 - mcdurdin - Fix record packing
                    27 Mar 2008 - mcdurdin - refactor constants into kmxfileconsts
                    28 Jul 2008 - mcdurdin - Use Compile Targets
                    10 Dec 2010 - mcdurdin - I2556 - Crash in Keyman Developer compiling KeymanWeb keyboard
                    17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys
                    11 May 2015 - mcdurdin - I4706 - V9.0 - Update compile logging for silent and warning-as-error cleanness
                    22 Jun 2015 - mcdurdin - I4770 - If kmcmpdll.dll does not exist in debug path, try default paths
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile

*)
unit compile;

interface

uses
  Winapi.Windows,

  kmxfileconsts;

const
{$IFDEF WIN64}
  kmcmpdll_lib = 'kmcmpdll.x64.dll';
{$ELSE}
  kmcmpdll_lib = 'kmcmpdll.dll';
{$ENDIF}

{$IFDEF WIN64}
{$A16}
{$ENDIF}

type
  // These structures must match the structures in compfile.h
  FILE_STORE = record
  	dwSystemID: DWORD;
  	szName: array[0..SZMAX_STORENAME-1] of WCHAR;	// the name of the store
	  dpString: PWideChar;	    				// from start of store structure
    fIsStore: BOOL;     // I2556
    fIsReserved: BOOL;  // I2556
    fIsOption: BOOL;    // I2556
    fIsDebug: BOOL;     // I2556
    fIsCall: BOOL;      // I2556
    line: Integer;
  end;

  PFILE_STORE = ^FILE_STORE;

  FILE_KEY = record
    Key: WCHAR;            // WCHAR -- actually a WORD
    LineStoreIndex: WORD;
    Line: DWORD;
    ShiftFlags: DWORD;
    dpOutput: PWideChar;		// from start of key structure
    dpContext:PWideChar;		// from start of key structure
	end;

  PFILE_KEY = ^FILE_KEY;

  FILE_GROUP  = record
    szName: array[0..SZMAX_GROUPNAME-1] of WCHAR;
    dpKeyArray: PFILE_KEY;         // address of first item in key array, from start of group structure
    dpMatch: PWideChar;                // from start of group structure
    dpNoMatch: PWideChar;              // from start of group structure
    cxKeyArray: DWORD;             // in array items
    fUsingKeys: BOOL;              // group(xx) [using keys] <-- specified or not
    fReadOnly: BOOL;
    Line: DWORD;
	end;

  PFILE_GROUP = ^FILE_GROUP;

  FILE_DEADKEY = record
	  szName: array[0..SZMAX_DEADKEYNAME-1] of WCHAR;
  end;

  PFILE_DEADKEY  = ^FILE_DEADKEY;

  FILE_VKDICTIONARY = record  // I3438
	  szName: array[0..SZMAX_VKDICTIONARYNAME-1] of WCHAR;
  end;

  PFILE_VKDICTIONARY = ^FILE_VKDICTIONARY;

  FILE_KEYBOARD = record
    KeyboardID: DWORD;			// as stored in HKEY_LOCAL_MACHINE//system//currentcontrolset//control//keyboard layouts

    version: DWORD;				// keyboard file version with VERSION keyword

    dpStoreArray: PFILE_STORE;	// address of first item in store array, from start of store structure
    dpGroupArray: PFILE_GROUP;	// address of first item in group array, from start of group structure

    cxStoreArray: DWORD;			// in number of items
    cxGroupArray: DWORD;			// in number of items
    StartGroup: array[0..1] of DWORD;		// index of starting groups [ANSI=0, Unicode=1]

    dwHotKey: DWORD;				// standard windows hotkey (hiword=shift/ctrl/alt stuff, loword=vkey)

    szName: array[0..SZMAX_KEYBOARDNAME-1] of WCHAR;			// Keyboard layout name
    szLanguageName: array[0..SZMAX_LANGUAGENAME-1] of WCHAR;	// Language name
    szCopyright: array[0..SZMAX_COPYRIGHT-1] of WCHAR;			// Copyright information
    szMessage: array[0..SZMAX_MESSAGE-1] of WCHAR;				// General information about the keyboard
    lpBitmap: PBYTE;
    dwBitmapSize: DWORD;
    dwFlags: DWORD;					// Flags for the keyboard file

    currentGroup: DWORD;				// temp - current processing group
    currentStore: DWORD;				// temp - current processing store
    cxDeadKeyArray: DWORD;
    dpDeadKeyArray: PFILE_DEADKEY;	// temp - dead key array
    cxVKDictionary: DWORD;  // I3438
    dpVKDictionary: PFILE_VKDICTIONARY; // I3438  // temp - virtual key dictionary

    extra: Pointer;
	end;

  PFILE_KEYBOARD = ^FILE_KEYBOARD;

{$IFDEF WIN64}
{$A8}
{$ENDIF}

type
  TCompilerCallback = function( line: Integer; msgcode: LongWord; text: PAnsiChar): Integer; stdcall;  // I3310
  TCompilerCallbackW = function( line: Integer; msgcode: LongWord; const text: string): Integer; // not available to C++ in this form

const
  CKF_KEYMAN = 0;
  CKF_KEYMANWEB = 1;

const
  CERR_FATAL   = $00008000;
  CERR_ERROR   = $00004000;
  CERR_WARNING = $00002000;
  CERR_HINT    = $00001000;
  CWARN_Info =   $0000208A;

const
  // kcframe --sizeof returns these values
{$IFDEF WIN64}
  FILE_KEYBOARD_SIZE = 2992;
  FILE_GROUP_SIZE = 200;
  FILE_STORE_SIZE = 200;
  FILE_KEY_SIZE = 32;
  FILE_DEADKEY_SIZE = 160;
{$ELSE}
  FILE_KEYBOARD_SIZE = 2956;
  FILE_GROUP_SIZE = 188;
  FILE_STORE_SIZE = 192;
  FILE_KEY_SIZE = 20;
  FILE_DEADKEY_SIZE = 160;
{$ENDIF}

function CompileKeyboardFile(kmnFile, kmxFile: PChar; FSaveDebug, CompilerWarningsAsErrors, WarnDeprecatedCode: BOOL; CallBack: TCompilerCallback): Integer; cdecl;   // I4865   // I4866
function CompileKeyboardFileToBuffer(kmnFile: PChar; buf: PFILE_KEYBOARD; CompilerWarningsAsErrors, WarnDeprecatedCode: BOOL; CallBack: TCompilerCallback; Target: Integer): Integer; cdecl;   // I4865   // I4866
function Compiler_Diagnostic(mode: Integer): Integer;
procedure Compiler_Diagnostic_Console(mode: Integer);

type
  TCompilerOptions = record
    dwSize: DWORD;
    ShouldAddCompilerVersion: BOOL;
  end;

  COMPILER_OPTIONS = TCompilerOptions;
  PCOMPILER_OPTIONS = ^COMPILER_OPTIONS;

function SetCompilerOptions(options: PCOMPILER_OPTIONS; CallBack: TCompilerCallback): BOOL; cdecl;

//
// For unit tests, point to a known-current version of kmcmpdll.dll
//
var
  FUnitTestKMCmpDllPath: string = '';

implementation

uses
  System.SysUtils,

  RegistryKeys,
  RedistFiles;

var
  HKMCmpDll: THandle = 0;

type
  TCompileKeyboardFile = function (kmnFile, kmxFile: PAnsiChar; FSaveDebug, CompilerWarningsAsErrors, WarnDeprecatedCode: BOOL;  // I3310   // I4865   // I4866
    CallBack: TCompilerCallback): Integer; cdecl;        // TODO: K9: Convert to Unicode

  TCompileKeyboardFileToBuffer = function (kmnFile: PAnsiChar; buf: PFILE_KEYBOARD;  // I3310
    CompilerWarningsAsErrors, WarnDeprecatedCode: BOOL;   // I4865   // I4866
    CallBack: TCompilerCallback; Target: Integer): Integer; cdecl;         // TODO: K9: Convert to Unicode

  TSetCompilerOptions = function (options: PCOMPILER_OPTIONS): BOOL; cdecl;

function LoadCompiler(CallBack: TCompilerCallback = nil): Boolean;
var
  s: string;
begin
  if HKMCmpDll = 0 then
  begin
    s := FUnitTestKMCmpDllPath;
    if s = '' then
    begin
      s := GetDebugKMCmpDllPath;
      if (s <> '') and not FileExists(s + kmcmpdll_lib) then   // I4770
        s := '';
      if s = '' then
      begin
        try
          s := GetDeveloperRootPath;
        except
          s := '';
        end;
        if s = '' then s := ExtractFilePath(ParamStr(0));
      end;
    end;

    HKMCmpDll := LoadLibrary(PChar(s+kmcmpdll_lib));
    if HKMCmpDll = 0 then
    begin
      if Assigned(Callback) then
        Callback(0, $8000, PAnsiChar(AnsiString('Could not load the compiler library '+s+kmcmpdll_lib+'.  '+   // I4706
          'Check that '+kmcmpdll_lib+' is in the program directory '+
          'and that it is not corrupt.'#13#10+'Windows error message: '+SysErrorMessage(GetLastError))));
      Exit(False);
    end;
  end;
  Result := True;
end;

function SetCompilerOptions(options: PCOMPILER_OPTIONS; CallBack: TCompilerCallback): BOOL;
var
  sco: TSetCompilerOptions;
begin
  if not LoadCompiler(Callback) then
    Exit(False);

  @sco := GetProcAddress(HKMCmpDll, 'SetCompilerOptions');
  if not Assigned(@sco) then
  begin
    Callback(0, $8000, PAnsiChar(AnsiString('Could not access the compiler.  Check that '+kmcmpdll_lib+' is in the program directory '+   // I4706
      'and that it is not corrupt.'#13#10+'Windows error message: '+SysErrorMessage(GetLastError))));
    Exit(False);
  end;

  Result := sco(options);
end;

function CompileKeyboardFile(kmnFile, kmxFile: PChar; FSaveDebug, CompilerWarningsAsErrors, WarnDeprecatedCode: BOOL; CallBack: TCompilerCallback): Integer;   // I4865   // I4866
var
  ckf: TCompileKeyboardFile;
begin
  //Result := 0;

  if not LoadCompiler(Callback) then Exit(-1);

  @ckf := GetProcAddress(HKMCmpDll, 'CompileKeyboardFile');
  if not Assigned(@ckf) then
  begin
    Callback(0, $8000, PAnsiChar(AnsiString('Could not access the compiler.  Check that '+kmcmpdll_lib+' is in the program directory '+   // I4706
      'and that it is not corrupt.'#13#10+'Windows error message: '+SysErrorMessage(GetLastError))));
    Result := -1;
    Exit;
  end;

  Result := ckf(PAnsiChar(AnsiString(kmnFile)), PAnsiChar(AnsiString(kmxFile)), FSaveDebug, CompilerWarningsAsErrors, WarnDeprecatedCode, Callback);  // I3310   // I4865   // I4866
end;

function CompileKeyboardFileToBuffer(kmnFile: PChar; buf: PFILE_KEYBOARD; CompilerWarningsAsErrors, WarnDeprecatedCode: BOOL; CallBack: TCompilerCallback; Target: Integer): Integer;   // I4865   // I4866
var
  ckf: TCompileKeyboardFileToBuffer;
begin
  //Result := 0;

  if not LoadCompiler(Callback) then Exit(-1);

  @ckf := GetProcAddress(HKMCmpDll, 'CompileKeyboardFileToBuffer');
  if not Assigned(@ckf) then
  begin
    Callback(0, $8000, PAnsiChar(AnsiString('Could not access the compiler.  Check that '+kmcmpdll_lib+' is in the program directory '+   // I4706
      'and that it is not corrupt.'#13#10+'Windows error message: '+SysErrorMessage(GetLastError))));
    Result := -1;
    Exit;
  end;

  Result := ckf(PAnsiChar(AnsiString(kmnFile)), buf, CompilerWarningsAsErrors, WarnDeprecatedCode, Callback, Target);  // I3310   // I4865   // I4866
end;

type
  TKeyman_Diagnostic = procedure(mode: Integer); cdecl;

function Compiler_Diagnostic(mode: Integer): Integer;
var
  keyman_diagnostic: TKeyman_Diagnostic;
begin
  if not LoadCompiler then Exit(-1);

  @keyman_diagnostic := GetProcAddress(HKMCmpDll, 'Keyman_Diagnostic');
  if Assigned(@keyman_diagnostic) then
  begin
    keyman_diagnostic(mode);
    Exit(0);
  end;

  Result := 2;
end;

procedure Compiler_Diagnostic_Console(mode: Integer);
begin
  case compile.Compiler_Diagnostic(0) of
    0: writeln('Should not have got here');
    1: writeln('Test failed: could not find kmcmpdll.dll');
    2: writeln('Test failed: could not find keyman_diagnostic in kmcmpdll.dll');
    else writeln('This should not be possible');
  end;
end;

initialization
  // We want to early load the compiler because we need it loaded for
  // sentry symbolication: https://github.com/getsentry/sentry-native/issues/213
  LoadCompiler;
  try
    Assert(sizeof(FILE_KEYBOARD) = FILE_KEYBOARD_SIZE, 'Assertion failure: sizeof(FILE_KEYBOARD) = FILE_KEYBOARD_SIZE');
    Assert(sizeof(FILE_GROUP) = FILE_GROUP_SIZE, 'Assertion failure: sizeof(FILE_GROUP) = FILE_GROUP_SIZE');
    Assert(sizeof(FILE_STORE) = FILE_STORE_SIZE, 'Assertion failure: sizeof(FILE_STORE) = FILE_STORE_SIZE');
    Assert(sizeof(FILE_KEY) = FILE_KEY_SIZE, 'Assertion failure: sizeof(FILE_KEY) = FILE_KEY_SIZE');
    Assert(sizeof(FILE_DEADKEY) = FILE_DEADKEY_SIZE, 'Assertion failure: sizeof(FILE_DEADKEY) = FILE_DEADKEY_SIZE');
  except
    on E:Exception do
    begin
      // We emit to the console manually because this exception is otherwise
      // completely silent, which is a pain for testing.
      writeln(E.Message);
      raise;
    end;
  end;
finalization
  if HKMCmpDll > 0 then
    FreeLibrary(HKMCmpDll);
  HKMCmpDll := 0;
end.
