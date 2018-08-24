(*
  Name:             CompileKeymanWeb
  Copyright:        Copyright (C) SIL International.
  Documentation:    
  Description:      
  Create Date:      26 Apr 2006

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:    
  Dependencies:     

  Bugs:             
  Todo:             
  Notes:            
  History:          26 Apr 2006 - mcdurdin - Add support for ContextEx and Context, and lots of polish
                    21 Jun 2006 - mcdurdin - Add embedded javascript output (for IMX support)
                    21 Jun 2006 - mcdurdin - Add semicolons as necessary to javascript
                    21 Jun 2006 - mcdurdin - Add right-to-left support
                    06 Oct 2006 - mcdurdin - Merge forward v6.2 keymanweb compiler code (v7.0 code was old)
                    04 Dec 2006 - mcdurdin - Add ContextEx, LHS ContextEx
                    19 Mar 2007 - mcdurdin - I714 - Compiler not using correct system stores
                    19 Mar 2007 - mcdurdin - I716 - Update to KMW build 83
                    19 Mar 2007 - mcdurdin - I732 - Added partial support for mnemonic layouts (compiler code only, KMW code under development)
                    30 Apr 2007 - mcdurdin - I783 - Fix index() statement for KMW compiler
                    05 Jun 2007 - mcdurdin - I853 - Avoid crash when compiling ANSI keyboards
                    27 Mar 2008 - mcdurdin - I1370 - Rename keyboards to avoid invalid characters during compile
                    27 Mar 2008 - mcdurdin - Refactor incxstr into kmxfileutils
                    14 Jun 2008 - mcdurdin - I1474 - Strip UTF8 prolog from help text
                    28 Jul 2008 - mcdurdin - Compile target support
                    28 Aug 2008 - mcdurdin - I1585 - spacebar not being accepted in positional KMW keyboards
                    28 Aug 2008 - mcdurdin - I783 - Problems with index/any in KMW keyboards
                    16 Jan 2009 - mcdurdin - I1592 - Fix crash compiling KMW keyboard when file is missing
                    25 May 2009 - mcdurdin - I1971 - Don't succeed when error compiling
                    25 May 2009 - mcdurdin - I1520 - Don't allow specific unsupported functions in stores for 'any'
                    25 May 2009 - mcdurdin - I1959 - Fix bugs with multiple groups
                    25 May 2009 - mcdurdin - I1964 - Fix crash when compiling keyboard with empty group
                    22 Mar 2010 - mcdurdin - I2224 - Fix nomatch not working in KMW Compiler
                    22 Mar 2010 - mcdurdin - I2242 - Fixup CR, LF not compiling in correctly in KMW
                    22 Mar 2010 - mcdurdin - I2243 - Add support for nul in context of rules
                    19 Apr 2010 - mcdurdin - I2308 - KeymanWeb call functions could be out of sync due to sorted list in compiler
                    26 Jul 2010 - mcdurdin - I2468 - Eliminate KeymanWeb Pack
                    18 May 2012 - mcdurdin - I3306 - V9.0 - Remove TntControls + Win9x support
                    18 May 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    08 Jun 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    08 Jun 2012 - mcdurdin - I3337 - V9.0 - Review of input/output for Unicode
                    17 Aug 2012 - mcdurdin - I3429 - V9.0 - Add support for if, set, reset, save to KeymanWeb compiler
                    17 Aug 2012 - mcdurdin - I3430 - V9.0 - Add support for if(&platform) and if(&baselayout) to compilers
                    17 Aug 2012 - mcdurdin - I3310 - V9.0 - Unicode in Delphi fixes
                    27 Aug 2012 - mcdurdin - I3437 - V9.0 - Add support for set(&layer) and layer()
                    27 Aug 2012 - mcdurdin - I3438 - V9.0 - Add support for custom virtual keys
                    19 Oct 2012 - mcdurdin - I3474 - V9.0 - KeymanWeb Compiler should no longer emit UTF-8 prologue
                    24 Oct 2012 - mcdurdin - I3482 - V9.0 - If compiler dll is missing, TIKE crashes after showing error message
                    24 Oct 2012 - mcdurdin - I3483 - V9.0 - Add support for compiling in the layout file
                    13 Dec 2012 - mcdurdin - I3659 - V9.0 - KSAVE has logical design issue with how stores are reloaded from cookie
                    13 Dec 2012 - mcdurdin - I3681 - V9.0 - KeymanWeb compiler should output formatted js when debug=1
                    13 Dec 2012 - mcdurdin - I3317 - Add KS to KeymanWeb keyboard compiler for keyboards with SMP chars
                    13 Dec 2012 - mcdurdin - I3683 - V9.0 - If embedjs file is missing the kmw compiler will crash
                    13 Dec 2012 - mcdurdin - I3684 - V9.0 - KeymanWeb compiler needs to emit only non-system stores and a couple of system stores
                    01 Jan 2013 - mcdurdin - I3690 - V9.0 - KSAVE wrongly includes initial _ in cookie name
                    11 Aug 2013 - mcdurdin - I3886 - V9.0 - KMW 2.0 compiler needs to publish DisplayUnderling for KVK as Keyboard.KDU
                    11 Aug 2013 - mcdurdin - I3643 - V9.0 - KMW compiler does not validate format of &layoutfile
                    15 Oct 2013 - mcdurdin - I3910 - KeymanWeb compiler calculates indexes incorrectly when deadkeys are on LHS
                    07 Nov 2013 - mcdurdin - I3946 - V9.0 - KDU flag needs to be added to keyboard object, not VK object in kmw compiler
                    07 Nov 2013 - mcdurdin - I3947 - V9.0 - If KVK file is missing then KMW compiler will crash
                    08 Nov 2013 - mcdurdin - I3956 - V9.0 - I3946 KDU insertion fails due to syntax error in compiled file
                    29 Nov 2013 - mcdurdin - I3980 - V9.0 - KeymanWeb compiler does not support context() on LHS
                    29 Nov 2013 - mcdurdin - I3981 - V9.0 - Keyman Engine for Web Compiler does not support notany()
                    21 Feb 2014 - mcdurdin - I4060 - V9.0 - KeymanWeb compiler should validate the layout file
                    21 Feb 2014 - mcdurdin - I4061 - V9.0 - KeymanWeb compiler needs defined codes for some errors
                    06 Mar 2014 - mcdurdin - I4118 - V9.0 - KMW compiler should warn when extended shift flags are used
                    06 Mar 2014 - mcdurdin - I4119 - V9.0 - KMW compiler should only warn unassociated keys that are not special keys
                    19 Mar 2014 - mcdurdin - I4139 - V9.0 - Compress layout file when compiling to KMW
                    19 Mar 2014 - mcdurdin - I4140 - V9.0 - Add keyboard version information to keyboards
                    19 Mar 2014 - mcdurdin - I4141 - V9.0 - Warn when unusable key ids are used
                    19 Mar 2014 - mcdurdin - I4142 - V9.0 - Validate key ids are in an acceptable format
                    21 Mar 2014 - mcdurdin - I4155 - V9.0 - Keyboard version is not compiled into kmw js
                    21 Mar 2014 - mcdurdin - I4154 - V9.0 - keyboardversion is not read by KeymanWeb compiler and applied to filename.
                    01 May 2014 - mcdurdin - I4198 - V9.0 - Block U_0000-U_001F and U_0080-U_009F in layout files
                    10 Jun 2014 - mcdurdin - I4259 - V9.0 - Generate a .json file when compiling keyboard for web
                    12 Jun 2014 - mcdurdin - I4263 - V9.0 - Compile to web and test fails when keyboard version is not 1.0
                    12 Aug 2014 - mcdurdin - I4368 - V9.0 - Add custom stylesheet to kmw compile
                    12 Aug 2014 - mcdurdin - I4373 - V9.0 - Add line number comments to js when compiling with debug
                    28 Aug 2014 - mcdurdin - I4384 - V9.0 - KMW compiler adds line number comment in non-debug mode in some cases
                    13 Oct 2014 - mcdurdin - I4447 - V9.0 - The dismiss keyboard and tab buttons should not be required by Keyman Developer now
                    04 Nov 2014 - mcdurdin - I4505 - V9.0 - Add JSON metadata editor to keyboard wizard
                    07 Mar 2015 - mcdurdin - I4611 - V9.0 - context statement in output could fail in some situations in KeymanWeb compiler
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    27 May 2015 - mcdurdin - I4724 - Compiler generates warnings for JSON files if output path is source path
                    24 Aug 2015 - mcdurdin - I4872 - OSK font and Touch Layout font should be the same in Developer
                    24 Aug 2015 - mcdurdin - I4866 - Add warn on deprecated features to project and compile
                    
                    24 Aug 2015 - mcdurdin - I4865 - Add treat hints and warnings as errors into project
                    28 Feb 2018 - jahorton - GH 281 - Changed the compilation targets for KMW 10 to better support deadkeys.
                    
*)
unit CompileKeymanWeb;  // I3306  // I3310

interface

uses
  Winapi.Windows,
  System.Character,
  System.Classes,
  System.UITypes,

  compile,
  kmxfile,
  kmxfileconsts,
  ProjectFile;

type
  TSentinelRecordAny = record
    StoreIndex: Integer;
    Store: PFILE_STORE;
  end;

  TSentinelRecordIndex = record
    StoreIndex: Integer;
    Store: PFILE_STORE;
    Index: Integer;
  end;

  TSentinelRecordContextEx = record
    Index: Integer;
  end;

  TSentinelRecordDeadkey = record
    DeadKey: Integer;
  end;

  TSentinelRecordUse = record
    GroupIndex: Integer;
    Group: PFILE_GROUP;
  end;

  TSentinelRecordCall = record
    StoreIndex: Integer;
    Store: PFILE_STORE;
  end;

  TSentinelRecordIfOpt = record  // I3429
    StoreIndex1: Integer;
    Store1: PFILE_STORE;
    StoreIndex2: Integer;
    Store2: PFILE_STORE;
    IsNot: Integer;
  end;

  TSentinelRecordSetOpt = record  // I3429
    StoreIndex1: Integer;
    Store1: PFILE_STORE;
    StoreIndex2: Integer;
    Store2: PFILE_STORE;
  end;

  TSentinelRecordResetOpt = record  // I3429
    StoreIndex: Integer;
    Store: PFILE_STORE;
  end;

  TSentinelRecordSaveOpt = record  // I3429
    StoreIndex: Integer;
    Store: PFILE_STORE;
  end;

  TSentinelRecordIfSystemStore = record  // I3430
    dwSystemID: DWORD;
    SystemStore: PFILE_STORE;
    StoreIndex: Integer;
    Store: PFILE_STORE;
    IsNot: Integer;
  end;

  TSentinelRecordSetSystemStore = record  // I3437
    dwSystemID: DWORD;
    SystemStore: PFILE_STORE;
    StoreIndex: Integer;
    Store: PFILE_STORE;
  end;

  TSentinelRecord = record
    IsSentinel: Boolean;
    Code: Integer;
    Any: TSentinelRecordAny;
    Index: TSentinelRecordIndex;
    Deadkey: TSentinelRecordDeadkey;
    Use: TSentinelRecordUse;
    Call: TSentinelRecordCall;
    ContextEx: TSentinelRecordContextEx;
    IfOpt: TSentinelRecordIfOpt;  // I3429
    IfSystemStore: TSentinelRecordIfSystemStore;  // I3430
    SetOpt: TSentinelRecordSetOpt;  // I3429
    SetSystemStore: TSentinelRecordSetSystemStore;  // I3437
    ResetOpt: TSentinelRecordResetOpt;  // I3429
    SaveOpt: TSentinelRecordSaveOpt;  // I3429
    //Use: Integer;
    ChrVal: DWord;
  end;

  TCompileKeymanWeb = class
  private
    FError: Boolean;  // I1971
    FCallback: TCompilerCallback;
    FCallFunctions: TStringList;
    FOutFile, FInFile: string;
    FKeyboardVersion: string;
    fk: FILE_KEYBOARD;
    nl: string;   // I3681
    FDebug: Boolean;   // I3681
    FTabStop: string;   // I3681
    fMnemonic: Boolean;
    FCompilerWarningsAsErrors: Boolean;
    FTouchLayoutFont: string;   // I4872

    function JavaScript_String(ch: DWord): string;  // I2242

    function IsKeyboardVersion10OrLater: Boolean;

    procedure ReportError(line: Integer; msgcode: LongWord; const text: string);  // I1971
    function ExpandSentinel(pwsz: PWideChar): TSentinelRecord;
    function CallFunctionName(s: WideString): WideString;
    function JavaScript_Name(i: Integer; pwszName: PWideChar; UseNameForRelease: Boolean = False): string;   // I3659
    function JavaScript_Store(line: Integer; pwsz: PWideChar): string;
    function JavaScript_Shift(fkp: PFILE_KEY; FMnemonic: Boolean): Integer;
    function JavaScript_ShiftAsString(fkp: PFILE_KEY; FMnemonic: Boolean): string;   // I4872
    function JavaScript_Key(fkp: PFILE_KEY; FMnemonic: Boolean): Integer;
    function JavaScript_KeyAsString(fkp: PFILE_KEY; FMnemonic: Boolean): string;
    function JavaScript_ContextLength(Context: PWideChar): Integer;
    function JavaScript_OutputString(fkp: PFILE_KEY; pwszOutput: PWideChar; fgp: PFILE_GROUP): string;
    function JavaScript_ContextMatch(fkp: PFILE_KEY; context: PWideChar): string;
    function JavaScript_CompositeContextValue(fkp: PFILE_KEY; pwsz: PWideChar): string;
    function JavaScript_FullContextValue(fkp: PFILE_KEY; pwsz: PWideChar): string;
    function RuleIsExcludedByPlatform(fkp: PFILE_KEY): Boolean;
    function RequotedString(s: WideString): string;
    function VisualKeyboardFromFile(
      const FVisualKeyboardFileName: string): WideString;
    function WriteCompiledKeyboard: string;
    procedure CheckStoreForInvalidFunctions(key: PFILE_KEY; store: PFILE_STORE);  // I1971
    function GetCodeName(code: Integer): string;  // I3438
    function HasSupplementaryPlaneChars: Boolean;   // I3317
    function ValidateLayoutFile(var sLayoutFile: string; const sVKDictionary: string): Boolean;   // I4139
    function GetKeyboardModifierBitmask: string;
    function FormatModifierAsBitflags(FBitMask: Cardinal): string;
    function FormatKeyAsString(key: Integer): string;
    function JavaScript_SetupDebug: string;
    function JavaScript_SetupEpilog: string;
    function JavaScript_SetupProlog: string;
  public
    function Compile(AOwnerProject: TProject; const InFile: string; const OutFile: string; Debug: Boolean; Callback: TCompilerCallback): Boolean;   // I3681   // I4140   // I4688   // I4866
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Vcl.Graphics,
  System.JSON,
  System.Math,
  System.StrUtils,
  System.SysUtils,
  System.TypInfo,

  CompileErrorCodes,
  JsonUtil,
  KeyboardParser,
  Keyman.System.KeyboardUtils,
  KeymanWebKeyCodes,
  kmxfileutils,
  TouchLayout,
  TouchLayoutUtils,
  Unicode,
  UnicodeData,
  utilstr,
  VisualKeyboard,
  VKeys;

const
  SValidIdentifierCharSet = ['A'..'Z','a'..'z','0'..'9','_'];

function IsValidKeyboardVersionString(value: string): Boolean;   // I4140
var
  p: PChar;
begin
  p := PChar(value);
  while p^ <> #0 do
  begin
    if not CharInSet(p^, ['0'..'9']) then
      Exit(False);
    while CharInSet(p^, ['0'..'9']) do Inc(p);

    if p^ = '.' then
    begin
      Inc(p);
      if not CharInSet(p^, ['0'..'9']) then   // I4263
        Exit(False);
    end;
  end;

  Result := True;
end;

function TCompileKeymanWeb.Compile(AOwnerProject: TProject; const InFile: string; const OutFile: string; Debug: Boolean; Callback: TCompilerCallback): Boolean;   // I3681   // I4140   // I4688   // I4866   // I4865
var
  WarnDeprecatedCode: Boolean;
  Data: string;
begin
  FCallback := Callback;
  FInFile := InFile;
  FOutFile := OutFile;   // I4140   // I4155   // I4154
  FDebug := Debug;   // I3681
  FError := False;  // I1971

  if FileExists(OutFile) then
    DeleteFile(OutFile);

  if FDebug then   // I3681
  begin
    nl := #13#10;
    FTabStop := '  ';
  end
  else
  begin
    nl := '';
    FTabStop := '';
  end;

  if Assigned(AOwnerProject) then   // I4865   // I4866
  begin
    FCompilerWarningsAsErrors := AOwnerProject.Options.CompilerWarningsAsErrors;
    WarnDeprecatedCode := AOwnerProject.Options.WarnDeprecatedCode;
  end
  else
  begin
    FCompilerWarningsAsErrors := False;
    WarnDeprecatedCode := True;
  end;

  FCallFunctions := TStringList.Create;
  try
    if CompileKeyboardFileToBuffer(PChar(InFile), @fk,
      FCompilerWarningsAsErrors, WarnDeprecatedCode,
      Callback, CKF_KEYMANWEB) > 0 then  // I3482   // I4866   // I4865
      // TODO: Free fk
    begin
      if Assigned(AOwnerProject) and
          Assigned(AOwnerProject.CompilerMessageFile) and
          AOwnerProject.CompilerMessageFile.HasCompileWarning and
          FCompilerWarningsAsErrors then
        FError := True;

      if not FError then
      begin
        Data := WriteCompiledKeyboard;

        if not FError then
          with TStringStream.Create(Data, TEncoding.UTF8) do
          try
            SaveToFile(OutFile);
          finally
            Free;
          end;
      end;

      Result := not FError;  // I1971
    end
    else
    begin
      Result := False;
    end;
  finally
    FreeAndNil(FCallFunctions);
  end;
end;

constructor TCompileKeymanWeb.Create;
begin
  FillChar(fk, sizeof(fk), 0);
end;

destructor TCompileKeymanWeb.Destroy;
begin
  // TODO: Free FK values
  inherited;
end;

function TCompileKeymanWeb.JavaScript_ContextLength(Context: PWideChar): Integer;
begin
  Result := xstrlen_printing(Context);
end;

// Used when targeting versions prior to 10.0, before the introduction of FullContextMatch/KFCM.
function TCompileKeymanWeb.JavaScript_CompositeContextValue(fkp: PFILE_KEY; pwsz: PWideChar): string;
  // Reference: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
  function IsRegExpSpecialChar(ch: WideChar): Boolean;
  begin
    Result := Pos('"\^$*+?{}.()|[]/', ch) > 0;
  end;

var
  StartQuotes, Len, Cur: Integer;
  InQuotes: Boolean;
  rec: TSentinelRecord;
begin
  Result := '';

  InQuotes := False;
  Len := JavaScript_ContextLength(pwsz);
  StartQuotes := -1;
  Cur := 0;

	while pwsz^ <> #0 do
  begin
    rec := ExpandSentinel(pwsz);
    if rec.IsSentinel then
    begin
      if InQuotes then
      begin
        Result := Result + Format('",%d)', [Cur - StartQuotes]);
        InQuotes := False;
      end;
      if Result <> '' then Result := Result + '&&';

      case rec.Code of
        CODE_ANY:
          begin
            CheckStoreForInvalidFunctions(fkp, rec.Any.Store);  // I1520
            Result := Result + Format('k.KA(%d,k.KC(%d,1,t),this.s%s)', [Cur, Len - Cur, JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.szName)]);
          end;
        CODE_DEADKEY:
          begin
            Result := Result + Format('k.KDM(%d,t,%d)', [Len-Cur, rec.Deadkey.Deadkey]);
            Dec(Cur); // don't increment on deadkeys -- correlates with AdjustIndex function   // I3910
          end;
        CODE_NUL:    // I2243
          begin
            Result := Result + Format('k.KN(%d,t)', [Len-Cur]);
            Dec(Cur); // don't increment on nul -- correlates with AdjustIndex function   // I3910
          end;
        CODE_IFOPT:    // I3429
          begin
            Result := Result + Format('this.s%s%sthis.s%s',
              [JavaScript_Name(rec.IfOpt.StoreIndex1, rec.IfOpt.Store1.szName),
              IfThen(rec.IfOpt.IsNot = 0, '!==', '==='),
              JavaScript_Name(rec.IfOpt.StoreIndex2,rec.IfOpt.Store2.szName)]);  // I3429   // I3659   // I3681
            Dec(Cur); // don't increment on ifopt -- correlates with AdjustIndex function   // I3910
          end;
        CODE_IFSYSTEMSTORE:     // I3430
          begin
            Result := Result + Format('%sk.KIFS(%d,this.s%s,t)',
              [IfThen(rec.IfSystemStore.IsNot = 0, '!', ''),
              rec.IfSystemStore.dwSystemID,
              JavaScript_Name(rec.IfSystemStore.StoreIndex,rec.IfSystemStore.Store.szName)]);  // I3430   // I3659   // I3681
            Dec(Cur); // don't increment on ifsystemstore -- correlates with AdjustIndex function   // I3910
          end;
        CODE_CONTEXTEX:   // I3980
          begin
            Result := Result + Format('k.KCCM(%d,%d,t)', [Len-Cur,Len-rec.ContextEx.Index+1]);
          end;
        CODE_NOTANY:   // I3981
          begin
            CheckStoreForInvalidFunctions(fkp, rec.Any.Store);  // I1520
            Result := Result + Format('k.KC(%d,1,t)!=""&&!k.KA(%d,k.KC(%d,1,t),this.s%s)', [Len - Cur, Cur, Len - Cur, JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.szName)]);
          end;
        else

        begin
          ReportError(fkp.Line, CERR_NotSupportedInKeymanWebContext, Format('Statement %s is not currently supported in context', [GetCodeName(rec.Code)]));  // I1971   // I4061
        //CODE_NUL: ;     // todo: check if context is longer than that...
          Result := Result + '/*.*/ 0 ';
        end;
      end;


(*
			switch(*(Context+1))
			{
			case CODE_NUL: /* todo: check if context is longer than that... */
				break;
			case CODE_INDEX:
				/*todo: check index
				s = &lpActiveKeyboard->Keyboard->dpStoreArray[(*(p+2))-1];
				*indexp = n = IndexStack[(*(p+3))-1];

				for(temp = s->dpString; *temp && n > 0; temp = incxstr(temp), n--);
				if(n != 0) return FALSE;
				if(GetSuppChar(temp) != GetSuppChar(q)) return FALSE;

				*/
				break;
			case CODE_CONTEXT:

				/* if(GetSuppChar(q) != GetSuppChar(qbuf)) return FALSE; */
				break;
			case CODE_CONTEXTEX:

				// only the nth character
				/* for(n = *(p+2) - 1, temp = qbuf; temp < q && n > 0; n--, temp = incxstr(temp));
				if(n == 0)
					if(GetSuppChar(temp) != GetSuppChar(q)) return FALSE; */
				break;
			default:
				return "<ERROR>";
			}
*)
		end
		else
    begin
      if not InQuotes then
      begin
        if Result <> '' then Result := Result + '&&';
        Result := Result + Format('k.KCM(%d,t,"', [Len - Cur]);
        StartQuotes := Cur;
        InQuotes := True;
      end;
      if rec.ChrVal in [Ord('"'), Ord('\')] then Result := Result + '\';
      Result := Result + Javascript_String(rec.ChrVal);  // I2242
		end;

    Inc(Cur);
		pwsz := incxstr(pwsz);
	end;

  if InQuotes then
    Result := Result + Format('",%d)', [Cur - StartQuotes]);
end;

// Used when targeting versions >= 10.0, after the introduction of FullContextMatch/KFCM.
function TCompileKeymanWeb.JavaScript_FullContextValue(fkp: PFILE_KEY; pwsz: PWideChar): string;
  // Reference: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions
  function IsRegExpSpecialChar(ch: WideChar): Boolean;
  begin
    Result := Pos('"\^$*+?{}.()|[]/', ch) > 0;
  end;

var
  Len: Integer;
  rec: TSentinelRecord;
  FullContext, Suffix: string;
begin
  Result := '';
  FullContext := '';
  Suffix := '';
  Len := xstrlen(pwsz);

  while pwsz^ <> #0 do
  begin
    if FullContext <> '' then
      FullContext := FullContext + ',';

    rec := ExpandSentinel(pwsz);
    if rec.IsSentinel then
    begin
      case rec.Code of
        CODE_ANY:
          begin
            CheckStoreForInvalidFunctions(fkp, rec.Any.Store);  // I1520
            FullContext := FullContext + Format('{t:''a'',a:this.s%s}', [JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.szName)]);
          end;
        CODE_DEADKEY:
          begin
            FullContext := FullContext + Format('{t:''d'',d:%d}', [rec.Deadkey.Deadkey]);
          end;
        CODE_NUL:    // I2243
          begin
            FullContext := FullContext + '{t:''n''}';
          end;
        CODE_IFOPT:    // I3429
          begin
            Dec(Len);
            if Suffix <> '' then Suffix := Suffix + '&&';
            if FullContext = ',' then FullContext := '';
            Suffix := Suffix + Format('this.s%s%sthis.s%s',
              [JavaScript_Name(rec.IfOpt.StoreIndex1, rec.IfOpt.Store1.szName),
              IfThen(rec.IfOpt.IsNot = 0, '!==', '==='),
              JavaScript_Name(rec.IfOpt.StoreIndex2,rec.IfOpt.Store2.szName)]);  // I3429   // I3659   // I3681
          end;
        CODE_IFSYSTEMSTORE:     // I3430
          begin
            Dec(Len);
            if Suffix <> '' then Suffix := Suffix + '&&';
            if FullContext = ',' then FullContext := '';
            Suffix := Suffix + Format('%sk.KIFS(%d,this.s%s,t)',
              [IfThen(rec.IfSystemStore.IsNot = 0, '!', ''),
              rec.IfSystemStore.dwSystemID,
              JavaScript_Name(rec.IfSystemStore.StoreIndex,rec.IfSystemStore.Store.szName)]);  // I3430   // I3659   // I3681
          end;
        CODE_NOTANY:   // I3981
          begin
            CheckStoreForInvalidFunctions(fkp, rec.Any.Store);  // I1520
            FullContext := FullContext + Format('{t:''a'',a:this.s%s,n:1}', [JavaScript_Name(rec.Any.StoreIndex, rec.Any.Store.szName)]);
          end;
        CODE_CONTEXTEX:
          begin
            FullContext := FullContext + Format('{t:''c'',c:%d}', [rec.ContextEx.Index]);   // I4611
          end;
        CODE_INDEX:
          begin
            FullContext := FullContext + Format('{t:''i'',i:this.s%s,o:%d}',
              [JavaScript_Name(rec.Index.StoreIndex, rec.Index.Store.szName), rec.Index.Index]);   // I4611
          end;
        else

        begin
          ReportError(fkp.Line, CERR_NotSupportedInKeymanWebContext, Format('Statement %s is not currently supported in context', [GetCodeName(rec.Code)]));  // I1971   // I4061
          //CODE_NUL: ;     // todo: check if context is longer than that...
          Result := Result + '/*.*/ 0 ';
        end;
      end;
    end
		else
    begin // Simple context character.
      FullContext := FullContext + '''';
      if rec.ChrVal in [Ord('"'), Ord('\'), Ord('''')] then
        FullContext := FullContext + '\';
      FullContext := FullContext + Javascript_String(rec.ChrVal) + '''';  // I2242
    end;

		pwsz := incxstr(pwsz);
	end;

  if FullContext <> '' then
    Result := Format('k.KFCM(%d,t,[%s])', [Len, FullContext]);

  if (Result <> '') and (Suffix <> '')
    then Result := Result + '&&' + Suffix
  else if Suffix <> '' then
    Result := Suffix;

end;

function TCompileKeymanWeb.JavaScript_ContextMatch(fkp: PFILE_KEY; context: PWideChar): string;
begin
  if IsKeyboardVersion10OrLater
    then Result := JavaScript_FullContextValue(fkp, context)
    else Result := JavaScript_CompositeContextValue(fkp, context);
end;

const // I1585 - add space to conversion
  USEnglishUnshift: WideString = ' `'     + '1234567890' + '-' +  '='  + 'qwertyuiop' + '['  + ']'  + '\'  + 'asdfghjkl' + ';'  + '''' + 'zxcvbnm' + ','  + '.'  + '/';
  USEnglishShift: WideString   = #$FF'~'  + '!@#$%^&*()' + '_' +  '+'  + 'QWERTYUIOP' + '{'  + '}'  + '|'  + 'ASDFGHJKL' + ':'  + '"'  + 'ZXCVBNM' + '<'  + '>'  + '?';
  USEnglishValues: WideString  = #$20#$c0 + '1234567890' + #$bd + #$bb + 'QWERTYUIOP' + #$db + #$dd + #$dc + 'ASDFGHJKL' + #$ba + #$de + 'ZXCVBNM' + #$bc + #$be + #$bf;
  UnreachableKeyCodes: array[0..19] of Integer =   // I4141
    ($00,				// &H0
    VK_LBUTTON,			// &H1
    VK_RBUTTON,			// &H2
    VK_CANCEL,		   	// &H3
    VK_MBUTTON,			// &H4
    $05,				// &H5
    $06,				// &H6
    $07,				// &H7
    $0A,				// &HA
    $0B,				// &HB
    $0E,				// &HE
    $0F,				// &HF
    VK_SHIFT,				// &H10
    VK_CONTROL,			// &H11
    VK_MENU,				// &H12
    VK_PAUSE,				// &H13
    VK_CAPITAL,				// &H14
    VK_ESCAPE,				// &H1B

    VK_NUMLOCK,			// &H90
    VK_SCROLL);			// &H91


function TCompileKeymanWeb.JavaScript_Key(fkp: PFILE_KEY; FMnemonic: Boolean): Integer;
var
  n: Integer;
  i: Integer;
begin
  if not FMnemonic then
  begin
    if (fkp.ShiftFlags and KMX_ISVIRTUALKEY) = KMX_ISVIRTUALKEY then
    begin
      Result := Ord(fkp.Key);
    end
    else
    begin
      // Convert the character to a virtual key
      n := Pos(fkp.Key, USEnglishShift);
      if n = 0 then n := Pos(fkp.Key, USEnglishUnshift);
      if n = 0
        then Result := 0
        else Result := Ord(USEnglishValues[n]);
    end;
  end
  else
  begin
    Result := Ord(fkp.Key);
  end;

  // Check that key is not unreachable (e.g. K_SHIFT, touch-specific special keys 50,000+)

  for i := 0 to High(UnreachableKeyCodes) do   // I4141
    if Result = UnreachableKeyCodes[i] then
      Result := 0;

  if (Result = 0) or (Result >= Ord(Low(TKeymanWebTouchStandardKey))) then   // I4141
  begin
    ReportError(fkp.Line, CWARN_UnreachableKeyCode, 'The rule will never be matched because its key code is never fired.');
  end;
end;

///
/// Returns a Javascript representation of a key value, either as a constant (debug mode)
/// or as an integer.
///
/// @param fkp         Pointer to key record
/// @param FMnemonic   True if the keyboard is a mnemonic layout
///
/// @return string representation of the key value, e.g. 'keyCodes.K_A /* 0x41 */' or '65'
///
function TCompileKeymanWeb.JavaScript_KeyAsString(fkp: PFILE_KEY; FMnemonic: Boolean): string;
begin
  if FDebug
    then Result := ' '+FormatKeyAsString(JavaScript_Key(fkp, FMnemonic))
    else Result := IntToStr(JavaScript_Key(fkp, FMnemonic));
end;

function TCompileKeymanWeb.JavaScript_Name(i: Integer; pwszName: PWideChar; UseNameForRelease: Boolean): string;   // I3659
begin
  if not Assigned(pwszName) or (pwszName^ = #0) or (not Self.FDebug and not UseNameForRelease) then   // I3659   // I3681
    Result := IntToStr(i) // for uniqueness
  else
  begin
    if UseNameForRelease   // I3659
      then Result := '' // Potential for overlap in theory but in practice we only use this for named option stores so can never overlap
      else Result := '_'; // Ensures we cannot overlap numbered instances
    while pwszName^ <> #0 do
    begin
      if CharInSet(PChar(pwszName)^, SValidIdentifierCharSet)   // I3681
        then Result := Result + PChar(pwszName)^
        else Result := Result + '_';
      Inc(pwszName);
    end;
  end;
end;

function TCompileKeymanWeb.JavaScript_OutputString(fkp: PFILE_KEY; pwszOutput: PWideChar; fgp: PFILE_GROUP): string;
var
  i, n, len: Integer;
  InQuotes: Boolean;
  rec: TSentinelRecord;
  pwszcontext,pwsz: PWideChar;
  Index: Integer;   // I3910
  nlt: string;

  function AdjustIndex(pwszContext: PWideChar; Index: Integer): Integer;   // I3910
  var
    recContext: TSentinelRecord;
    I: Integer;
  begin
    Result := Index;
    for I := 1 to Index-1 do
    begin
      recContext := ExpandSentinel(pwszContext);

      if IsKeyboardVersion10OrLater then
      begin
        if recContext.IsSentinel and (recContext.Code in [CODE_NUL, CODE_IFOPT, CODE_IFSYSTEMSTORE]) then
          Dec(Result);
      end
      else
      begin
        if recContext.IsSentinel and (recContext.Code in [CODE_DEADKEY, CODE_NUL, CODE_IFOPT, CODE_IFSYSTEMSTORE]) then
          Dec(Result);
      end;
      pwszContext := incxstr(pwszContext);
    end;
  end;

  function ContextChar(ContextIndex: Integer; pwszContext: PWideChar): string;   // I4611
  var
    Index: Integer;
    recContext: TSentinelRecord;
  begin
    Result := '';
    recContext := ExpandSentinel(pwszContext);
    if recContext.IsSentinel then
    begin
      if InQuotes then   // I4611
      begin
        Result := Result + '");';
        InQuotes := False;
      end;

      case recContext.Code of
        CODE_ANY:
          begin
            Index := AdjustIndex(fkp.dpContext, ContextIndex);   // I3910   // I4611
            Result := Result + nlt + Format('k.KIO(%d,this.s%s,%d,t);', [len, JavaScript_Name(recContext.Any.StoreIndex, recContext.Any.Store.szName), Index]);   // I4611
          end;
        CODE_DEADKEY:
          Result := Result + nlt + Format('k.KDO(%d,t,%d);', [len, recContext.Deadkey.Deadkey]);   // I4611
        else
        begin
          ReportError(fkp.Line, CERR_NotSupportedInKeymanWebContext, Format('Statement %s is not currently supported in CODE_CONTEXT match', [GetCodeName(recContext.Code)]));  // I1971   // I4061
        //CODE_NUL: ;     // todo: check if context is longer than that...
          Result := Result + nlt + '/*.*/ ';   // I4611
        end;
      end;
    end
    else
    begin
      if not InQuotes then
      begin
        Result := Result + nlt + Format('k.KO(%d,t,"', [len]);   // I4611
        InQuotes := True;
      end;

      if recContext.ChrVal in [Ord('"'), Ord('\')] then Result := Result + '\';
      Result := Result + Javascript_String(recContext.ChrVal);  // I2242
    end;
  end;

begin
  nlt := nl + FTabstop+FTabstop+FTabstop;   // I3681
  Result := '';
	InQuotes := False;

  pwsz := pwszOutput;

  if Assigned(fkp) then
  begin
    if IsKeyboardVersion10OrLater
    // KMW >=10.0 use the full, sentinel-based length for context deletions.
    then
    begin
      len := xstrlen(fkp.dpContext);
      n := len;
      pwszContext := fkp.dpContext;

      //CODE_IFOPT:    // I3429
      //CODE_IFSYSTEMSTORE:     // I3430
      for i := 1 to n do
      begin
        rec := ExpandSentinel(pwszContext);
        if rec.IsSentinel and (rec.Code in [CODE_IFOPT, CODE_IFSYSTEMSTORE]) then
          Dec(len);
        pwszContext := incxstr(pwszContext);
      end;
    end
    // KMW < 10.0 exclude all sentinel-based characters, including deadkeys, from direct context deletion.
    // Deadkeys have alternative special handling.
    else len := xstrlen_printing(fkp.dpContext);
  end
  else
    len := -1;

  if IsKeyboardVersion10OrLater() and (pwsz^ <> #0) then
    begin
      Result := Result + nlt+Format('k.KDC(%d,t);', [ len ] );   // I3681
      len := -1;
    end;

	while pwsz^ <> #0 do
  begin
    rec := ExpandSentinel(pwsz);
    if rec.IsSentinel then
    begin
			if InQuotes then
      begin
        Result := Result + '");';
        InQuotes := False;
      end;

      case rec.Code of
        CODE_CONTEXT:
          begin
            if (pwsz <> pwszOutput) or (len = -1) then
            begin
              pwszContext := fkp.dpContext;
              n := 1;
              while pwszContext^ <> '' do   // I4611
              begin
                Result := Result + ContextChar(n, pwszContext);
                Inc(n);
                pwszContext := incxstr(pwszContext);
              end;

              //Result := Result + Format('k.KO(%d,t,k.KC(%d,%d,t));', [len, xstrlen_printing(fkp.dpContext), xstrlen_printing(fkp.dpContext)]);
            end;
              { else, we don't need to output anything - just don't delete the context }
            len := -1;
          end;
        CODE_CONTEXTEX:
          begin
            pwszContext := fkp.dpContext; for i := 1 to rec.ContextEx.Index - 1 do pwszContext := incxstr(pwszContext);
            Result := Result + ContextChar(rec.ContextEx.Index, pwszContext);   // I4611

            len := -1;
          end;
        CODE_BEEP:
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);   // I3681
            Result := Result + nlt+'k.KB(t);';   // I3681
            len := -1;
          end;
        CODE_NUL:
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);   // I3681
            len := -1;
          end;
        CODE_INDEX:
          begin
            CheckStoreForInvalidFunctions(fkp, rec.Index.Store); // I1520

            // This code was wrong.  We need to ignore CODE_NUL, CODE_DEADKEY in LHS context index counter.
            // This is why the compiler goes wrong -- and why the previous fix was inconsistent.
            // The I783 test did not test either of these cases.  It seems some of the keyboards were
            // compiled in-between the original fix and I783 re-fix, and then happened to work due to
            // their simplicity.

            Index := AdjustIndex(fkp.dpContext, rec.Index.Index);   // I3910

            Result := Result + nlt+Format('k.KIO(%d,this.s%s,%d,t);', [len, JavaScript_Name(rec.Index.StoreIndex, rec.Index.Store.szName),
              // I783 - was: rec.Index.Index [2007-06-04]
              // I783 again.  Returned to rec.Index.Index.  Was previously: [2008-08-15]
              //              xstrlen(fkp.dpContext) + 1 - rec.Index.Index]);
              //      this was wrong.  Can't find any reason why this change was made
              //      which suggests it was in response to another bug and poorly traced (bad Marc)
              //      and not properly tested (bad, bad Marc).  Anyway, now tested with test_i783
              Index]);   // I3681   // I3910
            len := -1;
          end;
        CODE_DEADKEY:
          begin
            Result := Result + nlt+Format('k.KDO(%d,t,%d);', [len, rec.Deadkey.Deadkey]);   // I3681
            len := -1;
          end;
        CODE_USE:
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);   // I3681
            Result := Result + nlt+Format('r=this.g%s(t,e);', [JavaScript_Name(rec.Use.GroupIndex, rec.Use.Group.szName)]);    // I1959   // I3681
            len := -1;
          end;
        CODE_CALL:
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);   // I3681
            n := FCallFunctions.IndexOf(CallFunctionName(rec.Call.Store.dpString));
            if n = -1 then
              n := FCallFunctions.Add(CallFunctionName(rec.Call.Store.dpString));
            Result := Result + nlt+Format('r=this.c%d(t,e);', [n]);    // I1959   // I3681
            len := -1;
          end;
        CODE_SETOPT:    // I3429
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);   // I3681
            Result := Result + nlt+Format('this.s%s=this.s%s;',
              [JavaScript_Name(rec.SetOpt.StoreIndex1,rec.SetOpt.Store1.szName),
              JavaScript_Name(rec.SetOpt.StoreIndex2,rec.SetOpt.Store2.szName)]);  // I3429   // I3681
            len := -1;
          end;
        CODE_RESETOPT:  // I3429
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);   // I3681

            Result := Result + nlt+Format('this.s%s=k.KLOAD(this.KI,"%s",%s);',
              [JavaScript_Name(rec.ResetOpt.StoreIndex,rec.ResetOpt.Store.szName),
              JavaScript_Name(rec.ResetOpt.StoreIndex,rec.ResetOpt.Store.szName,True),
              JavaScript_Store(fkp.Line, rec.ResetOpt.Store.dpString)]);  // I3429   // I3681   // I3659
            len := -1;
          end;
        CODE_SAVEOPT:  // I3429
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);
            Result := Result + nlt+Format('k.KSAVE("%s",this.s%s);',
              [JavaScript_Name(rec.SaveOpt.StoreIndex,rec.SaveOpt.Store.szName,True),   // I3690
              JavaScript_Name(rec.SaveOpt.StoreIndex,rec.SaveOpt.Store.szName)]); // I3429   // I3659   // I3681
            len := -1;
          end;
        CODE_SETSYSTEMSTORE:  // I3437
          begin
            if len > 0 then Result := Result + nlt+Format('k.KO(%d,t,"");', [len]);   // I3681
            Result := Result + nlt+Format('k.KSETS(%d,this.s%s,t);',   // I3681
              [rec.SetSystemStore.dwSystemID,
              JavaScript_Name(rec.SetSystemStore.StoreIndex, rec.SetSystemStore.Store.szName)]);
            len := -1;
          end;
        else
        begin
          if Assigned(fkp)
            then ReportError(fkp.Line, CERR_NotSupportedInKeymanWebOutput, Format('Statement %s is not currently supported in output', [GetCodeName(rec.Code)]))  // I1971   // I4061
            else ReportError(0, CERR_NotSupportedInKeymanWebOutput, Format('Statement %s is not currently supported in output', [GetCodeName(rec.Code)]));  // I1971   // I4061
          Result := Result + '';
        end;
      end;
    end
    else
    begin
			if not InQuotes then
      begin
        Result := Result + nlt+Format('k.KO(%d,t,"', [len]);   // I3681
        InQuotes := True; len := -1;
      end;

      if rec.ChrVal in [Ord('"'), Ord('\')] then Result := Result + '\';
      Result := Result + Javascript_String(rec.ChrVal);  // I2242
    end;

    pwsz := incxstr(pwsz);
	end;

	if InQuotes then Result := Result + '");';
end;

function TCompileKeymanWeb.JavaScript_Shift(fkp: PFILE_KEY; FMnemonic: Boolean): Integer;
begin
  if FMnemonic then
  begin
    if (fkp.ShiftFlags and KMX_VIRTUALCHARKEY) = KMX_VIRTUALCHARKEY then
    begin
      ReportError(fkp.Line, CERR_VirtualCharacterKeysNotSupportedInKeymanWeb, 'Virtual character keys not currently supported in KeymanWeb');  // I1971   // I4061
      Exit(0);
    end;

    if ((fkp.ShiftFlags and KMX_ISVIRTUALKEY) = KMX_ISVIRTUALKEY) and (Ord(fkp.Key) <= 255) then
    begin
      // We prohibit K_ keys for mnemonic layouts. We don't block T_ and U_ keys.
      // TODO: this doesn't resolve the issue of, e.g. SHIFT+K_SPACE
      // https://github.com/keymanapp/keyman/issues/265
      ReportError(fkp.Line, CERR_VirtualKeysNotValidForMnemonicLayouts, 'Virtual keys are not valid for mnemonic layouts');  // I1971   // I4061
      Exit(0);
    end;
  end;

  if (fkp.ShiftFlags and KMX_ISVIRTUALKEY) = KMX_ISVIRTUALKEY then
  begin
    if IsKeyboardVersion10OrLater then
    begin
      // Full chiral modifier and state key support starts with KeymanWeb 10.0
      Result := fkp.ShiftFlags;
    end
    else
    begin
      // Non-chiral support only and no support for state keys
      if (fkp.ShiftFlags and (
        KMX_LCTRLFLAG or KMX_RCTRLFLAG or KMX_LALTFLAG or KMX_RALTFLAG)) <> 0 then   // I4118
      begin
        ReportError(fkp.Line, CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb, 'Extended shift flags LALT, RALT, LCTRL, RCTRL are not supported in KeymanWeb');
      end;

      if (fkp.ShiftFlags and (
        KMX_CAPITALFLAG or KMX_NOTCAPITALFLAG or KMX_NUMLOCKFLAG or KMX_NOTNUMLOCKFLAG or KMX_SCROLLFLAG or KMX_NOTSCROLLFLAG)) <> 0 then   // I4118
      begin
        ReportError(fkp.Line, CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb, 'Extended shift flags CAPS and NCAPS are not supported in KeymanWeb');
      end;

      Result := KMX_ISVIRTUALKEY or (Integer(fkp.ShiftFlags) and (KMX_SHIFTFLAG or KMX_CTRLFLAG or KMX_ALTFLAG));
    end;
  end
  else
  begin
    Result := KMX_ISVIRTUALKEY;
    if Pos(fkp.Key, USEnglishShift) > 0 then
      Result := Result or KMX_SHIFTFLAG;
  end;
end;

///
/// Returns a Javascript representation of a key modifier state, either as a constant (debug mode)
/// or as an integer.
///
/// @param fkp         Pointer to key record
/// @param FMnemonic   True if the keyboard is a mnemonic layout
///
/// @return string representation of the key modifier state, e.g.
///         'modCodes.SHIFT | modCodes.CAPS | modCodes.VIRTUAL_KEY /* 0x4110 */' or
///         '16656'
///
function TCompileKeymanWeb.JavaScript_ShiftAsString(fkp: PFILE_KEY; FMnemonic: Boolean): string;
begin
  if not FDebug
    then Result := IntToStr(JavaScript_Shift(fkp, FMnemonic))
    else Result := ' '+FormatModifierAsBitflags(JavaScript_Shift(fkp, FMnemonic));
end;

function TCompileKeymanWeb.JavaScript_Store(line: Integer; pwsz: PWideChar): string;
var
  ch: DWord;
  n: Integer;
  rec: TSentinelRecord;
const
  wcsentinel: WideString = #$FFFF;
begin
  n := Pos(wcsentinel, pwsz);

  // Start:  plain text store.  Always use for < 10.0, conditionally for >= 10.0.
  if (n = 0) or not IsKeyboardVersion10OrLater then
  begin
    Result := '"';
    while pwsz^ <> #0 do
    begin
      if PWord(pwsz)^ = UC_SENTINEL then
      begin
        Result := Result + '.'; // UC_SENTINEL values are not supported in stores for KMW < 10.0.
      end
      else
      begin
        ch := GetSuppChar(pwsz);
        if ch in [Ord('"'), Ord('\')] then Result := Result + '\';
        Result := Result + Javascript_String(ch);  // I2242
      end;

      pwsz := incxstr(pwsz);
    end;
    Result := Result + '"';
  end
  else
  begin
    Result := '[';
    while pwsz^ <> #0 do
    begin
      if Result <> '[' then Result := Result + ',';

      rec := ExpandSentinel(pwsz);
      if rec.IsSentinel then
      begin
        if rec.Code = CODE_DEADKEY then
        begin
          Result := Result + Format('{t:''d'',d:%d}', [rec.Deadkey.DeadKey]);
        end
        else if rec.Code = CODE_BEEP then
        begin
          Result := Result + '{t:''b''}'
        end
        else //if rec.Code = CODE_EXTENDED then
        begin
          // At some point, we may wish to filter which codes are safe to stub out like this
          // versus which ones should be an error.  The commented-out-code shows the way to
          // handle such cases.
          Result := Result + '''''';
        end
//        else
//        begin
//          //ReportError(line, CERR_SomewhereIGotItWrong, 'Internal Error: unexpected sentinel character in store definition');
//        end;
      end
      else
      begin
        ch := GetSuppChar(pwsz);
        Result := Result + '"';
        // TODO:  Refactor the section below into JavaScript_String, as it's
        // quite common in our code base.
        if ch in [Ord('"'), Ord('\')] then Result := Result + '\';
        Result :=  Result + Javascript_String(ch) + '"';  // I2242
      end;

      pwsz := incxstr(pwsz);
    end;
    Result := Result + ']';
  end;
end;

function TCompileKeymanWeb.JavaScript_String(ch: DWord): string;  // I2242
begin
  if ch < 32 then
  begin
    case ch of
      9:  Result := '\t';
      10: Result := '\n';
      13: Result := '\r';
      else Result := '\x'+IntToHex(ch,1);
    end;
  end
  else
    Result := Char.ConvertFromUtf32(ch);  // I3310
end;

procedure TCompileKeymanWeb.ReportError(line: Integer; msgcode: LongWord; const text: string);  // I1971
var
  flag: LongWord;
begin
  flag := CERR_FLAG or CFATAL_FLAG;
  if FCompilerWarningsAsErrors then
    flag := flag or CWARN_FLAG;
  if (msgcode and flag) <> 0 then FError := True;
  FCallback(line, msgcode, PAnsiChar(AnsiString(text)));  // I3310 /// TODO: K9: convert to Unicode
end;

function TCompileKeymanWeb.RequotedString(s: WideString): string;
var
  i: Integer;
begin
  i := 1;
  while i <= Length(s) do
  begin
    if (s[i] = '"') or (s[i] = '\') then begin s := Copy(s, 1, i-1)+'\'+Copy(s, i, Length(s)); Inc(i); end   // I4368
    else if (s[i] = #13) then   // I4368
    begin
      s := Copy(s, 1, i-1) + '\n' + Copy(s,i+1, Length(s));
      Inc(i);
    end
    else if (s[i] = #10) then   // I4368
      s[i] := ' ';
    Inc(i);
  end;
  Result := s;
end;

{**
  Determine if a rule should be ignored by the KeymanWeb compiler because it is
  targeted at a .kmx-based or KMFL platform with the platform() or if(&platform)
  statement.

  Parameters:      fkp     Pointer to the rule

  Return Value:    True if the rule should be excluded by the compiler
*}
function TCompileKeymanWeb.RuleIsExcludedByPlatform(fkp: PFILE_KEY): Boolean;
var
  rec: TSentinelRecord;
  pwsz: PChar;
begin
  pwsz := fkp.dpContext;
  if not Assigned(pwsz) then
    Exit(False);

	while pwsz^ <> #0 do
  begin
    rec := ExpandSentinel(pwsz);
    if rec.IsSentinel and
        (rec.Code = CODE_IFSYSTEMSTORE) and
        (rec.IfSystemStore.dwSystemID = TSS_PLATFORM) and
        (Pos('native', rec.IfSystemStore.Store.dpString) > 0) then
    begin
      if (Pos('windows', rec.IfSystemStore.Store.dpString) > 0) or
          (Pos('desktop', rec.IfSystemStore.Store.dpString) > 0) or
          (Pos('macosx', rec.IfSystemStore.Store.dpString) > 0) or
          (Pos('linux', rec.IfSystemStore.Store.dpString) > 0) then
        Exit(True);
    end;
    pwsz := incxstr(pwsz);
  end;

  Result := False;
end;

function TCompileKeymanWeb.VisualKeyboardFromFile(const FVisualKeyboardFileName: string): WideString;

  function WideQuote(s: WideString): WideString;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(s) do
      if (s[i] = '"') or (s[i] = '\') then Result := Result + '\'+s[i] else Result := Result + s[i];
  end;

  function VKShiftToLayerName(Shift: Integer): string;
  const
    masks: array[0..6] of string = (
      'leftctrl',
      'rightctrl',
      'leftalt',
      'rightalt',
      'shift',
      'ctrl',
      'alt'
    );
  var
    i: Integer;
  begin
    shift := VkShiftStateToKmxShiftState(shift);
    if shift = 0 then
      Result := 'default'
    else
    begin
      Result := '';
      for i := 0 to 6 do
        if shift and (1 shl i) <> 0 then
          Result := Result + masks[i] + '-';
      Delete(Result, Length(Result), 1);
    end;
  end;

  function VisualKeyboardToKLS(FVK: TVisualKeyboard): string;
  type
    TLayer = record
      Shift: Integer;
      Name: string;
      Keys: array[0..64] of string;
    end;
  var
    i: Integer;
    layers: array of TLayer;  // TDictionary may be faster but not worth the extra dev cost
    n, j: Integer;
    Found: Boolean;
  begin
    // Discover the layers used in the visual keyboard

    for i := 0 to FVK.Keys.Count - 1 do
    begin
      if kvkkUnicode in FVK.Keys[i].Flags then
      begin
        // Find the index of the key in KMW VK arrays
        n := CKeymanWebKeyCodes[FVK.Keys[i].VKey];
        if n = $FF then Continue;

        Found := False;
        for j := 0 to High(layers) do
          if layers[j].Shift = FVK.Keys[i].Shift then
          begin
            Found := True;
            layers[j].Keys[n] := FVK.Keys[i].Text;
            Break;
          end;

        if not Found then
        begin
          SetLength(layers, Length(layers)+1);
          layers[High(Layers)].Shift := FVK.Keys[i].Shift;
          layers[High(Layers)].Keys[n] := FVK.Keys[i].Text;
        end;
      end;
    end;

    // Build the layer array

    Result := nl+FTabStop+'this.KV.KLS={'+nl;

    for i := 0 to High(layers) do
    begin
      Result := Result + Format('%s%s"%s": [', [FTabStop,FTabStop,VKShiftToLayerName(layers[i].Shift)]);
      for j := 0 to High(layers[i].Keys)-1 do
      begin
        Result := Result + '"'+WideQuote(layers[i].Keys[j])+'",';
      end;
      Result := Result + '"'+WideQuote(layers[i].Keys[High(layers[i].Keys)])+'"]';
      if i < High(Layers) then
        Result := Result + ',' + nl;
    end;
    Result := Result + nl+FTabStop+'}';
  end;

  function BuildBKFromKLS: string;
  const func =
    'function(x){var e=Array.apply(null,Array(65)).map(String.prototype.valueOf,"")'+
    ',r=[],v,i,m=[''default'',''shift'',''ctrl'',''shift-ctrl'',''alt'',''shift-alt'','+
    '''ctrl-alt'',''shift-ctrl-alt''];for(i=m.length-1;i>=0;i--)if((v=x[m[i]])||r.length)'+
    'r=(v?v:e).slice().concat(r);return r}';
  func_debug =
    'function(x){'#13#10+
    '    var'#13#10+
    '      empty=Array.apply(null, Array(65)).map(String.prototype.valueOf,""),'#13#10+
    '      result=[], v, i,'#13#10+
    '      modifiers=[''default'',''shift'',''ctrl'',''shift-ctrl'',''alt'',''shift-alt'',''ctrl-alt'',''shift-ctrl-alt''];'#13#10+
    '    for(i=modifiers.length-1;i>=0;i--) {'#13#10+
    '      v = x[modifiers[i]];'#13#10+
    '      if(v || result.length > 0) {'#13#10+
    '        result=(v ? v : empty).slice().concat(result);'#13#10+
    '      }'#13#10+
    '    }'#13#10+
    '    return result;'#13#10+
    '  }';
  begin
    Result := nl+FTabStop+'this.KV.BK=('+IfThen(FDebug,func_debug,func)+')(this.KV.KLS)';
  end;

var
  FVK: TVisualKeyboard;
  f102, fbold, fitalic: string;
  fDisplayUnderlying: string;
begin
  Result := '';
  FVK := TVisualKeyboard.Create;
  try
    FVK.LoadFromFile(FVisualKeyboardFileName);
    if fsBold in FVK.Header.UnicodeFont.Style then fbold := 'bold ' else fbold := '';
    if fsItalic in FVK.Header.UnicodeFont.Style then fitalic := 'italic ' else fitalic := '';
    if kvkh102 in FVK.Header.Flags then f102 := '1' else f102 := '0';
    if kvkhDisplayUnderlying in FVK.Header.Flags then fDisplayUnderlying := '1' else fDisplayUnderlying := '0';   // I3886

    Result := Format('{F:''%s%s 1em "%s"'',K102:%s', [fitalic, fbold, FVK.Header.UnicodeFont.Name, f102]);   // I3886   // I3956
    Result := Result + Format('};%s%sthis.KDU=%s', [nl,FTabStop,fDisplayUnderlying]);   // I3946   // I3956
    Result := Result + ';'+VisualKeyboardToKLS(FVK);
    Result := Result + ';'+BuildBKFromKLS;
  finally
    FVK.Free;
  end;
end;

type
  TRequiredKey = (K_LOPT, K_BKSP, K_ENTER);   // I4447
  TRequiredKeys = set of TRequiredKey;

function RequiredKeysToString(keys: TRequiredKeys): string;   // I4060
var
  k: TRequiredKey;
begin
  Result := '';
  for k in keys do
    Result := Result + ', ' + GetEnumName(TypeInfo(TRequiredKey), Ord(k));
  Delete(Result, 1, 2);
end;

function TCompileKeymanWeb.ValidateLayoutFile(var sLayoutFile: string; const sVKDictionary: string): Boolean;   // I4060   // I4139
type
  TKeyIdType = (Key_Invalid, Key_Constant, Key_Touch, Key_Unicode);   // I4142
const
  CRequiredKeys: TRequiredKeys = [K_LOPT, K_BKSP, K_ENTER];   // I4447
var
  FPlatform: TTouchLayoutPlatform;
  FLayer: TTouchLayoutLayer;
  FRow: TTouchLayoutRow;
  FKey: TTouchLayoutKey;
  FSubKey: TTouchLayoutSubKey;
  FRequiredKeys: set of TRequiredKey;
  FDictionary: TStringList;

    function IsValidUnicodeValue(ch: Integer): Boolean;   // I4198
    begin
      Result :=
        ((ch >= $0020) and (ch <= $7F)) or
        ((ch >= $00A0) and (ch <= $10FFFF));
    end;

    function KeyIdType(FId: string): TKeyIdType;   // I4142
    begin
      Result := Key_Invalid;
      case UpCase(FId[1]) of
        'T': Result := Key_Touch;
        'U': if (Copy(FId, 1, 2) = 'U_') and IsValidUnicodeValue(StrToIntDef('$'+Copy(FId,3,MaxInt), 0)) then Result := Key_Unicode;   // I4198
        'K': if FindVKeyName(FId) <> $FFFF then Result := Key_Constant;
      end;
    end;

    procedure CheckKey(FId, FNextLayer: string; FKeyType: TTouchKeyType);   // I4119
    var
      FValid: TKeyIdType;
      v: Integer;
    begin
      //
      // Check that each touch layer has K_LOPT, K_ROPT, K_BKSP, K_ENTER
      //

      v := GetEnumValue(TypeInfo(TRequiredKey), FId);
      if v >= 0 then Include(FRequiredKeys, TRequiredKey(v));

      //
      // Check that each layer referenced exists
      //

      if FNextLayer <> '' then
      begin
        if FPlatform.Layers.IndexOfId(FNextLayer) < 0 then
          ReportError(0, CWARN_TouchLayoutMissingLayer, 'Key "'+FId+'" on platform "'+FPlatform.Name+'", layer "'+FLayer.Id+'", platform "'+FPlatform.Name+'", references a missing layer "'+FNextLayer+'".');
      end;

      //
      // Check that the key has a valid id   // I4142
      //

      if Trim(FId) = '' then
      begin
        if not (FKeyType in [tktBlank, tktSpacer]) and (FNextLayer = '') then
          ReportError(0, CWARN_TouchLayoutUnidentifiedKey, 'A key on layer "'+FLayer.Id+'" has no identifier.');
        Exit;
      end;

      FValid := KeyIdType(FId);

      if FValid = Key_Invalid then
      begin
        ReportError(0, CERR_TouchLayoutInvalidIdentifier, 'Key "'+FId+'" on "'+FPlatform.Name+'", layer "'+FLayer.Id+'" has an invalid identifier.');
      end;

      //
      // Check that each custom key code has at least *a* rule associated with it
      //

      if (FValid = Key_Touch) and (FNextLayer = '') and (FKeyType in [tktNormal, tktDeadKey]) then   // I4119
      begin
        // Search for the key in the key dictionary - ignore K_LOPT, K_ROPT...
        if FDictionary.IndexOf(FId) < 0 then
          ReportError(0, CWARN_TouchLayoutCustomKeyNotDefined, 'Key "'+FId+'" on layer "'+FLayer.Id+'", platform "'+FPlatform.Name+'", is a custom key but has no corresponding rule in the source.');
      end;
    end;

    procedure CheckDictionaryKeyValidity;   // I4142
    var
      gp, kp: Cardinal;
      i: Integer;
      fgp: PFILE_GROUP;
    fkp: PFILE_KEY;
    begin
      for i := 0 to FDictionary.Count - 1 do
      begin
        if FDictionary[i] = '' then
          Continue;
        if KeyIdType(FDictionary[i]) in [Key_Invalid, Key_Constant] then
        begin
          gp := 0;
          fgp := fk.dpGroupArray;
          while gp < fk.cxGroupArray do
          begin
            if fgp.fUsingKeys then
            begin
              kp := 0;
              fkp := fgp.dpKeyArray;
              while kp < fgp.cxKeyArray do
              begin
                if JavaScript_Key(fkp, fMnemonic) = i+256 then
                begin
                  ReportError(fkp.Line, CERR_InvalidKeyCode, 'Invalid key identifier "'+FDictionary[i]+'"');
                end;
                Inc(kp);
                Inc(fkp);
              end;
            end;
            Inc(gp);
            Inc(fgp);
          end;
        end;
      end;
    end;
begin
  FDictionary := TStringList.Create;
  try
    FDictionary.Delimiter := ' ';
    FDictionary.DelimitedText := sVKDictionary;

    CheckDictionaryKeyValidity;   // I4142

    with TTouchLayout.Create do
    try
      OnMessage :=
        procedure(Sender: TObject; const sMsg:string)
        begin
          ReportError(0, CERR_InvalidTouchLayoutFile, sMsg);
        end;

      if not Load(sLayoutFile) then
        Exit(False);

      FTouchLayoutFont := '';   // I4872

      for FPlatform in Data.Platforms do
      begin
        // Test that the font matches on all platforms   // I4872

        if FTouchLayoutFont = '' then
          FTouchLayoutFont := FPlatform.Font
        else if not SameText(FPlatform.Font, FTouchLayoutFont) then
        begin
          ReportError(0, CWARN_TouchLayoutFontShouldBeSameForAllPlatforms, 'The touch layout font should be the same for all platforms.');
        end;

        // Test that all required keys are present
        for FLayer in FPlatform.Layers do
        begin
          FRequiredKeys := [];
          for FRow in FLayer.Rows do
            for FKey in FRow.Keys do
            begin
              CheckKey(FKey.Id, FKey.NextLayer, FKey.SpT);   // I4119
              for FSubKey in FKey.Sk do
                CheckKey(FSubKey.Id, FSubKey.NextLayer, FSubKey.SpT);   // I4119
            end;

          if FRequiredKeys <> CRequiredKeys then
            ReportError(0, CWARN_TouchLayoutMissingRequiredKeys, 'Layer "'+FLayer.Id+'" on platform "'+FPlatform.Name+'" is missing the required key(s) '+RequiredKeysToString(CRequiredKeys-FRequiredKeys)+'.');
        end;
      end;

      if not FDebug then   // I4139
      begin
        // This strips out formatting for a big saving in file size
        sLayoutFile := Save(False);
      end;
    finally
      Free;
    end;
  finally
    FDictionary.Free;
  end;

  Result := True;
end;

//{$WARNINGS OFF} // bug in Delphi compiler returning W1035 return value undefined?!?
function TCompileKeymanWeb.WriteCompiledKeyboard: string; {UTF8}
    function Requote(const S: string): string;
    var
      I: Integer;
    begin
      Result := S;
      for I := Length(Result) downto 1 do
        if CharInSet(Result[I], ['''', '\']) then Insert('\', Result, I);
      Result := '''' + Result + '''';
    end;
var
	fgp: PFILE_GROUP;
	fsp: PFILE_STORE;
	fkp: PFILE_KEY;

  i, j, n: Integer;  // I1964 - crash with empty group

  vMnemonic: Integer;
  s, sRTL, sHelp, sHelpFile, sName, sEmbedJS, sEmbedCSS: string;
  sVisualKeyboard, sFullName: WideString;
  sLayoutFile, sVKDictionary: string;
  linecomment: string;  // I3438
  HasRules: Boolean;
  sModifierBitmask: string;
begin
  Result := '';//UTF16SignatureW;  // + '// compiled by Keyman Developer'+nl;  // I3474
	{ Locate the name of the keyboard }
  fsp := fk.dpStoreArray;
  sHelp := '''''';
  sFullName := '';
  sHelpFile := '';
  sEmbedJS := '';
  sEmbedCSS := '';   // I4368
  sVisualKeyboard := '';
  sLayoutFile := '';  // I3483
  FKeyboardVersion := '1.0';   // I4155
  sRTL := '';
  vMnemonic := 0;
	for i := 0 to fk.cxStoreArray - 1 do
  begin
    if fsp.dwSystemID = TSS_NAME then
      sFullName := fsp.dpString
    else if fsp.dwSystemID = TSS_KEYBOARDVERSION then   // I4155
      FKeyboardVersion := fsp.dpString   // I4155
    else if (fsp.szName = 'HelpFile') or (fsp.dwSystemID = TSS_KMW_HELPFILE) then
      sHelpFile := fsp.dpString
    else if (fsp.szName = 'Help') or (fsp.dwSystemID = TSS_KMW_HELPTEXT) then
      sHelp := '"'+RequotedString(fsp.dpString)+'"'
    else if (fsp.szName = 'VisualKeyboard') or (fsp.dwSystemID = TSS_VISUALKEYBOARD) then
      sVisualKeyboard := fsp.dpString
    else if (fsp.szName = 'EmbedJS') or (fsp.dwSystemID = TSS_KMW_EMBEDJS) then
      sEmbedJS := fsp.dpString
    else if (fsp.szName = 'EmbedCSS') or (fsp.dwSystemID = TSS_KMW_EMBEDCSS) then   // I4368
      sEmbedCSS := fsp.dpString
    else if (fsp.szName = 'RTL') or (fsp.dwSystemID = TSS_KMW_RTL) then
      if AnsiCompareText(fsp.dpString, '1') = 0 then sRTL := FTabStop+'this.KRTL=1;'+nl else sRTL := ''   // I3681
    else if fsp.dwSystemID = TSS_MNEMONIC then
      if AnsiCompareText(fsp.dpString, '1') = 0 then vMnemonic := 1 else vMnemonic := 0
    else if fsp.dwSystemID = TSS_VKDICTIONARY then  // I3438
      sVKDictionary := fsp.dpString
    else if fsp.dwSystemID = TSS_LAYOUTFILE then  // I3483
      sLayoutFile := fsp.dpString;
    Inc(fsp);
  end;

  sName := 'Keyboard_'+TKeyboardUtils.GetKeymanWebCompiledNameFromFileName(FInFile);

  if sHelpFile <> '' then
  begin
    sHelp := '';
    with TStringList.Create do
    try
      try
        LoadFromFile(ExtractFilePath(FInFile) + sHelpFile, TEncoding.UTF8);  // I3337
        for n := 0 to Count - 1 do
          sHelp := sHelp + Strings[n] + ' ';
      except
        on E:EFOpenError do
        begin
          ReportError(0, CWARN_HelpFileMissing, E.Message);  // I1971   // I4061
          sHelp := '';
        end;
      end;
    finally
      Free;
    end;

    sHelp := Requote(sHelp);
  end;

  if sEmbedJS <> '' then
  begin
    try
      with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(FInFile) + sEmbedJS, TEncoding.UTF8);  // I3337
        sEmbedJS := Text;
      finally
        Free;
      end;
    except
      on E:EFOpenError do   // I3683
      begin
        ReportError(0, CWARN_EmbedJsFileMissing, E.Message);   // I4061
        sEmbedJS := '';
      end;
    end;
  end;

  if sEmbedCSS <> '' then   // I4368
  begin
    try
      with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(FInFile) + sEmbedCSS, TEncoding.UTF8);  // I3337
        sEmbedCSS := Text;
      finally
        Free;
      end;
    except
      on E:EFOpenError do   // I3683
      begin
        ReportError(0, CWARN_EmbedJsFileMissing, E.Message);   // I4061
        sEmbedCSS := '';
      end;
    end;
  end;

  if sLayoutFile <> '' then  // I3483
  begin
    try
      with TStringList.Create do
      try
        LoadFromFile(ExtractFilePath(FInFile) + sLayoutFile, TEncoding.UTF8);
        sLayoutFile := Text;
        if not ValidateLayoutFile(sLayoutFile, sVKDictionary) then   // I4060
        begin
          sLayoutFile := '';
        end;
      finally
        Free;
      end;
    except
      on E:EFOpenError do   // I3683
      begin
        ReportError(0, CWARN_TouchLayoutFileMissing, E.Message);   // I4061
        sLayoutFile := '';
      end;
    end;
  end;

  if sVisualKeyboard <> '' then
  begin
    try
      // The Keyman .kmx compiler will change the value of this store from a
      // .kvks to a .kvk during the build. Earlier in the build, the visual keyboard
      // would have been compiled, so we need to account for that and use that file.

      sVisualKeyboard := VisualKeyboardFromFile(ExtractFilePath(FOutFile) + sVisualKeyboard);
    except
      on E:EFOpenError do   // I3947
      begin
        ReportError(0, CWARN_VisualKeyboardFileMissing, E.Message);   // I4061
        sVisualKeyboard := 'null';
      end;
    end;

    { viskbd = array(...); viskbdfont = ...
    sVisualKeyboard := 'null'; }
  end
  else
    sVisualKeyboard := 'null';

  sModifierBitmask := GetKeyboardModifierBitmask;

  fMnemonic := vMnemonic = 1;

  Result := Result + Format(
    '%s%s'+
    'KeymanWeb.KR(new %s());%s'+
    '%s%s'+
    'function %s()%s'+
    '{%s'+
    '%s%s%s'+
    '%sthis.KI="%s";%s'+
    '%sthis.KN="%s";%s'+
    '%sthis.KMINVER="%d.%d";%s'+
    '%sthis.KV=%s;%s'+
    '%sthis.KH=%s;%s'+
    '%sthis.KM=%d;%s'+
    '%sthis.KBVER="%s";%s'+   // I4155
    '%sthis.KMBM=%s;%s'+
    '%s',
    [
    JavaScript_SetupProlog, nl,
    sName, nl,
    JavaScript_SetupEpilog, nl,
    sName, nl,
    nl,
    FTabStop, JavaScript_SetupDebug, nl,
    FTabStop, sName, nl,
    FTabStop, RequotedString(sFullName), nl,
    FTabStop, (fk.version and VERSION_MASK_MAJOR) shr 8, fk.version and VERSION_MASK_MINOR, nl,
    FTabStop, sVisualKeyboard, nl,
    FTabStop, sHelp, nl,
    FTabStop, vMnemonic, nl,
    FTabStop, FKeyboardVersion, nl,   // I4155
    FTabStop, sModifierBitmask, nl,
    sRTL]);   // I3681

  if HasSupplementaryPlaneChars then
    Result := Result + Format('%sthis.KS=1;%s', [FTabStop, nl]);   // I3317

  if sVKDictionary <> '' then  // I3438
    Result := Result + Format('%sthis.KVKD="%s";%s', [FTabStop, RequotedString(sVKDictionary), nl]);   // I3681

  if sLayoutFile <> '' then  // I3483
  begin
    // Layout file format should be JSON: e.g: {...}
    Result := Result + Format('%sthis.KVKL=%s;%s', [FTabStop, sLayoutFile, nl]);   // I3681
  end;

  if sEmbedCSS <> '' then   // I4368
    Result := Result + Format('%sthis.KCSS="%s";%s', [FTabStop, RequotedString(sEmbedCSS), nl]);

	{ Write the stores out }
  fsp := fk.dpStoreArray;
	for i := 0 to fk.cxStoreArray - 1 do
  begin
    // I3438 - Save all system stores to the keyboard, for now   // I3684

    if not fsp.fIsDebug then // and not (fsp.dwSystemID in [TSS_BITMAP, TSS_NAME, TSS_VERSION, TSS_CUSTOMKEYMANEDITION, TSS_CUSTOMKEYMANEDITIONNAME, TSS_KEYMANCOPYRIGHT]) then
    begin
      if fsp.dwSystemID = TSS_COMPARISON then
        Result := Result + Format('%sthis.s%s=%s;%s', [FTabStop, JavaScript_Name(i, fsp.szName), JavaScript_Store(fsp.line, fsp.dpString), nl])
      else if fsp.dwSystemID = TSS_COMPILEDVERSION then
        Result := Result + Format('%sthis.KVER=%s;%s', [FTabStop, JavaScript_Store(fsp.line, fsp.dpString), nl])
      //else if fsp.dwSystemID = TSS_VKDICTIONARY then // I3438, required for vkdictionary
      //  Result := Result + Format('%sthis.s%s=%s;%s', [FTabStop, JavaScript_Name(i, fsp.szName), JavaScript_Store(fsp.line, fsp.dpString), nl])
      else if fsp.fIsOption and not fsp.fIsReserved then
        Result := Result + Format('%sthis.s%s=KeymanWeb.KLOAD(this.KI,"%s",%s);%s',
          [FTabstop,
          JavaScript_Name(i,fsp.szName),
          JavaScript_Name(i,fsp.szName,True),
          JavaScript_Store(fsp.line, fsp.dpString),
          nl])  // I3429
      else if fsp.dwSystemID = TSS_NONE then
        Result := Result + Format('%sthis.s%s=%s;%s', [FTabStop, JavaScript_Name(i, fsp.szName), JavaScript_Store(fsp.line, fsp.dpString), nl]);   // I3681
    end;
    Inc(fsp);
  end;

	{ Write the groups out }

  // I853 - begin unicode missing causes crash
{  if fk.StartGroup[BEGIN_UNICODE] = $FFFFFFFF then
  begin
    FCallback(fkp.Line, $4005, PChar('A "begin unicode" statement is required to compile a KeymanWeb keyboard'));
    Exit;
  end;}


	fgp := fk.dpGroupArray; Inc(fgp, fk.StartGroup[BEGIN_UNICODE]);
  Result := Result + Format(
    '%sthis.gs=function(t,e) {%s'+
    '%sreturn this.g%s(t,e);%s'+
    '%s};%s',
    [FTabStop, nl,
     FTabStop+FTabStop, JavaScript_Name(fk.StartGroup[BEGIN_UNICODE], fgp.szName), nl,
     FTabStop, nl]);   // I3681

  fgp := fk.dpGroupArray;
	for i := 0 to Integer(fk.cxGroupArray)-1 do  // I1964
  begin
    Result := Result + Format(
      '%sthis.g%s=function(t,e) {%s'+
      '%svar k=KeymanWeb,r=%d,m=0;%s',     //I1959
      [FTabstop, JavaScript_Name(i, fgp.szName), nl,
       FTabstop+FTabstop, IfThen(fgp.fUsingKeys,0,1), nl]);   // I3681

    fkp := fgp.dpKeyArray;
    HasRules := False;
		for j := 0 to Integer(fgp.cxKeyArray) - 1 do    // I1964
    begin
      if not RuleIsExcludedByPlatform(fkp) then
      begin
        Result := Result + FTabstop+FTabstop;   // I3681
        if HasRules then Result := Result + 'else ';
        HasRules := TRue;
        if fgp.fUsingKeys then
        begin
            Result := Result + Format('if(k.KKM(e,%s,%s)',
              [JavaScript_ShiftAsString(fkp, fMnemonic),
              JavaScript_KeyAsString(fkp, fMnemonic)]);
        end;

        if xstrlen(fkp.dpContext) > 0 then
        begin
          if not fgp.fUsingKeys
            then Result := Result + 'if('
            else Result := Result + '&&';

          Result := Result + JavaScript_ContextMatch(fkp, fkp.dpContext);
        end
        else if not fgp.fUsingKeys then
          Result := Result + 'if(1';

        if (fkp.Line > 0) and FDebug   // I4384
          then linecomment := Format('   // Line %d', [fkp.Line])   // I4373
          else linecomment := '';

        Result := Result + Format(
          ') {%s%s'+
          '%s',
          [linecomment, nl,
          FTabstop+FTabstop+FTabstop]);   // I3681
        if(fgp.fUsingKeys)                                                                                        // I1959
          then Result := Result + Format('r=m=1;%s', [JavaScript_OutputString(fkp, fkp.dpOutput, fgp)])    // I1959   // I3681
          else Result := Result + Format('m=1;%s', [JavaScript_OutputString(fkp, fkp.dpOutput, fgp)]);    // I1959   // I3681
        Result := Result + Format('%s%s}%s', [nl, FTabstop+FTabstop, nl]);   // I3681
      end;
      Inc(fkp);
		end;

		if Assigned(fgp.dpMatch) then
      Result := Result + Format(
        '%sif(m) {%s'+
        '%s%s%s'+
        '%s}%s',
        [FTabstop+FTabstop, nl,
        FTabstop+Ftabstop, JavaScript_OutputString(nil, fgp.dpMatch, fgp), nl,
        FTabstop+FTabstop, nl]);   // I3681
		if Assigned(fgp.dpNoMatch) then
      if fgp.fUsingKeys then    // I1382 - fixup m=1 to m=g()
        Result := Result + Format(
          '%sif(!m&&k.KIK(e)) {%s'+
          '%sr=1;%s%s'+
          '%s}%s',
          [FTabstop+FTabstop, nl,
          FTabstop+FTabstop+FTabstop, JavaScript_OutputString(nil, fgp.dpNoMatch, fgp), nl,
          FTabstop+FTabstop, nl])   // I1959. part 2, I2224   // I3681
      else
        Result := Result + Format(
          '%sif(!m) {%s'+
          '%s%s%s'+
          '%s}%s',
          [FTabstop+FTabstop, nl,
          FTabstop+FTabstop, JavaScript_OutputString(nil, fgp.dpNoMatch, fgp), nl,
          FTabstop+FTabstop, nl]);  // I1959   // I3681

    Result := Result + Format('%sreturn r;%s'+
                              '%s};%s',
                              [FTabstop+FTabstop, nl,
                              FTabstop, nl]); // I1959   // I3681
    Inc(fgp);
  end;

  for n := 0 to FCallFunctions.Count - 1 do
  begin
    s := ExtractFilePath(FInFile) + FCallFunctions[n] + '.call_js';
    if FileExists(s) then
      with TStringList.Create do
      try
        LoadFromFile(s, TEncoding.UTF8);  // I3337
        Result := Result + Format('%sthis.c%d=function(t,e){%s};%s', [FTabstop, n, Trim(Text), nl]);   // I3681
      finally
        Free;
      end
    else
      Result := Result + Format('%sthis.c%d=function(t,e){alert("call(%s) not defined");};%s', [FTabstop, n, FCallFunctions[n], nl]);   // I3681
  end;

  Result := Result + sEmbedJS + '}' + nl;   // I3681
end;

function TCompileKeymanWeb.GetCodeName(code: Integer): string;  // I1971
begin
  if (code >= Low(KMXCodeNames)) and (code <= High(KMXCodeNames))
    then Result := KMXCodeNames[code]
    else Result := IntToStr(code);
end;

function TCompileKeymanWeb.HasSupplementaryPlaneChars: Boolean;   // I3317
  function StringHasSuppChars(p: PWideChar): Boolean;
  begin
    if not Assigned(p) then
      Exit(False);

    while p^ <> #0 do
    begin
      if Char.IsSurrogate(p, 0) then
        Exit(True);
      p := incxstr(p);
    end;

    Result := False;
  end;

var
  I: Integer;
  fsp: PFILE_STORE;
  fgp: PFILE_GROUP;
  j: Integer;
  fkp: PFILE_KEY;
begin
  fsp := fk.dpStoreArray;
  for i := 0 to Integer(fk.cxStoreArray) - 1 do
  begin
    if StringHasSuppChars(fsp.dpString) then
      Exit(True);
    Inc(fsp);
  end;

  fgp := fk.dpGroupArray;
  for i := 0 to Integer(fk.cxGroupArray) - 1 do
  begin
    fkp := fgp.dpKeyArray;
    for j := 0 to Integer(fgp.cxKeyArray) - 1 do
    begin
      if StringHasSuppChars(fkp.dpContext) or
         StringHasSuppChars(fkp.dpOutput) then
        Exit(True);
      Inc(fkp);
    end;

    if StringHasSuppChars(fgp.dpMatch) or
       StringHasSuppChars(fgp.dpNoMatch) then
      Exit(True);

    Inc(fgp);
  end;

  Result := False;
end;

function TCompileKeymanWeb.IsKeyboardVersion10OrLater: Boolean;
begin
  Result := fk.version >= VERSION_100;
end;

procedure TCompileKeymanWeb.CheckStoreForInvalidFunctions(key: PFILE_KEY; store: PFILE_STORE);  // I1520
var
  n: Integer;
  pwsz: PWideChar;
  rec: TSentinelRecord;
const
  wcsentinel: WideString = #$FFFF;
begin
  n := Pos(wcsentinel, store.dpString);
  // Disable the check with versions >= 10.0, since we now support deadkeys in stores.
  if (n > 0) and not IsKeyboardVersion10OrLater then
  begin
    pwsz := PWideChar(store.dpString);
    Inc(pwsz, n-1);
    rec := ExpandSentinel(pwsz);
    ReportError(key.Line, CERR_NotSupportedInKeymanWebStore, Format('%s is not currently supported in store ''%s'' when used by any or index', [GetCodeName(rec.Code), store.szName]));   // I4061
  end;
end;

//{$WARNINGS ON} // bug in Delphi compiler returning W1035 return value undefined?!?


function TCompileKeymanWeb.ExpandSentinel(
  pwsz: PWideChar): TSentinelRecord;
var
  i: Integer;
  Found: Boolean;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Ord(pwsz^) = UC_SENTINEL then
  begin
    Result.IsSentinel := True;
    Inc(pwsz);
    Result.Code := Ord(pwsz^);
    Inc(pwsz);
    case Result.Code of
      CODE_ANY, CODE_NOTANY:      // I3981
        begin
          Result.Any.StoreIndex := Ord(pwsz^) - 1;
          Result.Any.Store := fk.dpStoreArray;
          Inc(Result.Any.Store, Result.Any.StoreIndex);
        end;
      CODE_INDEX:
        begin
          Result.Index.StoreIndex := Ord(pwsz^) - 1;
          Result.Index.Store := fk.dpStoreArray;
          Inc(Result.Index.Store, Result.Index.StoreIndex);
          Inc(pwsz);
          Result.Index.Index := Ord(pwsz^);
        end;
      CODE_DEADKEY:
        Result.DeadKey.DeadKey := Ord(pwsz^) - 1;
      CODE_USE:
        begin
          Result.Use.GroupIndex := Ord(pwsz^) - 1;
          Result.Use.Group := fk.dpGroupArray;
          Inc(Result.Use.Group, Result.Use.GroupIndex);
        end;
      CODE_CALL:
        begin
          Result.Call.StoreIndex := Ord(pwsz^) - 1;
          Result.Call.Store := fk.dpStoreArray;
          Inc(Result.Call.Store, Result.Call.StoreIndex);
        end;
      CODE_CONTEXTEX:
        Result.ContextEx.Index := Ord(pwsz^);
      CODE_SETOPT:    // I3429
        begin
          Result.SetOpt.StoreIndex1 := Ord(pwsz^) - 1;
          Result.SetOpt.Store1 := fk.dpStoreArray;
          Inc(Result.SetOpt.Store1, Result.SetOpt.StoreIndex1);
          Inc(pwsz);
          Result.SetOpt.StoreIndex2 := Ord(pwsz^) - 1;
          Result.SetOpt.Store2 := fk.dpStoreArray;
          Inc(Result.SetOpt.Store2, Result.SetOpt.StoreIndex2);
        end;
      CODE_SETSYSTEMSTORE:  // I3437
        begin
          Result.SetSystemStore.dwSystemID := Ord(pwsz^) - 1;
          Result.SetSystemStore.SystemStore := fk.dpStoreArray;
          Found := False;

          for i := 0 to fk.cxStoreArray - 1 do
          begin
            if Result.SetSystemStore.SystemStore.dwSystemID = Result.SetSystemStore.dwSystemID then
            begin
              Found := True;
              Break;
            end;
            Inc(Result.SetSystemStore.SystemStore);
          end;

          if not Found then Result.SetSystemStore.SystemStore := nil;

          Inc(pwsz);
          Result.SetSystemStore.StoreIndex := Ord(pwsz^) - 1;
          Result.SetSystemStore.Store := fk.dpStoreArray;
          Inc(Result.SetSystemStore.Store, Result.SetSystemStore.StoreIndex);
        end;
      CODE_RESETOPT:  // I3429
        begin
          Result.ResetOpt.StoreIndex := Ord(pwsz^) - 1;
          Result.ResetOpt.Store := fk.dpStoreArray;
          Inc(Result.ResetOpt.Store, Result.ResetOpt.StoreIndex);
        end;
      CODE_SAVEOPT:  // I3429
        begin
          Result.SaveOpt.StoreIndex := Ord(pwsz^) - 1;
          Result.SaveOpt.Store := fk.dpStoreArray;
          Inc(Result.SaveOpt.Store, Result.SaveOpt.StoreIndex);
        end;
      CODE_IFOPT:  // I3429
        begin
          Result.IfOpt.StoreIndex1 := Ord(pwsz^) - 1;
          Result.IfOpt.Store1 := fk.dpStoreArray;
          Inc(Result.IfOpt.Store1, Result.IfOpt.StoreIndex1);
          Inc(pwsz);
          Result.IfOpt.IsNot := Ord(pwsz^) - 1;  // I3429
          Inc(pwsz);
          Result.IfOpt.StoreIndex2 := Ord(pwsz^) - 1;
          Result.IfOpt.Store2 := fk.dpStoreArray;
          Inc(Result.IfOpt.Store2, Result.IfOpt.StoreIndex2);
        end;
      CODE_IFSYSTEMSTORE:  // I3430
        begin
          Result.IfSystemStore.dwSystemID := Ord(pwsz^) - 1;
          Result.IfSystemStore.SystemStore := fk.dpStoreArray;

          Found := False;

          for i := 0 to fk.cxStoreArray - 1 do
          begin
            if Result.IfSystemStore.SystemStore.dwSystemID = Result.IfSystemStore.dwSystemID then
            begin
              Found := True;
              Break;
            end;
            Inc(Result.IfSystemStore.SystemStore);
          end;

          if not Found then Result.IfSystemStore.SystemStore := nil;
          Inc(pwsz);
          Result.IfSystemStore.IsNot := Ord(pwsz^) - 1;  // I3430
          Inc(pwsz);
          Result.IfSystemStore.StoreIndex := Ord(pwsz^) - 1;
          Result.IfSystemStore.Store := fk.dpStoreArray;
          Inc(Result.IfSystemStore.Store, Result.IfSystemStore.StoreIndex);
        end;
    end;
  end
  else
    Result.ChrVal := GetSuppChar(pwsz);
end;

function TCompileKeymanWeb.CallFunctionName(s: WideString): WideString;
var
  n: Integer;
begin
  n := Pos(':', s);
  if n = 0 then Result := s
  else Result := Copy(s,n+1,Length(s));
end;

///
/// If debug mode, then returns Javascript code necessary for
/// accessing constants in the compiled keyboard
///
/// @return string of JavaScript code
///
function TCompileKeymanWeb.JavaScript_SetupDebug: string;
begin
  if IsKeyboardVersion10OrLater then
  begin
    if FDebug then
      Result := 'var modCodes = keyman.osk.modifierCodes;'+nl+
                FTabStop+'var keyCodes = keyman.osk.keyCodes;'+nl
    else
      Result := '';
  end
  else
    Result := '';
end;

function TCompileKeymanWeb.JavaScript_SetupProlog: string;
begin
  if IsKeyboardVersion10OrLater then
  begin
    Result := 'if(typeof keyman === ''undefined'') {'+nl+
      FTabStop+'console.log(''Keyboard requires KeymanWeb 10.0 or later'');'+nl+
      FTabStop+'if(typeof tavultesoft !== ''undefined'') tavultesoft.keymanweb.util.alert("This keyboard requires KeymanWeb 10.0 or later");'+nl+
      '} else {';
  end
  else
    Result := '';
end;

function TCompileKeymanWeb.JavaScript_SetupEpilog: string;
begin
  if IsKeyboardVersion10OrLater then
    Result := '}'
  else
    Result := '';
end;
///
/// Converts a modifier bit mask integer into its component bit flags
///
/// @param FBitMask A KMX modifier bitmask value
///
/// @return string of JavaScript code, e.g. 'modCodes.SHIFT | modCodes.CTRL /* 0x0030 */'
///
function TCompileKeymanWeb.FormatModifierAsBitflags(FBitMask: Cardinal): string;
const
  mask: array[0..14] of string = (
    'LCTRL',             // 0X0001
    'RCTRL',             // 0X0002
    'LALT',              // 0X0004
    'RALT',              // 0X0008

    'SHIFT',             // 0X0010
    'CTRL',              // 0X0020
    'ALT',               // 0X0040

    '???',               // Reserved

    'CAPS',              // 0X0100
    'NO_CAPS',           // 0X0200

    'NUM_LOCK',          // 0X0400
    'NO_NUM_LOCK',       // 0X0800

    'SCROLL_LOCK',       // 0X1000
    'NO_SCROLL_LOCK',    // 0X2000

    'VIRTUAL_KEY'        // 0X4000
  );
var
  i: Integer;
begin
  //TODO: We need to think about mnemonic layouts which are incompletely supported at present
  //tavultesoft.keymanweb.osk.

  if IsKeyboardVersion10OrLater then
  begin
    // This depends on flags defined in KeymanWeb 10.0
    Result := '';

    for i := 0 to High(mask) do
    begin
      if FBitMask and (1 shl i) <> 0 then
      begin
        if Result <> '' then Result := Result + ' | ';
        Result := Result + 'modCodes.'+mask[i];
      end;
    end;

    if Result = '' then
      Result := '0';

    Result := Result + ' /* 0x' + IntToHex(FBitMask, 4) + ' */'
  end
  else
    Result := '0x'+IntToHex(FBitMask, 4);
end;

///
/// Converts a key value into a constant
///
/// @param key A virtual key code
///
/// @return string of JavaScript code, e.g. 'keyCodes.K_A /* 0x41 */'
///
function TCompileKeymanWeb.FormatKeyAsString(key: Integer): string;
begin
  if IsKeyboardVersion10OrLater then
  begin
    // Depends on flags defined in KeymanWeb 10.0
    if (key <= 255) and (KMWVKeyNames[key] <> '')
      then Result := 'keyCodes.'+KMWVKeyNames[key]+ ' /* 0x' + IntToHex(key, 2) + ' */'
      else Result := '0x' + IntToHex(key, 2);
  end
  else
    Result := '0x' + IntToHex(key, 2);
end;

///
/// Determine the modifiers used in the target keyboard and return a bitmask
/// representing them, or an integer value when not in debug mode
///
/// @return string of JavaScript code, e.g. 'modCodes.SHIFT | modCodes.CTRL /* 0x0030 */'
///
function TCompileKeymanWeb.GetKeyboardModifierBitmask: string;
var
  i: Integer;
  gp: PFILE_GROUP;
  j: Integer;
  kp: PFILE_KEY;
  FBitMask: Integer;
begin
  FBitMask := 0;
  gp := fk.dpGroupArray;
  if fk.cxGroupArray > 0 then
  begin
    for i := 0 to fk.cxGroupArray-1 do
    begin
      if gp.fUsingKeys then
      begin
        kp := gp.dpKeyArray;
        if gp.cxKeyArray > 0 then
        begin
          for j := 0 to gp.cxKeyArray-1 do
          begin
            if not RuleIsExcludedByPlatform(kp) then
              FBitMask := FBitMask or JavaScript_Shift(kp, fMnemonic);
            Inc(kp);
          end;
        end;
      end;
      Inc(gp);
    end;
  end;

  if ((FBitMask and KMX_MASK_MODIFIER_CHIRAL) <> 0) and
    ((FBitMask and KMX_MASK_MODIFIER_NONCHIRAL) <> 0) then
  begin
    ReportError(0, CWARN_DontMixChiralAndNonChiralModifiers, 'This keyboard contains Ctrl,Alt and LCtrl,LAlt,RCtrl,RAlt sets of modifiers. Use only one or the other set for web target.');
  end;

  if FDebug
    then Result := FormatModifierAsBitflags(FBitMask and KMX_MASK_KEYS) // Exclude KMX_ISVIRTUALKEY, KMX_VIRTUALCHARKEY
    else Result := '0x'+IntToHex(FBitMask and KMX_MASK_KEYS, 4);
end;

end.

