/*
  Name:             Comperr
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      23 Aug 2006

  Modified Date:    6 Mar 2014
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          23 Aug 2006 - mcdurdin - Add Keyman 7.0 feature error
                    25 May 2010 - mcdurdin - I1632 - Keyboard Options
                    21 Feb 2014 - mcdurdin - I4061 - V9.0 - KeymanWeb compiler needs defined codes for some errors
                    06 Mar 2014 - mcdurdin - I4118 - V9.0 - KMW compiler should warn when extended shift flags are used
                    22 Jul 2020 - eddieantonio - Add lexical model range https://github.com/keymanapp/keyman/pull/3385
*/
   // I4061
#ifndef _kmn_compiler_errors_h
#define _kmn_compiler_errors_h

// Compiler Error Masks -- matches compiler-interfaces.ts

namespace CompilerErrorSeverity {
  // We use a namespace to stop the enum names leaking out into global namespace scope
  enum {
    Info =          0x000000, // Informational, not necessarily a problem
    Hint =          0x100000, // Something the user might want to be aware of
    Warn =          0x200000, // Warning: Not great, but we can keep going.
    Error =         0x300000, // Severe error where we can't continue
    Fatal =         0x400000, // OOM or should-not-happen internal problem
  };
};

#define MESSAGE_SEVERITY_MASK    0x00F00000  // includes reserved bits, 16 possible severity levels
#define MESSAGE_ERROR_MASK       0x000FFFFF  // error | namespace
#define MESSAGE_NAMESPACE_MASK   0x000FF000  // 256 possible namespaces
#define MESSAGE_BASEERROR_MASK   0x00000FFF  // error code, 2,048 possible error codes per namespace
#define MESSAGE_RESERVED_MASK    0xFF000000  // do not use these error values at this time

#define MESSAGE_NAMESPACE_KmnCompiler      0x2000

// Severity codes + namespace for shorthand use
#define SevFatal                                         (MESSAGE_NAMESPACE_KmnCompiler | CompilerErrorSeverity::Fatal)
#define SevError                                         (MESSAGE_NAMESPACE_KmnCompiler | CompilerErrorSeverity::Error)
#define SevWarn                                          (MESSAGE_NAMESPACE_KmnCompiler | CompilerErrorSeverity::Warn)
#define SevHint                                          (MESSAGE_NAMESPACE_KmnCompiler | CompilerErrorSeverity::Hint)
#define SevInfo                                          (MESSAGE_NAMESPACE_KmnCompiler | CompilerErrorSeverity::Info)

// These two values are not true error codes; they are used as
// return values in kmcmplib and are never passed to kmc-kmn
#define STATUS_Success                                       0x00000000 // NOTE: Not a message code
#define STATUS_EndOfFile                                     0x00000001 // NOTE: Not a message code

// Message codes
//
// All errors here and below are mirrored in
// kmc-kmn/src/compiler/kmn-compiler-messages.ts; if you add a new error here be
// sure to update that file also. Note that this correlation is currently
// maintained manually. All values must be below 0x1000 (exclusive of severity
// code).

namespace KmnCompilerMessages {
  enum {
    FATAL_BadCallParams =                                 SevFatal | 0x002,
    FATAL_CannotAllocateMemory =                          SevFatal | 0x004,
    ERROR_InfileNotExist =                                SevError | 0x005,   // #10678: reduced from fatal to error in 17.0
//    ERROR_CannotCreateOutfile =                           SevError | 0x006,   // #10678: reduced from fatal to error in 17.0, but unused
//    FATAL_UnableToWriteFully =                            SevFatal | 0x007,     // unused
    ERROR_CannotReadInfile =                              SevError | 0x008,   // #10678: reduced from fatal to error in 17.0
    FATAL_SomewhereIGotItWrong =                          SevFatal | 0x009,

    ERROR_InvalidToken =                                  SevError | 0x00A,
    ERROR_InvalidBegin =                                  SevError | 0x00B,
    ERROR_InvalidName =                                   SevError | 0x00C,
    ERROR_InvalidVersion =                                SevError | 0x00D,
    ERROR_InvalidLanguageLine =                           SevError | 0x00E,
    ERROR_LayoutButNoLanguage =                           SevError | 0x00F,
    ERROR_InvalidLayoutLine =                             SevError | 0x010,
    ERROR_NoVersionLine =                                 SevError | 0x011,
    ERROR_InvalidGroupLine =                              SevError | 0x012,
    ERROR_InvalidStoreLine =                              SevError | 0x013,
    ERROR_InvalidCodeInKeyPartOfRule =                    SevError | 0x014,
    ERROR_InvalidDeadkey =                                SevError | 0x015,
    ERROR_InvalidValue =                                  SevError | 0x016,
    ERROR_ZeroLengthString =                              SevError | 0x017,
    ERROR_TooManyIndexToKeyRefs =                         SevError | 0x018,
    ERROR_UnterminatedString =                            SevError | 0x019,
//    ERROR_StringInVirtualKeySection =                     SevError | 0x01A, no longer used, see #12612
//    ERROR_AnyInVirtualKeySection =                        SevError | 0x01B, no longer used, see #12612
    ERROR_InvalidAny =                                    SevError | 0x01C,
    ERROR_StoreDoesNotExist =                             SevError | 0x01D,
//    ERROR_BeepInVirtualKeySection =                       SevError | 0x01E, no longer used, see #12612
//    ERROR_IndexInVirtualKeySection =                      SevError | 0x01F, no longer used, see #12612
    ERROR_InvalidIndex =                                  SevError | 0x020,
//    ERROR_OutsInVirtualKeySection =                       SevError | 0x021, no longer used, see #12612
    ERROR_InvalidOuts =                                   SevError | 0x022,
//    ERROR_ContextInVirtualKeySection =                    SevError | 0x024, no longer used, see #12612
    ERROR_InvalidUse =                                    SevError | 0x025,
    ERROR_GroupDoesNotExist =                             SevError | 0x026,
    ERROR_VirtualKeyNotAllowedHere =                      SevError | 0x027,
    ERROR_InvalidSwitch =                                 SevError | 0x028,
    ERROR_NoTokensFound =                                 SevError | 0x029,
    ERROR_InvalidLineContinuation =                       SevError | 0x02A,
    ERROR_LineTooLong =                                   SevError | 0x02B,
    ERROR_InvalidCopyright =                              SevError | 0x02C,
    ERROR_CodeInvalidInThisSection =                      SevError | 0x02D,
    ERROR_InvalidMessage =                                SevError | 0x02E,
    ERROR_InvalidLanguageName =                           SevError | 0x02F,
    ERROR_InvalidBitmapLine =                             SevError | 0x030,
    ERROR_CannotReadBitmapFile =                          SevError | 0x031,
    ERROR_IndexDoesNotPointToAny =                        SevError | 0x032,
    ERROR_ReservedCharacter =                             SevError | 0x033,
    ERROR_InvalidCharacter =                              SevError | 0x034,
    ERROR_InvalidCall =                                   SevError | 0x035,
//    ERROR_CallInVirtualKeySection =                       SevError | 0x036, no longer used, see #12612
    ERROR_CodeInvalidInKeyStore =                         SevError | 0x037,
    ERROR_CannotLoadIncludeFile =                         SevError | 0x038,

    ERROR_60FeatureOnly_EthnologueCode =                  SevError | 0x039,
    ERROR_60FeatureOnly_MnemonicLayout =                  SevError | 0x03A,
    ERROR_60FeatureOnly_OldCharPosMatching =              SevError | 0x03B,
    ERROR_60FeatureOnly_NamedCodes =                      SevError | 0x03C,
    ERROR_60FeatureOnly_Contextn =                        SevError | 0x03D,
    ERROR_501FeatureOnly_Call =                           SevError | 0x03E,

    ERROR_InvalidNamedCode =                              SevError | 0x03F,
    ERROR_InvalidSystemStore =                            SevError | 0x040,

    ERROR_60FeatureOnly_VirtualCharKey =                  SevError | 0x044,

    ERROR_VersionAlreadyIncluded =                        SevError | 0x045,

    ERROR_70FeatureOnly =                                 SevError | 0x046,

    ERROR_80FeatureOnly =                                 SevError | 0x047,
//    ERROR_InvalidInVirtualKeySection =                    SevError | 0x048, no longer used, see #12612
    ERROR_InvalidIf =                                     SevError | 0x049,
    ERROR_InvalidReset =                                  SevError | 0x04A,
    ERROR_InvalidSet =                                    SevError | 0x04B,
    ERROR_InvalidSave =                                   SevError | 0x04C,

    ERROR_InvalidEthnologueCode =                         SevError | 0x04D,

    FATAL_CannotCreateTempfile =                          SevFatal | 0x04E,

    ERROR_90FeatureOnly_IfSystemStores =                  SevError | 0x04F,
    ERROR_IfSystemStore_NotFound =                        SevError | 0x050,
    ERROR_90FeatureOnly_SetSystemStores =                 SevError | 0x051,
    ERROR_SetSystemStore_NotFound =                       SevError | 0x052,
    ERROR_90FeatureOnlyVirtualKeyDictionary =             SevError | 0x053,

    ERROR_NotSupportedInKeymanWebContext =                SevError | 0x054,
    ERROR_NotSupportedInKeymanWebOutput =                 SevError | 0x055,
    ERROR_NotSupportedInKeymanWebStore =                  SevError | 0x056,
    ERROR_VirtualCharacterKeysNotSupportedInKeymanWeb =   SevError | 0x057,
    ERROR_VirtualKeysNotValidForMnemonicLayouts =         SevError | 0x058,
    ERROR_InvalidTouchLayoutFile =                        SevError | 0x059,
    ERROR_TouchLayoutInvalidIdentifier =                  SevError | 0x05A,
    ERROR_InvalidKeyCode =                                SevError | 0x05B,
    ERROR_90FeatureOnlyLayoutFile =                       SevError | 0x05C,
    ERROR_90FeatureOnlyKeyboardVersion =                  SevError | 0x05D,
    ERROR_KeyboardVersionFormatInvalid =                  SevError | 0x05E,
    ERROR_ContextExHasInvalidOffset =                     SevError | 0x05F,
    ERROR_90FeatureOnlyEmbedCSS =                         SevError | 0x060,
    ERROR_90FeatureOnlyTargets =                          SevError | 0x061,
    ERROR_ContextAndIndexInvalidInMatchNomatch =          SevError | 0x062,
    ERROR_140FeatureOnlyContextAndNotAnyWeb =             SevError | 0x063,

    ERROR_ExpansionMustFollowCharacterOrVKey =            SevError | 0x064,
    ERROR_VKeyExpansionMustBeFollowedByVKey =             SevError | 0x065,
    ERROR_CharacterExpansionMustBeFollowedByCharacter =   SevError | 0x066,
    ERROR_VKeyExpansionMustUseConsistentShift =           SevError | 0x067,
    ERROR_ExpansionMustBePositive =                       SevError | 0x068,

    ERROR_CasedKeysMustContainOnlyVirtualKeys =           SevError | 0x069,
    ERROR_CasedKeysMustNotIncludeShiftStates =            SevError | 0x06A,
    ERROR_CasedKeysNotSupportedWithMnemonicLayout =       SevError | 0x06B,

    ERROR_CannotUseReadWriteGroupFromReadonlyGroup =      SevError | 0x06C,
    ERROR_StatementNotPermittedInReadonlyGroup =          SevError | 0x06D,
    ERROR_OutputInReadonlyGroup =                         SevError | 0x06E,
    ERROR_NewContextGroupMustBeReadonly =                 SevError | 0x06F,
    ERROR_PostKeystrokeGroupMustBeReadonly =              SevError | 0x070,

    ERROR_DuplicateGroup =                                SevError | 0x071,
    ERROR_DuplicateStore =                                SevError | 0x072,
    ERROR_RepeatedBegin =                                 SevError | 0x073,
    ERROR_VirtualKeyInContext =                           SevError | 0x074,

    ERROR_OutsTooLong =                                   SevError | 0x075,
    ERROR_ExtendedStringTooLong =                         SevError | 0x076,
    ERROR_VirtualKeyExpansionTooLong =                    SevError | 0x077,
    ERROR_CharacterRangeTooLong =                         SevError | 0x078,
    ERROR_NonBMPCharactersNotSupportedInKeySection =      SevError | 0x079,

    ERROR_InvalidTarget =                                 SevError | 0x07A,
    ERROR_NoTargetsSpecified =                            SevError | 0x07B,

    WARN_TooManyWarnings =                              SevWarn | 0x080,
    WARN_OldVersion =                                   SevWarn | 0x081,
    WARN_BitmapNotUsed =                                SevWarn | 0x082,
    WARN_CustomLanguagesNotSupported =                  SevWarn | 0x083,
    WARN_KeyBadLength =                                 SevWarn | 0x084,
    WARN_IndexStoreShort =                              SevWarn | 0x085,
    WARN_UnicodeInANSIGroup =                           SevWarn | 0x086,
    WARN_ANSIInUnicodeGroup =                           SevWarn | 0x087,
    WARN_UnicodeSurrogateUsed =                         SevWarn | 0x088,
    WARN_ReservedCharacter =                            SevWarn | 0x089,

    INFO_MinimumCoreEngineVersion =                     SevInfo | 0x08A, // renamed from INFO_Info in 18.0-alpha

    WARN_VirtualKeyWithMnemonicLayout =                 SevWarn | 0x08B,
    WARN_VirtualCharKeyWithPositionalLayout =           SevWarn | 0x08C,
    WARN_StoreAlreadyUsedAsOptionOrCall =               SevWarn | 0x08D,
    WARN_StoreAlreadyUsedAsStoreOrCall =                SevWarn | 0x08E,
    WARN_StoreAlreadyUsedAsStoreOrOption =              SevWarn | 0x08F,

    WARN_PunctuationInEthnologueCode =                  SevWarn | 0x090,

    WARN_TouchLayoutMissingLayer =                      SevWarn | 0x091,
    WARN_TouchLayoutCustomKeyNotDefined =               SevWarn | 0x092,
    WARN_TouchLayoutMissingRequiredKeys =               SevWarn | 0x093,
    WARN_HelpFileMissing =                              SevWarn | 0x094,
    WARN_EmbedJsFileMissing =                           SevWarn | 0x095,
    // WARN_TouchLayoutFileMissing =                       SevWarn | 0x096,
    // WARN_VisualKeyboardFileMissing =                    SevWarn | 0x097,
    WARN_ExtendedShiftFlagsNotSupportedInKeymanWeb =    SevWarn | 0x098,   // I4118
    WARN_TouchLayoutUnidentifiedKey =                   SevWarn | 0x099,
    HINT_UnreachableKeyCode =                           SevHint | 0x09A,

    // WARN_CouldNotCopyJsonFile =                         SevWarn | 0x09B,
    WARN_PlatformNotInTargets =                         SevWarn | 0x09C,

    WARN_HeaderStatementIsDeprecated =                  SevWarn | 0x09D,
    WARN_UseNotLastStatementInRule =                    SevWarn | 0x09E,

    WARN_TouchLayoutFontShouldBeSameForAllPlatforms =   SevWarn | 0x09F,
    // WARN_InvalidJSONMetadataFile =                      SevWarn | 0x0A0,
    // WARN_JSONMetadataOSKFontShouldMatchTouchFont =      SevWarn | 0x0A1,
    WARN_KVKFileIsInSourceFormat =                      SevWarn | 0x0A2,

    WARN_DontMixChiralAndNonChiralModifiers =           SevWarn | 0x0A3,
    WARN_MixingLeftAndRightModifiers =                  SevWarn | 0x0A4,

    WARN_LanguageHeadersDeprecatedInKeyman10 =          SevWarn | 0x0A5,

    HINT_NonUnicodeFile =                               SevHint | 0x0A6,

    // WARN_TooManyErrorsOrWarnings =                      SevWarn | 0x0A7,

    WARN_HotkeyHasInvalidModifier =                     SevWarn | 0x0A8,

    WARN_TouchLayoutSpecialLabelOnNormalKey =           SevWarn | 0x0A9,

    WARN_OptionStoreNameInvalid =                       SevWarn | 0x0AA,

    WARN_NulNotFirstStatementInContext =                SevWarn | 0x0AB,
    WARN_IfShouldBeAtStartOfContext =                   SevWarn | 0x0AC,

    WARN_KeyShouldIncludeNCaps =                        SevWarn | 0x0AD,

    HINT_UnreachableRule =                              SevHint | 0x0AE,

    WARN_VirtualKeyInOutput =                           SevWarn | 0x0AF,

    HINT_IndexStoreLong =                               SevHint | 0x0B0,

    FATAL_BufferOverflow =                                SevFatal | 0x0C0
//    FATAL_Break =                                         SevFatal | 0x0C1,      unused
  };
}

#endif  // _kmn_compiler_errors_h
