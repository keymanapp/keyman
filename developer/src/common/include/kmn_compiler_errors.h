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

#define MESSAGE_NAMESPACE_KmnCompiler      0x2000

// Severity codes
//
// Note: these may not be combined, and are not a bitmask, for historical
// reasons they are separate bits
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

#define FATAL_BadCallParams                                 0x00008002
#define FATAL_CannotAllocateMemory                          0x00008004
#define CERR_InfileNotExist                                0x00004005   // #10678: reduced from fatal to error in 17.0
// #define CERR_CannotCreateOutfile                           0x00004006   // #10678: reduced from fatal to error in 17.0, but unused
// #define FATAL_UnableToWriteFully                            0x00008007     // unused
#define CERR_CannotReadInfile                              0x00004008   // #10678: reduced from fatal to error in 17.0
#define FATAL_SomewhereIGotItWrong                          0x00008009

#define CERR_InvalidToken                                  0x0000400A
#define CERR_InvalidBegin                                  0x0000400B
#define CERR_InvalidName                                   0x0000400C
#define CERR_InvalidVersion                                0x0000400D
#define CERR_InvalidLanguageLine                           0x0000400E
#define CERR_LayoutButNoLanguage                           0x0000400F
#define CERR_InvalidLayoutLine                             0x00004010
#define CERR_NoVersionLine                                 0x00004011
#define CERR_InvalidGroupLine                              0x00004012
#define CERR_InvalidStoreLine                              0x00004013
#define CERR_InvalidCodeInKeyPartOfRule                    0x00004014
#define CERR_InvalidDeadkey                                0x00004015
#define CERR_InvalidValue                                  0x00004016
#define CERR_ZeroLengthString                              0x00004017
#define CERR_TooManyIndexToKeyRefs                         0x00004018
#define CERR_UnterminatedString                            0x00004019
#define CERR_StringInVirtualKeySection                     0x0000401A
#define CERR_AnyInVirtualKeySection                        0x0000401B
#define CERR_InvalidAny                                    0x0000401C
#define CERR_StoreDoesNotExist                             0x0000401D
#define CERR_BeepInVirtualKeySection                       0x0000401E
#define CERR_IndexInVirtualKeySection                      0x0000401F
#define CERR_InvalidIndex                                  0x00004020
#define CERR_OutsInVirtualKeySection                       0x00004021
#define CERR_InvalidOuts                                   0x00004022
#define CERR_ContextInVirtualKeySection                    0x00004024
#define CERR_InvalidUse                                    0x00004025
#define CERR_GroupDoesNotExist                             0x00004026
#define CERR_VirtualKeyNotAllowedHere                      0x00004027
#define CERR_InvalidSwitch                                 0x00004028
#define CERR_NoTokensFound                                 0x00004029
#define CERR_InvalidLineContinuation                       0x0000402A
#define CERR_LineTooLong                                   0x0000402B
#define CERR_InvalidCopyright                              0x0000402C
#define CERR_CodeInvalidInThisSection                      0x0000402D
#define CERR_InvalidMessage                                0x0000402E
#define CERR_InvalidLanguageName                           0x0000402F
#define CERR_InvalidBitmapLine                             0x00004030
#define CERR_CannotReadBitmapFile                          0x00004031
#define CERR_IndexDoesNotPointToAny                        0x00004032
#define CERR_ReservedCharacter                             0x00004033
#define CERR_InvalidCharacter                              0x00004034
#define CERR_InvalidCall                                   0x00004035
#define CERR_CallInVirtualKeySection                       0x00004036
#define CERR_CodeInvalidInKeyStore                         0x00004037
#define CERR_CannotLoadIncludeFile                         0x00004038

#define CERR_60FeatureOnly_EthnologueCode                  0x00004039
#define CERR_60FeatureOnly_MnemonicLayout                  0x0000403A
#define CERR_60FeatureOnly_OldCharPosMatching              0x0000403B
#define CERR_60FeatureOnly_NamedCodes                      0x0000403C
#define CERR_60FeatureOnly_Contextn                        0x0000403D
#define CERR_501FeatureOnly_Call                           0x0000403E

#define CERR_InvalidNamedCode                              0x0000403F
#define CERR_InvalidSystemStore                            0x00004040

#define CERR_60FeatureOnly_VirtualCharKey                  0x00004044

#define CERR_VersionAlreadyIncluded                        0x00004045

#define CERR_70FeatureOnly                                 0x00004046

#define CERR_80FeatureOnly                                 0x00004047
#define CERR_InvalidInVirtualKeySection                    0x00004048
#define CERR_InvalidIf                                     0x00004049
#define CERR_InvalidReset                                  0x0000404A
#define CERR_InvalidSet                                    0x0000404B
#define CERR_InvalidSave                                   0x0000404C

#define CERR_InvalidEthnologueCode                         0x0000404D

#define FATAL_CannotCreateTempfile                          0x0000804E

#define CERR_90FeatureOnly_IfSystemStores                  0x0000404F
#define CERR_IfSystemStore_NotFound                        0x00004050
#define CERR_90FeatureOnly_SetSystemStores                 0x00004051
#define CERR_SetSystemStore_NotFound                       0x00004052
#define CERR_90FeatureOnlyVirtualKeyDictionary             0x00004053

#define CERR_NotSupportedInKeymanWebContext                0x00004054
#define CERR_NotSupportedInKeymanWebOutput                 0x00004055
#define CERR_NotSupportedInKeymanWebStore                  0x00004056
#define CERR_VirtualCharacterKeysNotSupportedInKeymanWeb   0x00004057
#define CERR_VirtualKeysNotValidForMnemonicLayouts         0x00004058
#define CERR_InvalidTouchLayoutFile                        0x00004059
#define CERR_TouchLayoutInvalidIdentifier                  0x0000405A
#define CERR_InvalidKeyCode                                0x0000405B
#define CERR_90FeatureOnlyLayoutFile                       0x0000405C
#define CERR_90FeatureOnlyKeyboardVersion                  0x0000405D
#define CERR_KeyboardVersionFormatInvalid                  0x0000405E
#define CERR_ContextExHasInvalidOffset                     0x0000405F
#define CERR_90FeatureOnlyEmbedCSS                         0x00004060
#define CERR_90FeatureOnlyTargets                          0x00004061
#define CERR_ContextAndIndexInvalidInMatchNomatch          0x00004062
#define CERR_140FeatureOnlyContextAndNotAnyWeb             0x00004063

#define CERR_ExpansionMustFollowCharacterOrVKey            0x00004064
#define CERR_VKeyExpansionMustBeFollowedByVKey             0x00004065
#define CERR_CharacterExpansionMustBeFollowedByCharacter   0x00004066
#define CERR_VKeyExpansionMustUseConsistentShift           0x00004067
#define CERR_ExpansionMustBePositive                       0x00004068

#define CERR_CasedKeysMustContainOnlyVirtualKeys           0x00004069
#define CERR_CasedKeysMustNotIncludeShiftStates            0x0000406A
#define CERR_CasedKeysNotSupportedWithMnemonicLayout       0x0000406B

#define CERR_CannotUseReadWriteGroupFromReadonlyGroup      0x0000406C
#define CERR_StatementNotPermittedInReadonlyGroup          0x0000406D
#define CERR_OutputInReadonlyGroup                         0x0000406E
#define CERR_NewContextGroupMustBeReadonly                 0x0000406F
#define CERR_PostKeystrokeGroupMustBeReadonly              0x00004070

#define CERR_DuplicateGroup                                0x00004071
#define CERR_DuplicateStore                                0x00004072
#define CERR_RepeatedBegin                                 0x00004073
#define CERR_VirtualKeyInContext                           0x00004074

#define CERR_OutsTooLong                                   0x00004075
#define CERR_ExtendedStringTooLong                         0x00004076
#define CERR_VirtualKeyExpansionTooLong                    0x00004077
#define CERR_CharacterRangeTooLong                         0x00004078
#define CERR_NonBMPCharactersNotSupportedInKeySection      0x00004079

#define CERR_InvalidTarget                                 0x0000407A
#define CERR_NoTargetsSpecified                            0x0000407B

#define CWARN_TooManyWarnings                              0x00002080
#define CWARN_OldVersion                                   0x00002081
#define CWARN_BitmapNotUsed                                0x00002082
#define CWARN_CustomLanguagesNotSupported                  0x00002083
#define CWARN_KeyBadLength                                 0x00002084
#define CWARN_IndexStoreShort                              0x00002085
#define CWARN_UnicodeInANSIGroup                           0x00002086
#define CWARN_ANSIInUnicodeGroup                           0x00002087
#define CWARN_UnicodeSurrogateUsed                         0x00002088
#define CWARN_ReservedCharacter                            0x00002089
// Note: CWARN_Info has an "info" severity; this changed in 17.0. Earlier versions
// had a special case for CWARN_Info in message output.
#define CWARN_Info                                         0x0000008A
#define CINFO_Info                                         CWARN_Info
#define CWARN_VirtualKeyWithMnemonicLayout                 0x0000208B
#define CWARN_VirtualCharKeyWithPositionalLayout           0x0000208C
#define CWARN_StoreAlreadyUsedAsOptionOrCall               0x0000208D
#define CWARN_StoreAlreadyUsedAsStoreOrCall                0x0000208E
#define CWARN_StoreAlreadyUsedAsStoreOrOption              0x0000208F

#define CWARN_PunctuationInEthnologueCode                  0x00002090

#define CWARN_TouchLayoutMissingLayer                      0x00002091
#define CWARN_TouchLayoutCustomKeyNotDefined               0x00002092
#define CWARN_TouchLayoutMissingRequiredKeys               0x00002093
#define CWARN_HelpFileMissing                              0x00002094
#define CWARN_EmbedJsFileMissing                           0x00002095
#define CWARN_TouchLayoutFileMissing                       0x00002096
#define CWARN_VisualKeyboardFileMissing                    0x00002097
#define CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb    0x00002098   // I4118
#define CWARN_TouchLayoutUnidentifiedKey                   0x00002099
#define CHINT_UnreachableKeyCode                           0x0000109A

#define CWARN_CouldNotCopyJsonFile                         0x0000209B
#define CWARN_PlatformNotInTargets                         0x0000209C

#define CWARN_HeaderStatementIsDeprecated                  0x0000209D
#define CWARN_UseNotLastStatementInRule                    0x0000209E

#define CWARN_TouchLayoutFontShouldBeSameForAllPlatforms   0x0000209F
#define CWARN_InvalidJSONMetadataFile                      0x000020A0
#define CWARN_JSONMetadataOSKFontShouldMatchTouchFont      0x000020A1
#define CWARN_KVKFileIsInSourceFormat                      0x000020A2

#define CWARN_DontMixChiralAndNonChiralModifiers           0x000020A3
#define CWARN_MixingLeftAndRightModifiers                  0x000020A4

#define CWARN_LanguageHeadersDeprecatedInKeyman10          0x000020A5

#define CHINT_NonUnicodeFile                               0x000010A6

#define CWARN_TooManyErrorsOrWarnings                      0x000020A7

#define CWARN_HotkeyHasInvalidModifier                     0x000020A8

#define CWARN_TouchLayoutSpecialLabelOnNormalKey           0x000020A9

#define CWARN_OptionStoreNameInvalid                       0x000020AA

#define CWARN_NulNotFirstStatementInContext                0x000020AB
#define CWARN_IfShouldBeAtStartOfContext                   0x000020AC

#define CWARN_KeyShouldIncludeNCaps                        0x000020AD

#define CHINT_UnreachableRule                              0x000010AE

#define CWARN_VirtualKeyInOutput                           0x000020AF

#define CHINT_IndexStoreLong                               0x000010B0

#define FATAL_BufferOverflow                                0x000080C0
// #define FATAL_Break                                         0x000080C1      unused

#endif  // _kmn_compiler_errors_h
