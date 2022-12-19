import { FCompilerWarningsAsErrors } from "./compiler-globals.js";

export let FError = false;

export function ReportError(line: number, msgcode: number, text: string) {  // I1971
  let flag = CERR_FLAG | CFATAL_FLAG;
  if(FCompilerWarningsAsErrors) {
    flag |= CWARN_FLAG;
  }
  if (msgcode & flag) {
    FError = true;
  }
  FCallback(line, msgcode, text);  // I3310
}

export const
  CHINT_FLAG = 0x1000,
  CWARN_FLAG = 0x2000,
  CERR_FLAG = 0x4000,
  CFATAL_FLAG = 0x8000,

// Any messages from the lexical model compiler occupy this range:
  CERR_LEXICAL_MODEL_MIN                           = 0x0800,
  CERR_LEXICAL_MODEL_MAX                           = 0x08FF,

  CERR_None                                        = 0x0000,
  CERR_EndOfFile                                   = 0x0001,

  CERR_BadCallParams                               = 0x8002,
  CERR_CannotAllocateMemory                        = 0x9004,
  CERR_InfileNotExist                              = 0x8005,
  CERR_CannotCreateOutfile                         = 0x8006,
  CERR_UnableToWriteFully                          = 0x8007,
  CERR_CannotReadInfile                            = 0x8008,
  CERR_SomewhereIGotItWrong                        = 0x8009,

  CERR_InvalidToken                                = 0x400A,
  CERR_InvalidBegin                                = 0x400B,
  CERR_InvalidName                                 = 0x400C,
  CERR_InvalidVersion                              = 0x400D,
  CERR_InvalidLanguageLine                         = 0x400E,
  CERR_LayoutButNoLanguage                         = 0x400F,
  CERR_InvalidLayoutLine                           = 0x4010,
  CERR_NoVersionLine                               = 0x4011,
  CERR_InvalidGroupLine                            = 0x4012,
  CERR_InvalidStoreLine                            = 0x4013,
  CERR_InvalidCodeInKeyPartOfRule                  = 0x4014,
  CERR_InvalidDeadkey                              = 0x4015,
  CERR_InvalidValue                                = 0x4016,
  CERR_ZeroLengthString                            = 0x4017,
  CERR_TooManyIndexToKeyRefs                       = 0x4018,
  CERR_UnterminatedString                          = 0x4019,
  CERR_StringInVirtualKeySection                   = 0x401A,
  CERR_AnyInVirtualKeySection                      = 0x401B,
  CERR_InvalidAny                                  = 0x401C,
  CERR_StoreDoesNotExist                           = 0x401D,
  CERR_BeepInVirtualKeySection                     = 0x401E,
  CERR_IndexInVirtualKeySection                    = 0x401F,
  CERR_InvalidIndex                                = 0x4020,
  CERR_OutsInVirtualKeySection                     = 0x4021,
  CERR_InvalidOuts                                 = 0x4022,
  CERR_ContextInVirtualKeySection                  = 0x4024,
  CERR_InvalidUse                                  = 0x4025,
  CERR_GroupDoesNotExist                           = 0x4026,
  CERR_VirtualKeyNotAllowedHere                    = 0x4027,
  CERR_InvalidSwitch                               = 0x4028,
  CERR_NoTokensFound                               = 0x4029,
  CERR_InvalidLineContinuation                     = 0x402A,
  CERR_LineTooLong                                 = 0x402B,
  CERR_InvalidCopyright                            = 0x402C,
  CERR_CodeInvalidInThisSection                    = 0x402D,
  CERR_InvalidMessage                              = 0x402E,
  CERR_InvalidLanguageName                         = 0x402F,
  CERR_InvalidBitmapLine                           = 0x4030,
  CERR_CannotReadBitmapFile                        = 0x4031,
  CERR_IndexDoesNotPointToAny                      = 0x4032,
   CERR_ReservedCharacter                          = 0x4033,
  CERR_InvalidCharacter                            = 0x4034,
  CERR_InvalidCall                                 = 0x4035,
  CERR_CallInVirtualKeySection                     = 0x4036,
  CERR_CodeInvalidInKeyStore                       = 0x4037,
  CERR_CannotLoadIncludeFile                       = 0x4038,

  CERR_60FeatureOnly_EthnologueCode                = 0x4039,
  CERR_60FeatureOnly_MnemonicLayout                = 0x403A,
  CERR_60FeatureOnly_OldCharPosMatching            = 0x403B,
  CERR_60FeatureOnly_NamedCodes                    = 0x403C,
  CERR_60FeatureOnly_Contextn                      = 0x403D,
  CERR_501FeatureOnly_Call                         = 0x403E,

  CERR_InvalidNamedCode                            = 0x403F,
  CERR_InvalidSystemStore                          = 0x4040,

  CERR_CallIsProfessionalFeature                   = 0x4041,
  CERR_IncludeCodesIsProfessionalFeature           = 0x4042,
  CERR_MnemonicLayoutIsProfessionalFeature         = 0x4043,
  CERR_60FeatureOnly_VirtualCharKey                = 0x4044,

  CERR_VersionAlreadyIncluded                      = 0x4045,

  CERR_70FeatureOnly                               = 0x4046,

  CERR_80FeatureOnly                               = 0x4047,
  CERR_InvalidInVirtualKeySection                  = 0x4048,
  CERR_InvalidIf                                   = 0x4049,
  CERR_InvalidReset                                = 0x404A,
  CERR_InvalidSet                                  = 0x404B,
  CERR_InvalidSave                                 = 0x404C,

  CERR_InvalidEthnologueCode                       = 0x404D,

  CERR_CannotCreateTempfile                        = 0x804E,

  CERR_90FeatureOnly_IfSystemStores                = 0x404F,
  CERR_IfSystemStore_NotFound                      = 0x4050,
  CERR_90FeatureOnly_SetSystemStores               = 0x4051,
  CERR_SetSystemStore_NotFound                     = 0x4052,
  CERR_90FeatureOnlyVirtualKeyDictionary           = 0x4053,

  CERR_NotSupportedInKeymanWebContext               = 0x4054,
  CERR_NotSupportedInKeymanWebOutput                = 0x4055,
  CERR_NotSupportedInKeymanWebStore                 = 0x4056,
  CERR_VirtualCharacterKeysNotSupportedInKeymanWeb  = 0x4057,
  CERR_VirtualKeysNotValidForMnemonicLayouts        = 0x4058,
  CERR_InvalidTouchLayoutFile                       = 0x4059,
  CERR_TouchLayoutInvalidIdentifier                 = 0x405A,   // I4142
  CERR_InvalidKeyCode                               = 0x405B,   // I4142

  CERR_90FeatureOnlyLayoutFile                     = 0x405C,
  CERR_90FeatureOnlyKeyboardVersion                = 0x405D,
  CERR_KeyboardVersionFormatInvalid                = 0x405E,
  CERR_ContextExHasInvalidOffset                   = 0x405F,
  CERR_90FeatureOnlyEmbedCSS                       = 0x4060,
  CERR_90FeatureOnlyTargets                        = 0x4061,
  CERR_ContextAndIndexInvalidInMatchNomatch        = 0x4062,
  CERR_140FeatureOnlyContextAndNotAnyWeb           = 0x4063,

  CERR_ExpansionMustFollowCharacterOrVKey          = 0x4064,
  CERR_VKeyExpansionMustBeFollowedByVKey           = 0x4065,
  CERR_CharacterExpansionMustBeFollowedByCharacter = 0x4066,
  CERR_VKeyExpansionMustUseConsistentShift         = 0x4067,
  CERR_ExpansionMustBePositive                     = 0x4068,

  CERR_CasedKeysMustContainOnlyVirtualKeys         = 0x4069,
  CERR_CasedKeysMustNotIncludeShiftStates          = 0x406A,
  CERR_CasedKeysNotSupportedWithMnemonicLayout     = 0x406B,

  CERR_CannotUseReadWriteGroupFromReadonlyGroup    = 0x406C,
  CERR_StatementNotPermittedInReadonlyGroup        = 0x406D,
  CERR_OutputInReadonlyGroup                       = 0x406E,
  CERR_NewContextGroupMustBeReadonly               = 0x406F,
  CERR_PostKeystrokeGroupMustBeReadonly            = 0x4070,

  CERR_DuplicateGroup                              = 0x4071,
  CERR_DuplicateStore                              = 0x4072,

  CWARN_TooManyWarnings                            = 0x2080,
  CWARN_OldVersion                                 = 0x2081,
  CWARN_BitmapNotUsed                              = 0x2082,
  CWARN_CustomLanguagesNotSupported                = 0x2083,
  CWARN_KeyBadLength                               = 0x2084,
  CWARN_IndexStoreShort                            = 0x2085,
  CWARN_UnicodeInANSIGroup                         = 0x2086,
  CWARN_ANSIInUnicodeGroup                         = 0x2087,
  CWARN_UnicodeSurrogateUsed                       = 0x2088,
  CWARN_ReservedCharacter                          = 0x2089,
  CWARN_Info                                       = 0x208A,
  CWARN_VirtualKeyWithMnemonicLayout               = 0x208B,
  CWARN_VirtualCharKeyWithPositionalLayout         = 0x208C,
  CWARN_StoreAlreadyUsedAsOptionOrCall             = 0x208D,
  CWARN_StoreAlreadyUsedAsStoreOrCall              = 0x208E,
  CWARN_StoreAlreadyUsedAsStoreOrOption            = 0x208F,

  CWARN_PunctuationInEthnologueCode                = 0x2090,

  CWARN_TouchLayoutMissingLayer                     = 0x2091,
  CWARN_TouchLayoutCustomKeyNotDefined              = 0x2092,
  CWARN_TouchLayoutMissingRequiredKeys              = 0x2093,
  CWARN_HelpFileMissing                             = 0x2094,
  CWARN_EmbedJsFileMissing                          = 0x2095,
  CWARN_TouchLayoutFileMissing                      = 0x2096,
  CWARN_VisualKeyboardFileMissing                   = 0x2097,
  CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb   = 0x2098,   // I4118
  CWARN_TouchLayoutUnidentifiedKey                  = 0x2099,   // I4142
  CHINT_UnreachableKeyCode                          = 0x109A,   // I4141

  CWARN_CouldNotCopyJsonFile                        = 0x209B,   // I4688
  CWARN_PlatformNotInTargets                        = 0x209C,

  CWARN_HeaderStatementIsDeprecated                 = 0x209D,
  CWARN_UseNotLastStatementInRule                   = 0x209E,

  CWARN_TouchLayoutFontShouldBeSameForAllPlatforms  = 0x209F,   // I4872
  CWARN_InvalidJSONMetadataFile                     = 0x20A0,   // I4872
  CWARN_JSONMetadataOSKFontShouldMatchTouchFont     = 0x20A1,   // I4872

  CWARN_DontMixChiralAndNonChiralModifiers          = 0x20A3,
  CWARN_MixingLeftAndRightModifiers                 = 0x20A4,

  CWARN_LanguageHeadersDeprecatedInKeyman10         = 0x20A5,

  CHINT_NonUnicodeFile                              = 0x10A6,

  CWARN_TooManyErrorsOrWarnings                     = 0x20A7,

  CWARN_HotkeyHasInvalidModifier                    = 0x20A8,

  CWARN_TouchLayoutSpecialLabelOnNormalKey          = 0x20A9,

  CWARN_OptionStoreNameInvalid                      = 0x20AA,

  CWARN_NulNotFirstStatementInContext               = 0x20AB,
  CWARN_IfShouldBeAtStartOfContext                  = 0x20AC,

  CWARN_KeyShouldIncludeNCaps                       = 0x20AD,

  CHINT_UnreachableRule                             = 0x10AE,
  CHINT_FilenameHasDifferingCase                    = 0x10AF,
  CWARN_MissingFile                                 = 0x20B0;
