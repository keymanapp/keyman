(*
  Name:             CompileErrorCodes
  Copyright:        Copyright (C) SIL International.
  Documentation:
  Description:
  Create Date:      6 Mar 2014

  Modified Date:    24 Aug 2015
  Authors:          mcdurdin
  Related Files:
  Dependencies:

  Bugs:
  Todo:
  Notes:
  History:          06 Mar 2014 - mcdurdin - I4118 - V9.0 - KMW compiler should warn when extended shift flags are used
                    19 Mar 2014 - mcdurdin - I4141 - V9.0 - Warn when unusable key ids are used
                    19 Mar 2014 - mcdurdin - I4142 - V9.0 - Validate key ids are in an acceptable format
                    04 May 2015 - mcdurdin - I4688 - V9.0 - Add build path to project settings
                    24 Aug 2015 - mcdurdin - I4872 - OSK font and Touch Layout font should be the same in Developer
                    28 Feb 2018 - jahorton - Imported the "SomewhereIGotItWrong" error code for use in KMW compilation
                    22 Jul 2020 - eddieantonio - Add lexical model range https://github.com/keymanapp/keyman/pull/3385
*)
unit CompileErrorCodes;

interface

const
  CHINT_FLAG = $1000;
  CWARN_FLAG = $2000;
  CERR_FLAG = $4000;
  CFATAL_FLAG = $8000;

// Any messages from the lexical model compiler occupy this range:
  CERR_LEXICAL_MODEL_MIN                           = $0800;
  CERR_LEXICAL_MODEL_MAX                           = $08FF;

  CERR_None                                        = $0000;
  CERR_EndOfFile                                   = $0001;

  CERR_BadCallParams                               = $8002;
  CERR_CannotAllocateMemory                        = $9004;
  CERR_InfileNotExist                              = $8005;
  CERR_CannotCreateOutfile                         = $8006;
  CERR_UnableToWriteFully                          = $8007;
  CERR_CannotReadInfile                            = $8008;
  CERR_SomewhereIGotItWrong                        = $8009;

  CERR_InvalidToken                                = $400A;
  CERR_InvalidBegin                                = $400B;
  CERR_InvalidName                                 = $400C;
  CERR_InvalidVersion                              = $400D;
  CERR_InvalidLanguageLine                         = $400E;
  CERR_LayoutButNoLanguage                         = $400F;
  CERR_InvalidLayoutLine                           = $4010;
  CERR_NoVersionLine                               = $4011;
  CERR_InvalidGroupLine                            = $4012;
  CERR_InvalidStoreLine                            = $4013;
  CERR_InvalidCodeInKeyPartOfRule                  = $4014;
  CERR_InvalidDeadkey                              = $4015;
  CERR_InvalidValue                                = $4016;
  CERR_ZeroLengthString                            = $4017;
  CERR_TooManyIndexToKeyRefs                       = $4018;
  CERR_UnterminatedString                          = $4019;
  CERR_StringInVirtualKeySection                   = $401A;
  CERR_AnyInVirtualKeySection                      = $401B;
  CERR_InvalidAny                                  = $401C;
  CERR_StoreDoesNotExist                           = $401D;
  CERR_BeepInVirtualKeySection                     = $401E;
  CERR_IndexInVirtualKeySection                    = $401F;
  CERR_InvalidIndex                                = $4020;
  CERR_OutsInVirtualKeySection                     = $4021;
  CERR_InvalidOuts                                 = $4022;
  CERR_ContextInVirtualKeySection                  = $4024;
  CERR_InvalidUse                                  = $4025;
  CERR_GroupDoesNotExist                           = $4026;
  CERR_VirtualKeyNotAllowedHere                    = $4027;
  CERR_InvalidSwitch                               = $4028;
  CERR_NoTokensFound                               = $4029;
  CERR_InvalidLineContinuation                     = $402A;
  CERR_LineTooLong                                 = $402B;
  CERR_InvalidCopyright                            = $402C;
  CERR_CodeInvalidInThisSection                    = $402D;
  CERR_InvalidMessage                              = $402E;
  CERR_InvalidLanguageName                         = $402F;
  CERR_InvalidBitmapLine                           = $4030;
  CERR_CannotReadBitmapFile                        = $4031;
  CERR_IndexDoesNotPointToAny                      = $4032;
   CERR_ReservedCharacter                          = $4033;
  CERR_InvalidCharacter                            = $4034;
  CERR_InvalidCall                                 = $4035;
  CERR_CallInVirtualKeySection                     = $4036;
  CERR_CodeInvalidInKeyStore                       = $4037;
  CERR_CannotLoadIncludeFile                       = $4038;

  CERR_60FeatureOnly_EthnologueCode                = $4039;
  CERR_60FeatureOnly_MnemonicLayout                = $403A;
  CERR_60FeatureOnly_OldCharPosMatching            = $403B;
  CERR_60FeatureOnly_NamedCodes                    = $403C;
  CERR_60FeatureOnly_Contextn                      = $403D;
  CERR_501FeatureOnly_Call                         = $403E;

  CERR_InvalidNamedCode                            = $403F;
  CERR_InvalidSystemStore                          = $4040;

  CERR_CallIsProfessionalFeature                   = $4041;
  CERR_IncludeCodesIsProfessionalFeature           = $4042;
  CERR_MnemonicLayoutIsProfessionalFeature         = $4043;
  CERR_60FeatureOnly_VirtualCharKey                = $4044;

  CERR_VersionAlreadyIncluded                      = $4045;

  CERR_70FeatureOnly                               = $4046;

  CERR_80FeatureOnly                               = $4047;
  CERR_InvalidInVirtualKeySection                  = $4048;
  CERR_InvalidIf                                   = $4049;
  CERR_InvalidReset                                = $404A;
  CERR_InvalidSet                                  = $404B;
  CERR_InvalidSave                                 = $404C;

  CERR_InvalidEthnologueCode                       = $404D;

  CERR_CannotCreateTempfile                        = $804E;

  CERR_90FeatureOnly_IfSystemStores                = $404F;
  CERR_IfSystemStore_NotFound                      = $4050;
  CERR_90FeatureOnly_SetSystemStores               = $4051;
  CERR_SetSystemStore_NotFound                     = $4052;
  CERR_90FeatureOnlyVirtualKeyDictionary           = $4053;

  CERR_NotSupportedInKeymanWebContext               = $4054;
  CERR_NotSupportedInKeymanWebOutput                = $4055;
  CERR_NotSupportedInKeymanWebStore                 = $4056;
  CERR_VirtualCharacterKeysNotSupportedInKeymanWeb  = $4057;
  CERR_VirtualKeysNotValidForMnemonicLayouts        = $4058;
  CERR_InvalidTouchLayoutFile                       = $4059;
  CERR_TouchLayoutInvalidIdentifier                 = $405A;   // I4142
  CERR_InvalidKeyCode                               = $405B;   // I4142

  CERR_90FeatureOnlyLayoutFile                     = $405C;
  CERR_90FeatureOnlyKeyboardVersion                = $405D;
  CERR_KeyboardVersionFormatInvalid                = $405E;
  CERR_ContextExHasInvalidOffset                   = $405F;
  CERR_90FeatureOnlyEmbedCSS                       = $4060;
  CERR_90FeatureOnlyTargets                        = $4061;
  CERR_ContextAndIndexInvalidInMatchNomatch        = $4062;
  CERR_140FeatureOnlyContextAndNotAnyWeb           = $4063;

  CERR_ExpansionMustFollowCharacterOrVKey          = $4064;
  CERR_VKeyExpansionMustBeFollowedByVKey           = $4065;
  CERR_CharacterExpansionMustBeFollowedByCharacter = $4066;
  CERR_VKeyExpansionMustUseConsistentShift         = $4067;
  CERR_ExpansionMustBePositive                     = $4068;

  CERR_CasedKeysMustContainOnlyVirtualKeys         = $4069;
  CERR_CasedKeysMustNotIncludeShiftStates          = $406A;
  CERR_CasedKeysNotSupportedWithMnemonicLayout     = $406B;

  CERR_CannotUseReadWriteGroupFromReadonlyGroup    = $406C;
  CERR_StatementNotPermittedInReadonlyGroup        = $406D;
  CERR_OutputInReadonlyGroup                       = $406E;
  CERR_NewContextGroupMustBeReadonly               = $406F;
  CERR_PostKeystrokeGroupMustBeReadonly            = $4070;

  CERR_DuplicateGroup                              = $4071;
  CERR_DuplicateStore                              = $4072;
  CERR_RepeatedBegin                               = $4073;
  CERR_VirtualKeyInContext                         = $4074;

  CWARN_TooManyWarnings                            = $2080;
  CWARN_OldVersion                                 = $2081;
  CWARN_BitmapNotUsed                              = $2082;
  CWARN_CustomLanguagesNotSupported                = $2083;
  CWARN_KeyBadLength                               = $2084;
  CWARN_IndexStoreShort                            = $2085;
  CWARN_UnicodeInANSIGroup                         = $2086;
  CWARN_ANSIInUnicodeGroup                         = $2087;
  CWARN_UnicodeSurrogateUsed                       = $2088;
  CWARN_ReservedCharacter                          = $2089;
  CWARN_Info                                       = $208A;
  CWARN_VirtualKeyWithMnemonicLayout               = $208B;
  CWARN_VirtualCharKeyWithPositionalLayout         = $208C;
  CWARN_StoreAlreadyUsedAsOptionOrCall             = $208D;
  CWARN_StoreAlreadyUsedAsStoreOrCall              = $208E;
  CWARN_StoreAlreadyUsedAsStoreOrOption            = $208F;

  CWARN_PunctuationInEthnologueCode                = $2090;

  CWARN_TouchLayoutMissingLayer                     = $2091;
  CWARN_TouchLayoutCustomKeyNotDefined              = $2092;
  CWARN_TouchLayoutMissingRequiredKeys              = $2093;
  CWARN_HelpFileMissing                             = $2094;
  CWARN_EmbedJsFileMissing                          = $2095;
  CWARN_TouchLayoutFileMissing                      = $2096;
  CWARN_VisualKeyboardFileMissing                   = $2097;
  CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb   = $2098;   // I4118
  CWARN_TouchLayoutUnidentifiedKey                  = $2099;   // I4142
  CHINT_UnreachableKeyCode                          = $109A;   // I4141

  CWARN_CouldNotCopyJsonFile                        = $209B;   // I4688
  CWARN_PlatformNotInTargets                        = $209C;

  CWARN_HeaderStatementIsDeprecated                 = $209D;
  CWARN_UseNotLastStatementInRule                   = $209E;

  CWARN_TouchLayoutFontShouldBeSameForAllPlatforms  = $209F;   // I4872
  CWARN_InvalidJSONMetadataFile                     = $20A0;   // I4872
  CWARN_JSONMetadataOSKFontShouldMatchTouchFont     = $20A1;   // I4872

  CWARN_DontMixChiralAndNonChiralModifiers          = $20A3;
  CWARN_MixingLeftAndRightModifiers                 = $20A4;

  CWARN_LanguageHeadersDeprecatedInKeyman10         = $20A5;

  CHINT_NonUnicodeFile                              = $10A6;

  CWARN_TooManyErrorsOrWarnings                     = $20A7;

  CWARN_HotkeyHasInvalidModifier                    = $20A8;

  CWARN_TouchLayoutSpecialLabelOnNormalKey          = $20A9;

  CWARN_OptionStoreNameInvalid                      = $20AA;

  CWARN_NulNotFirstStatementInContext               = $20AB;
  CWARN_IfShouldBeAtStartOfContext                  = $20AC;

  CWARN_KeyShouldIncludeNCaps                       = $20AD;

  CHINT_UnreachableRule                             = $10AE;

implementation

end.
