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
*/
   // I4061
#ifndef _COMPERR_H
#define _COMPERR_H

#define CERR_FATAL						0x00008000
#define CERR_ERROR						0x00004000
#define CERR_WARNING					0x00002000
#define CERR_MEMORY						0x00001000

#define CERR_None						0x00000000
#define CERR_EndOfFile					0x00000001

#define CERR_BadCallParams				0x00008002
#define CERR_CannotAllocateMemory		0x00009004
#define CERR_InfileNotExist				0x00008005
#define CERR_CannotCreateOutfile		0x00008006
#define CERR_UnableToWriteFully			0x00008007
#define CERR_CannotReadInfile			0x00008008
#define CERR_SomewhereIGotItWrong		0x00008009

#define CERR_InvalidToken				0x0000400A
#define CERR_InvalidBegin				0x0000400B
#define CERR_InvalidName				0x0000400C
#define CERR_InvalidVersion				0x0000400D
#define CERR_InvalidLanguageLine		0x0000400E
#define CERR_LayoutButNoLanguage		0x0000400F
#define CERR_InvalidLayoutLine			0x00004010
#define CERR_NoVersionLine				0x00004011
#define CERR_InvalidGroupLine			0x00004012
#define CERR_InvalidStoreLine			0x00004013
#define CERR_InvalidCodeInKeyPartOfRule	0x00004014
#define CERR_InvalidDeadkey				0x00004015
#define CERR_InvalidValue				0x00004016
#define CERR_ZeroLengthString			0x00004017
#define CERR_TooManyIndexToKeyRefs		0x00004018
#define CERR_UnterminatedString			0x00004019
#define CERR_StringInVirtualKeySection	0x0000401A
#define CERR_AnyInVirtualKeySection		0x0000401B
#define CERR_InvalidAny					0x0000401C
#define CERR_StoreDoesNotExist			0x0000401D
#define CERR_BeepInVirtualKeySection	0x0000401E
#define CERR_IndexInVirtualKeySection	0x0000401F
#define CERR_InvalidIndex				0x00004020
#define CERR_OutsInVirtualKeySection	0x00004021
#define CERR_InvalidOuts				0x00004022
#define CERR_ContextInVirtualKeySection	0x00004024
#define CERR_InvalidUse					0x00004025
#define CERR_GroupDoesNotExist			0x00004026
#define CERR_VirtualKeyNotAllowedHere	0x00004027
#define CERR_InvalidSwitch				0x00004028
#define CERR_NoTokensFound				0x00004029
#define CERR_InvalidLineContinuation	0x0000402A
#define CERR_LineTooLong				0x0000402B
#define CERR_InvalidCopyright			0x0000402C
#define CERR_CodeInvalidInThisSection	0x0000402D
#define CERR_InvalidMessage				0x0000402E
#define CERR_InvalidLanguageName		0x0000402F
#define CERR_InvalidBitmapLine			0x00004030
#define CERR_CannotReadBitmapFile		0x00004031
#define CERR_IndexDoesNotPointToAny		0x00004032
#define	CERR_ReservedCharacter			0x00004033
#define CERR_InvalidCharacter			0x00004034
#define CERR_InvalidCall				0x00004035
#define CERR_CallInVirtualKeySection	0x00004036
#define CERR_CodeInvalidInKeyStore		0x00004037
#define CERR_CannotLoadIncludeFile		0x00004038

#define CERR_60FeatureOnly_EthnologueCode			0x00004039
#define CERR_60FeatureOnly_MnemonicLayout			0x0000403A
#define CERR_60FeatureOnly_OldCharPosMatching		0x0000403B
#define CERR_60FeatureOnly_NamedCodes				0x0000403C
#define CERR_60FeatureOnly_Contextn					0x0000403D
#define CERR_501FeatureOnly_Call					0x0000403E

#define CERR_InvalidNamedCode						0x0000403F
#define CERR_InvalidSystemStore						0x00004040

#define CERR_CallIsProfessionalFeature				0x00004041
#define CERR_IncludeCodesIsProfessionalFeature		0x00004042
#define CERR_MnemonicLayoutIsProfessionalFeature 	0x00004043
#define CERR_60FeatureOnly_VirtualCharKey			0x00004044

#define CERR_VersionAlreadyIncluded					0x00004045

#define CERR_70FeatureOnly							0x00004046

#define CERR_80FeatureOnly        					0x00004047
#define CERR_InvalidInVirtualKeySection 			0x00004048
#define CERR_InvalidIf            					0x00004049
#define CERR_InvalidReset         					0x0000404A
#define CERR_InvalidSet           					0x0000404B
#define CERR_InvalidSave          					0x0000404C

#define CERR_InvalidEthnologueCode  				0x0000404D

#define CERR_CannotCreateTempfile 					0x0000804E

#define CERR_90FeatureOnly_IfSystemStores  			0x0000404F
#define CERR_IfSystemStore_NotFound        			0x00004050
#define CERR_90FeatureOnly_SetSystemStores 			0x00004051
#define CERR_SetSystemStore_NotFound       			0x00004052
#define CERR_90FeatureOnlyVirtualKeyDictionary  	0x00004053

#define CERR_NotSupportedInKeymanWeb				0x00004054
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

#define CWARN_TooManyWarnings						0x00002080
#define CWARN_OldVersion							0x00002081
//#define CWARN_HotkeyNotUsed						0x0000202F
#define CWARN_BitmapNotUsed							0x00002082
#define CWARN_CustomLanguagesNotSupported 			0x00002083
#define CWARN_KeyBadLength							0x00002084
#define CWARN_IndexStoreShort						0x00002085
#define CWARN_UnicodeInANSIGroup					0x00002086
#define CWARN_ANSIInUnicodeGroup					0x00002087
#define CWARN_UnicodeSurrogateUsed					0x00002088
#define CWARN_ReservedCharacter						0x00002089
#define CWARN_Info									0x0000208A
#define CWARN_VirtualKeyWithMnemonicLayout			0x0000208B
#define CWARN_VirtualCharKeyWithPositionalLayout	0x0000208C
#define CWARN_StoreAlreadyUsedAsOptionOrCall      	0x0000208D
#define CWARN_StoreAlreadyUsedAsStoreOrCall       	0x0000208E
#define CWARN_StoreAlreadyUsedAsStoreOrOption     	0x0000208F

#define CWARN_PunctuationInEthnologueCode         	0x00002090

#define CWARN_TouchLayoutMissingLayer          0x00002091
#define CWARN_TouchLayoutCustomKeyNotDefined   0x00002092
#define CWARN_TouchLayoutMissingRequiredKeys   0x00002093
#define CWARN_HelpFileMissing                  0x00002094
#define CWARN_EmbedJsFileMissing               0x00002095
#define CWARN_TouchLayoutFileMissing           0x00002096
#define CWARN_VisualKeyboardFileMissing        0x00002097
#define CWARN_ExtendedShiftFlagsNotSupportedInKeymanWeb	0x00002098   // I4118
#define CWARN_TouchLayoutUnidentifiedKey       0x00002099
#define CWARN_UnreachableKeyCode               0x0000209A

#define CWARN_CouldNotCopyJsonFile             0x0000209B
#define CWARN_PlatformNotInTargets             0x0000209C

#define CWARN_HeaderStatementIsDeprecated      0x0000209D
#define CWARN_UseNotLastStatementInRule        0x0000209E

#define CWARN_TouchLayoutFontShouldBeSameForAllPlatforms 0x0000209F
#define CWARN_InvalidJSONMetadataFile          0x000020A0
#define CWARN_JSONMetadataOSKFontShouldMatchTouchFont 0x000020A1
#define CWARN_KVKFileIsInSourceFormat          0x000020A2

#define CERR_BufferOverflow							0x000080C0
#define CERR_Break									0x000080C1

#endif	// _COMPERR_H
