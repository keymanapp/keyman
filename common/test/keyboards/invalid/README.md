# **Tests for kmcompxtest()**
<br/>

This folder contains several kmn-files that are used for testing resulting Errorcodes.

Most of these tests are copied from balochi_phonetic and contain alterations of the data to produce (at least) 1 Error.
<br/>
<br/>

---

##  Naming Convention of kmn-files for using with kmcompxtest()

    CERR_404D_balochi_phonetic.kmn


The naming convention of the files is as follows:

* All Files **MUST** start with CERR_ followed by 4 characters and an underscore.

* After the second underscore all combination of char can be used.

* The 4 characters following CERR_ correspond to the last 4 digits of the Error-Code which is expected to be produced by this file.

    ( e.g. CERR_404D_balochi_phonetic.kmn should produce Error 0x0000404D).

<br/>

---
<br/>

While running kmcompxtest extracts the 4 chars from the Filename and compares them to the actual Error found.

* If they correspond the test will be marked as OK. (Code 0)

* If the file is supposed to produce an Error and does not the test will be marked as FAILED  (Code 1)

* If the filename is not correct the test will be marked as FAILED  (Code 1)

* If more than 1 Error is produced only the Error coded in the Filename will be detected


### Tests are available for the following CERR_:
  (&emsp;) &emsp; 00000800  &emsp; CERR_LEXICAL_MODEL_MIN
  (&emsp;) &emsp; 000008FF  &emsp; CERR_LEXICAL_MODEL_MAX
  (&emsp;) &emsp; 00000000  &emsp; CERR_None
  (&emsp;) &emsp; 00000001  &emsp; CERR_EndOfFile
  (&emsp;) &emsp; 00008002  &emsp; CERR_BadCallParams
  (&emsp;) &emsp; 00008004  &emsp; CERR_CannotAllocateMemory
  (&emsp;) &emsp; 00008005  &emsp; CERR_InfileNotExist
  (&emsp;) &emsp; 00008006  &emsp; CERR_CannotCreateOutfile
  (&emsp;) &emsp; 00008007  &emsp; CERR_UnableToWriteFully
  (&emsp;) &emsp; 00008008  &emsp; CERR_CannotReadInfile
  (&emsp;) &emsp; 00008009  &emsp; CERR_SomewhereIGotItWrong
  ( **x** ) &emsp; 0000400A  &emsp; CERR_InvalidToken
  (&emsp;) &emsp; 0000400B  &emsp; CERR_InvalidBegin
  (&emsp;) &emsp; 0000400C  &emsp; CERR_InvalidName
  ( **x** ) &emsp; 0000400D  &emsp; CERR_InvalidVersion
  ( **x** ) &emsp; 0000400E  &emsp; CERR_InvalidLanguageLine
  (&emsp;) &emsp; 0000400F  &emsp; CERR_LayoutButNoLanguage
  (&emsp;) &emsp; 00004010  &emsp; CERR_InvalidLayoutLine
  (&emsp;) &emsp; 00004011  &emsp; CERR_NoVersionLine
  (&emsp;) &emsp; 00004012  &emsp; CERR_InvalidGroupLine
  ( **x** ) &emsp; 00004013  &emsp; CERR_InvalidStoreLine
  (&emsp;) &emsp; 00004014  &emsp; CERR_InvalidCodeInKeyPartOfRule
  ( **x** ) &emsp; 00004015  &emsp; CERR_InvalidDeadkey
  ( **x** ) &emsp; 00004016  &emsp; CERR_InvalidValue
  ( **x** ) &emsp; 00004017  &emsp; CERR_ZeroLengthString
  (&emsp;) &emsp; 00004018  &emsp; CERR_TooManyIndexToKeyRefs
  ( **x** ) &emsp; 00004019  &emsp; CERR_UnterminatedString
  (&emsp;) &emsp; 0000401A  &emsp; CERR_StringInVirtualKeySection
  (&emsp;) &emsp; 0000401B  &emsp; CERR_AnyInVirtualKeySection
  (&emsp;) &emsp; 0000401C  &emsp; CERR_InvalidAny
  ( **x** ) &emsp; 0000401D  &emsp; CERR_StoreDoesNotExist
  (&emsp;) &emsp; 0000401E  &emsp; CERR_BeepInVirtualKeySection
  (&emsp;) &emsp; 0000401F  &emsp; CERR_IndexInVirtualKeySection
  (&emsp;) &emsp; 00004020  &emsp; CERR_InvalidIndex
  (&emsp;) &emsp; 00004021  &emsp; CERR_OutsInVirtualKeySection
  (&emsp;) &emsp; 00004022  &emsp; CERR_InvalidOuts
  (&emsp;) &emsp; 00004024  &emsp; CERR_ContextInVirtualKeySection
  (&emsp;) &emsp; 00004025  &emsp; CERR_InvalidUse
  ( **x** ) &emsp; 00004026  &emsp; CERR_GroupDoesNotExist
  (&emsp;) &emsp; 00004027  &emsp; CERR_VirtualKeyNotAllowedHere
  (&emsp;) &emsp; 00004028  &emsp; CERR_InvalidSwitch
  ( **x** ) &emsp; 00004029  &emsp; CERR_NoTokensFound
  (&emsp;) &emsp; 0000402A  &emsp; CERR_InvalidLineContinuation
  ( **x** ) &emsp; 0000402B  &emsp; CERR_LineTooLong
  (&emsp;) &emsp; 0000402C  &emsp; CERR_InvalidCopyright
  ( **x** ) &emsp; 0000402D  &emsp; CERR_CodeInvalidInThisSection
  (&emsp;) &emsp; 0000402E  &emsp; CERR_InvalidMessage
  (&emsp;) &emsp; 0000402F  &emsp; CERR_InvalidLanguageName
  (&emsp;) &emsp; 00004030  &emsp; CERR_InvalidBitmapLine
  ( **x** ) &emsp; 00004031  &emsp; CERR_CannotReadBitmapFile
  (&emsp;) &emsp; 00004032  &emsp; CERR_IndexDoesNotPointToAny
  (&emsp;) &emsp; 00004033  &emsp; CERR_ReservedCharacter
  ( **x** ) &emsp; 00004034  &emsp; CERR_InvalidCharacter
  (&emsp;) &emsp; 00004035  &emsp; CERR_InvalidCall
  (&emsp;) &emsp; 00004036  &emsp; CERR_CallInVirtualKeySection
  (&emsp;) &emsp; 00004037  &emsp; CERR_CodeInvalidInKeyStore
  ( **x** ) &emsp; 00004038  &emsp; CERR_CannotLoadIncludeFile
 ( **x** ) &emsp; 00004039  &emsp; CERR_60FeatureOnly_EthnologueCode
 ( **x** ) &emsp; 0000403A  &emsp; CERR_60FeatureOnly_MnemonicLayout
  (&emsp;) &emsp; 0000403B  &emsp; CERR_60FeatureOnly_OldCharPosMatching
  (&emsp;) &emsp; 0000403C  &emsp; CERR_60FeatureOnly_NamedCodes
  (&emsp;) &emsp; 0000403D  &emsp; CERR_60FeatureOnly_Contextn
  ( **x** ) &emsp; 0000403E  &emsp; CERR_501FeatureOnly_Call
  (&emsp;) &emsp; 0000403F  &emsp; CERR_InvalidNamedCode
  ( **x** ) &emsp; 00004040  &emsp; CERR_InvalidSystemStore
  (&emsp;) &emsp; 00004041  &emsp; CERR_CallIsProfessionalFeature
  (&emsp;) &emsp; 00004042  &emsp; CERR_IncludeCodesIsProfessionalFeature
  (&emsp;) &emsp; 00004043  &emsp; CERR_MnemonicLayoutIsProfessionalFeature
  ( **x** ) &emsp; 00004044  &emsp; CERR_60FeatureOnly_VirtualCharKey
  ( **x** ) &emsp; 00004045  &emsp; CERR_VersionAlreadyIncluded
  ( **x** ) &emsp; 00004046  &emsp; CERR_70FeatureOnly
  ( **x** ) &emsp; 00004047  &emsp; CERR_80FeatureOnly
  (&emsp;) &emsp; 00004048  &emsp; CERR_InvalidInVirtualKeySection
  ( **x** ) &emsp; 00004049  &emsp; CERR_InvalidIf
  ( **x** ) &emsp; 0000404A  &emsp; CERR_InvalidReset
  ( **x** ) &emsp; 0000404B  &emsp; CERR_InvalidSet
  ( **x** ) &emsp; 0000404C  &emsp; CERR_InvalidSave
  ( **x** ) &emsp; 0000404D  &emsp; CERR_InvalidEthnologueCode
  (&emsp;) &emsp; 0000804E  &emsp; CERR_CannotCreateTempfile
  ( **x** ) &emsp; 0000404F  &emsp; CERR_90FeatureOnly_IfSystemStores
  (&emsp;) &emsp; 00004050  &emsp; CERR_IfSystemStore_NotFound
  (&emsp;) &emsp; 00004051  &emsp; CERR_90FeatureOnly_SetSystemStores
  (&emsp;) &emsp; 00004052  &emsp; CERR_SetSystemStore_NotFound
  (&emsp;) &emsp; 00004053  &emsp; CERR_90FeatureOnlyVirtualKeyDictionary
  (&emsp;) &emsp; 00004054  &emsp; CERR_NotSupportedInKeymanWebContext
  (&emsp;) &emsp; 00004055  &emsp; CERR_NotSupportedInKeymanWebOutput
  (&emsp;) &emsp; 00004056  &emsp; CERR_NotSupportedInKeymanWebStore
  (&emsp;) &emsp; 00004057  &emsp; CERR_VirtualCharacterKeysNotSupportedInKeymanWeb
  (&emsp;) &emsp; 00004058  &emsp; CERR_VirtualKeysNotValidForMnemonicLayouts
  (&emsp;) &emsp; 00004059  &emsp; CERR_InvalidTouchLayoutFile
  (&emsp;) &emsp; 0000405A  &emsp; CERR_TouchLayoutInvalidIdentifier
  (&emsp;) &emsp; 0000405B  &emsp; CERR_InvalidKeyCode
  ( **x** ) &emsp; 0000405C  &emsp; CERR_90FeatureOnlyLayoutFile
  ( **x** ) &emsp; 0000405D  &emsp; CERR_90FeatureOnlyKeyboardVersion
  ( **x** ) &emsp; 0000405E  &emsp; CERR_KeyboardVersionFormatInvalid
  (&emsp;) &emsp; 0000405F  &emsp; CERR_ContextExHasInvalidOffset
  ( **x** ) &emsp; 00004060  &emsp; CERR_90FeatureOnlyEmbedCSS
  ( **x** ) &emsp; 00004061  &emsp; CERR_90FeatureOnlyTargets
  (&emsp;) &emsp; 00004062  &emsp; CERR_ContextAndIndexInvalidInMatchNomatch
  (&emsp;) &emsp; 00004063  &emsp; CERR_140FeatureOnlyContextAndNotAnyWeb
  ( **x** ) &emsp; 00004064  &emsp; CERR_ExpansionMustFollowCharacterOrVKey
  (&emsp;) &emsp; 00004065  &emsp; CERR_VKeyExpansionMustBeFollowedByVKey
  (&emsp;) &emsp; 00004066  &emsp; CERR_CharacterExpansionMustBeFollowedByCharacter
  (&emsp;) &emsp; 00004067  &emsp; CERR_VKeyExpansionMustUseConsistentShift
  (&emsp;) &emsp; 00004068  &emsp; CERR_ExpansionMustBePositive
  (&emsp;) &emsp; 00004069  &emsp; CERR_CasedKeysMustContainOnlyVirtualKeys
  (&emsp;) &emsp; 0000406A  &emsp; CERR_CasedKeysMustNotIncludeShiftStates
  (&emsp;) &emsp; 0000406B  &emsp; CERR_CasedKeysNotSupportedWithMnemonicLayout
  (&emsp;) &emsp; 0000406C  &emsp; CERR_CannotUseReadWriteGroupFromReadonlyGroup
  (&emsp;) &emsp; 0000406D  &emsp; CERR_StatementNotPermittedInReadonlyGroup
  (&emsp;) &emsp; 0000406E  &emsp; CERR_OutputInReadonlyGroup
  (&emsp;) &emsp; 0000406F  &emsp; CERR_NewContextGroupMustBeReadonly
  (&emsp;) &emsp; 00004070  &emsp; CERR_PostKeystrokeGroupMustBeReadonly
  ( **x** ) &emsp; 00004071  &emsp; CERR_DuplicateGroup
  ( **x** ) &emsp; 00004072  &emsp; CERR_DuplicateStore
  ( **x** ) &emsp; 00004073  &emsp; CERR_RepeatedBegin
