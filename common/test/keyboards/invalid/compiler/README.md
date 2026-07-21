# **Tests for kmcompxtest()**
<br/>

This folder contains several kmn-files that are used for testing resulting error codes.

Most of these tests are copied from balochi_phonetic and contain alterations of the data to produce (at least) 1 error.
<br/>
<br/>

##  Naming Convention of kmn-files for using with kmcompxtest()

    CERR_404D_balochi_phonetic.kmn


The naming convention of the files is as follows:

* All Files **MUST** start with CERR_ followed by 4 characters and an underscore.

* After the second underscore all combination of char can be used.

* The 4 characters following CERR_ correspond to the last 4 digits of the error code which is expected to be produced by this file.

    ( e.g. CERR_404D_balochi_phonetic.kmn should produce error 0x0000404D).

<br/>
<br/>

While running, kmcompxtest extracts those 4 characters from the Filename (e.g. 404D for CERR_404D_balochi_phonetic) and compares those to the actual error found.

* If these 4 characters correspond to the actual error given, the test will be marked as OK. (Code 0)
<br/>
* If these 4 characters signify an error but **no error** does occur, the test will be marked as FAILED  (Code 1)
<br/>
* If these 4 characters signify an error but this **specific error** does not occur, the test will be marked as FAILED  (Code 4)
<br/>
* If these 4 characters signify an error number that is **not correct**, the test will be marked as FAILED  (Code 5)
<br/>
* If more than 1 error is produced, only the error coded in the Filename will be detected

<br/>
<br/>

### Tests are available for the following CERR_:
<font size="1">

|     | Error Number | CERR_
| :-: | :-----------:| :-------------------------------------
|     | 00000800     |  CERR_LEXICAL_MODEL_MIN
|     | 000008FF     |  CERR_LEXICAL_MODEL_MAX
|     | 00000800     |  CERR_LEXICAL_MODEL_MIN
|     | 000008FF     |  CERR_LEXICAL_MODEL_MAX
|     | 00000000     |  CERR_None
|     | 00000001     |  CERR_EndOfFile
|     | 00008002     |  CERR_BadCallParams
|     | 00008004     |  CERR_CannotAllocateMemory
|     | 00004005     |  CERR_InfileNotExist
|     | 00004006     |  // CERR_CannotCreateOutfile
|     | 00008007     |  CERR_UnableToWriteFully
|     | 00004008     |  CERR_CannotReadInfile
|     | 00008009     |  CERR_SomewhereIGotItWrong
|**X**| 0000400A     |  CERR_InvalidToken
|     | 0000400B     |  CERR_InvalidBegin
|     | 0000400C     |  CERR_InvalidName
|**X**| 0000400D     |  CERR_InvalidVersion
|**X**| 0000400E     |  CERR_InvalidLanguageLine
|     | 0000400F     |  CERR_LayoutButNoLanguage
|     | 00004010     |  CERR_InvalidLayoutLine
|     | 00004011     |  CERR_NoVersionLine
|     | 00004012     |  CERR_InvalidGroupLine
|**X**| 00004013     |  CERR_InvalidStoreLine
|     | 00004014     |  CERR_InvalidCodeInKeyPartOfRule
|**X**| 00004015     |  CERR_InvalidDeadkey
|**X**| 00004016     |  CERR_InvalidValue
|**X**| 00004017     |  CERR_ZeroLengthString
|     | 00004018     |  CERR_TooManyIndexToKeyRefs
|**X**| 00004019     |  CERR_UnterminatedString
|     | 0000401A     |  CERR_StringInVirtualKeySection
|     | 0000401B     |  CERR_AnyInVirtualKeySection
|     | 0000401C     |  CERR_InvalidAny
|**X**| 0000401D     |  CERR_StoreDoesNotExist
|     | 0000401E     |  CERR_BeepInVirtualKeySection
|     | 0000401F     |  CERR_IndexInVirtualKeySection
|     | 00004020     |  CERR_InvalidIndex
|     | 00004021     |  CERR_OutsInVirtualKeySection
|     | 00004022     |  CERR_InvalidOuts
|     | 00004024     |  CERR_ContextInVirtualKeySection
|     | 00004025     |  CERR_InvalidUse
|**X**| 00004026     |  CERR_GroupDoesNotExist
|     | 00004027     |  CERR_VirtualKeyNotAllowedHere
|     | 00004028     |  CERR_InvalidSwitch
|**X**| 00004029     |  CERR_NoTokensFound
|     | 0000402A     |  CERR_InvalidLineContinuation
|**X**| 0000402B     |  CERR_LineTooLong
|     | 0000402C     |  CERR_InvalidCopyright
|**X**| 0000402D     |  CERR_CodeInvalidInThisSection
|     | 0000402E     |  CERR_InvalidMessage
|     | 0000402F     |  CERR_InvalidLanguageName
|     | 00004030     |  CERR_InvalidBitmapLine
|**X**| 00004031     |  CERR_CannotReadBitmapFile
|     | 00004032     |  CERR_IndexDoesNotPointToAny
|     | 00004033     |  CERR_ReservedCharacter
|**X**| 00004034     |  CERR_InvalidCharacter
|     | 00004035     |  CERR_InvalidCall
|     | 00004036     |  CERR_CallInVirtualKeySection
|     | 00004037     |  CERR_CodeInvalidInKeyStore
|**X**| 00004038     |  CERR_CannotLoadIncludeFile
|**X**| 00004039     |  CERR_60FeatureOnly_EthnologueCode
|**X**| 0000403A     |  CERR_60FeatureOnly_MnemonicLayout
|     | 0000403B     |  CERR_60FeatureOnly_OldCharPosMatching
|     | 0000403C     |  CERR_60FeatureOnly_NamedCodes
|     | 0000403D     |  CERR_60FeatureOnly_Contextn
|**X**| 0000403E     |  CERR_501FeatureOnly_Call
|     | 0000403F     |  CERR_InvalidNamedCode
|**X**| 00004040     |  CERR_InvalidSystemStore
|**X**| 00004044     |  CERR_60FeatureOnly_VirtualCharKey
|**X**| 00004045     |  CERR_VersionAlreadyIncluded
|**X**| 00004046     |  CERR_70FeatureOnly
|**X**| 00004047     |  CERR_80FeatureOnly
|     | 00004048     |  CERR_InvalidInVirtualKeySection
|**X**| 00004049     |  CERR_InvalidIf
|**X**| 0000404A     |  CERR_InvalidReset
|**X**| 0000404B     |  CERR_InvalidSet
|**X**| 0000404C     |  CERR_InvalidSave
|**X**| 0000404D     |  CERR_InvalidEthnologueCode
|     | 0000804E     |  CERR_CannotCreateTempfile
|**X**| 0000404F     |  CERR_90FeatureOnly_IfSystemStores
|     | 00004050     |  CERR_IfSystemStore_NotFound
|     | 00004051     |  CERR_90FeatureOnly_SetSystemStores
|     | 00004052     |  CERR_SetSystemStore_NotFound
|     | 00004053     |  CERR_90FeatureOnlyVirtualKeyDictionary
|     | 00004054     |  CERR_NotSupportedInKeymanWebContext
|     | 00004055     |  CERR_NotSupportedInKeymanWebOutput
|     | 00004056     |  CERR_NotSupportedInKeymanWebStore
|     | 00004057     |  CERR_VirtualCharacterKeysNotSupportedInKeymanWeb
|     | 00004058     |  CERR_VirtualKeysNotValidForMnemonicLayouts
|     | 00004059     |  CERR_InvalidTouchLayoutFile
|     | 0000405A     |  CERR_TouchLayoutInvalidIdentifier
|     | 0000405B     |  CERR_InvalidKeyCode
|**X**| 0000405C     |  CERR_90FeatureOnlyLayoutFile
|**X**| 0000405D     |  CERR_90FeatureOnlyKeyboardVersion
|**X**| 0000405E     |  CERR_KeyboardVersionFormatInvalid
|     | 0000405F     |  CERR_ContextExHasInvalidOffset
|**X**| 00004060     |  CERR_90FeatureOnlyEmbedCSS
|**X**| 00004061     |  CERR_90FeatureOnlyTargets
|     | 00004062     |  CERR_ContextAndIndexInvalidInMatchNomatch
|     | 00004063     |  CERR_140FeatureOnlyContextAndNotAnyWeb
|**X**| 00004064     |  CERR_ExpansionMustFollowCharacterOrVKey
|**X**| 00004065     |  CERR_VKeyExpansionMustBeFollowedByVKey
|**X**| 00004066     |  CERR_CharacterExpansionMustBeFollowedByCharacter
|**X**| 00004067     |  CERR_VKeyExpansionMustUseConsistentShift
|**X**| 00004068     |  CERR_ExpansionMustBePositive
|**X**| 00004069     |  CERR_CasedKeysMustContainOnlyVirtualKeys
|**X**| 0000406A     |  CERR_CasedKeysMustNotIncludeShiftStates
|**X**| 0000406B     |  CERR_CasedKeysNotSupportedWithMnemonicLayout
|**X**| 0000406C     |  CERR_CannotUseReadWriteGroupFromReadonlyGroup
|     | 0000406D     |  CERR_StatementNotPermittedInReadonlyGroup
|**X**| 0000406E     |  CERR_OutputInReadonlyGroup
|**X**| 0000406F     |  CERR_NewContextGroupMustBeReadonly
|**X**| 00004070     |  CERR_PostKeystrokeGroupMustBeReadonly
|**X**| 00004071     |  CERR_DuplicateGroup
|**X**| 00004072     |  CERR_DuplicateStore
|**X**| 00004073     |  CERR_RepeatedBegin
|**X**| 000080C0     |  CERR_BufferOverflow
