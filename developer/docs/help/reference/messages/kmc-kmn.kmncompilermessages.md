---
title: Compiler Messages Reference for @keymanapp/kmc-kmn
---

 Code | Identifier | Message
------|------------|---------
[KM02001](km02001) | `INFO_EndOfFile` | \(no error \- reserved code\)
[KM02002](km02002) | `FATAL_BadCallParams` | CompileKeyboardFile was called with bad parameters
[KM02004](km02004) | `FATAL_CannotAllocateMemory` | Out of memory
[KM02005](km02005) | `ERROR_InfileNotExist` | Cannot find the input file
[KM02007](km02007) | `FATAL_UnableToWriteFully` | Unable to write the file completely
[KM02008](km02008) | `ERROR_CannotReadInfile` | Cannot read the input file
[KM02009](km02009) | `FATAL_SomewhereIGotItWrong` | Internal error: contact Keyman
[KM0200A](km0200a) | `ERROR_InvalidToken` | Invalid token found
[KM0200B](km0200b) | `ERROR_InvalidBegin` | A "begin unicode" statement is required to compile a KeymanWeb keyboard
[KM0200C](km0200c) | `ERROR_InvalidName` | Invalid 'name' command
[KM0200D](km0200d) | `ERROR_InvalidVersion` | Invalid 'version' command
[KM0200E](km0200e) | `ERROR_InvalidLanguageLine` | Invalid 'language' command
[KM0200F](km0200f) | `ERROR_LayoutButNoLanguage` | Layout command found but no language command
[KM02010](km02010) | `ERROR_InvalidLayoutLine` | Invalid 'layout' command
[KM02011](km02011) | `ERROR_NoVersionLine` | No version line found for file
[KM02012](km02012) | `ERROR_InvalidGroupLine` | Invalid 'group' command
[KM02013](km02013) | `ERROR_InvalidStoreLine` | Invalid 'store' command
[KM02014](km02014) | `ERROR_InvalidCodeInKeyPartOfRule` | Invalid command or code found in key part of rule
[KM02015](km02015) | `ERROR_InvalidDeadkey` | Invalid 'deadkey' or 'dk' command
[KM02016](km02016) | `ERROR_InvalidValue` | Invalid value in extended string
[KM02017](km02017) | `ERROR_ZeroLengthString` | A string of zero characters was found
[KM02018](km02018) | `ERROR_TooManyIndexToKeyRefs` | Too many index commands refering to key string
[KM02019](km02019) | `ERROR_UnterminatedString` | Unterminated string in line
[KM0201A](km0201a) | `ERROR_StringInVirtualKeySection` | extend string illegal in virtual key section
[KM0201B](km0201b) | `ERROR_AnyInVirtualKeySection` | 'any' command is illegal in virtual key section
[KM0201C](km0201c) | `ERROR_InvalidAny` | Invalid 'any' command
[KM0201D](km0201d) | `ERROR_StoreDoesNotExist` | Store referenced does not exist
[KM0201E](km0201e) | `ERROR_BeepInVirtualKeySection` | 'beep' command is illegal in virtual key section
[KM0201F](km0201f) | `ERROR_IndexInVirtualKeySection` | 'index' command is illegal in virtual key section
[KM02020](km02020) | `ERROR_InvalidIndex` | Invalid 'index' command
[KM02021](km02021) | `ERROR_OutsInVirtualKeySection` | 'outs' command is illegal in virtual key section
[KM02022](km02022) | `ERROR_InvalidOuts` | Invalid 'outs' command
[KM02024](km02024) | `ERROR_ContextInVirtualKeySection` | 'context' command is illegal in virtual key section
[KM02025](km02025) | `ERROR_InvalidUse` | Invalid 'use' command
[KM02026](km02026) | `ERROR_GroupDoesNotExist` | Group does not exist
[KM02027](km02027) | `ERROR_VirtualKeyNotAllowedHere` | Virtual key is not allowed here
[KM02028](km02028) | `ERROR_InvalidSwitch` | Invalid 'switch' command
[KM02029](km02029) | `ERROR_NoTokensFound` | No tokens found in line
[KM0202A](km0202a) | `ERROR_InvalidLineContinuation` | Invalid line continuation
[KM0202B](km0202b) | `ERROR_LineTooLong` | Line too long
[KM0202C](km0202c) | `ERROR_InvalidCopyright` | Invalid 'copyright' command
[KM0202D](km0202d) | `ERROR_CodeInvalidInThisSection` | This line is invalid in this section of the file
[KM0202E](km0202e) | `ERROR_InvalidMessage` | Invalid 'message' command
[KM0202F](km0202f) | `ERROR_InvalidLanguageName` | Invalid 'languagename' command
[KM02030](km02030) | `ERROR_InvalidBitmapLine` | Invalid 'bitmaps' command
[KM02031](km02031) | `ERROR_CannotReadBitmapFile` | Cannot open the bitmap or icon file for reading
[KM02032](km02032) | `ERROR_IndexDoesNotPointToAny` | An index\(\) in the output does not have a corresponding any\(\) statement
[KM02033](km02033) | `ERROR_ReservedCharacter` | A reserved character was found
[KM02034](km02034) | `ERROR_InvalidCharacter` | A character was found that is outside the valid Unicode range \(U\+0000 \- U\+10FFFF\)
[KM02035](km02035) | `ERROR_InvalidCall` | The 'call' command is invalid
[KM02036](km02036) | `ERROR_CallInVirtualKeySection` | 'call' command is illegal in virtual key section
[KM02037](km02037) | `ERROR_CodeInvalidInKeyStore` | The command is invalid inside a store that is used in a key part of the rule
[KM02038](km02038) | `ERROR_CannotLoadIncludeFile` | Cannot load the included file: it is either invalid or does not exist
[KM02039](km02039) | `ERROR_60FeatureOnly_EthnologueCode` | EthnologueCode system store requires VERSION 6\.0 or higher
[KM0203A](km0203a) | `ERROR_60FeatureOnly_MnemonicLayout` | MnemonicLayout functionality requires VERSION 6\.0 or higher
[KM0203B](km0203b) | `ERROR_60FeatureOnly_OldCharPosMatching` | OldCharPosMatching system store requires VERSION 6\.0 or higher
[KM0203C](km0203c) | `ERROR_60FeatureOnly_NamedCodes` | Named character constants requires VERSION 6\.0 or higher
[KM0203D](km0203d) | `ERROR_60FeatureOnly_Contextn` | Context\(n\) requires VERSION 6\.0 or higher
[KM0203E](km0203e) | `ERROR_501FeatureOnly_Call` | Call\(\) requires VERSION 5\.01 or higher
[KM0203F](km0203f) | `ERROR_InvalidNamedCode` | Invalid named code constant
[KM02040](km02040) | `ERROR_InvalidSystemStore` | Invalid system store name found
[KM02044](km02044) | `ERROR_60FeatureOnly_VirtualCharKey` | Virtual character keys require VERSION 6\.0 or higher
[KM02045](km02045) | `ERROR_VersionAlreadyIncluded` | Only one VERSION or store\(version\) line allowed in a source file\.
[KM02046](km02046) | `ERROR_70FeatureOnly` | This feature requires store\(version\) '7\.0' or higher
[KM02047](km02047) | `ERROR_80FeatureOnly` | This feature requires store\(version\) '8\.0' or higher
[KM02048](km02048) | `ERROR_InvalidInVirtualKeySection` | This statement is not valid in a virtual key section
[KM02049](km02049) | `ERROR_InvalidIf` | The if\(\) statement is not valid
[KM0204A](km0204a) | `ERROR_InvalidReset` | The reset\(\) statement is not valid
[KM0204B](km0204b) | `ERROR_InvalidSet` | The set\(\) statement is not valid
[KM0204C](km0204c) | `ERROR_InvalidSave` | The save\(\) statement is not valid
[KM0204D](km0204d) | `ERROR_InvalidEthnologueCode` | Invalid ethnologuecode format
[KM0204E](km0204e) | `FATAL_CannotCreateTempfile` | Cannot create temp file
[KM0204F](km0204f) | `ERROR_90FeatureOnly_IfSystemStores` | if\(store\) requires store\(version\) '9\.0' or higher
[KM02050](km02050) | `ERROR_IfSystemStore_NotFound` | System store in if\(\) not found
[KM02051](km02051) | `ERROR_90FeatureOnly_SetSystemStores` | set\(store\) requires store\(version\) '9\.0' or higher
[KM02052](km02052) | `ERROR_SetSystemStore_NotFound` | System store in set\(\) not found
[KM02053](km02053) | `ERROR_90FeatureOnlyVirtualKeyDictionary` | Custom virtual key names require store\(version\) '9\.0'
[KM02054](km02054) | `ERROR_NotSupportedInKeymanWebContext` | Statement '&lt;param&gt;' is not currently supported in context for web and touch targets
[KM02055](km02055) | `ERROR_NotSupportedInKeymanWebOutput` | Statement '&lt;param&gt;' is not currently supported in output for web and touch targets
[KM02056](km02056) | `ERROR_NotSupportedInKeymanWebStore` | '&lt;param&gt;' is not currently supported in store '&lt;param&gt;' when used by any or index for web and touch targets
[KM02057](km02057) | `ERROR_VirtualCharacterKeysNotSupportedInKeymanWeb` | Virtual character keys not currently supported in KeymanWeb
[KM02058](km02058) | `ERROR_VirtualKeysNotValidForMnemonicLayouts` | Virtual keys are not valid for mnemonic layouts
[KM02059](km02059) | `ERROR_InvalidTouchLayoutFile` | Touch layout file &lt;param&gt; is not valid
[KM0205A](km0205a) | `ERROR_TouchLayoutInvalidIdentifier` | Key "&lt;param&gt;" on "&lt;param&gt;", layer "&lt;param&gt;" has an invalid identifier\.
[KM0205B](km0205b) | `ERROR_InvalidKeyCode` | Invalid key identifier "&lt;param&gt;"
[KM0205C](km0205c) | `ERROR_90FeatureOnlyLayoutFile` | Touch layout file reference requires store\(version\) '9\.0'or higher
[KM0205D](km0205d) | `ERROR_90FeatureOnlyKeyboardVersion` | KeyboardVersion system store requires store\(version\) '9\.0'or higher
[KM0205E](km0205e) | `ERROR_KeyboardVersionFormatInvalid` | KeyboardVersion format is invalid, expecting dot\-separated integers
[KM0205F](km0205f) | `ERROR_ContextExHasInvalidOffset` | context\(\) statement has offset out of range
[KM02060](km02060) | `ERROR_90FeatureOnlyEmbedCSS` | Embedding CSS requires store\(version\) '9\.0'or higher
[KM02061](km02061) | `ERROR_90FeatureOnlyTargets` | TARGETS system store requires store\(version\) '9\.0'or higher
[KM02062](km02062) | `ERROR_ContextAndIndexInvalidInMatchNomatch` | context and index statements cannot be used in a match or nomatch statement
[KM02063](km02063) | `ERROR_140FeatureOnlyContextAndNotAnyWeb` | For web and touch platforms, context\(\) statement referring to notany\(\) requires store\(version\) '14\.0'or higher
[KM02064](km02064) | `ERROR_ExpansionMustFollowCharacterOrVKey` | An expansion must follow a character or a virtual key
[KM02065](km02065) | `ERROR_VKeyExpansionMustBeFollowedByVKey` | A virtual key expansion must be terminated by a virtual key
[KM02066](km02066) | `ERROR_CharacterExpansionMustBeFollowedByCharacter` | A character expansion must be terminated by a character key
[KM02067](km02067) | `ERROR_VKeyExpansionMustUseConsistentShift` | A virtual key expansion must use the same shift state for both terminators
[KM02068](km02068) | `ERROR_ExpansionMustBePositive` | An expansion must have positive difference \(i\.e\. A\-Z, not Z\-A\)
[KM02069](km02069) | `ERROR_CasedKeysMustContainOnlyVirtualKeys` | The &amp;CasedKeys system store must contain only virtual keys or characters found on a US English keyboard
[KM0206A](km0206a) | `ERROR_CasedKeysMustNotIncludeShiftStates` | The &amp;CasedKeys system store must not include shift states
[KM0206B](km0206b) | `ERROR_CasedKeysNotSupportedWithMnemonicLayout` | The &amp;CasedKeys system store is not supported with mnemonic layouts
[KM0206C](km0206c) | `ERROR_CannotUseReadWriteGroupFromReadonlyGroup` | Group used from a readonly group must also be readonly
[KM0206D](km0206d) | `ERROR_StatementNotPermittedInReadonlyGroup` | Statement is not permitted in output of readonly group
[KM0206E](km0206e) | `ERROR_OutputInReadonlyGroup` | Output is not permitted in a readonly group
[KM0206F](km0206f) | `ERROR_NewContextGroupMustBeReadonly` | Group used in begin newContext must be readonly
[KM02070](km02070) | `ERROR_PostKeystrokeGroupMustBeReadonly` | Group used in begin postKeystroke must be readonly
[KM02071](km02071) | `ERROR_DuplicateGroup` | A group with this name has already been defined\.
[KM02072](km02072) | `ERROR_DuplicateStore` | A store with this name has already been defined\.
[KM02073](km02073) | `ERROR_RepeatedBegin` | Begin has already been set
[KM02074](km02074) | `ERROR_VirtualKeyInContext` | Virtual keys are not permitted in context
[KM02075](km02075) | `ERROR_OutsTooLong` | Store cannot be inserted with outs\(\) as it makes the extended string too long
[KM02076](km02076) | `ERROR_ExtendedStringTooLong` | Extended string is too long
[KM02077](km02077) | `ERROR_VirtualKeyExpansionTooLong` | Virtual key expansion is too large
[KM02078](km02078) | `ERROR_CharacterRangeTooLong` | Character range is too large and cannot be expanded
[KM02079](km02079) | `ERROR_NonBMPCharactersNotSupportedInKeySection` | Characters with codepoints over U\+FFFF are not supported in the key part of the rule
[KM02080](km02080) | `WARN_TooManyWarnings` | Too many warnings or errors
[KM02081](km02081) | `WARN_OldVersion` | The keyboard file is an old version
[KM02082](km02082) | `WARN_BitmapNotUsed` | The 'bitmaps' statement is obsolete and only the first bitmap referred to will be used, you should use 'bitmap'\.
[KM02083](km02083) | `WARN_CustomLanguagesNotSupported` | Languages over 0x1FF, 0x1F are not supported correctly by Windows\. You should use no LANGUAGE line instead\.
[KM02084](km02084) | `WARN_KeyBadLength` | There are too many characters in the keystroke part of the rule\.
[KM02085](km02085) | `WARN_IndexStoreShort` | The store referenced in index\(\) is shorter than the store referenced in any\(\)
[KM02086](km02086) | `WARN_UnicodeInANSIGroup` | A Unicode character was found in an ANSI group
[KM02087](km02087) | `WARN_ANSIInUnicodeGroup` | An ANSI character was found in a Unicode group
[KM02088](km02088) | `WARN_UnicodeSurrogateUsed` | A Unicode surrogate character was found\. You should use Unicode scalar values to represent values &gt; U\+FFFF
[KM02089](km02089) | `WARN_ReservedCharacter` | A Unicode character was found that should not be used
[KM0208A](km0208a) | `INFO_Info` | Information
[KM0208B](km0208b) | `WARN_VirtualKeyWithMnemonicLayout` | Virtual key used instead of virtual character key with a mnemonic layout
[KM0208C](km0208c) | `WARN_VirtualCharKeyWithPositionalLayout` | Virtual character key used with a positional layout instead of mnemonic layout
[KM0208D](km0208d) | `WARN_StoreAlreadyUsedAsOptionOrCall` | Store already used as an option or in a call statement and should not be used as a normal store
[KM0208E](km0208e) | `WARN_StoreAlreadyUsedAsStoreOrCall` | Store already used as a normal store or in a call statement and should not be used as an option
[KM0208F](km0208f) | `WARN_StoreAlreadyUsedAsStoreOrOption` | Store already used as a normal store or as an option and should not be used in a call statement
[KM02090](km02090) | `WARN_PunctuationInEthnologueCode` | Punctuation should not be used to separate Ethnologue codes; instead use spaces
[KM02091](km02091) | `WARN_TouchLayoutMissingLayer` | Key "&lt;param&gt;" on platform "&lt;param&gt;", layer "&lt;param&gt;", references a missing layer "&lt;param&gt;"
[KM02092](km02092) | `WARN_TouchLayoutCustomKeyNotDefined` | Key "&lt;param&gt;" on platform "&lt;param&gt;", layer "&lt;param&gt;", is a custom key but has no corresponding rule in the source\.
[KM02093](km02093) | `WARN_TouchLayoutMissingRequiredKeys` | Layer "&lt;param&gt;" on platform "&lt;param&gt;" is missing the required key\(s\) '&lt;param&gt;'\.
[KM02094](km02094) | `WARN_HelpFileMissing` | File &lt;param&gt; could not be loaded: 
[KM02095](km02095) | `WARN_EmbedJsFileMissing` | File &lt;param&gt; could not be loaded: 
[KM02098](km02098) | `WARN_ExtendedShiftFlagsNotSupportedInKeymanWeb` | Extended shift flags &lt;param&gt; are not supported in KeymanWeb
[KM02099](km02099) | `WARN_TouchLayoutUnidentifiedKey` | A key on layer "&lt;param&gt;" has no identifier\.
[KM0209A](km0209a) | `HINT_UnreachableKeyCode` | The rule will never be matched for key &lt;param&gt; because its key code is never fired\.
[KM0209C](km0209c) | `WARN_PlatformNotInTargets` | The specified platform is not a target platform
[KM0209D](km0209d) | `WARN_HeaderStatementIsDeprecated` | Header statements are deprecated; use instead the equivalent system store
[KM0209E](km0209e) | `WARN_UseNotLastStatementInRule` | A rule with use\(\) statements in the output should not have other content following the use\(\) statements
[KM0209F](km0209f) | `WARN_TouchLayoutFontShouldBeSameForAllPlatforms` | The touch layout font should be the same for all platforms\.
[KM020A2](km020a2) | `WARN_KVKFileIsInSourceFormat` | \.kvk file should be binary but is an XML file
[KM020A3](km020a3) | `WARN_DontMixChiralAndNonChiralModifiers` | This keyboard contains Ctrl,Alt and LCtrl,LAlt,RCtrl,RAlt sets of modifiers\. Use only one or the other set for web target\.
[KM020A4](km020a4) | `WARN_MixingLeftAndRightModifiers` | Left and right modifiers should not both be used in the same rule
[KM020A5](km020a5) | `WARN_LanguageHeadersDeprecatedInKeyman10` | This language header has been deprecated in Keyman 10\. Instead, add language metadata in the package file
[KM020A6](km020a6) | `HINT_NonUnicodeFile` | Keyman Developer has detected that the file has ANSI encoding\. Consider converting this file to UTF\-8
[KM020A8](km020a8) | `WARN_HotkeyHasInvalidModifier` | Hotkey has modifiers that are not supported\. Use only SHIFT, CTRL and ALT
[KM020A9](km020a9) | `WARN_TouchLayoutSpecialLabelOnNormalKey` | Key "&lt;param&gt;" on platform "&lt;param&gt;", layer "&lt;param&gt;" does not have the key type "Special" or "Special \(active\)" but has the label "&lt;param&gt;"\. This feature is only supported in Keyman 14 or later
[KM020AA](km020aa) | `WARN_OptionStoreNameInvalid` | The option store &lt;param&gt; should be named with characters in the range A\-Z, a\-z, 0\-9 and \_ only\.
[KM020AB](km020ab) | `WARN_NulNotFirstStatementInContext` | nul must be the first statement in the context
[KM020AC](km020ac) | `WARN_IfShouldBeAtStartOfContext` | if, platform and baselayout should be at start of context \(after nul, if present\)
[KM020AD](km020ad) | `WARN_KeyShouldIncludeNCaps` | Other rules which reference this key include CAPS or NCAPS modifiers, so this rule must include NCAPS modifier to avoid inconsistent matches
[KM020AE](km020ae) | `HINT_UnreachableRule` | This rule will never be matched as another rule takes precedence
[KM020AF](km020af) | `WARN_VirtualKeyInOutput` | Virtual keys are not supported in output
[KM020C0](km020c0) | `FATAL_BufferOverflow` | The compiler memory buffer overflowed
[KM020C1](km020c1) | `FATAL_Break` | Compiler interrupted by user
[KM02900](km02900) | `FATAL_UnexpectedException` | This is an internal error; the message will vary
[KM02901](km02901) | `FATAL_MissingWasmModule` | This is an internal error; the message will vary
[KM02903](km02903) | `FATAL_CallbacksNotSet` | This is an internal error; the message will vary
[KM02904](km02904) | `FATAL_UnicodeSetOutOfRange` | This is an internal error; the message will vary
[KM02905](km02905) | `ERROR_UnicodeSetHasStrings` | uset contains strings, not allowed
[KM02906](km02906) | `ERROR_UnicodeSetHasProperties` | uset contains properties, not allowed
[KM02907](km02907) | `ERROR_UnicodeSetSyntaxError` | uset had a Syntax Error while parsing
[KM02908](km02908) | `ERROR_InvalidKvksFile` | Error encountered parsing &lt;param&gt;: unknown error
[KM02909](km02909) | `WARN_InvalidVkeyInKvksFile` | Invalid virtual key &lt;param&gt; found in &lt;param&gt;
[KM0290A](km0290a) | `ERROR_InvalidDisplayMapFile` | Error encountered parsing display map &lt;param&gt;: unknown error
[KM0290B](km0290b) | `ERROR_InvalidKvkFile` | Error encountered loading &lt;param&gt;: unknown error
[KM0290C](km0290c) | `ERROR_FileNotFound` | File &lt;param&gt; was not found
