---
title: Compiler Messages Reference for @keymanapp/kmc-ldml
---

 Code | Identifier | Message
------|------------|---------
[KM00001](km00001) | `HINT_NormalizationDisabled` | normalization=disabled is not recommended\.
[KM00002](km00002) | `ERROR_InvalidLocale` | Invalid BCP 47 locale form '&lt;param&gt;'
[KM00003](km00003) | `ERROR_HardwareLayerHasTooManyRows` | 'hardware' layer has too many rows
[KM00004](km00004) | `ERROR_RowOnHardwareLayerHasTooManyKeys` | Row \#&lt;param&gt; on 'hardware' &lt;param&gt; layer for modifier none has too many keys
[KM00005](km00005) | `ERROR_KeyNotFoundInKeyBag` | Key '&lt;param&gt;' in position \#&lt;param&gt; on row \#&lt;param&gt; of layer &lt;param&gt;, form '&lt;param&gt;' not found in key bag
[KM00006](km00006) | `HINT_OneOrMoreRepeatedLocales` | After minimization, one or more locales is repeated and has been removed
[KM00007](km00007) | `ERROR_InvalidFile` | The source file has an invalid structure: &lt;param&gt;
[KM00008](km00008) | `HINT_LocaleIsNotMinimalAndClean` | Locale '&lt;param&gt;' is not minimal or correctly formatted and should be '&lt;param&gt;'
[KM00009](km00009) | `ERROR_InvalidScanCode` | Form '&lt;param&gt;' has invalid/unknown scancodes '&lt;param&gt;'
[KM0000A](km0000a) | `WARN_CustomForm` | Custom &lt;form id="&lt;param&gt;"&gt; element\. Key layout may not be as expected\.
[KM0000B](km0000b) | `ERROR_GestureKeyNotFoundInKeyBag` | Key '&lt;param&gt;' not found in key bag, referenced from other '&lt;param&gt;' in &lt;param&gt;
[KM0000D](km0000d) | `ERROR_InvalidVersion` | Version number '&lt;param&gt;' must be a semantic version format string\.
[KM0000E](km0000e) | `ERROR_MustBeAtLeastOneLayerElement` | The source file must contain at least one layer element\.
[KM00010](km00010) | `ERROR_DisplayIsRepeated` | display  has more than one display entry\.
[KM00011](km00011) | `ERROR_KeyMissingToGapOrSwitch` | key id='&lt;param&gt;' must have either output=, gap=, or layerId=\.
[KM00012](km00012) | `ERROR_ExcessHardware` | layers formId=&lt;param&gt;: Can only have one non\-'touch' element
[KM00013](km00013) | `ERROR_InvalidHardware` | layers has invalid value formId=&lt;param&gt;
[KM00014](km00014) | `ERROR_InvalidModifier` | layer has invalid modifiers='&lt;param&gt;' on layer id=&lt;param&gt;
[KM00015](km00015) | `ERROR_MissingFlicks` | key id=&lt;param&gt; refers to missing flickId=&lt;param&gt;
[KM00016](km00016) | `ERROR_DuplicateVariable` | duplicate variables: id=&lt;param&gt;
[KM00018](km00018) | `ERROR_InvalidTransformsType` | Invalid transforms types: '&lt;param&gt;'
[KM00019](km00019) | `ERROR_DuplicateTransformsType` | Duplicate transforms types: '&lt;param&gt;'
[KM0001A](km0001a) | `ERROR_MixedTransformGroup` | transformGroup cannot contain both reorder and transform elements
[KM0001B](km0001b) | `ERROR_EmptyTransformGroup` | transformGroup must have either reorder or transform elements
[KM0001C](km0001c) | `ERROR_MissingStringVariable` | Reference to undefined string variable: $\{&lt;param&gt;\}
[KM0001D](km0001d) | `ERROR_MissingSetVariable` | Reference to undefined set variable: $\[&lt;param&gt;\]
[KM0001E](km0001e) | `ERROR_MissingUnicodeSetVariable` | Reference to undefined UnicodeSet variable: $\[&lt;param&gt;\]
[KM0001F](km0001f) | `ERROR_NeedSpacesBetweenSetVariables` | Need spaces between set variables: &lt;param&gt;
[KM00020](km00020) | `ERROR_CantReferenceSetFromUnicodeSet` | Illegal use of set variable from within UnicodeSet: $\[&lt;param&gt;\]
[KM00021](km00021) | `ERROR_MissingMarkers` | Markers used for matching but not defined: &lt;param&gt;
[KM00022](km00022) | `ERROR_DisplayNeedsToOrId` | display  needs output= or keyId=, but not both
[KM00023](km00023) | `HINT_PUACharacters` | File contains &lt;param&gt; PUA character\(s\), including Illegal \(U\+NAN\)
[KM00024](km00024) | `WARN_UnassignedCharacters` | File contains &lt;param&gt; unassigned character\(s\), including Illegal \(U\+NAN\)
[KM00025](km00025) | `ERROR_IllegalCharacters` | File contains &lt;param&gt; illegal character\(s\), including Illegal \(U\+NAN\)
[KM00026](km00026) | `HINT_CharClassImplicitDenorm` | File has character classes which span non\-NFD character\(s\), including Illegal \(U\+NAN\)\. These will not match any text\.
[KM00027](km00027) | `WARN_CharClassExplicitDenorm` | File has character classes which include non\-NFD characters\(s\), including Illegal \(U\+NAN\)\. These will not match any text\.
[KM00028](km00028) | `ERROR_UnparseableReorderSet` | Illegal UnicodeSet "&lt;param&gt;" in reorder "&lt;param&gt;
[KM00030](km00030) | `ERROR_InvalidQuadEscape` | Invalid escape "\\u0000"\. Hint: Use "\\u\{&lt;param&gt;\}"
[KM00F00](km00f00) | `ERROR_UnparseableTransformFrom` | Invalid transform from="&lt;param&gt;": "&lt;param&gt;"
[KM00F01](km00f01) | `ERROR_IllegalTransformDollarsign` | Invalid transform from="&lt;param&gt;": Unescaped dollar\-sign \($\) is not valid transform syntax\.
[KM00F02](km00f02) | `ERROR_TransformFromMatchesNothing` | Invalid transfom from="&lt;param&gt;": Matches an empty string\.
[KM00F03](km00f03) | `ERROR_IllegalTransformPlus` | Invalid transform from="&lt;param&gt;": Unescaped plus \(\+\) is not valid transform syntax\.
[KM00F04](km00f04) | `ERROR_IllegalTransformAsterisk` | Invalid transform from="&lt;param&gt;": Unescaped asterisk \(\*\) is not valid transform syntax\.
