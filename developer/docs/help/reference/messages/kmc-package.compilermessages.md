---
title: Compiler Messages Reference for @keymanapp/kmc-package
---

 Code | Identifier | Message
------|------------|---------
[KM04001](km04001) | `FATAL_UnexpectedException` | This is an internal error; the message will vary
[KM04002](km04002) | `WARN_AbsolutePath` | File &lt;param&gt; has an absolute path, which is not portable\.
[KM04003](km04003) | `ERROR_FileDoesNotExist` | File &lt;param&gt; does not exist\.
[KM04004](km04004) | `ERROR_FileCouldNotBeRead` | File &lt;param&gt; could not be read: unknown error\.
[KM04005](km04005) | `WARN_FileIsNotABinaryKvkFile` | File &lt;param&gt; does not appear to be a valid binary \.kvk file; this may be an old package that includes an xml\-format \.kvk file\. You must update the package to include the compiled \.kvk file in the package\.
[KM04006](km04006) | `ERROR_FollowKeyboardVersionNotAllowedForModelPackages` | FollowKeyboardVersion is not allowed in model packages
[KM04007](km04007) | `ERROR_FollowKeyboardVersionButNoKeyboards` | FollowKeyboardVersion is set, but the package contains no keyboards
[KM04008](km04008) | `ERROR_KeyboardContentFileNotFound` | Keyboard &lt;param&gt; was listed in &lt;Keyboards&gt; but a corresponding \.kmx file was not found in &lt;Files&gt;
[KM04009](km04009) | `ERROR_KeyboardFileNotValid` | Keyboard file &lt;param&gt; is not a valid \.kmx file: unknown error
[KM0400A](km0400a) | `INFO_KeyboardFileHasNoKeyboardVersion` | Keyboard file &lt;param&gt; has no &amp;KeyboardVersion store, using default '0\.0'
[KM0400B](km0400b) | `ERROR_PackageCannotContainBothModelsAndKeyboards` | The package contains both lexical models and keyboards, which is not permitted\.
[KM0400C](km0400c) | `HINT_PackageShouldNotRepeatLanguages` | Two language tags in &lt;param&gt; &lt;param&gt;, '&lt;param&gt;' and '&lt;param&gt;', reduce to the same minimal tag '&lt;param&gt;'\.
[KM0400D](km0400d) | `WARN_PackageNameDoesNotFollowLexicalModelConventions` | The package file &lt;param&gt; does not follow the recommended model filename conventions\. The name should be all lower case, include only alphanumeric characters and underscore \(\_\), not start with a digit, and should have the structure &lt;author&gt;\.&lt;bcp47&gt;\.&lt;uniq&gt;\.model\.kps\.
[KM0400E](km0400e) | `WARN_PackageNameDoesNotFollowKeyboardConventions` | The package file &lt;param&gt; does not follow the recommended keyboard filename conventions\. The name should be all lower case, include only alphanumeric characters and underscore \(\_\), and not start with a digit\.
[KM0400F](km0400f) | `WARN_FileInPackageDoesNotFollowFilenameConventions` | The file &lt;param&gt; does not follow the recommended filename conventions\. The extension should be all lower case, and the filename should include only alphanumeric characters, \-, \_, \+ and \.
[KM04010](km04010) | `ERROR_PackageNameCannotBeBlank` | Package name cannot be an empty string\.
[KM04011](km04011) | `ERROR_KeyboardFileNotFound` | Keyboard file &lt;param&gt; was not found\. Has it been compiled?
[KM04012](km04012) | `WARN_KeyboardVersionsDoNotMatch` | Keyboard &lt;param&gt; version &lt;param&gt; does not match keyboard &lt;param&gt; version &lt;param&gt;\.
[KM04014](km04014) | `ERROR_LanguageTagIsNotValid` | Language tag '&lt;param&gt;' in &lt;param&gt; &lt;param&gt; is invalid\.
[KM04015](km04015) | `HINT_LanguageTagIsNotMinimal` | Language tag '&lt;param&gt;' in &lt;param&gt; &lt;param&gt; is not minimal, and should be '&lt;param&gt;'\.
[KM04016](km04016) | `ERROR_ModelMustHaveAtLeastOneLanguage` | The lexical model &lt;param&gt; must have at least one language specified\.
[KM04017](km04017) | `WARN_RedistFileShouldNotBeInPackage` | The Keyman system file '&lt;param&gt;' should not be compiled into the package\.
[KM04018](km04018) | `WARN_DocFileDangerous` | Microsoft Word \.doc or \.docx files \('&lt;param&gt;'\) are not portable\. You should instead use HTML or PDF format\.
[KM04019](km04019) | `ERROR_PackageMustContainAModelOrAKeyboard` | Package must contain a lexical model or a keyboard\.
[KM0401A](km0401a) | `WARN_JsKeyboardFileIsMissing` | Keyboard &lt;param&gt; targets touch devices but corresponding &lt;param&gt;\.js file is not in the package\.
[KM0401B](km0401b) | `WARN_KeyboardShouldHaveAtLeastOneLanguage` | The keyboard &lt;param&gt; should have at least one language specified\.
[KM0401C](km0401c) | `HINT_JsKeyboardFileHasNoTouchTargets` | The keyboard &lt;param&gt; has been included for touch platforms, but does not include a touch layout\.
[KM0401D](km0401d) | `HINT_PackageContainsSourceFile` | The source file &lt;param&gt; should not be included in the package; instead include the compiled result\.
[KM0401E](km0401e) | `ERROR_InvalidPackageFile` | Package source file is invalid: unknown error
[KM0401F](km0401f) | `ERROR_FileRecordIsMissingName` | File record in the package with description 'undefined' is missing a filename\.
[KM04020](km04020) | `ERROR_InvalidAuthorEmail` | Invalid author email: &lt;param&gt;
[KM04021](km04021) | `ERROR_PackageFileHasEmptyVersion` | Package version is not following keyboard version, but the package version field is blank\.
