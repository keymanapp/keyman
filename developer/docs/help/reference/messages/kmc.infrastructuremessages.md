---
title: Compiler Messages Reference for @keymanapp/kmc
---

 Code | Identifier | Message
------|------------|---------
[KM05001](km05001) | `FATAL_UnexpectedException` | This is an internal error; the message will vary
[KM05002](km05002) | `INFO_BuildingFile` | Building &lt;param&gt;
[KM05003](km05003) | `ERROR_FileDoesNotExist` | File &lt;param&gt; does not exist
[KM05004](km05004) | `ERROR_FileTypeNotRecognized` | Unrecognised input file &lt;param&gt;, expecting &lt;param&gt;, or project folder
[KM05005](km05005) | `ERROR_OutFileNotValidForProjects` | \-\-out\-file should not be specified for project builds
[KM05006](km05006) | `INFO_FileBuiltSuccessfully` | &lt;param&gt; built successfully\.
[KM05007](km05007) | `INFO_FileNotBuiltSuccessfully` | &lt;param&gt; failed to build\.
[KM05008](km05008) | `ERROR_InvalidProjectFile` | Project file is not valid: &lt;param&gt;
[KM05009](km05009) | `HINT_FilenameHasDifferingCase` | File on disk '&lt;param&gt;' does not match case of '&lt;param&gt;' in source file; this is an error on platforms with case\-sensitive filesystems\.
[KM0500A](km0500a) | `ERROR_UnknownFileFormat` | Unknown file format &lt;param&gt;; only Markdown \(\.md\), JSON \(\.json\), and Text \(\.txt\) are supported\.
[KM0500B](km0500b) | `INFO_ProjectBuiltSuccessfully` | Project &lt;param&gt; built successfully\.
[KM0500C](km0500c) | `INFO_ProjectNotBuiltSuccessfully` | Project &lt;param&gt; failed to build\.
[KM0500D](km0500d) | `INFO_TooManyMessages` | More than &lt;param&gt; warnings or errors received; suppressing further messages\.
[KM0500E](km0500e) | `ERROR_FileTypeNotFound` | A file of type &lt;param&gt; was not found in the project\.
[KM0500F](km0500f) | `ERROR_NotAProjectFile` | File &lt;param&gt; must have a \.kpj extension to be treated as a project\.
[KM05010](km05010) | `INFO_WarningsHaveFailedBuild` | The build failed because option "treat warnings as errors" is enabled and there are one or more warnings\.
[KM05011](km05011) | `ERROR_CannotCreateFolder` | This is an internal error; the message will vary
[KM05012](km05012) | `ERROR_InvalidProjectFolder` | The folder &lt;param&gt; does not appear to be a Keyman Developer project\.
[KM05013](km05013) | `ERROR_UnsupportedProjectVersion` | Project version &lt;param&gt; is not supported by this version of Keyman Developer\.
[KM05014](km05014) | `HINT_ProjectIsVersion10` | The project file is an older version and can be upgraded to version 17\.0
[KM05015](km05015) | `ERROR_OutFileCanOnlyBeSpecifiedWithSingleInfile` | Parameter \-\-out\-file can only be used with a single input file\.
[KM05016](km05016) | `ERROR_InvalidMessageFormat` | Invalid parameter: \-\-message &lt;param&gt; must match format '\[KM\]\#\#\#\#\#\[:Disable\|Info\|Hint\|Warn\|Error\]'
[KM05017](km05017) | `ERROR_MessageNamespaceNotFound` | Invalid parameter: \-\-message &lt;param&gt; does not have a recognized namespace
[KM05018](km05018) | `ERROR_MessageCodeNotFound` | Invalid parameter: \-\-message undefined is not a recognized code
[KM05019](km05019) | `ERROR_MessageCannotBeCoerced` | Invalid parameter: \-\-message &lt;param&gt; is not of type 'info', 'hint' or 'warn', and cannot be coerced
[KM0501A](km0501a) | `ERROR_UnrecognizedMessageCode` | Invalid parameter: message identifier '&lt;param&gt;' must match format '\[KM\]\#\#\#\#\#'
[KM0501B](km0501b) | `ERROR_MustSpecifyMessageCode` | Must specify at least one message code or \-a for all messages
[KM0501C](km0501c) | `ERROR_MessagesCannotBeFilteredForMarkdownFormat` | Messages cannot be filtered for markdown format
[KM0501D](km0501d) | `ERROR_OutputPathMustBeSpecifiedForMarkdownFormat` | Output path must be specified with \-o for markdown output format
[KM0501E](km0501e) | `ERROR_OutputPathMustExistAndBeADirectory` | Output path &lt;param&gt; must exist and must be a folder
