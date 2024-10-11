---
title: kmc API Reference
---

kmc is implemented in Typescript as a set of compiler modules. Each module has a
public API, documented in README.md in the corresponding repository folder.

This section is intended for software developers who want to integrate kmc into
their own toolchains. For documentation on using `kmc`, see [kmc command line
compiler](../cli).

# Modules

Module Name                                      | NPM Package                                           | GitHub Source                      | Description
-------------------------------------------------|-------------------------------------------------------|------------------------------------|---------------
[kmc](../cli)                                    | [@keymanapp/kmc][kmc-npm]                             | [GitHub][kmc-github]               | The official command-line interface for all of the various compilers in Keyman Developer.
[kmc-analyze](../../api/kmc-analyze)             | [@keymanapp/kmc-analyze][kmc-analyze-npm]             | [GitHub][kmc-analyze-github]       | Provides Keyman keyboard analysis tools.
[kmc-keyboard-info](../../api/kmc-keyboard-info) | [@keymanapp/kmc-keyboard-info][kmc-keyboard-info-npm] | [GitHub][kmc-keyboard-info-github] | Builds a .keyboard_info file from a Keyman keyboard project.
[kmc-kmn](../../api/kmc-kmn)                     | [@keymanapp/kmc-kmn][kmc-kmn-npm]                     | [GitHub][kmc-kmn-github]           | Builds .kmn keyboards into .kmx binary keyboard files.
[kmc-ldml](../../api/kmc-ldml)                   | [@keymanapp/kmc-ldml][kmc-ldml-npm]                   | [GitHub][kmc-ldml-github]          | Builds LDML .xml keyboards into Keyman .kmx binary keyboard files.
[kmc-model](../../api/kmc-model)                 | [@keymanapp/kmc-model][kmc-model-npm]                 | [GitHub][kmc-model-github]         | Builds .model.ts lexical models into .model.js files.
[kmc-model-info](../../api/kmc-model-info)       | [@keymanapp/kmc-model-info][kmc-model-info-npm]       | [GitHub][kmc-model-info-github]    | Builds a .model_info file from a Keyman lexical model project.
[kmc-package](../../api/kmc-package)             | [@keymanapp/kmc-package][kmc-package-npm]             | [GitHub][kmc-package-github]       | Builds .kps Keyman package source files into binary .kmp Keyman package files.

[kmc-npm]: https://npmjs.com/package/@keymanapp/kmc
[kmc-analyze-npm]: https://npmjs.com/package/@keymanapp/kmc-analyze
[kmc-keyboard-info-npm]: https://npmjs.com/package/@keymanapp/kmc-keyboard-info
[kmc-kmn-npm]: https://npmjs.com/package/@keymanapp/kmc-kmn
[kmc-ldml-npm]: https://npmjs.com/package/@keymanapp/kmc-ldml
[kmc-model-npm]: https://npmjs.com/package/@keymanapp/kmc-model
[kmc-model-info-npm]: https://npmjs.com/package/@keymanapp/kmc-model-info
[kmc-package-npm]: https://npmjs.com/package/@keymanapp/kmc-package
[common-types-npm]: https://npmjs.com/package/@keymanapp/common-types

[kmc-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc
[kmc-analyze-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc-analyze
[kmc-keyboard-info-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc-keyboard-info
[kmc-kmn-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc-kmn
[kmc-ldml-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc-ldml
[kmc-model-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc-model
[kmc-model-info-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc-model-info
[kmc-package-github]: https://github.com/keymanapp/keyman/tree/master/developer/src/kmc-package
[common-types-github]: https://github.com/keymanapp/keyman/tree/master/common/web/types
