---
title: What's new in Keyman Developer 18.0
---

Keyman Developer 18 has a number of significant changes:

* Updated to Unicode 16.0 (#12393)
* Improve automatic detection of minimum Keyman version for a keyboard during
  compilation (#11981, 311982, #11965, #11957)
* Generate keyboards and lexical models from templates, with `kmc generate`
  (#11014)
* Clone existing keyboard and lexical model projects, both from local file
  system and also from any open source online Keyman keyboard in Keyman Cloud or
  GitHub, with `kmc copy` and New Project dialogs (#12555, #12586, #13076)
* Support extending existing `&displaymap` data files when adding new characters
  (#12622)
* Font settings for on screen keyboards are now kept consistent with package
  metadata during compilation (#12949)
* New npm module @keymanapp/langtags makes langtags.json easily accessible
  (#13046)
* Compiler messages now have links to additional documentation (#13156)
