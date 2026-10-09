---
title: What's New in Keyman Engine 19.0 for Android
---

* Added API for selecting and controlling what types of suggestions are offered
for different `Editable` input types (#16644)
* **BREAKING** Apps that use the functionality of Keyman Engine for Android will have to add `androidx.webkit:webkit:1.14.0` as a dependency (#16146)
* **KNOWN ISSUE** `sil_euro_latin` keyboard must be included in an app if `setDefaultKeyboard()` is not called during initialization (#16215)
* Added [Intent `com.tavultesoft.kmapro.keyboard_changed`](KMAPro/) which is broadcast when Keyman system keyboard changes and includes font name (#15193)
* Added [`KMManager.getKeyboardHeightMax()`](KMManager/getKeyboardHeightMax) API (#13663)
* Added [`KMManager.getKeyboardHeightMin()`](KMManager/getKeyboardHeightMin) API (#13663)
* Clarified font filename vs facename in various APIs (#16211)

## See Also
* [Keyman Engine for Android Documentation](index)