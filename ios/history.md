# Keyman for iPhone and iPad Version History

## 10.0 alpha
* Updated versioning scheme for uniformity across all Keyman products.

## 2017-08-26 2.6.4 stable
* Fixed bug with blank keyboard on some devices (#218)
* Fixed bug with keyboard width being incorrect on iPhone 7, iPhone 7+ (#224) 

## 2017-08-18 2.6.0 beta
* Numerous keyboarding bugfixes
* Replaced an outdated internal library

## 2017-02-21 2.5.2 stable
* Fixed bug with long-press keys not working on some newer iPhones

## 2017-02-09 2.5.1 stable
* Keyman is now distributed by SIL International

## 2016-10-14 2.4.2 stable
* Keyman Pro is renamed to Keyman and is now free!
* Separate free edition discontinued

## 2015-11-03 2.4.1 stable
* Now rotates correctly on iOS 9
* Optimized for iOS 9
* Fixed performance issues on iOS 8

## 2015-06-29 2.2.0 stable
* Faster load, keyboard switching and more responsive touches
* More stable, reduced memory requirements and addressed crashes
* Improved look and feel including smaller banner and improved long-press menus
* Smoother touch interactions and rapid touch interactions
* Handles touches just outside a key more intelligently
* Minor bug fixes and improvements

## 2015-01-27 2.1.0 stable
* Built-in browser enables language display where standard browsers do not (Keyman Pro)
* Other bug fixes & improvements

## 2.0.2 stable
* Fixes OSK display issues on iOS 6 & 7 (only Keyman Free supports iOS 6 & 7)
* Fixes Navigation bar background image for iPhone 6 & 6 Plus
* Fixes display of Get started & activity indicator for iPhone 6 Plus on landscape
* Now displays version and build number in info page
* Other bug fixes & improvements

## 2014-11-10 2.0.0 stable
### New Features
* Use any Keyman keyboard throughout your entire iOS 8 device
* Custom installable fonts with iOS 8
* Updated keyboard styling

### Bug Fixes
* Custom font issues experienced in iOS 7.1+ are resolved when you update to iOS 8

## 1.4.0 stable
### New Features

* iPhone & iPod Touch devices now show a key preview when a key is touched
* Installed keyboards now have keyboard version information and help link available in keyboard picker
* A-Z index in language/keyboard list is now enabled on iOS 7.1 and later versions
* Hebrew, Arabic and other right-to-left languages are now correctly displayed flowing right-to-left on iOS 7 and later
* European Latin keyboard no longer has desktop-based shortcuts enabled (e.g. .c no longer outputs ċ)
* Default European Latin keyboard upgraded to latest version (1.2)
* Keyboards can now be re-downloaded if the font fails to download
* Keyboard is now reloaded after updating/re-installing a custom keyboard to allow any changes to appear immediately
* Keyman help page link can now link to keyboard help
* Other minor bug fixes, a potential memory leak, and performance enhancements

### Known Issues

* iOS 7.1 has a problem with installed font profiles: once you restart your device, and in certain other situations, the installed font profiles become unavailable and language fonts will fail to display. While this is not a bug in Keyman as such, it can impact usage of your language in some apps. 
* We have received some reports of a bug with fonts in version 1.2: for some users, their language font fails to display after upgrading to version 1.2 from an earlier version. This is happening due to a change in the cached data for your language. To fix the problem on your iPhone or iPad: 
	* Open the Keyboards list by touching the globe button on the keyboard. 
	* Swipe left on the keyboard with the issue to delete it. 
	* Click the Add (+) button to reinstall the keyboard. You should not need to reinstall the font profile. 

## 1.3.0 stable
### New Features

* Enhancement: Keyboard is slightly taller on 3.5" and 4" devices to make it easier to use
* Bug fix: A slightly longer press on a key would sometimes fail to input the keystroke
* Default English keyboard is now enhanced for European language diacritics

### Bug Fixes

* Font display in iOS 7.1 and later is improved
* Egyptian Hieroglyphic keyboard works correctly
* Click sounds when typing now correspond precisely to each touch
* Some keyboards which displayed UTF-8 encoding bugs now load correctly.

### Known Issues

* iOS 7.1 has a problem with installed font profiles: once you restart your device, and in certain other situations, the installed font profiles become unavailable and language fonts will fail to display. While this is not a bug in Keyman as such, it can impact usage of your language in some apps. 
* We have received some reports of a bug with fonts in version 1.2: for some users, their language font fails to display after upgrading to version 1.2 from an earlier version. This is happening due to a change in the cached data for your language. To fix the problem on your iPhone or iPad: 
	* Open the Keyboards list by touching the globe button on the keyboard. 
	* Swipe left on the keyboard with the issue to delete it. 
	* Click the Add (+) button to reinstall the keyboard. You should not need to reinstall the font profile. 

## 1.2.0 stable
### New Features

* Install custom keyboards created with Keyman Developer 9 (free download for Windows) 
* User interface updated for iOS 7 
* Performance improvements 

### Bug Fixes

* Font display in iOS 7.1 and later is improved
* Egyptian Hieroglyphic keyboard works correctly
* Click sounds when typing now correspond precisely to each touch
* Some keyboards which displayed UTF-8 encoding bugs now load correctly.

### Known Issues

* iOS 7.1 has a problem with installed font profiles: once you restart your device, and in certain other situations, the installed font profiles become unavailable and language fonts will fail to display. While this is not a bug in Keyman as such, it can impact usage of your language in some apps. 
* We have received some reports of a bug with fonts in version 1.2: for some users, their language font fails to display after upgrading to version 1.2 from an earlier version. This is happening due to a change in the cached data for your language. To fix the problem on your iPhone or iPad: 
	* Open the Keyboards list by touching the globe button on the keyboard. 
	* Swipe left on the keyboard with the issue to delete it. 
	* Click the Add (+) button to reinstall the keyboard. You should not need to reinstall the font profile. 

## 1.1.0 stable
### New Features

* Font installation for your language for iOS 7+ 
* Keyman now opens in response to keyman://localhost/open links on websites 
* Text and text size are now saved and loaded back automatically after restarting the app. 

### Bug Fixes

* Copy/Paste no longer fails if the text contains apostrophe character 
* Fix for Javascript code injection
* Popup keys no longer appear unexpectedly after key touch 
* (>) button in keyboard list now operates 
* Action button popups no longer overlap the button on iPad
* Keyboard height now always cleanly matches available height in landscape mode
* Help bubble for keyboard change button now shows on first use
* Small message appended to mail and Facebook posts to help friends read text that may not display without fonts

## 1.0.0 stable
* First release!
