//
//  KMManager.h
//  Keyman Engine : Build 2.4.118
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//
//
//	A one-stop hub for:
//     - global settings and behaviour for KeymanTextView and KeymanTextField
//     - displaying the keyboard picker/downloader
//     - all interaction with the Keyman servers (polling for info, downloading keyboards, etc)
//
//	
//	This manager should be setup when the app first launches.
//
//  Example 1 (bare minimum):
//
//      [KMManager sharedInstance];
//
//
//  Example 2:
//	
//		[[KMManager sharedInstance] setDebugPrintingOn:YES];
//      NSString *kbID = @"european2";
//      NSString *langID = @"eng";
//      NSString *kbName = @"EuroLatin2 Keyboard";
//      NSString *langName = @"English";
//      NSString *kbFont = @"DejaVuSans.ttf";
//      [[KMManager sharedInstance] setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:kbFont, oskFont:nil];
//
//
//  NOTES: 1) The easiest way to use this SDK is to simply use KeymanTextViews and KeymanTextFields in place of their UIKit counterparts
//         2) All keyboard downloading and picking can be handled by globe key on the keyboard, displaying a KMKeyboardPickerButton or a
//            KMKeyboardPickerBarButtonItem
//             - when tapped, these will display a UI for the user to navigate all available Keyman keyboards
//         3) Only use the more advance functionality of KMManager if you're comfortable with asynchronous
//            calls and the associated error handling

#import <Foundation/Foundation.h>
#import "CWLSynthesizeSingleton.h"
#import <WebKit/WebKit.h>

// Subscribe to these notifications to handle your app's behaviour after a Keyman event
extern NSString *const kKeymanLanguagesUpdatedNotification;
extern NSString *const kKeymanLanguagesDownloadFailedNotification;
extern NSString *const kKeymanKeyboardDownloadStartedNotification;
extern NSString *const kKeymanKeyboardDownloadCompletedNotification;
extern NSString *const kKeymanKeyboardDownloadFailedNotification;
extern NSString *const kKeymanKeyboardChangedNotification;
extern NSString *const kKeymanKeyboardLoadedNotification;
extern NSString *const kKeymanKeyboardPickerDismissedNotification;
extern NSString *const kKeymanKeyboardRemovedNotification; // probably won't need this! *To be deleted*
extern NSString *const kKeymanSubKeysMenuWillShowNotification;
extern NSString *const kKeymanSubKeysMenuDismissedNotification;
extern NSString *const kKeymanDebugLogNotification;

// Common dictionary keys when handling language/keyboard info
extern NSString *const kKeymanIdKey;
extern NSString *const kKeymanNameKey;
extern NSString *const kKeymanKeyboardIdKey; // If there is no value for this key, revert to kKeymanIdKey
extern NSString *const kKeymanLanguageIdKey; // If there is no value for this key, revert to kKeymanIdKey
extern NSString *const kKeymanKeyboardNameKey; // If there is no value for this key, revert to kKeymanNameKey
extern NSString *const kKeymanLanguageNameKey; // If there is no value for this key, revert to kKeymanNameKey
extern NSString *const kKeymanKeyboardVersionKey; // If there is no value for this key, revert to kKeymanNameKey
extern NSString *const kKeymanKeyboardRTLKey;
extern NSString *const kKeymanLanguageKeyboardsKey;
extern NSString *const kKeymanKeyboardFileSizeKey;
extern NSString *const kKeymanKeyboardGZipFileSizeKey;	// If there is no value for this key, revert to kKeymanKeyboardFileSizeKey
extern NSString *const kKeymanFontKey;
extern NSString *const kKeymanOskFontKey;
extern NSString *const kKeymanFontFamilyKey;
extern NSString *const kKeymanFontSourceKey;
extern NSString *const kKeymanFontFilesKey;
extern NSString *const kKeymanFontNameKey;
extern NSString *const kKeymanFontRegisteredKey;
extern NSString *const kKeymanKeyboardFilenameKey;
extern NSString *const kKeymanKeyboardModifiedKey;
extern NSString *const kKeymanKeyboardInfoKey;
extern NSString *const kKeymanCustomKeyboardKey;

// Default keyboard Info
extern NSString *const kKeymanDefaultKeyboardID;
extern NSString *const kKeymanDefaultKeyboardName;
extern NSString *const kKeymanDefaultLanguageID;
extern NSString *const kKeymanDefaultLanguageName;
extern NSString *const kKeymanDefaultKeyboardRTL;
extern NSString *const kKeymanDefaultKeyboardFont;

// Common User defaults keys
extern NSString *const kKeymanUserKeyboardsListKey; // stores array of user keyboards info list > (NSArray *)
extern NSString *const kKeymanUserCurrentKeyboardKey; // stores currently/last selected keyboard info > (NSDictionary *)

// Possible states that a keyboard can be in
typedef enum {
	kKMKeyboardStateNeedsDownload,
	kKMKeyboardStateNeedsUpdate,
	kKMKeyboardStateUpToDate,
	kKMKeyboardStateDownloading,
	kKMKeyboardStateNone
} eKMKeyboardState;

@class Reachability;
@class HTTPDownloader;
@class HTTPDownloadRequest;

@interface KMManager : NSObject <UIWebViewDelegate, WKNavigationDelegate, WKScriptMessageHandler> {
	BOOL debugPrintingOn_;
    NSString *languageID_;
	NSString *keyboardID_;
	NSArray *languages_;
	NSMutableDictionary *keyboardsInfo_;
    NSMutableDictionary *keyboardsDictionary_;
    NSMutableDictionary *keymanFonts_;
	
	id __weak webDelegate_;
  id __weak inputDelegate_;
  HTTPDownloader *downloadQueue_;
  HTTPDownloader *sharedQueue_;
  HTTPDownloadRequest *currentRequest_;
	Reachability *reachability_;
}

// Set this to YES to have debug statements printed to the console
//   - defaults to NO
@property (nonatomic, assign) BOOL debugPrintingOn;

// Set this to NO to disable help bubbles on first use
//  - default value is YES
@property (nonatomic, assign) BOOL keymanHelpOn;

// Set this to NO to prevent users adding new keyboards in the keyboard picker
//  - default value is YES
//  - setting this to NO will also disable keyboard removal, to enable keyboard removal you should set canRemoveKeyboards to YES
//  - if set to NO, calling fetchKeyboardList is unnecessary and should be avoided unless you want to use auto keyboard update check feature of the keyboard picker
@property (nonatomic, assign) BOOL canAddNewKeyboards;

// Set this to NO to prevent users removing keyboards in the keyboard picker
//  - default value is YES
//  - the default keyboard in the keyboard picker cannot be removed even if this is set to YES
//  see canRemoveDefaultKeyboard to change this behaviour
@property (nonatomic, assign) BOOL canRemoveKeyboards;

// Set this to YES to allow users to remove the default keyboard from the keyboards list
// The last keyboard cannot be removed, so the default keyboard cannot be removed if it
// is the only keyboard in the list, even if this is set to YES
//  - default value is NO
@property (nonatomic, assign) BOOL canRemoveDefaultKeyboard;

// the list of Keyman languages (once they've been fetched)
//   - each language is an NSDictionary with a name, id, and a list of keyboards
//   - each keyboard is itself an NSDictionary with id, name, etc
//   - all you should ever require from this list are names and IDs
//   - if you find yourself using other info (like the URI), there is probably a KMManager method that does what you want already
//   - this list won't be available until the kKeymanLanguagesUpdatedNotification has been broadcasted
@property (nonatomic, strong, readonly) NSArray *languages;

// dictionary of Keyman keyboards to store language and keyboard names etc.
//   - key format: languageID_keyboardID i.e. eng_european2 returns the dictionary for English EuroLatin2 Keyboard
@property (nonatomic, strong, readonly) NSDictionary *keyboardsDictionary;

// dictionary of available Keyman keyboard fonts
@property (nonatomic, strong, readonly) NSDictionary *keymanFonts;

// Set application group identifier for shared container
+ (void)setApplicationGroupIdentifier:(NSString *)groupID;

// Returns the keyboard height
+ (CGFloat)keyboardHeight;

// Set keyboard with keyboardID and languageID
//   - this method looks up some values in language/keyboard list, so it should be available (see fetchKeyboardsList) in order to function correctly
//   - keyboard must be downloaded (see downloadKeyboardWithID)
- (BOOL)setKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID;

// Set keyboard with keyboardID, languageID, keyboardName and languageName
//   - use this method to set custom keyboard name and language name which are displayed on the space bar key
//   - this method looks up some values in language/keyboard list, so it should be available (see fetchKeyboardsList) in order to function correctly
//   - keyboard must be downloaded (see downloadKeyboardWithID)
//   - if the keyboardName or languageName is nil, it will call setKeyboardWithID without keyboardName and languageName, which will try to find those values in language/keyboard list if it has been fetched
- (BOOL)setKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID keyboardName:(NSString *)kbName languageName:(NSString *)langName;

// Set keyboard with keyboardID, languageID, keyboardName, languageName and fontName
//   - use this method to set custom keyboard name and language name which are displayed on the space bar key, and load a font for the keyboard
//   - this method doesn't look up any values in language/keyboard list, so it is safe to use without fetching the list
//   - keyboard must be downloaded (see downloadKeyboardWithID) or preloaded (see preloadLanguageFileAtPath)
//   - font should be JSON font object as string (see keyboardsDictionary) or the file name of the font which you have preloaded (see preloadFontFileAtPath)
//     or empty string if the keyboard doesn't have a custom font.
//   - if the font and the oskFont are both nil, it will call setKeyboardWithID without these parameters, which will try to find the font & the oskFont in language/keyboard list if it has been
//     fetched
- (BOOL)setKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID keyboardName:(NSString *)keyboardName languageName:(NSString *)languageName font:(NSString *)font oskFont:(NSString *)oskFont;

// Displays a list of available keyboards and allows a user to add/download new keyboards or remove existing ones
//   - it is recommended that you pass in your current UIViewController rather than the navigation controller
//   - to enable/disable the keyboard picker on the keyboard, see KeymanTextView/KeymanTextField class reference
- (void)showKeyboardPickerInViewController:(UIViewController *)viewController shouldAddKeyboard:(BOOL)shouldAddKeyboard;
- (void)dismissKeyboardPicker:(UIViewController *)viewController;

// Asynchronously fetches the dictionary of possible languages/keyboards to be displayed in the keyboard picker
//   - If not called before the picker is shown, the dictionary will be fetch automatically
//   - This method allows you to fetch the info in advance at a time that's appropriate for your app
//   - See the list of notifications at the top of this file to handle download failure/completion
//   - To save bandwidth, a cached version is used if:
//       a) the Keyman server is unreachable
//       b) the list has been recently fetched
- (void)fetchKeyboardsList;

// Returns YES if the keyboard exists in user keyboards list or NO otherwise
- (BOOL)userKeyboardExistsWithID:(NSString *)kbID languageID:(NSString *)langID;

// Returns the index of the keyboard if it exist in user keyboards list or -1 otherwise
- (NSInteger)indexForUserKeyboardWithID:(NSString *)kbID languageID:(NSString *)langID;

// Returns the current keyboard info
- (NSDictionary *)currentKeyboardInfo;

// Adds a new keyboard to the list in the keyboard picker if it doesn't already exist
//   - keyboard must be downloaded (see downloadKeyboardWithID) or preloaded (see preloadLanguageFileAtPath)
//   - isRTL should be YES if the writing direction of the language is right to left
//   - isCustom should be YES if the keyboard is not provided by Keyman
//   - font should be JSON font object as string (see keyboardsDictionary) or nil if the keyboard doesn't have a custom font.
- (void)addKeyboardWithID:(NSString *)kbID languageID:(NSString *)langID keyboardName:(NSString *)kbName languageName:(NSString *)langName isRTL:(BOOL)isRTL isCustom:(BOOL)isCustom font:(NSString *)font oskFont:(NSString *)oskFont;

// Removes a keyboard from the list in the keyboard picker if it exists
- (void)removeKeyboardWithID:(NSString *)kbID languageID:(NSString *)langID;

// Removes the keyboard at index from the keyboards list
- (BOOL)removeKeyboardAtIndex:(NSUInteger)index;

// Asynchronously fetches the .js file for the keyboard with given IDs.
//   - See the list of notifications at the top of this file to handle download failure/completion
//   - Set isUpdate to YES to update an existing keyboard, setting this to NO will delete the keyboard files on failure
- (void)downloadKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID isUpdate:(BOOL)isUpdate;

// Downloads a custom keyboard from the URL
//   - URL must be a link to JSON file
//   - isDirect should normally set to NO to let Keyman download the keyboard via Keyman Engine Cloud Services, in order to permit caching and prevent overloading a
//     target server. If you are hosting a keyboard file on an intranet, you can set isDirect to YES to download the keyboard directly.
- (void)downloadKeyboardFromUrl:(NSURL *)jsonUrl isDirect:(BOOL)isDirect;

// Downloads a custom keyboard from dictionary
//   - Dictionary must be a JSON dictionary which is usually fetched from a JSON file
- (void)downloadKeyboardFromDictionary:(NSDictionary *)jsonDict;

// The current state for a keyboard
//   - Used to determine if a keyboard is downloaded or not, amongst other things
- (eKMKeyboardState)stateForKeyboardWithID:(NSString *)keyboardID;

// Preloads a .js file for a language so that this keyboard is available to the user without downloading it
//   - the .js filename must remain the same as when you obtained it from Keyman
//   - to use this, you must include the .js file in your app
- (void)preloadLanguageFileAtPath:(NSString *)languagePath shouldOverwrite:(BOOL)shouldOverwrite;

// Preloads a .ttf or .otf file for a language so that this font is available to the user without downloading it
//   - to use this, you must include the font file in your app
- (void)preloadFontFileAtPath:(NSString *)fontPath shouldOverwrite:(BOOL)shouldOverwrite;

// Registers all new fonts found in the font path
//   - call this after you have preloaded all your font files with preloadFontFileAtPath
//
- (void)registerCustomFonts;

// Unregisters all registered fonts in the font path
- (void)unregisterCustomFonts;

// Returns the font name for the given keyboard ID and languageID, or returns nil if;
//   - keyboard doesn't have a font
//   - keyboard info is not available in user keyboards list or in keyboardsDictionary.
- (NSString *)fontNameForKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID;

// Returns the OSK font name for the given keyboard ID and languageID, or returns nil if;
//   - keyboard doesn't have an OSK font
//   - keyboard info is not available in user keyboards list or in keyboardsDictionary.
- (NSString *)oskFontNameForKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID;

// Returns the version of the Keyman SDK you're currently using.
- (NSString *)sdkVersion;

// Facilitates KeymanEngine internal logging.
- (void)KMLog:(NSString *)logStr checkDebugPrinting:(BOOL)checkDebugPrinting;

CWL_DECLARE_SINGLETON_FOR_CLASS_WITH_ACCESSOR(KMManager, sharedInstance);

@end
