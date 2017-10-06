//
//  KMManager.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-05-16.
//  Updated by Serkan Kurt on 17/05/2013.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMManager.h"
#import "KMManager+Internal.h"
#import "UIView+Helpers.h"
#import <QuartzCore/QuartzCore.h>
#import <CoreText/CoreText.h>
#import <CommonCrypto/CommonDigest.h>
#import "UIButton+Helpers.h"
#import <objc/message.h>
#import "KeymanEngine/KeymanEngine-Swift.h"

#pragma mark - Constants

static BOOL enableWebKit = YES;

// Keyman system-wide keyboard
static BOOL kIsKeymanSystemKeyboard = NO;

// Group Identifier
static NSString *applicationGroupIdentifier = @"";

// Notifications
NSString *const kKeymanLanguagesUpdatedNotification = @"KeymanLanguagesUpdatedNotification";
NSString *const kKeymanLanguagesDownloadFailedNotification = @"kKeymanLanguagesDownloadFailedNotification";
NSString *const kKeymanKeyboardDownloadStartedNotification = @"kKeymanKeyboardDownloadStartedNotification";
NSString *const kKeymanKeyboardDownloadCompletedNotification = @"kKeymanKeyboardDownloadCompletedNotification";
NSString *const kKeymanKeyboardDownloadFailedNotification = @"kKeymanKeyboardDownloadFailedNotification";
NSString *const kKeymanKeyboardChangedNotification = @"kKeymanKeyboardChangedNotification";
NSString *const kKeymanKeyboardLoadedNotification = @"kKeymanKeyboardLoadedNotification";
NSString *const kKeymanKeyboardPickerDismissedNotification = @"kKeymanKeyboardPickerDismissedNotification";
NSString *const kKeymanKeyboardRemovedNotification = @"kKeymanKeyboardRemovedNotification";
NSString *const kKeymanSubKeysMenuWillShowNotification = @"kKeymanSubKeysMenuWillShowNotification";
NSString *const kKeymanSubKeysMenuDismissedNotification = @"kKeymanSubKeysMenuDismissedNotification";
NSString *const kKeymanDebugLogNotification = @"kKeymanDebugLogNotification";

// External dictionary keys
NSString *const kKeymanIdKey = @"id";
NSString *const kKeymanNameKey = @"name";
NSString *const kKeymanKeyboardIdKey = @"kbId";
NSString *const kKeymanLanguageIdKey = @"langId";
NSString *const kKeymanKeyboardNameKey = @"kbName";
NSString *const kKeymanLanguageNameKey = @"langName";
NSString *const kKeymanKeyboardVersionKey = @"version";
NSString *const kKeymanKeyboardKey = @"keyboard";
NSString *const kKeymanLanguageKeyboardsKey = @"keyboards";
NSString *const kKeymanKeyboardFileSizeKey = @"fileSize";
NSString *const kKeymanKeyboardGZipFileSizeKey = @"fileSizeGzip";
NSString *const kKeymanFontKey = @"font";
NSString *const kKeymanOskFontKey = @"oskFont";
NSString *const kKeymanFontFamilyKey = @"family";
NSString *const kKeymanFontSourceKey = @"source";
NSString *const kKeymanFontFilesKey = @"files";
NSString *const kKeymanFontFilenameKey = @"filename"; // Font filename is deprecated
NSString *const kKeymanFontNameKey = @"fontname";
NSString *const kKeymanFontRegisteredKey = @"fontregistered";
NSString *const kKeymanKeyboardFilenameKey = @"filename";
NSString *const kKeymanKeyboardModifiedKey = @"lastModified";
NSString *const kKeymanKeyboardRTLKey = @"rtl";
NSString *const kKeymanKeyboardInfoKey = @"keyboardInfo";
NSString *const kKeymanCustomKeyboardKey = @"CustomKeyboard";

// External user defaults keys
NSString *const kKeymanUserKeyboardsListKey = @"UserKeyboardsList";
NSString *const kKeymanUserCurrentKeyboardKey = @"UserCurrentKeyboard";
// Internal user defaults keys
NSString *const kKeymanEngineVersion = @"KeymanEngineVersion";
NSString *const kKeymanKeyboardPickerDisplayedKey = @"KeyboardPickerDisplayed"; // stores YES if the keyboard picker has been displayed at least once > BOOL
NSString *const kKeymanSynchronizeSWKeyboardKey = @"KeymanSynchronizeSWKeyboard";

// Views
static id webView = nil;
static PopoverView *helpBubbleView = nil;
static KeyPreviewView *keyPreviewView = nil;
static SubKeysView *subKeysView = nil;
static KeyboardMenuView *keyboardMenuView = nil;

// Arrays
static NSMutableArray *subKeyIDs = nil;
static NSMutableArray *subKeyTexts = nil;
static NSMutableArray *subKeys = nil;

// Key frames
static CGRect keyFrame;
static CGRect menuKeyFrame;

// Dictionaries
NSMutableDictionary *options_ = nil;

// Strings
static NSString *const kKeymanKeyboardChangeHelpText = @"Tap here to change keyboard";
static NSString *specialOSKFont = nil;

// URLs and Filenames
NSString *const kKeymanApiBaseURL = @"https://r.keymanweb.com/api/3.0/";
static NSString *const kKeymanApiRemoteURL = @"https://r.keymanweb.com/api/2.0/remote?url=";
static NSString *const kKeymanFileName = @"keyboard";
static NSString *const kKeymanFileExtension = @"html";
static NSString *const kKeymaniOSCodeFileName = @"keymanios.js";
static NSString *const kKeymanHostName = @"r.keymanweb.com";
#define kKeymanFullFileName [NSString stringWithFormat:@"%@.%@", kKeymanFileName, kKeymanFileExtension]

// External Default Keyboard Info
NSString *const kKeymanDefaultKeyboardID = @"european2";
NSString *const kKeymanDefaultLanguageID = @"eng";
NSString *const kKeymanDefaultKeyboardName = @"EuroLatin2 Keyboard";
NSString *const kKeymanDefaultLanguageName = @"English";
NSString *const kKeymanDefaultKeyboardRTL = @"N";
NSString *const kKeymanDefaultKeyboardFont = @"{\"family\":\"LatinWeb\",\"files\":[\"DejaVuSans.ttf\",\"DejaVuSans.mobileconfig\"]}";

// JSON keys for language REST calls
NSString *const kKeymanOptionsKey = @"options";
NSString *const kKeymanLanguageKey = @"language";
// *** TO DO: Check if it matches with the key in Keyman Cloud API ***
NSString *const kKeymanKeyboardCopyrightKey = @"copyright";
//
static NSString *const kKeymanLanguagesKey = @"languages";
static NSString *const kKeymanKeyboardBaseURIKey = @"keyboardBaseUri";
static NSString *const kKeymanFontBaseURIKey = @"fontBaseUri";
static NSString *const kKeymanKeyboardURIKey = @"uri";

// Other keys
static NSString *const kKeymanUpdateKey = @"update";

// UI In-App Keyboard Constants
static CGFloat kKeymanPhonePortraitInAppKeyboardHeight = 183.0f;
static CGFloat kKeymanPhoneLandscapeInAppKeyboardHeight = 183.0f;
static CGFloat kKeymanPadPortraitInAppKeyboardHeight = 385.0f;
static CGFloat kKeymanPadLandscapeInAppKeyboardHeight = 385.0f;

// UI System Keyboard Constants
static CGFloat kKeymanPhonePortraitSystemKeyboardHeight = 216.0f;
static CGFloat kKeymanPhoneLandscapeSystemKeyboardHeight = 162.0f;
static CGFloat kKeymanPadPortraitSystemKeyboardHeight = 264.0f;
static CGFloat kKeymanPadLandscapeSystemKeyboardHeight = 352.0f;

// Sub key colors
static UIColor *subKeyColor;
static UIColor *subKeyColorHighlighted;

BOOL didResizeToOrientation = NO;

#pragma mark - Private Interface

@interface KMManager() <HTTPDownloadDelegate>
// Keyboard Info
+ (CGFloat)keyboardHeight;
+ (CGFloat)keyboardHeightWithOrientation:(UIInterfaceOrientation)orientation;
+ (CGFloat)keyboardWidth;

// Local file storage
- (void)copyWebFilesToLibrary;
- (NSString *)defaultKeymanDirectory;
- (NSString *)defaultLanguageDirectory;
- (NSString *)defaultFontDirectory;
- (NSString *)keyboardPathForID:(NSString *)keyboardID keyboardVersion:(NSString *)kbVersion;

// Keyman interaction
- (void)resizeKeyboard;
- (UIScrollView *)keymanScrollView;

// Misc
- (void)createKeyboardsInfo;
//- (NSDate *)dateFromKeymanDateString:(NSString *)dateString; // No longer used
@end

#pragma mark - Implementation

@implementation KMManager

CWL_SYNTHESIZE_SINGLETON_FOR_CLASS_WITH_ACCESSOR(KMManager, sharedInstance);

@synthesize debugPrintingOn = debugPrintingOn_;
@synthesize keymanHelpOn = keymanHelpOn_;
@synthesize canAddNewKeyboards = canAddNewKeyboards_;
@synthesize canRemoveKeyboards = canRemoveKeyboards_;
@synthesize canRemoveDefaultKeyboard = canRemoveDefaultKeyboard_;
@synthesize languageID = languageID_;
@synthesize keyboardID = keyboardID_;
@synthesize webDelegate = webDelegate_;
@synthesize inputDelegate = inputDelegate_;
@synthesize downloadQueue = downloadQueue_;
@synthesize sharedQueue = sharedQueue_;
@synthesize currentRequest = currentRequest_;
@synthesize reachability = reachability_;
@synthesize languages = languages_;
@synthesize options = options_;
@synthesize keyboardsInfo = keyboardsInfo_;
@synthesize keyboardsDictionary = keyboardsDictionary_;
@synthesize keymanFonts = keymanFonts_;
@synthesize shouldReloadKeyboard;
@synthesize didSynchronize;

#pragma mark - Object Admin

- (void)dealloc {
    [[NSNotificationCenter defaultCenter] removeObserver:self];
    if(currentRequest_.userInfo) {
      [currentRequest_.userInfo setValue:nil forKey:@"completionBlock"];
    }
    webView = nil;
    subKeys = nil;
}

- (id)init {
    if (self = [super init]) {
        self.debugPrintingOn = NO;
        self.keymanHelpOn = YES;
        self.canAddNewKeyboards = YES;
        self.canRemoveKeyboards = YES;
        self.canRemoveDefaultKeyboard = NO;
        subKeyColor = [UIColor colorWithRed:244.0/255.0 green:244.0/255.0  blue:244.0/255.0  alpha:1.0];
        subKeyColorHighlighted = [UIColor colorWithRed:136.0/255.0 green:136.0/255.0 blue:255.0/255.0 alpha:1.0];
        
        [NSURLProtocol registerClass:[KeymanURLProtocol class]];
        NSDictionary *infoDict = [[NSBundle mainBundle] infoDictionary];
        NSDictionary *extensionInfo = [infoDict objectForKey:@"NSExtension"];
        NSString *extensionID = [extensionInfo objectForKey:@"NSExtensionPointIdentifier"];
        if ([extensionID isEqualToString:@"com.apple.keyboard-service"])
            kIsKeymanSystemKeyboard = YES;
        
        int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
        if (iosVersion < 9)
            enableWebKit = NO;
        
        if (!kIsKeymanSystemKeyboard) {
            [self copyUserDefaultsToSharedContainer];
            [self copyKeymanFilesToSharedContainer];
            
            NSUserDefaults *userData = [self activeUserDefaults];
            BOOL isKPDisplayed = [userData boolForKey:kKeymanKeyboardPickerDisplayedKey];
            if (isKPDisplayed) {
                self.keymanHelpOn = NO;
            }
        }
        else {
            self.keymanHelpOn = NO;
        }
        
        [self copyWebFilesToLibrary];
        [self renameOldKeyboardFiles];
        
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillShow:) name:UIKeyboardWillShowNotification object:nil];
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillHide:) name:UIKeyboardWillHideNotification object:nil];
        
        /* No longer used
         dateFormatter_ = [[NSDateFormatter alloc] init];
         [self.dateFormatter setDateFormat:@"yyyy-MM-dd'T'HH:mm:ssZZZZ"];
         [self.dateFormatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
         */
        
        NSString *kbVersion = [self latestKeyboardFileVersionWithID:kKeymanDefaultKeyboardID];
        [self updateKeyboardVersionForID:kKeymanDefaultKeyboardID newKeyboardVersion:kbVersion];
        
        [[self class] inputView]; // Pre-load keyboard
        
        // Set UILongPressGestureRecognizer to show sub keys
        UILongPressGestureRecognizer *hold = [[UILongPressGestureRecognizer alloc]
                                              initWithTarget:self
                                              action:@selector(holdAction:)];
        hold.minimumPressDuration = 0.5f;
        hold.delegate = (id)self;
        [[[self class] inputView] addGestureRecognizer:hold];
      
        self.reachability = [Reachability reachabilityWithHostName:kKeymanHostName];
        
        [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(reachabilityChanged:) name:kReachabilityChangedNotification object:self.reachability];
        [self.reachability startNotifier];
      
        /* HTTPDownloader only uses this for its delegate methods.  So long as we don't
         * set the queue running, this should be perfectly fine.
         */
        self.sharedQueue = [[HTTPDownloader alloc] init:self];
        
        [self registerCustomFonts];
    }
    
    return self;
}

+ (BOOL)isKeymanSystemKeyboard {
    return kIsKeymanSystemKeyboard;
}

+ (BOOL)isWebKitEnabled {
    return (NSClassFromString(@"WKWebView") && enableWebKit);
}

#pragma mark - Keyboard Notifications

- (void)keyboardWillShow:(NSNotification *)notification {
    [self dismissSubKeys]; // dismiss sub keys window
    [self dismissKeyPreview]; // dismiss key preview window
    
    [self resizeKeyboard];
    
    if (self.keymanHelpOn) {
        [helpBubbleView removeFromSuperview];
        [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(showHelpBubble) object:nil];
        [self performSelector:@selector(showHelpBubble) withObject:nil afterDelay:1.5];
    }
}

- (void)keyboardWillHide:(NSNotification *)notification {
    [self dismissHelpBubble];
    [self dismissSubKeys]; // dismiss sub keys view
    [self dismissKeyPreview]; // dismiss key preview window
}

#pragma mark - Public Methods

- (BOOL)setKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID {
    if (languageID != languageID_ || keyboardID != keyboardID_) {
        [self KMLog:[NSString stringWithFormat:@"Setting language: %@_%@", languageID, keyboardID] checkDebugPrinting:YES];
        if ([self usingTempFolder])
            [self copyKeymanFilesToTemp];
        
        if ([keyboardID length] && [languageID length]) {
            eKMKeyboardState kbState = [[KMManager sharedInstance] stateForKeyboardWithID:keyboardID];
            if (kbState == kKMKeyboardStateNeedsDownload) {
                [self KMLog:[NSString stringWithFormat:@"Could not set keyboardID to %@ because the keyboard file does not exist", keyboardID] checkDebugPrinting:NO];
                if ((keyboardID_ == nil || languageID_ == nil) && ![keyboardID isEqualToString:kKeymanDefaultKeyboardID]) {
                    [self setKeyboardWithID:kKeymanDefaultKeyboardID
                                 languageID:kKeymanDefaultLanguageID
                               keyboardName:kKeymanDefaultKeyboardName
                               languageName:kKeymanDefaultLanguageName
                                       font:kKeymanDefaultKeyboardFont
                                    oskFont:nil];
                }
                return NO;
            }
            
            languageID_ = [languageID copy];
            keyboardID_ = [keyboardID copy];
            NSString *key = [NSString stringWithFormat:@"%@_%@", self.languageID, self.keyboardID];
            
            NSUserDefaults *userData = [self activeUserDefaults];
            NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
            NSInteger index = [self indexForUserKeyboardWithID:self.keyboardID
                                                    languageID:self.languageID];
            NSDictionary *kbDict = nil;
            if (index >= 0)
                kbDict = [userKeyboards objectAtIndex:index];
            else
                kbDict = [self.keyboardsDictionary objectForKey:key];
            
            NSString *langName = [kbDict objectForKey:kKeymanLanguageNameKey];
            NSString *kbName = [kbDict objectForKey:kKeymanKeyboardNameKey];
            NSString *kbVersion = [self latestKeyboardFileVersionWithID:keyboardID];
            if (kbVersion == nil)
                kbVersion = @"1.0";
            NSString *isRTL = [kbDict objectForKey:kKeymanKeyboardRTLKey];
            
            if ([key isEqualToString:[NSString stringWithFormat:@"%@_%@", kKeymanDefaultLanguageID, kKeymanDefaultKeyboardID]]) {
                if (langName == nil || kbName == nil) {
                    langName = kKeymanDefaultLanguageName;
                    kbName = kKeymanDefaultKeyboardName;
                }
            }
            
            NSString *jsFont = [self jsFontForKeyboardID:keyboardID languageID:languageID];
            NSString *jsOskFont = [self jsOskFontForKeyboardID:keyboardID languageID:languageID];
            if (jsOskFont == nil || [jsOskFont isEqualToString:@"''"])
                jsOskFont = jsFont;
            
            langName = [langName stringByReplacingOccurrencesOfString:@"'" withString:@"\\'"];
            kbName = [kbName stringByReplacingOccurrencesOfString:@"'" withString:@"\\'"];
            
            if ([jsFont isEqualToString:@"''"])
                jsFont = @"undefined";
            if ([jsOskFont isEqualToString:@"''"])
                jsOskFont = @"undefined";
            NSString *jsString = [NSString stringWithFormat:@"setKeymanLanguage('%@','%@','%@','%@','%@',%@,%@);", kbName, self.keyboardID, langName, self.languageID, kbVersion, jsFont, jsOskFont];
            [self KMLog:[NSString stringWithFormat:@"Evaluating JavaScript: %@", jsString] checkDebugPrinting:YES];
            if ([[self class] isWKWebView:webView])
                [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
            else
                [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
            
            if (kIsKeymanSystemKeyboard)
                userData = [NSUserDefaults standardUserDefaults];
            else
                userData = [self activeUserDefaults];
            
            [userData setObject:[NSDictionary dictionaryWithObjectsAndKeys:
                                 self.keyboardID, kKeymanKeyboardIdKey,
                                 self.languageID, kKeymanLanguageIdKey,
                                 [kbName stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"], kKeymanKeyboardNameKey,
                                 [langName stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"], kKeymanLanguageNameKey,
                                 kbVersion, kKeymanKeyboardVersionKey,
                                 (isRTL != nil)?isRTL:@"N", kKeymanKeyboardRTLKey,
                                 jsFont, kKeymanFontKey,
                                 nil] forKey:kKeymanUserCurrentKeyboardKey];
            [userData synchronize];
            
            if (self.keymanHelpOn) {
                [helpBubbleView removeFromSuperview];
                [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(showHelpBubble) object:nil];
                [self performSelector:@selector(showHelpBubble) withObject:nil afterDelay:1.5];
            }
            
            NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                                    keyboardID, kKeymanKeyboardIdKey,
                                    languageID, kKeymanLanguageIdKey,
                                    kbName, kKeymanKeyboardNameKey,
                                    langName, kKeymanLanguageNameKey,
                                    kbVersion, kKeymanKeyboardVersionKey,
                                    jsFont, kKeymanFontKey,
                                    nil];
            [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardChangedNotification object:self
                                                              userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbInfo, kKeymanKeyboardInfoKey, nil]];
            
            return YES;
        }
    }
    
    return NO;
}

- (BOOL)setKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID keyboardName:(NSString *)keyboardName languageName:(NSString *)languageName {
    // if keyboardName or languageName is nil call setKeyboardWithID without these parameters
    if (keyboardName == nil || languageName == nil)
        return [self setKeyboardWithID:keyboardID languageID:languageID];
    
    if (languageID != languageID_ || keyboardID != keyboardID_) {
        [self KMLog:[NSString stringWithFormat:@"Setting language: %@_%@", languageID, keyboardID] checkDebugPrinting:YES];
        if ([self usingTempFolder])
            [self copyKeymanFilesToTemp];
        
        if ([keyboardID length] && [languageID length]) {
            eKMKeyboardState kbState = [[KMManager sharedInstance] stateForKeyboardWithID:keyboardID];
            if (kbState == kKMKeyboardStateNeedsDownload) {
                [self KMLog:[NSString stringWithFormat:@"Could not set keyboardID to %@ because the keyboard file does not exist", keyboardID] checkDebugPrinting:NO];
                if ((keyboardID_ == nil || languageID_ == nil) && ![keyboardID isEqualToString:kKeymanDefaultKeyboardID]) {
                    [self setKeyboardWithID:kKeymanDefaultKeyboardID
                                 languageID:kKeymanDefaultLanguageID
                               keyboardName:kKeymanDefaultKeyboardName
                               languageName:kKeymanDefaultLanguageName
                                       font:kKeymanDefaultKeyboardFont
                                    oskFont:nil];
                }
                return NO;
            }
            
            languageID_ = [languageID copy];
            keyboardID_ = [keyboardID copy];
            NSString *key = [NSString stringWithFormat:@"%@_%@", self.languageID, self.keyboardID];
            
            NSUserDefaults *userData = [self activeUserDefaults];
            NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
            NSInteger index = [self indexForUserKeyboardWithID:self.keyboardID
                                                    languageID:self.languageID];
            NSDictionary *kbDict = nil;
            if (index >= 0)
                kbDict = [userKeyboards objectAtIndex:index];
            else
                kbDict = [self.keyboardsDictionary objectForKey:key];
            
            NSString *kbVersion = [self latestKeyboardFileVersionWithID:keyboardID];
            if (kbVersion == nil)
                kbVersion = @"1.0";
            NSString *isRTL = [kbDict objectForKey:kKeymanKeyboardRTLKey];
            
            NSString *jsFont = [self jsFontForKeyboardID:keyboardID languageID:languageID];
            NSString *jsOskFont = [self jsOskFontForKeyboardID:keyboardID languageID:languageID];
            if (jsOskFont == nil || [jsOskFont isEqualToString:@"''"])
                jsOskFont = jsFont;
            
            NSString *langName = [NSString stringWithString:[languageName stringByReplacingOccurrencesOfString:@"'" withString:@"\\'"]];
            NSString *kbName = [NSString stringWithString:[keyboardName stringByReplacingOccurrencesOfString:@"'" withString:@"\\'"]];
            
            if ([jsFont isEqualToString:@"''"])
                jsFont = @"undefined";
            if ([jsOskFont isEqualToString:@"''"])
                jsOskFont = @"undefined";
            NSString *jsString = [NSString stringWithFormat:@"setKeymanLanguage('%@','%@','%@','%@','%@',%@,%@);", kbName, self.keyboardID, langName, self.languageID, kbVersion, jsFont, jsOskFont];
            [self KMLog:[NSString stringWithFormat:@"Evaluating JavaScript: %@", jsString] checkDebugPrinting:YES];
            if ([[self class] isWKWebView:webView])
                [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
            else
                [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
            
            if (kIsKeymanSystemKeyboard)
                userData = [NSUserDefaults standardUserDefaults];
            else
                userData = [self activeUserDefaults];
            
            [userData setObject:[NSDictionary dictionaryWithObjectsAndKeys:
                                 self.keyboardID, kKeymanKeyboardIdKey,
                                 self.languageID, kKeymanLanguageIdKey,
                                 [kbName stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"], kKeymanKeyboardNameKey,
                                 [langName stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"], kKeymanLanguageNameKey,
                                 kbVersion, kKeymanKeyboardVersionKey,
                                 (isRTL != nil)?isRTL:@"N", kKeymanKeyboardRTLKey,
                                 jsFont, kKeymanFontKey,
                                 nil] forKey:kKeymanUserCurrentKeyboardKey];
            [userData synchronize];
            
            if (self.keymanHelpOn) {
                [helpBubbleView removeFromSuperview];
                [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(showHelpBubble) object:nil];
                [self performSelector:@selector(showHelpBubble) withObject:nil afterDelay:1.5];
            }
            
            NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                                    keyboardID, kKeymanKeyboardIdKey,
                                    languageID, kKeymanLanguageIdKey,
                                    keyboardName, kKeymanKeyboardNameKey,
                                    langName, kKeymanLanguageNameKey,
                                    kbVersion, kKeymanKeyboardVersionKey,
                                    jsFont, kKeymanFontKey,
                                    nil];
            [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardChangedNotification
                                                                object:self
                                                              userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbInfo, kKeymanKeyboardInfoKey, nil]];
            return YES;
        }
    }
    
    return NO;
}

- (BOOL)setKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID keyboardName:(NSString *)keyboardName languageName:(NSString *)languageName font:(NSString *)font oskFont:(NSString *)oskFont {
    // if the font and the oskFont are both nil call setKeyboardWithID without these parameters
    if (font == nil && oskFont == nil)
        return [self setKeyboardWithID:keyboardID languageID:languageID keyboardName:keyboardName languageName:languageName];
    
    if (keyboardID != keyboardID_ || languageID != languageID_) {
        [self KMLog:[NSString stringWithFormat:@"Setting language: %@_%@", languageID, keyboardID] checkDebugPrinting:YES];
        if ([self usingTempFolder])
            [self copyKeymanFilesToTemp];
        
        if ([keyboardID length] && [languageID length]) {
            eKMKeyboardState kbState = [[KMManager sharedInstance] stateForKeyboardWithID:keyboardID];
            if (kbState == kKMKeyboardStateNeedsDownload) {
                [self KMLog:[NSString stringWithFormat:@"Could not set keyboardID to %@ because the keyboard file does not exist", keyboardID] checkDebugPrinting:NO];
                if ((keyboardID_ == nil || languageID_ == nil) && ![keyboardID isEqualToString:kKeymanDefaultKeyboardID]) {
                    [self setKeyboardWithID:kKeymanDefaultKeyboardID
                                 languageID:kKeymanDefaultLanguageID
                               keyboardName:kKeymanDefaultKeyboardName
                               languageName:kKeymanDefaultLanguageName
                                       font:kKeymanDefaultKeyboardFont
                                    oskFont:nil
                     ];
                }
                return NO;
            }
            
            keyboardID_ = [keyboardID copy];
            languageID_ = [languageID copy];
            NSString *key = [NSString stringWithFormat:@"%@_%@", self.languageID, self.keyboardID];
            
            NSUserDefaults *userData = [self activeUserDefaults];
            NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
            NSInteger index = [self indexForUserKeyboardWithID:self.keyboardID
                                                    languageID:self.languageID];
            NSDictionary *kbDict = nil;
            if (index >= 0)
                kbDict = [userKeyboards objectAtIndex:index];
            else
                kbDict = [self.keyboardsDictionary objectForKey:key];
            
            NSString *kbVersion = [self latestKeyboardFileVersionWithID:keyboardID];
            if (kbVersion == nil)
                kbVersion = @"1.0";
            
            NSString *jsFont = nil;
            if ([font rangeOfString:@".ttf"].location != NSNotFound || [font rangeOfString:@".otf"].location != NSNotFound) {
                if ([[font substringFromIndex:font.length-4] isEqualToString:@".ttf"] || [[font substringFromIndex:font.length-4] isEqualToString:@".otf"]) {
                    NSString *familyName = [NSString stringWithFormat:@"font_family_%@", [font substringToIndex:font.length-4]];
                    jsFont = ([font length])?[NSString stringWithFormat:@"{\"family\":\"%@\",\"files\":[\"%@\"]}", familyName, font]:@"''";
                }
                else
                    jsFont = ([font length])?font:@"''";
            }
            else
                jsFont = @"''";
            
            NSString *jsOskFont = nil;
            if (oskFont == nil || [oskFont isEqualToString:@"''"]) {
                jsOskFont = jsFont;
            }
            else {
                if ([oskFont rangeOfString:@".ttf"].location != NSNotFound || [oskFont rangeOfString:@".otf"].location != NSNotFound) {
                    if ([[oskFont substringFromIndex:oskFont.length-4] isEqualToString:@".ttf"] || [[oskFont substringFromIndex:oskFont.length-4] isEqualToString:@".otf"]) {
                        NSString *familyName = [NSString stringWithFormat:@"font_family_%@", [oskFont substringToIndex:oskFont.length-4]];
                        jsOskFont = ([oskFont length])?[NSString stringWithFormat:@"{\"family\":\"%@\",\"files\":[\"%@\"]}", familyName, oskFont]:@"''";
                    }
                    else
                        jsOskFont = ([oskFont length])?oskFont:@"''";
                }
                else
                    jsOskFont = @"''";
            }
            
            NSString *isRTL = [kbDict objectForKey:kKeymanKeyboardRTLKey];
            NSString *langName = [NSString stringWithString:[languageName stringByReplacingOccurrencesOfString:@"'" withString:@"\\'"]];
            NSString *kbName = [NSString stringWithString:[keyboardName stringByReplacingOccurrencesOfString:@"'" withString:@"\\'"]];
            
            if ([jsFont isEqualToString:@"''"])
                jsFont = @"undefined";
            if ([jsOskFont isEqualToString:@"''"])
                jsOskFont = @"undefined";
            NSString *jsString = [NSString stringWithFormat:@"setKeymanLanguage('%@','%@','%@','%@','%@',%@,%@);", kbName, self.keyboardID, langName, self.languageID, kbVersion, jsFont, jsOskFont];
            [self KMLog:[NSString stringWithFormat:@"Evaluating JavaScript: %@", jsString] checkDebugPrinting:YES];
            if ([[self class] isWKWebView:webView])
                [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
            else
                [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
            
            if (kIsKeymanSystemKeyboard)
                userData = [NSUserDefaults standardUserDefaults];
            else
                userData = [self activeUserDefaults];
            
            [userData setObject:[NSDictionary dictionaryWithObjectsAndKeys:
                                 self.keyboardID, kKeymanKeyboardIdKey,
                                 self.languageID, kKeymanLanguageIdKey,
                                 [kbName stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"], kKeymanKeyboardNameKey,
                                 [langName stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"], kKeymanLanguageNameKey,
                                 kbVersion, kKeymanKeyboardVersionKey,
                                 (isRTL != nil)?isRTL:@"N", kKeymanKeyboardRTLKey,
                                 jsFont, kKeymanFontKey,
                                 nil] forKey:kKeymanUserCurrentKeyboardKey];
            [userData synchronize];
            
            if (self.keymanHelpOn) {
                [helpBubbleView removeFromSuperview];
                [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(showHelpBubble) object:nil];
                [self performSelector:@selector(showHelpBubble) withObject:nil afterDelay:1.5];
            }
            
            NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                                    keyboardID, kKeymanKeyboardIdKey,
                                    languageID, kKeymanLanguageIdKey,
                                    keyboardName, kKeymanKeyboardNameKey,
                                    langName, kKeymanLanguageNameKey,
                                    kbVersion, kKeymanKeyboardVersionKey,
                                    jsFont, kKeymanFontKey,
                                    nil];
            [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardChangedNotification
                                                                object:self
                                                              userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbInfo, kKeymanKeyboardInfoKey, nil]];
            
            return YES;
        }
    }
    
    return NO;
}

- (NSInteger)switchToNextKeyboard {
    NSInteger index = [self currentKeyboardIndex];
    if (index < 0)
        return index;
    
    NSUserDefaults *userData = [self activeUserDefaults];
    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    NSUInteger len = [userKeyboards count];
    if (len > 1) {
        index++;
        index = index%len;
        NSDictionary *kb = [userKeyboards objectAtIndex:index];
        NSString *kbId = [kb objectForKey:kKeymanKeyboardIdKey];
        NSString *langId = [kb objectForKey:kKeymanLanguageIdKey];
        NSString *kbName = [kb objectForKey:kKeymanKeyboardNameKey];
        NSString *langName = [kb objectForKey:kKeymanLanguageNameKey];
        NSString *font = [kb objectForKey:kKeymanFontKey];
        NSString *oskFont = [kb objectForKey:kKeymanOskFontKey];
        [self setKeyboardWithID:kbId languageID:langId keyboardName:kbName languageName:langName font:font oskFont:oskFont];
    }
    
    return index;
}

- (NSInteger)currentKeyboardIndex {
    NSUserDefaults *userData = [self activeUserDefaults];
    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    NSUInteger len = [userKeyboards count];
    if (len < 1)
        return -1;
    
    NSDictionary *kb = nil;
    NSInteger index = -1;
    for (int i = 0; i < len; i++) {
        kb = [userKeyboards objectAtIndex:i];
        NSString *kbId = [kb objectForKey:kKeymanKeyboardIdKey];
        NSString *langId = [kb objectForKey:kKeymanLanguageIdKey];
        if ([keyboardID_ isEqualToString:kbId] && [languageID_ isEqualToString:langId]) {
            index = i;
            break;
        }
    }
    
    return index;
}

- (void)setCanAddNewKeyboards:(BOOL)canAddNewKeyboards {
    canAddNewKeyboards_ = canAddNewKeyboards;
    if (!canAddNewKeyboards)
        canRemoveKeyboards_ = NO;
}

- (BOOL)userKeyboardExistsWithID:(NSString *)kbID languageID:(NSString *)langID {
    BOOL exists = NO;
    NSArray *userKeyboards = [[self activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey];
    if (userKeyboards != nil && [userKeyboards count]) {
        for (NSDictionary *kb in userKeyboards) {
            if([kbID isEqualToString:[kb objectForKey:kKeymanKeyboardIdKey]] && [langID isEqualToString:[kb objectForKey:kKeymanLanguageIdKey]]) {
                exists = YES;
                break;
            }
        }
    }
    
    return exists;
}

- (NSInteger)indexForUserKeyboardWithID:(NSString *)kbID languageID:(NSString *)langID {
    BOOL exists = NO;
    NSInteger index = -1;
    NSArray *userKeyboards = [[self activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey];
    if (userKeyboards != nil && [userKeyboards count]) {
        for (NSDictionary *kb in userKeyboards) {
            index++;
            if([kbID isEqualToString:[kb objectForKey:kKeymanKeyboardIdKey]] && [langID isEqualToString:[kb objectForKey:kKeymanLanguageIdKey]]) {
                exists = YES;
                break;
            }
        }
    }
    
    if (exists)
        return index;
    else
        return -1;
}

- (NSDictionary *)currentKeyboardInfo {
    if (keyboardID_ == nil || languageID_ == nil)
        return nil;
    
    NSArray *userKeyboards = [[self activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey];
    NSDictionary *kbInfo = nil;
    for (NSDictionary *kb in userKeyboards) {
        if([keyboardID_ isEqualToString:[kb objectForKey:kKeymanKeyboardIdKey]] && [languageID_ isEqualToString:[kb objectForKey:kKeymanLanguageIdKey]]) {
            kbInfo = [NSDictionary dictionaryWithDictionary:kb];
            break;
        }
    }
    
    if (kbInfo == nil && keyboardsDictionary_ != nil) {
        NSString *kbKey = [NSString stringWithFormat:@"%@_%@", languageID_, keyboardID_];
        NSDictionary *kb = [keyboardsDictionary_ objectForKey:kbKey];
        if (kb != nil)
            kbInfo = [NSDictionary dictionaryWithDictionary:kb];
    }
    
    if (kbInfo == nil)
        kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:keyboardID_, kKeymanKeyboardIdKey,
                  languageID_, kKeymanLanguageIdKey, nil];
    
    return kbInfo;
}

- (void)addKeyboardWithID:(NSString *)kbID languageID:(NSString *)langID keyboardName:(NSString *)kbName languageName:(NSString *)langName isRTL:(BOOL)isRTL isCustom:(BOOL)isCustom font:(NSString *)font oskFont:(NSString *)oskFont {
    // Check if keyboard file exists
    NSString *kbVersion = [self latestKeyboardFileVersionWithID:kbID];
    if (kbVersion == nil) {
        [self KMLog:[NSString stringWithFormat:@"Could not add keyboard with ID: %@ because the keyboard file does not exist", kbID] checkDebugPrinting:NO];
        return;
    }
    
    // Get keyboards list if it exists in user defaults
    NSMutableArray *userKeyboards = [NSMutableArray arrayWithArray:[[self activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey]];
    
    // If user defaults for keyboards list does not exist, start creating a new one.
    if (userKeyboards == nil)
        userKeyboards = [[NSMutableArray alloc] init];
    
    // Get keyboard index if it exists in list.
    NSInteger x = [self indexForUserKeyboardWithID:kbID languageID:langID];
    
    // Add keyboard to the list if it doesn't exist, update otherwise.
    if (x < 0) {
        [userKeyboards addObject:[NSDictionary dictionaryWithObjectsAndKeys:
                                  langID, kKeymanLanguageIdKey,
                                  langName, kKeymanLanguageNameKey,
                                  kbID, kKeymanKeyboardIdKey,
                                  kbName, kKeymanKeyboardNameKey,
                                  kbVersion, kKeymanKeyboardVersionKey,
                                  isRTL?@"Y":@"N", kKeymanKeyboardRTLKey,
                                  isCustom?@"Y":@"N", kKeymanCustomKeyboardKey,
                                  font?font:@"''", kKeymanFontKey,
                                  oskFont, kKeymanOskFontKey,
                                  nil]];
    }
    else {
        [userKeyboards replaceObjectAtIndex:x withObject:[NSDictionary dictionaryWithObjectsAndKeys:
                                                          langID, kKeymanLanguageIdKey,
                                                          langName, kKeymanLanguageNameKey,
                                                          kbID, kKeymanKeyboardIdKey,
                                                          kbName, kKeymanKeyboardNameKey,
                                                          kbVersion, kKeymanKeyboardVersionKey,
                                                          isRTL?@"Y":@"N", kKeymanKeyboardRTLKey,
                                                          isCustom?@"Y":@"N", kKeymanCustomKeyboardKey,
                                                          font?font:@"''", kKeymanFontKey,
                                                          oskFont, kKeymanOskFontKey,
                                                          nil]];
    }
    
    NSUserDefaults *userData = [self activeUserDefaults];
    [userData setObject:userKeyboards forKey:kKeymanUserKeyboardsListKey];
    [userData setObject:[NSArray arrayWithObject:[NSDate date]] forKey:kKeymanSynchronizeSWKeyboardKey];
    [userData synchronize];
}

- (void)removeKeyboardWithID:(NSString *)kbID languageID:(NSString *)langID {
    NSUserDefaults *userData = [self activeUserDefaults];
    NSMutableArray *userKeyboards = [NSMutableArray arrayWithArray:[userData arrayForKey:kKeymanUserKeyboardsListKey]];
    
    // If user defaults for keyboards list does not exist, do nothing.
    if (userKeyboards == nil)
        return;
    
    // Check if the keyboard exists in list.
    NSInteger index = [self indexForUserKeyboardWithID:kbID languageID:langID];
    
    // Remove keyboard from the list if it exists
    if (index >= 0) {
        NSDictionary *kbDict = [NSDictionary dictionaryWithDictionary:[userKeyboards objectAtIndex:index]];
        [userKeyboards removeObjectAtIndex:index];
        [userData setObject:userKeyboards forKey:kKeymanUserKeyboardsListKey];
        [userData setObject:[NSArray arrayWithObject:[NSDate date]] forKey:kKeymanSynchronizeSWKeyboardKey];
        [userData synchronize];
        
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardRemovedNotification object:self
                                                          userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbDict, kKeymanKeyboardInfoKey, nil]];
    }
}

- (BOOL)removeKeyboardAtIndex:(NSUInteger)index {
    NSUserDefaults *userData = [self activeUserDefaults];
    NSMutableArray *userKeyboards = [[userData arrayForKey:kKeymanUserKeyboardsListKey] mutableCopy];
    
    if (userKeyboards != nil) {
        if (index < [userKeyboards count]) {
            [userKeyboards removeObjectAtIndex:index];
            [userData setObject:userKeyboards forKey:kKeymanUserKeyboardsListKey];
            [userData setObject:[NSArray arrayWithObject:[NSDate date]] forKey:kKeymanSynchronizeSWKeyboardKey];
            [userData synchronize];
            return YES;
        }
    }
    
    return NO;
}

- (void)updateKeyboardVersionForID:(NSString *)kbID newKeyboardVersion:(NSString *)kbVersion {
    NSUserDefaults *userData = [self activeUserDefaults];
    NSMutableArray *userKeyboards = [[userData arrayForKey:kKeymanUserKeyboardsListKey] mutableCopy];
    if (userKeyboards == nil)
        return;
    
    NSUInteger len = [userKeyboards count];
    for (int i = 0; i < len; i++) {
        NSDictionary *kb = [userKeyboards objectAtIndex:i];
        if ([kbID isEqualToString:[kb objectForKey:kKeymanKeyboardIdKey]]) {
            NSMutableDictionary *kbInfo = [kb mutableCopy];
            [kbInfo setObject:kbVersion forKey:kKeymanKeyboardVersionKey];
            [userKeyboards replaceObjectAtIndex:i withObject:kbInfo];
        }
    }
    
    [userData setObject:userKeyboards forKey:kKeymanUserKeyboardsListKey];
    [userData synchronize];
    
    if (kIsKeymanSystemKeyboard)
        userData = [NSUserDefaults standardUserDefaults];
    else
        userData = [self activeUserDefaults];
    
    NSMutableDictionary *userKb = [[userData objectForKey:kKeymanUserCurrentKeyboardKey] mutableCopy];
    if ([kbID isEqualToString:[userKb objectForKey:kKeymanKeyboardIdKey]]) {
        [userKb setObject:kbVersion forKey:kKeymanKeyboardVersionKey];
        [userData setObject:userKb forKey:kKeymanUserCurrentKeyboardKey];
    }
    
    [userData synchronize];
}

- (void)showKeyboardPickerInViewController:(UIViewController *)viewController shouldAddKeyboard:(BOOL)shouldAddKeyboard {
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad && [[[UIDevice currentDevice] systemVersion] intValue] >= 7) {
        id firstResponder = [[[UIApplication sharedApplication] keyWindow] findFirstResponder];
        if ([firstResponder isKindOfClass:[KeymanTextView class]])
            [(KeymanTextView *)firstResponder dismissKeyboard];
        else if ([firstResponder isKindOfClass:[KeymanTextField class]])
            [(KeymanTextField *)firstResponder dismissKeyboard];
    }
    
    KeyboardPickerViewController *vc = [[KeyboardPickerViewController alloc] init];
    UINavigationController *nc = [[UINavigationController alloc] initWithRootViewController:vc];
    nc.modalTransitionStyle = UIModalTransitionStyleCoverVertical;
    nc.modalPresentationStyle = UIModalPresentationPageSheet;
    [viewController presentViewController:nc animated:YES completion:^{
        if (shouldAddKeyboard) {
            [vc showAddKeyboard];
        }
        else {
            NSUserDefaults *userData = [self activeUserDefaults];
            [userData setBool:YES forKey:kKeymanKeyboardPickerDisplayedKey];
            [userData synchronize];
            self.keymanHelpOn = NO;
        }
    }];
}

- (void)dismissKeyboardPicker:(UIViewController *)viewController {
    [viewController dismissViewControllerAnimated:YES completion:nil];
    
    if (shouldReloadKeyboard)
        [self reloadKeyboard];
    
    [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardPickerDismissedNotification object:self];
}

- (void)reloadKeyboard {
    NSURL *codeURL = [NSURL fileURLWithPath:[[[KMManager sharedInstance] activeKeymanDirectory] stringByAppendingPathComponent:kKeymanFullFileName]];
    if ([[self class] isWKWebView:webView]) {
        /*
        [(WKWebView *)webView loadHTMLString:@"" baseURL:nil];
        [(WKWebView *)webView stopLoading];
        [(WKWebView *)webView setNavigationDelegate:nil];
        [(WKWebView *)webView setNavigationDelegate:[KMManager sharedInstance]];*/
        SEL sel = NSSelectorFromString(@"loadFileURL:allowingReadAccessToURL:");
        if ([(WKWebView *)webView respondsToSelector:sel]) {
            ((id (*)(id, SEL, id, id))objc_msgSend)((WKWebView *)webView, sel, codeURL, [NSURL URLWithString:[[codeURL absoluteString] stringByDeletingLastPathComponent]]);
        }
        else {
            // Copy Keyman files to the temp folder and load from there
            if ([self copyKeymanFilesToTemp]) {
                codeURL = [NSURL fileURLWithPath:[NSTemporaryDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"keyman/%@", kKeymanFullFileName]]];
                [(WKWebView *)webView loadRequest:[NSURLRequest requestWithURL:codeURL cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60.0]];
            }
        }
    }
    else {
        /*
        [(UIWebView *)webView loadHTMLString:@"" baseURL:nil];
        [(UIWebView *)webView stopLoading];
        [(UIWebView *)webView setDelegate:nil];
        [(UIWebView *)webView setDelegate:[KMManager sharedInstance]];*/
        [(UIWebView *)webView loadRequest:[NSURLRequest requestWithURL:codeURL cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60.0]];
    }
}

- (void)resetKeyboard {
    NSArray *userKeyboards = [[self activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey];
    NSInteger index = [self indexForUserKeyboardWithID:self.keyboardID languageID:self.languageID];
    if (index >= 0) {
        keyboardID_ = nil;
        languageID_ = nil;
        NSDictionary *kb = [userKeyboards objectAtIndex:index];
        NSString *kbId = [kb objectForKey:kKeymanKeyboardIdKey];
        NSString *langId = [kb objectForKey:kKeymanLanguageIdKey];
        NSString *kbName = [kb objectForKey:kKeymanKeyboardNameKey];
        NSString *langName = [kb objectForKey:kKeymanLanguageNameKey];
        NSString *font = [kb objectForKey:kKeymanFontKey];
        NSString *oskFont = [kb objectForKey:kKeymanOskFontKey];
        [self setKeyboardWithID:kbId languageID:langId keyboardName:kbName languageName:langName font:font oskFont:oskFont];
    }
    else {
        NSDictionary *kb = [userKeyboards objectAtIndex:0];
        if (kb != nil) {
            keyboardID_ = nil;
            languageID_ = nil;
            NSString *kbId = [kb objectForKey:kKeymanKeyboardIdKey];
            NSString *langId = [kb objectForKey:kKeymanLanguageIdKey];
            NSString *kbName = [kb objectForKey:kKeymanKeyboardNameKey];
            NSString *langName = [kb objectForKey:kKeymanLanguageNameKey];
            NSString *font = [kb objectForKey:kKeymanFontKey];
            NSString *oskFont = [kb objectForKey:kKeymanOskFontKey];
            [self setKeyboardWithID:kbId languageID:langId keyboardName:kbName languageName:langName font:font oskFont:oskFont];
        }
        else {
            keyboardID_ = nil;
            languageID_ = nil;
            [self setKeyboardWithID:kKeymanDefaultKeyboardID
                         languageID:kKeymanDefaultLanguageID
                       keyboardName:kKeymanDefaultKeyboardName
                       languageName:kKeymanDefaultLanguageName
                               font:kKeymanDefaultKeyboardFont
                            oskFont:nil];
        }
    }
}

- (BOOL)usingTempFolder {
    if ([[self class] isWKWebView:webView]) {
        SEL sel = NSSelectorFromString(@"loadFileURL:allowingReadAccessToURL:");
        if (![(WKWebView *)webView respondsToSelector:sel]) {
            return YES;
        }
    }
    
    return NO;
}

- (void)preloadLanguageFileAtPath:(NSString *)languagePath shouldOverwrite:(BOOL)shouldOverwrite {
    if (![languagePath length])
        return;
    
    NSFileManager *fileMan = [NSFileManager defaultManager];
    NSError *error = nil;
    NSString *langDir = [self activeLanguageDirectory];
    
    if (![langDir length]) {
        [self KMLog:@"Could not find/create the Keyman language directory" checkDebugPrinting:NO];
        return;
    }
    
    NSString *destinationPath = [langDir stringByAppendingPathComponent:[languagePath lastPathComponent]];
    if (![fileMan fileExistsAtPath:destinationPath])
        [fileMan copyItemAtPath:languagePath toPath:destinationPath error:&error];
    else if (shouldOverwrite) {
        [fileMan removeItemAtPath:destinationPath error:&error];
        if (error == nil)
            [fileMan copyItemAtPath:languagePath toPath:destinationPath error:&error];
    }
    
    if(error==nil)
        [self addSkipBackupAttributeToItemAtPath:destinationPath];
    else
        [self KMLog:[NSString stringWithFormat:@"Error copying language file: %@", error] checkDebugPrinting:NO];
}

- (void)preloadFontFileAtPath:(NSString *)fontPath shouldOverwrite:(BOOL)shouldOverwrite {
    if (![fontPath length])
        return;
    
    NSFileManager *fileMan = [NSFileManager defaultManager];
    NSError *error = nil;
    NSString *fontDir = [self activeFontDirectory];
    
    if (![fontDir length]) {
        [self KMLog:@"Could not find/create the Keyman font directory" checkDebugPrinting:NO];
        return;
    }
    
    NSString *destinationPath = [fontDir stringByAppendingPathComponent:[fontPath lastPathComponent]];
    if (![fileMan fileExistsAtPath:destinationPath])
        [fileMan copyItemAtPath:fontPath toPath:destinationPath error:&error];
    else if (shouldOverwrite) {
        [fileMan removeItemAtPath:destinationPath error:&error];
        if (error == nil)
            [fileMan copyItemAtPath:fontPath toPath:destinationPath error:&error];
    }
    
    if (error != nil)
        [self KMLog:[NSString stringWithFormat:@"Error copying font file: %@", error] checkDebugPrinting:NO];
    else
        [self addSkipBackupAttributeToItemAtPath:destinationPath];
}

- (void)fetchKeyboardsList {
    [[self class] fetchKeyboardsWithCompletionBlock:nil];
}

- (NSString *)sdkVersion {
    NSBundle *keymanBundle = [NSBundle bundleWithPath:[[NSBundle bundleForClass:[self class]] pathForResource:@"Keyman" ofType:@"bundle"]];
    NSDictionary *info = [NSDictionary dictionaryWithContentsOfFile:[keymanBundle pathForResource:@"KeymanEngine-Info" ofType:@"plist"]];
    return [info objectForKey:@"CFBundleVersion"];
}

- (void)setKeymanHelpOn:(BOOL)keymanHelpOn {
    // only in-app keyboard is allowed to enable/disable the help bubble, it is always disabled for system-wide keyboard
    if (!kIsKeymanSystemKeyboard) {
        keymanHelpOn_ = keymanHelpOn;
    }
}

- (void)showHelpBubble {
    // help bubble is always disabled for system-wide keyboard
    if (kIsKeymanSystemKeyboard || keyboardMenuView != nil)
        return;
    
    NSString *jsString = [NSString stringWithFormat:@"langMenuPos();"];
    if ([[self class] isWKWebView:webView]) {
        [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:^(id result, NSError *error) {
            if (result) {
                NSString *langMenuKeyPos = [NSString stringWithFormat:@"%@", result];
                if ([langMenuKeyPos length]) {
                    NSArray *pos = [langMenuKeyPos componentsSeparatedByString: @","];
                    CGFloat px = [[pos objectAtIndex:0] floatValue];
                    CGFloat py = [[pos objectAtIndex:1] floatValue];
                    [self showHelpBubbleAtPoint:CGPointMake(px, py)];
                }
            }
        }];
    }
    else {
        NSString *langMenuKeyPos = [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
        if ([langMenuKeyPos length]) {
            NSArray *pos = [langMenuKeyPos componentsSeparatedByString: @","];
            CGFloat px = [[pos objectAtIndex:0] floatValue];
            CGFloat py = [[pos objectAtIndex:1] floatValue];
            [self showHelpBubbleAtPoint:CGPointMake(px, py)];
        }
    }
}

- (void)showHelpBubbleAtPoint:(CGPoint)point {
    [helpBubbleView removeFromSuperview];
    helpBubbleView = [[PopoverView alloc] initWithFrame:CGRectZero];
    [helpBubbleView setBackgroundColor:[UIColor colorWithRed:253.0/255.0 green:244.0/255.0 blue:196.0/255.0 alpha:1.0]];
    [helpBubbleView setBackgroundColor2:[UIColor colorWithRed:233.0/255.0 green:224.0/255.0 blue:176.0/255.0 alpha:1.0]];
    [helpBubbleView setBorderColor:[UIColor colorWithRed:0.5 green:0.25 blue:0.25 alpha:1.0]];
    
    CGFloat frameWidth = 90.0;
    CGFloat frameHeight = 40.0 + helpBubbleView.arrowHeight;
    CGFloat fontSize = 10.0;
    
    BOOL isPad = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad;
    if (isPad) {
        frameWidth *= 1.5;
        frameHeight *= 1.5;
        fontSize *= 1.5;
    }
    
    CGRect inputViewFrame = [[[self class] inputView] frame];
    CGFloat screenWidth = inputViewFrame.size.width;
    
    BOOL isPortrait = YES;
    if (kIsKeymanSystemKeyboard)
        isPortrait = [KeymanInputViewController isPortrait];
    else {
        UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
        isPortrait = UIInterfaceOrientationIsPortrait(orientation);
    }
    
    CGFloat adjY;
    if (isPortrait)
        adjY = kIsKeymanSystemKeyboard?9.0:4.0;
    else
        adjY = kIsKeymanSystemKeyboard?3.0:4.0;
    
    CGFloat px = point.x;
    CGFloat py = point.y + adjY + (isPad?2.0:1.0);
    CGFloat x = px - frameWidth/2;
    CGFloat y = py - frameHeight;
    
    if(x < 0)
        x = 0;
    else if((x+frameWidth) > screenWidth)
        x = screenWidth - frameWidth;
    
    [helpBubbleView setFrame:CGRectMake(x, y, frameWidth, frameHeight)];
    if(x == 0)
        [helpBubbleView setArrowPosX:px];
    else if (x == (screenWidth - frameWidth))
        [helpBubbleView setArrowPosX:(px - x)];
    else
        [helpBubbleView setArrowPosX:frameWidth/2];
    
    UILabel *helpText = [[UILabel alloc] initWithFrame:CGRectMake(5, 0, frameWidth-10, frameHeight - helpBubbleView.arrowHeight)];
    [helpText setBackgroundColor:[UIColor clearColor]];
    [helpText setFont:[helpText.font fontWithSize:fontSize]];
    [helpText setTextAlignment:NSTextAlignmentCenter];
    [helpText setTextColor:[UIColor darkTextColor]];
    [helpText setLineBreakMode:NSLineBreakByWordWrapping];
    [helpText setNumberOfLines:0];
    [helpText setText:kKeymanKeyboardChangeHelpText];
    
    [helpBubbleView addSubview:helpText];
    [webView addSubview:helpBubbleView];
}

#pragma mark - Internal Methods

+ (UIView *)inputView {
    if ([self WKWebViewClass] && enableWebKit) {
        if (webView == nil) {
            WKWebViewConfiguration *config = [[WKWebViewConfiguration alloc] init];
            WKPreferences *prefs = [[WKPreferences alloc] init];
            [prefs setJavaScriptEnabled:YES];
            [config setPreferences:prefs];
            [config setSuppressesIncrementalRendering:NO];
            WKUserContentController *userContentController = [[WKUserContentController alloc] init];
            [userContentController addScriptMessageHandler:[self sharedInstance] name:@"keyman"];
            [config setUserContentController:userContentController];
            CGRect frame = CGRectMake(0.0f, 0.0f, [[self class] keyboardWidth], [[self class] keyboardHeight]);
            webView = [[WKWebView alloc] initWithFrame:frame configuration:config];
            //[(WKWebView *)webView setAutoresizingMask:UIViewAutoresizingFlexibleWidth|UIViewAutoresizingFlexibleHeight];
            [(WKWebView *)webView setOpaque:NO];
            [(WKWebView *)webView setBackgroundColor:[UIColor clearColor]];
            [(WKWebView *)webView setNavigationDelegate:[self sharedInstance]];
            
            KMManager *km = [self sharedInstance];
            [km keymanScrollView].scrollEnabled = NO;
            
            NSURL *codeURL = [NSURL fileURLWithPath:[[km activeKeymanDirectory] stringByAppendingPathComponent:kKeymanFullFileName]];
            SEL sel = NSSelectorFromString(@"loadFileURL:allowingReadAccessToURL:");
            if ([(WKWebView *)webView respondsToSelector:sel]) {
                ((id (*)(id, SEL, id, id))objc_msgSend)((WKWebView *)webView, sel, codeURL, [NSURL URLWithString:[[codeURL absoluteString] stringByDeletingLastPathComponent]]);
            }
            else {
                // Copy Keyman files to the temp folder and load from there
                if ([[self sharedInstance] copyKeymanFilesToTemp]) {
                    codeURL = [NSURL fileURLWithPath:[NSTemporaryDirectory() stringByAppendingPathComponent:[NSString stringWithFormat:@"keyman/%@", kKeymanFullFileName]]];
                    [(WKWebView *)webView loadRequest:[NSURLRequest requestWithURL:codeURL cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60.0]];
                }
            }
        }
        
        return webView;
    }
    else {
        if (webView == nil) {
            CGRect frame = CGRectMake(0.0f, 0.0f, [[self class] keyboardWidth], [[self class] keyboardHeight]);
            webView = [[UIWebView alloc] initWithFrame:frame];
            [(UIWebView *)webView setDataDetectorTypes:UIDataDetectorTypeNone];
            [(UIWebView *)webView setOpaque:NO];
            [(UIWebView *)webView setBackgroundColor:[UIColor clearColor]];
            [(UIWebView *)webView setDelegate:[self sharedInstance]];
            
            KMManager *km = [self sharedInstance];
            [km keymanScrollView].scrollEnabled = NO;
            
            NSURL *codeURL = [NSURL fileURLWithPath:[[km activeKeymanDirectory] stringByAppendingPathComponent:kKeymanFullFileName]];
            [(UIWebView *)webView loadRequest:[NSURLRequest requestWithURL:codeURL cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60.0]];
        }
        
        return webView;
    }
}

+ (Class)WKWebViewClass {
    return NSClassFromString(@"WKWebView");
}

+ (BOOL)isWKWebView:(id)webView {
    if (![self WKWebViewClass])
        return NO;
    
    return [webView isKindOfClass:[self WKWebViewClass]];
}

+ (void)setKMSelectionRange:(NSRange)range manually:(BOOL)manually {
    if (range.location != NSNotFound) {
        NSString *jsString = [NSString stringWithFormat:@"setCursorRange(%lu,%lu)", (unsigned long)range.location, (unsigned long)range.length];
        UIView *webView = [self inputView];
        if ([self isWKWebView:webView])
            [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
        else
            [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
    }
}

+ (void)clearText {
    [self setKMText:nil];
    [self setKMSelectionRange:NSMakeRange(0, 0) manually:YES];
    
    id firstResponder = [[[UIApplication sharedApplication] keyWindow] findFirstResponder];
    [[self sharedInstance] KMLog:[NSString stringWithFormat:@"%@: 0x%x  Cleared text.", [firstResponder class], (uint)firstResponder] checkDebugPrinting:YES];
}

+ (void)setKMText:(NSString *)text {
    NSString *kmText = text?[text stringByReplacingOccurrencesOfString:@"\\" withString:@"\\u005C"]:@"";
    kmText = [kmText stringByReplacingOccurrencesOfString:@"'" withString:@"\\u0027"];
    kmText = [kmText stringByReplacingOccurrencesOfString:@"\n" withString:@"\\n"];
    NSString *jsString = [NSString stringWithFormat:@"setKeymanVal('%@');", kmText];
    UIView *webView = [self inputView];
    if ([self isWKWebView:webView])
        [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
    else
        [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
}

  // This function appears to fetch the keyboard metadata from r.keymanweb.com.
+ (void)fetchKeyboardsWithCompletionBlock:(KMFetchKeyboardsBlock)completionBlock {
    __weak KMManager *_self = [KMManager sharedInstance];
    if (_self.currentRequest != nil) return;
    
    NSString *deviceType = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?@"iphone":@"ipad";
    NSURL *url = [NSURL URLWithString:[NSString stringWithFormat:@"%@languages?device=%@", kKeymanApiBaseURL, deviceType]];
  
    NSDictionary *userData = nil;
  
    if(completionBlock != nil) {
      userData = [NSDictionary dictionaryWithObjectsAndKeys:completionBlock, @"completionBlock", nil];
    }

    _self.currentRequest = [[HTTPDownloadRequest alloc] initWithUrl:url downloadType:DownloadTypeDownloadCachedData userInfo:userData];
  
    [_self.sharedQueue addRequest:_self.currentRequest];
    [_self.sharedQueue run];
}

- (NSArray *)keyboardsForIndex:(NSInteger)index {
    NSArray *retVal = nil;
    if (index < [self.languages count])
        retVal = [[self.languages objectAtIndex:index] objectForKey:kKeymanLanguageKeyboardsKey];
    
    return retVal;
}

- (NSDictionary *)keyboardInfoForLanguageIndex:(NSInteger)languageIndex keyboardIndex:(NSInteger)keyboardIndex {
    NSDictionary *retVal = nil;
    if (languageIndex < [self.languages count]) {
        NSArray *keyboards = [[self.languages objectAtIndex:languageIndex] objectForKey:kKeymanLanguageKeyboardsKey];
        if (keyboardIndex < [keyboards count])
            retVal = [keyboards objectAtIndex:keyboardIndex];
    }
    
    return retVal;
}

- (void)downloadKeyboardFromUrl:(NSURL *)jsonUrl isDirect:(BOOL)isDirect {
    NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                            @"", kKeymanKeyboardIdKey,
                            @"", kKeymanLanguageIdKey,
                            @"", kKeymanKeyboardNameKey,
                            @"", kKeymanLanguageNameKey,
                            @"Y", kKeymanCustomKeyboardKey,
                            nil];
    if ([self.reachability currentReachabilityStatus] == NotReachable) {
        NSError *error = [NSError errorWithDomain:@"Keyman"
                                             code:0
                                         userInfo:[NSDictionary
                                                   dictionaryWithObject:@"No connection"
                                                   forKey:NSLocalizedDescriptionKey]];
        [self downloadFailedForKeyboard:kbInfo error:error];
        return;
    }
    
    NSError *error = nil;
    BOOL failed = YES;
    if (jsonUrl != nil) {
        NSData *data = nil;
        if (isDirect)
            data = [[NSData alloc] initWithContentsOfURL:jsonUrl];
        else {
            NSString *deviceParam = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?@"iphone":@"ipad";
            NSString *encodedUrl = [[jsonUrl absoluteString] stringByAddingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
            NSURL *remoteURL = [NSURL URLWithString:[NSString stringWithFormat:@"%@%@&device=%@", kKeymanApiRemoteURL, encodedUrl, deviceParam]];
            data = [[NSData alloc] initWithContentsOfURL:remoteURL];
        }
        
        if (data != nil) {
            id jsonObj = [NSJSONSerialization JSONObjectWithData:data options:NSJSONReadingMutableContainers error:&error];
            if (error == nil) {
                NSDictionary *jsonDict = (NSDictionary *)jsonObj;
                [self downloadKeyboardFromDictionary:jsonDict];
                failed = NO;
            }
        }
        else {
            error = [NSError errorWithDomain:@"Keyman"
                                        code:0
                                    userInfo:[NSDictionary
                        dictionaryWithObject:@"Failed to fetch JSON file"
                                      forKey:NSLocalizedDescriptionKey]];
        }
    }
    
    if (failed) {
        if (error == nil) {
            error = [NSError errorWithDomain:@"Keyman"
                                             code:0
                                         userInfo:[NSDictionary
                            dictionaryWithObject:@"The keyboard could not be installed"
                                          forKey:NSLocalizedDescriptionKey]];
        }
        [self downloadFailedForKeyboard:kbInfo error:error];
    }
}

- (void)downloadKeyboardFromDictionary:(NSDictionary *)jsonDict {
    NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                            @"", kKeymanKeyboardIdKey,
                            @"", kKeymanLanguageIdKey,
                            @"", kKeymanKeyboardNameKey,
                            @"", kKeymanLanguageNameKey,
                            @"", kKeymanKeyboardVersionKey,
                            @"", kKeymanKeyboardRTLKey,
                            @"Y", kKeymanCustomKeyboardKey,
                            nil];
    
    @try {
        NSDictionary *options = [jsonDict objectForKey:kKeymanOptionsKey];
        NSString *kbBaseUri = [options objectForKey:kKeymanKeyboardBaseURIKey];
        NSString *fontBaseUri = [options objectForKey:kKeymanFontBaseURIKey];
        
        NSDictionary *keyboard = [jsonDict objectForKey:kKeymanKeyboardKey];
        NSString *fileName = [keyboard objectForKey:kKeymanKeyboardFilenameKey];
        NSString *kbID = [keyboard objectForKey:kKeymanIdKey];
        NSString *kbName = [keyboard objectForKey:kKeymanNameKey];
        NSString *kbVersion = [keyboard objectForKey:kKeymanKeyboardVersionKey];
        if (kbVersion == nil)
            kbVersion = @"1.0";
        NSString *isRTL = [keyboard objectForKey:kKeymanKeyboardRTLKey];
        isRTL = (isRTL == nil)?@"N":@"Y";
        NSArray *languages = [keyboard objectForKey:kKeymanLanguagesKey];
        NSDictionary *kbFont = [keyboard objectForKey:kKeymanFontKey];
        NSDictionary *kbOskFont = [keyboard objectForKey:kKeymanOskFontKey];

        NSString *jsFont = [self jsFontFromFontDictionary:kbFont];
        NSString *jsOskFont = kbOskFont?[self jsFontFromFontDictionary:kbOskFont]:nil;

        if (keyboard == nil || fileName == nil || kbID == nil || kbName == nil || languages == nil) {
            NSError *error = [NSError errorWithDomain:@"Keyman"
                                                 code:0
                                             userInfo:[NSDictionary
                                                       dictionaryWithObject:@"The keyboard could not be installed"
                                                       forKey:NSLocalizedDescriptionKey]];
            [self downloadFailedForKeyboard:kbInfo error:error];
            return;
        }
        
        NSDictionary *keyboardInfo = nil;
        NSMutableArray *keyboardsInfoArray = nil;
        if ([languages count] > 1) {
            keyboardsInfoArray = [NSMutableArray arrayWithCapacity:[languages count]];
            for (NSDictionary *language in languages) {
                NSDictionary *info = [NSDictionary dictionaryWithObjectsAndKeys:kbID, kKeymanKeyboardIdKey,
                                      [language objectForKey:kKeymanIdKey], kKeymanLanguageIdKey,
                                      kbName, kKeymanKeyboardNameKey,
                                      [language objectForKey:kKeymanNameKey], kKeymanLanguageNameKey,
                                      kbVersion, kKeymanKeyboardVersionKey,
                                      isRTL, kKeymanKeyboardRTLKey,
                                      @"Y", kKeymanCustomKeyboardKey,
                                      jsFont, kKeymanFontKey,
                                      jsOskFont, kKeymanOskFontKey, nil];
                [keyboardsInfoArray addObject:info];
            }
        }
        else {
            keyboardInfo = [NSDictionary dictionaryWithObjectsAndKeys:kbID, kKeymanKeyboardIdKey,
                            [[languages objectAtIndex:0] objectForKey:kKeymanIdKey], kKeymanLanguageIdKey,
                            kbName, kKeymanKeyboardNameKey,
                            [[languages objectAtIndex:0] objectForKey:kKeymanNameKey], kKeymanLanguageNameKey,
                            kbVersion, kKeymanKeyboardVersionKey,
                            isRTL, kKeymanKeyboardRTLKey,
                            @"Y", kKeymanCustomKeyboardKey,
                            jsFont, kKeymanFontKey,
                            jsOskFont, kKeymanOskFontKey, nil];
        }
        
        if (keyboardsInfoArray != nil)
            kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:keyboardsInfoArray, kKeymanKeyboardInfoKey, nil];
        else
            kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:keyboardInfo, kKeymanKeyboardInfoKey, nil];
        
        if (self.downloadQueue != nil) {
            // Download queue is active.
            NSError *error = [NSError errorWithDomain:@"Keyman"
                                                 code:0
                                             userInfo:[NSDictionary
                                                       dictionaryWithObject:@"Download queue is busy"
                                                       forKey:NSLocalizedDescriptionKey]];
            [self downloadFailedForKeyboard:kbInfo error:error];
            return;
        }
        
        if ([self.reachability currentReachabilityStatus] == NotReachable) {
            NSError *error = [NSError errorWithDomain:@"Keyman"
                                                 code:0
                                             userInfo:[NSDictionary
                                                       dictionaryWithObject:@"No internet connection"
                                                       forKey:NSLocalizedDescriptionKey]];
            [self downloadFailedForKeyboard:kbInfo error:error];
            return;
        }
        
        NSString *keyboardPath = [self keyboardPathForFilename:fileName keyboardVersion:kbVersion];
        NSURL *keyboardURL = [NSURL URLWithString:[kbBaseUri stringByAppendingString:fileName]];
        
        NSMutableArray *keyboardFontURLs;
        id fontSource = nil;
        if (kbFont != nil && kbFont.count) {
            fontSource = [kbFont objectForKey:kKeymanFontSourceKey];
            if (fontSource == nil)
                fontSource = [kbFont objectForKey:kKeymanFontFilenameKey]; // Font filename is deprecated
            if ([fontSource isKindOfClass:[NSArray class]]) {
                keyboardFontURLs = [NSMutableArray arrayWithCapacity:[fontSource count]];
                for (NSString *source in fontSource) {
                    if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                        [keyboardFontURLs addObject:[NSURL URLWithString:[fontBaseUri stringByAppendingString:source]]];
                    }
                }
            }
            else if ([fontSource isKindOfClass:[NSString class]]) {
                keyboardFontURLs = [NSMutableArray arrayWithCapacity:1];
                NSString *source = (NSString *)fontSource;
                if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                    [keyboardFontURLs addObject:[NSURL URLWithString:[fontBaseUri stringByAppendingString:source]]];
                }
            }
            else {
                NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:[NSString stringWithFormat:@"Unexpected error: %@ is not a valid type to be processed.", [fontSource class]] forKey:NSLocalizedDescriptionKey]];
                [self downloadFailedForKeyboard:kbInfo error:error];
                return;
            }
        }

        if (kbOskFont != nil && kbOskFont.count) {
            fontSource = [kbOskFont objectForKey:kKeymanFontSourceKey];
            if (fontSource == nil)
                fontSource = [kbOskFont objectForKey:kKeymanFontFilenameKey]; // Font filename is deprecated
            if ([fontSource isKindOfClass:[NSArray class]]) {
                if (keyboardFontURLs == nil)
                    keyboardFontURLs = [NSMutableArray arrayWithCapacity:[fontSource count]];
                for (NSString *source in fontSource) {
                    if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                        NSURL *fontUrl = [NSURL URLWithString:[fontBaseUri stringByAppendingString:source]];
                        if (![keyboardFontURLs containsObject:fontUrl])
                            [keyboardFontURLs addObject:fontUrl];
                    }
                }
            }
            else if ([fontSource isKindOfClass:[NSString class]]) {
                if (keyboardFontURLs == nil)
                    keyboardFontURLs = [NSMutableArray arrayWithCapacity:1];
                NSString *source = (NSString *)fontSource;
                if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                    NSURL *fontUrl = [NSURL URLWithString:[fontBaseUri stringByAppendingString:source]];
                    if (![keyboardFontURLs containsObject:fontUrl])
                        [keyboardFontURLs addObject:fontUrl];
                }
            }
            else {
                NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:[NSString stringWithFormat:@"Unexpected error: %@ is not a valid type to be processed.", [fontSource class]] forKey:NSLocalizedDescriptionKey]];
                [self downloadFailedForKeyboard:kbInfo error:error];
                return;
            }
        }
        
        BOOL isUpdate = ([self latestKeyboardFileVersionWithID:kbID] != nil);
      
      
        [self setDownloadQueue:[[HTTPDownloader alloc] init:self]];
        NSDictionary *commonUserData = [NSDictionary dictionaryWithObjectsAndKeys:[kbInfo objectForKey:kKeymanKeyboardInfoKey], kKeymanKeyboardInfoKey,
                                       [NSNumber numberWithBool:isUpdate], kKeymanUpdateKey, nil];
      
        [self.downloadQueue setUserInfo:commonUserData];
      
        HTTPDownloadRequest *request = [[HTTPDownloadRequest alloc] initWithUrl:keyboardURL userInfo:commonUserData];
        [request setDestinationFile:keyboardPath];
        [request setTag:0];
      
        [self.downloadQueue addRequest:request];
      
        if (keyboardFontURLs != nil) {
          for (int i=0; i<[keyboardFontURLs count]; i++) {
            NSString *fontPath = [self fontPathForFilename:[[keyboardFontURLs objectAtIndex:i] lastPathComponent]];
          
            request = [[HTTPDownloadRequest alloc] initWithUrl:[[NSURL alloc] initWithString:fontPath] userInfo:commonUserData];
            [request setDestinationFile:fontPath];
            [request setTag:i+1];
            [self.downloadQueue addRequest:request];
          }
        }
      
        [self.downloadQueue run];
    }
    @catch (NSException *exception) {
        if ([[exception description] rangeOfString:@"unrecognized selector"].location != NSNotFound) {
            NSError *error = [NSError errorWithDomain:@"Keyman"
                                                 code:0
                                             userInfo:[NSDictionary
                                                       dictionaryWithObject:@"Invalid JSON file"
                                                       forKey:NSLocalizedDescriptionKey]];
            [self downloadFailedForKeyboard:kbInfo error:error];
        }
        else {
            NSError *error = [NSError errorWithDomain:@"Keyman"
                                                 code:0
                                             userInfo:[NSDictionary
                                                       dictionaryWithObject:[exception description]
                                                       forKey:NSLocalizedDescriptionKey]];
            [self downloadFailedForKeyboard:kbInfo error:error];
        }
    }
    @finally {
        //
    }
}

- (void)downloadKeyboardForLanguageIndex:(NSInteger)languageIndex keyboardIndex:(NSInteger)keyboardIndex isUpdate:(BOOL)isUpdate {
    if (languageIndex >= [self.languages count]) return;
    
    NSArray *keyboards = [[self.languages objectAtIndex:languageIndex] objectForKey:kKeymanLanguageKeyboardsKey];
    if (keyboardIndex >= [keyboards count]) return;
    
    NSDictionary *keyboardDict = [keyboards objectAtIndex:keyboardIndex];
    NSString *kbID = [keyboardDict objectForKey:kKeymanIdKey];
    NSString *langID = [[self.languages objectAtIndex:languageIndex] objectForKey:kKeymanIdKey];
    
    [self downloadKeyboardWithID:kbID languageID:langID isUpdate:isUpdate];
}

- (void)downloadKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID isUpdate:(BOOL)isUpdate {
    NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:keyboardID, kKeymanKeyboardIdKey,
                            languageID, kKeymanLanguageIdKey,
                            nil];
    
    if (self.downloadQueue != nil) {
        // Download queue is active.
        return;
    }
    
    if ([self.reachability currentReachabilityStatus] == NotReachable) {
        NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"No internet connection" forKey:NSLocalizedDescriptionKey]];
        [self downloadFailedForKeyboard:kbInfo error:error];
        return;
    }
    
    if (self.keyboardsInfo == nil) {
        NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"Keyboard info has not yet been fetched. Call -[KMManager fetchKeyboardsList] first." forKey:NSLocalizedDescriptionKey]];
        [self downloadFailedForKeyboard:kbInfo error:error];
        return;
    }
    
    NSString *kbKey = [NSString stringWithFormat:@"%@_%@", languageID, keyboardID];
    NSDictionary *keyboardDict = [self.keyboardsDictionary objectForKey:kbKey];
    if (keyboardDict == nil) {
        NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:[NSString stringWithFormat:@"Keyboard not found with key: %@", kbKey] forKey:NSLocalizedDescriptionKey]];
        [self downloadFailedForKeyboard:kbInfo error:error];
        return;
    }
    
    NSString *urlStr = [[self.keyboardsInfo objectForKey:keyboardID] objectForKey:kKeymanKeyboardURIKey];
    NSURL *keyboardURL = [NSURL URLWithString:urlStr];
    NSMutableArray *keyboardFontURLs = nil;
    NSString *kbFontStr = [keyboardDict objectForKey:kKeymanFontKey];
    NSDictionary *kbFont = [NSJSONSerialization JSONObjectWithData:[kbFontStr dataUsingEncoding:NSUTF8StringEncoding] options:NSJSONReadingMutableContainers error:nil];
    
    id fontSource = nil;
    if (kbFont != nil) {
        fontSource = [kbFont objectForKey:kKeymanFontFilesKey];
        if ([fontSource isKindOfClass:[NSArray class]]) {
            keyboardFontURLs = [NSMutableArray arrayWithCapacity:[fontSource count]];
            for (NSString *source in fontSource) {
                if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                    [keyboardFontURLs addObject:[NSURL URLWithString:[NSString stringWithFormat:@"%@%@", [self.options objectForKey:kKeymanFontBaseURIKey], source]]];
                }
            }
        }
        else if ([fontSource isKindOfClass:[NSString class]]) {
            keyboardFontURLs = [NSMutableArray arrayWithCapacity:1];
            NSString *source = (NSString *)fontSource;
            if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                [keyboardFontURLs addObject:[NSURL URLWithString:[NSString stringWithFormat:@"%@%@", [self.options objectForKey:kKeymanFontBaseURIKey], source]]];
            }
        }
        else {
            NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:[NSString stringWithFormat:@"Unexpected error: %@ is not a valid type to be processed.", [fontSource class]] forKey:NSLocalizedDescriptionKey]];
            [self downloadFailedForKeyboard:kbInfo error:error];
            return;
        }
    }
    
    NSString *kbOskFontStr = [keyboardDict objectForKey:kKeymanOskFontKey];
    if (kbOskFontStr == nil)
        kbOskFontStr = @"";
    
    NSDictionary *kbOskFont = [NSJSONSerialization JSONObjectWithData:[kbOskFontStr dataUsingEncoding:NSUTF8StringEncoding] options:NSJSONReadingMutableContainers error:nil];
    if (kbOskFont != nil) {
        fontSource = [kbOskFont objectForKey:kKeymanFontFilesKey];
        if ([fontSource isKindOfClass:[NSArray class]]) {
            if (keyboardFontURLs == nil)
                keyboardFontURLs = [NSMutableArray arrayWithCapacity:[fontSource count]];
            for (NSString *source in fontSource) {
                if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                    NSURL *fontUrl = [NSURL URLWithString:[NSString stringWithFormat:@"%@%@", [self.options objectForKey:kKeymanFontBaseURIKey], source]];
                    if (![keyboardFontURLs containsObject:fontUrl])
                        [keyboardFontURLs addObject:fontUrl];
                }
            }
        }
        else if ([fontSource isKindOfClass:[NSString class]]) {
            if (keyboardFontURLs == nil)
                keyboardFontURLs = [NSMutableArray arrayWithCapacity:1];
            NSString *source = (NSString *)fontSource;
            if ([source rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [source rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                NSURL *fontUrl = [NSURL URLWithString:[NSString stringWithFormat:@"%@%@", [self.options objectForKey:kKeymanFontBaseURIKey], source]];
                if (![keyboardFontURLs containsObject:fontUrl])
                    [keyboardFontURLs addObject:fontUrl];
            }
        }
        else {
            NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:[NSString stringWithFormat:@"Unexpected error: %@ is not a valid type to be processed.", [fontSource class]] forKey:NSLocalizedDescriptionKey]];
            [self downloadFailedForKeyboard:kbInfo error:error];
            return;
        }
    }
  
    [self setDownloadQueue:[[HTTPDownloader alloc] init:self]];
    NSDictionary *commonUserData = [NSDictionary dictionaryWithObjectsAndKeys:kbInfo, kKeymanKeyboardInfoKey,
                                   [NSNumber numberWithBool:isUpdate], kKeymanUpdateKey, nil];
    [self.downloadQueue setUserInfo:commonUserData];
  
    NSString *kbVersion = [keyboardDict objectForKey:kKeymanKeyboardVersionKey];
    NSString *keyboardPath = [self keyboardPathForFilename:[keyboardURL lastPathComponent] keyboardVersion:kbVersion];
  
    HTTPDownloadRequest *request = [[HTTPDownloadRequest alloc] initWithUrl:keyboardURL userInfo:commonUserData];
    [request setDestinationFile:keyboardPath];
    [request setTag:0];
  
    [self.downloadQueue addRequest:request];
    
    if (keyboardFontURLs != nil) {
        for (int i=0; i<[keyboardFontURLs count]; i++) {
            NSString *fontPath = [self fontPathForFilename:[[keyboardFontURLs objectAtIndex:i] lastPathComponent]];
          
            request = [[HTTPDownloadRequest alloc] initWithUrl:[keyboardFontURLs objectAtIndex:i] userInfo:commonUserData];
            [request setDestinationFile:fontPath];
            [request setTag:i+1];
            [self.downloadQueue addRequest:request];
        }
    }
  
    [self.downloadQueue run];
}

- (void)downloadQueueFinished:(HTTPDownloader *)queue {
    if (self.debugPrintingOn) {
        NSArray *directoryContents = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:[self activeFontDirectory] error:nil];
        [self KMLog:[NSString stringWithFormat:@"Font Directory: %@", directoryContents] checkDebugPrinting:YES];
        NSArray *directoryContents2 = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:[self activeLanguageDirectory] error:nil];
        [self KMLog:[NSString stringWithFormat:@"Language Directory: %@", directoryContents2] checkDebugPrinting:YES];
    }
}
  
- (void)downloadRequestStarted:(HTTPDownloadRequest *)request {
    // If we're downloading a new keyboard.  The extra check is there to filter out other potential request types in the future.
    if (request.tag == 0 && request.typeCode == DownloadTypeDownloadFile) {
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardDownloadStartedNotification
                                                            object:self
                                                          userInfo:[request userInfo]];
    }
}

- (void)downloadRequestFinished:(HTTPDownloadRequest *)request {
  
  if(request.typeCode == DownloadTypeDownloadFile) {
  
    NSString *kbId = nil;
    NSString *langId = nil;
    NSString *kbVersion = nil;
    id kbInfo = [[request userInfo] objectForKey:kKeymanKeyboardInfoKey];
    if ([kbInfo isKindOfClass:[NSDictionary class]]) {
        kbId = [(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardIdKey];
        langId = [(NSDictionary *)kbInfo objectForKey:kKeymanLanguageIdKey];
        kbVersion = [(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardVersionKey];
    }
    else {
        kbId = [[(NSArray *)kbInfo objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
        langId = [[(NSArray *)kbInfo objectAtIndex:0] objectForKey:kKeymanLanguageIdKey];
        kbVersion = [[(NSArray *)kbInfo objectAtIndex:0] objectForKey:kKeymanKeyboardVersionKey];
    }
    BOOL isUpdate = [[[request userInfo] objectForKey:kKeymanUpdateKey] boolValue];
    
    int statusCode = (int)[request responseStatusCodeObjc];
    if (statusCode == 200) { // The request has succeeded.
        if ([self.downloadQueue requestsCount] == 0) { // Download queue finished.
            [self setDownloadQueue:nil];
            [self registerCustomFonts];
            [self KMLog:[NSString stringWithFormat:@"Downloaded keyboard: %@.", kbId] checkDebugPrinting:YES];
            
            [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardDownloadCompletedNotification
                                                                object:self
                                                              userInfo:[request userInfo]];
            if (isUpdate) {
                shouldReloadKeyboard = YES;
                [self reloadKeyboard];
            }
            
            NSUserDefaults *userData = [self activeUserDefaults];
            [userData setObject:[NSArray arrayWithObject:[NSDate date]] forKey:kKeymanSynchronizeSWKeyboardKey];
            [userData synchronize];
        }
    }
    else { // Possible request error (400 Bad Request, 404 Not Found, etc.)
        [self.downloadQueue cancelAllOperations];
        [self setDownloadQueue:nil];
        
        NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:[NSString stringWithFormat:@"%@ : %@", [request responseStatusMessage], [request url]] forKey:NSLocalizedDescriptionKey]];
        [self KMLog:[NSString stringWithFormat:@"Keyboard download failed: %@.", error] checkDebugPrinting:YES];
        
        if (!isUpdate) {
            NSFileManager *fileManager = [NSFileManager defaultManager];
            NSString *fileName = [[request url] lastPathComponent];
            if ([fileName rangeOfString:@".js" options:NSBackwardsSearch].location != NSNotFound) {
                NSString *kbPath = [self keyboardPathForFilename:fileName keyboardVersion:kbVersion];
                if ([fileManager fileExistsAtPath:kbPath])
                    [fileManager removeItemAtPath:kbPath error:nil];
            }
            
            if ([fileName rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [fileName rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                NSString *kbPath = [self keyboardPathForID:kbId keyboardVersion:kbVersion];
                if ([fileManager fileExistsAtPath:kbPath])
                    [fileManager removeItemAtPath:kbPath error:nil];
                
                NSString *fontPath = [[self activeFontDirectory] stringByAppendingPathComponent:fileName];
                if ([fileManager fileExistsAtPath:fontPath])
                    [fileManager removeItemAtPath:fontPath error:nil];
            }
        }
        
        [self downloadFailedForKeyboard:[request userInfo] error:error];
    }
  } else if(request.typeCode == DownloadTypeDownloadCachedData) {
    // TODO:  Implement.
    
    if(request == self.currentRequest) {
      @try {
        NSError *e;
        NSDictionary *responseDict = [NSJSONSerialization JSONObjectWithData:[self.currentRequest rawResponseData] options:NSJSONReadingMutableContainers error:&e];
        self.options = [responseDict objectForKey:kKeymanOptionsKey];
        
        NSSortDescriptor *sort = [NSSortDescriptor sortDescriptorWithKey:kKeymanNameKey ascending:YES selector:@selector(localizedCaseInsensitiveCompare:)];
        self.languages = [[[responseDict objectForKey:kKeymanLanguagesKey] objectForKey:kKeymanLanguagesKey] sortedArrayUsingDescriptors:[NSArray arrayWithObject:sort]];
        [self createKeyboardsInfo];
        
        [self KMLog:[NSString stringWithFormat:@"Request completed -- %lu languages.", (unsigned long)[self.languages count]] checkDebugPrinting:YES];
        
        KMFetchKeyboardsBlock completionBlock;
        if(self.currentRequest.userInfo != nil) {
          completionBlock = [self.currentRequest.userInfo objectForKey:@"completionBlock"];
        }
        
        self.currentRequest = nil;
        
        if (completionBlock != nil) {
          completionBlock(nil);
        }
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanLanguagesUpdatedNotification object:self];
      }
      @catch(NSException *exception) {
        [self KMLog:[NSString stringWithFormat:@"Failed: %@.", exception.reason] checkDebugPrinting:YES];
        
        NSError *error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:exception.reason forKey:NSLocalizedDescriptionKey]];
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanLanguagesDownloadFailedNotification object:self userInfo:[NSDictionary dictionaryWithObject:error forKey:NSUnderlyingErrorKey]];
      }
    }
  }
}

- (void)downloadRequestFailed:(HTTPDownloadRequest *)request {
  if(request.typeCode == DownloadTypeDownloadFile) {
    [self setDownloadQueue:nil];
    
    NSError *error = [request error];
    if (error == nil) error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:nil];
    [self KMLog:[NSString stringWithFormat:@"Keyboard download failed: %@.", error] checkDebugPrinting:YES];
    
    NSString *kbId = nil;
    NSString *kbVersion = nil;
    id kbInfo = [[request userInfo] objectForKey:kKeymanKeyboardInfoKey];
    if ([kbInfo isKindOfClass:[NSDictionary class]]) {
        kbId = [(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardIdKey];
        kbVersion = [(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardVersionKey];
    }
    else {
        kbId = [[(NSArray *)kbInfo objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
        kbVersion = [[(NSArray *)kbInfo objectAtIndex:0] objectForKey:kKeymanKeyboardVersionKey];
    }
    
    BOOL isUpdate = [[[request userInfo] objectForKey:kKeymanUpdateKey] boolValue];
    if (!isUpdate) {
        NSFileManager *fileManager = [NSFileManager defaultManager];
        NSString *fileName = [[request url] lastPathComponent];
        if ([fileName rangeOfString:@".js" options:NSBackwardsSearch].location != NSNotFound) {
            NSString *kbPath = [self keyboardPathForFilename:fileName keyboardVersion:kbVersion];
            if ([fileManager fileExistsAtPath:kbPath])
                [fileManager removeItemAtPath:kbPath error:nil];
        }
        
        if ([fileName rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [fileName rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
            NSString *kbPath = [self keyboardPathForID:kbId keyboardVersion:kbVersion];
            if ([fileManager fileExistsAtPath:kbPath])
                [fileManager removeItemAtPath:kbPath error:nil];
        }
    }
    
    [self downloadFailedForKeyboard:[request userInfo] error:error];
  } else if(request.typeCode == DownloadTypeDownloadCachedData) {
    if(request == self.currentRequest) {
      NSError *error = [self.currentRequest error];
      
      [self KMLog:[NSString stringWithFormat:@"Failed: %@.", error] checkDebugPrinting:YES];
      
      KMFetchKeyboardsBlock completionBlock;
      if(self.currentRequest.userInfo != nil) {
        completionBlock = [self.currentRequest.userInfo objectForKey:@"completionBlock"];
      }
      
      self.currentRequest = nil;
      
      if (completionBlock != nil) {
        completionBlock([NSDictionary dictionaryWithObject:error forKey:NSUnderlyingErrorKey]);
      }
      [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanLanguagesDownloadFailedNotification object:self userInfo:[NSDictionary dictionaryWithObject:error forKey:NSUnderlyingErrorKey]];
    };
  }
}
  
- (void)downloadFailedForKeyboard:(NSDictionary *)keyboardInfo error:(NSError *)error {
    id kbInfo = [keyboardInfo objectForKey:kKeymanKeyboardInfoKey];
    if (kbInfo == nil)
        kbInfo = keyboardInfo;
    
    [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardDownloadFailedNotification
                                                        object:self
                                                      userInfo:[NSDictionary dictionaryWithObjectsAndKeys:
                                                                kbInfo, kKeymanKeyboardInfoKey,
                                                                error, NSUnderlyingErrorKey,
                                                                nil]];
}

- (eKMKeyboardState)stateForKeyboardWithID:(NSString *)keyboardID {
    eKMKeyboardState retVal = kKMKeyboardStateNone;
    
    NSString *latestVersion = [self latestKeyboardFileVersionWithID:keyboardID];
    
    if ([[self keyboardIdForCurrentRequest] isEqualToString:keyboardID]) {
        retVal = kKMKeyboardStateDownloading;
    } else if (latestVersion == nil) {
        retVal = kKMKeyboardStateNeedsDownload;
    }
    else {
        // Check version
        if (self.keyboardsInfo != nil) {
            NSString *kbVersion = [[self.keyboardsInfo objectForKey:keyboardID] objectForKey:kKeymanKeyboardVersionKey];
            if (kbVersion == nil)
                kbVersion = @"1.0";
            
            if ([self compareVersions:latestVersion v2:kbVersion] >= 0)
                retVal = kKMKeyboardStateUpToDate;
            else
                retVal = kKMKeyboardStateNeedsUpdate;
        }
        else
            retVal = kKMKeyboardStateUpToDate;
    }
    
    /* // Check modified date (No longer used)
     NSDictionary *attrs = [fileMan attributesOfItemAtPath:filePath error:nil];
     NSDate *localModDate = [attrs objectForKey:NSFileModificationDate];
     
     NSDate *modDate = [self dateFromKeymanDateString:[[self.keyboardsInfo objectForKey:keyboardID] objectForKey:kKeymanKeyboardModifiedKey]];
     if (modDate != nil && localModDate != nil && [localModDate timeIntervalSinceDate:modDate] < 0)
     retVal = kKMKeyboardStateNeedsUpdate;
     else
     retVal = kKMKeyboardStateUpToDate;
     */
    
    return retVal;
}

- (NSString *)latestKeyboardFileVersionWithID:(NSString *)keyboardID {
    NSString *kbFileVersion = nil;
    NSArray *dirContents = [[NSFileManager defaultManager] contentsOfDirectoryAtPath:[self activeLanguageDirectory] error:nil];
    for (NSString *filename in dirContents) {
        NSInteger index = [filename rangeOfString:[NSString stringWithFormat:@"%@-", keyboardID]].location;
        if (index == 0) {
            NSInteger firstIndex = [filename rangeOfString:@"-" options:NSBackwardsSearch].location+1;
            NSInteger lastIndex = [filename rangeOfString:@".js" options:NSBackwardsSearch].location;
            NSRange range = NSMakeRange(firstIndex, lastIndex - firstIndex);
            NSString *v = [filename substringWithRange:range];
            if (kbFileVersion != nil) {
                if ([self compareVersions:v v2:kbFileVersion] > 0)
                    kbFileVersion = [v copy];
            }
            else if ([self compareVersions:v v2:v] == 0)
                kbFileVersion = [v copy];
        }
    }
    
    return kbFileVersion;
}

- (NSInteger)compareVersions:(NSString *)v1 v2:(NSString *)v2 {
    // returns;
    // -2 if v1 or v2 is invalid
    // 0 if v1 = v2
    // -1 if v1 < v2
    // 1 if v1 > v2
    
    if (v1 == nil || v2 == nil)
        return -2;
    
    if (!v1.length || !v2.length)
        return -2;
    
    NSArray *v1Values = [v1 componentsSeparatedByString:@"."];
    NSArray *v2Values = [v2 componentsSeparatedByString:@"."];
    
    NSUInteger len = (v1Values.count >= v2Values.count?v1Values.count:v2Values.count);
    for (int i = 0; i < len; i++) {
        NSString *vStr1 = @"0";
        if (i < v1Values.count)
            vStr1 = [v1Values objectAtIndex:i];
        
        NSString *vStr2 = @"0";
        if (i < v2Values.count)
            vStr2 = [v2Values objectAtIndex:i];
        
        NSNumberFormatter *numFormatter = [[NSNumberFormatter alloc] init];
        NSNumber *vInt1 = [numFormatter numberFromString:vStr1];
        NSNumber *vInt2 = [numFormatter numberFromString:vStr2];
        int iV1, iV2, iV1_, iV2_;
        
        if (vInt1 != nil) { iV1 = [vInt1 intValue]; iV1_ = 0; }
        else { iV1 = 0; iV1_ = 0; }
        
        if (vInt2 != nil) { iV2 = [vInt2 intValue]; iV2_ = 0; }
        else { iV2 = 0; iV2_ = 0; }
        
        if (vInt1 == nil) {
            if (i != (v1Values.count - 1))
                return -2;
            
            if ([vStr1.lowercaseString hasSuffix:@"b"]) {
                NSNumber *vInt1_ = [numFormatter numberFromString:[vStr1 substringToIndex:vStr1.length-1]];
                if (vInt1_ == nil)
                    return -2;
                
                iV1 = [vInt1_ intValue];
                iV1_ = -100;
            } else if ([vStr1.lowercaseString hasSuffix:@"a"]) {
                NSNumber *vInt1_ = [numFormatter numberFromString:[vStr1 substringToIndex:vStr1.length-1]];
                if (vInt1_ == nil)
                    return -2;
                
                iV1 = [vInt1_ intValue];
                iV1_ = -200;
            } else
                return -2;
        }
        
        if (vInt2 == nil) {
            if (i != (v2Values.count - 1))
                return -2;
            
            if ([vStr2.lowercaseString hasSuffix:@"b"]) {
                NSNumber *vInt2_ = [numFormatter numberFromString:[vStr2 substringToIndex:vStr2.length-1]];
                if (vInt2_ == nil)
                    return -2;
                
                iV2 = [vInt2_ intValue];
                iV2_ = -100;
            }
            else if ([vStr2.lowercaseString hasSuffix:@"a"]) {
                NSNumber *vInt2_ = [numFormatter numberFromString:[vStr2 substringToIndex:vStr2.length-1]];
                if (vInt2_ == nil)
                    return -2;
                
                iV2 = [vInt2_ intValue];
                iV2_ = -200;
            }
            else
                return -2;
        }
        
        if (iV1 == iV2) {
            if (iV1_ == iV2_)
                continue;
            if (iV1_ < iV2_)
                return -1;
            if (iV1_ > iV2_)
                return 1;
        }
        else if (iV1 < iV2) {
            return  -1;
        }
        else if (iV1 > iV2) {
            return 1;
        }
    }
    
    return 0;
}

- (NSString *)keyboardIdForCurrentRequest {
    NSString *retVal = nil;
    if (self.currentRequest != nil) {
        NSString *tmpStr = [[self.currentRequest.url absoluteString] lastPathComponent];
        NSRange dotRange = [tmpStr rangeOfString:@".js" options:NSBackwardsSearch];
        if (dotRange.location != NSNotFound) {
            retVal = [tmpStr substringToIndex:dotRange.location];
        }
    }
    else if (self.downloadQueue != nil) {
        NSString *kbId;
        id kbInfo = [[self.downloadQueue userInfo] objectForKey:kKeymanKeyboardInfoKey];
        if ([kbInfo isKindOfClass:[NSDictionary class]])
            kbId = [(NSDictionary *)kbInfo objectForKey:kKeymanKeyboardIdKey];
        else
            kbId = [[(NSArray *)kbInfo objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
        
        retVal = kbId;
    }
    
    return retVal;
}

+ (BOOL)retinaScreen {
    return ([[UIScreen mainScreen] respondsToSelector:@selector(displayLinkWithTarget:selector:)] && ([UIScreen mainScreen].scale == 2.0));
}

+ (CGFloat)keyboardHeight {
    BOOL isPortrait = YES;
    if (kIsKeymanSystemKeyboard)
        isPortrait = [KeymanInputViewController isPortrait];
    else {
        UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
        isPortrait = UIInterfaceOrientationIsPortrait(orientation);
    }
    
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad) {
        if (isPortrait) {
            return kIsKeymanSystemKeyboard?kKeymanPadPortraitSystemKeyboardHeight:kKeymanPadPortraitInAppKeyboardHeight;
        }
        else {
            return kIsKeymanSystemKeyboard?kKeymanPadLandscapeSystemKeyboardHeight:kKeymanPadLandscapeInAppKeyboardHeight;
        }
    }
    else {
        if (isPortrait) {
            return kIsKeymanSystemKeyboard?kKeymanPhonePortraitSystemKeyboardHeight:kKeymanPhonePortraitInAppKeyboardHeight;
        }
        else {
            return kIsKeymanSystemKeyboard?kKeymanPhoneLandscapeSystemKeyboardHeight:kKeymanPhoneLandscapeInAppKeyboardHeight;
        }
    }
}

#pragma mark - Private Methods

+ (CGFloat)keyboardHeightWithOrientation:(UIInterfaceOrientation)orientation {
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad) {
        if (UIInterfaceOrientationIsPortrait(orientation)) {
            return kIsKeymanSystemKeyboard?kKeymanPadPortraitSystemKeyboardHeight:kKeymanPadPortraitInAppKeyboardHeight;
        }
        else {
            return kIsKeymanSystemKeyboard?kKeymanPadLandscapeSystemKeyboardHeight:kKeymanPadLandscapeInAppKeyboardHeight;
        }
    }
    else {
        if (UIInterfaceOrientationIsPortrait(orientation)) {
            return kIsKeymanSystemKeyboard?kKeymanPhonePortraitSystemKeyboardHeight:kKeymanPhonePortraitInAppKeyboardHeight;
        }
        else {
            return kIsKeymanSystemKeyboard?kKeymanPhoneLandscapeSystemKeyboardHeight:kKeymanPhoneLandscapeInAppKeyboardHeight;
        }
    }
}

+ (CGFloat)keyboardWidth {
    CGSize screenSize = [[UIScreen mainScreen] bounds].size;
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion >= 8)
        return screenSize.width;
    
    UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
    if (UIInterfaceOrientationIsPortrait(orientation))
        return screenSize.width;
    else
        return screenSize.height;
}

- (void)resizeKeyboard {
    static CGFloat xWidth = 0;
    static CGFloat xHeight = 0;
    CGFloat kbWidth = [[self class] keyboardWidth];
    CGFloat kbHeight = [[self class] keyboardHeight];
    
    if (didResizeToOrientation && kIsKeymanSystemKeyboard && xWidth == kbWidth && xHeight == kbHeight) {
        didResizeToOrientation = NO;
        return;
    }
    else {
        xWidth = kbWidth;
        xHeight = kbHeight;
    }
    
    [webView setFrame:CGRectMake(0.0f, 0.0f, kbWidth, kbHeight)];
    
    // Workaround for WKWebView bug with landscape orientation
    if (kIsKeymanSystemKeyboard && [[self class] isWKWebView:webView])
        [self performSelector:@selector(resizeDelay) withObject:self afterDelay:1.0];
    
    NSInteger oskHeight = kbHeight;
    if (!kIsKeymanSystemKeyboard)
        oskHeight = oskHeight - oskHeight%20;
    else
        oskHeight = oskHeight - oskHeight%10;
    
    NSString *jsString = [NSString stringWithFormat:@"setOskWidth(%ld);", (long)kbWidth];
    if ([[self class] isWKWebView:webView])
        [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
    else
        [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
    
    jsString = [NSString stringWithFormat:@"setOskHeight(%ld);", (long)oskHeight];
    if ([[self class] isWKWebView:webView])
        [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
    else
        [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
}

- (void)resizeDelay {
    // + 1000 to work around iOS bug with resizing on landscape orientation. Technically we only
    // need this for landscape but it doesn't hurt to do it with both. 1000 is a big number that
    // should hopefully work on all devices.
    CGFloat kbWidth = [[self class] keyboardWidth];
    CGFloat kbHeight = [[self class] keyboardHeight];
    [webView setFrame:CGRectMake(0.0f, 0.0f, kbWidth, kbHeight + 1000)];
}

- (void)resizeKeyboardIfNeeded {
    static CGFloat xWidth = 0;
    static CGFloat xHeight = 0;
    CGFloat kbWidth = [[self class] keyboardWidth];
    CGFloat kbHeight = [[self class] keyboardHeight];
    if (kbWidth != xWidth || kbHeight != xHeight) {
        [self resizeKeyboard];
        xWidth = kbWidth;
        xHeight = kbHeight;
    }
}

- (void)resizeKeyboardWithOrientation:(UIInterfaceOrientation)orientation {
    CGFloat kbWidth = [[self class] keyboardWidth];
    CGFloat kbHeight = [[self class] keyboardHeightWithOrientation:orientation];
    [webView setFrame:CGRectMake(0.0f, 0.0f, kbWidth, kbHeight)];
    
    NSInteger oskHeight = kbHeight;
    if (!kIsKeymanSystemKeyboard)
        oskHeight = oskHeight - oskHeight%20;
    else
        oskHeight = oskHeight - oskHeight%10;
    
    NSString *jsString = [NSString stringWithFormat:@"setOskWidth(%ld);", (long)kbWidth];
    if ([[self class] isWKWebView:webView])
        [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
    else
        [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
    
    jsString = [NSString stringWithFormat:@"setOskHeight(%ld);", (long)oskHeight];
    if ([[self class] isWKWebView:webView])
        [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
    else
        [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
}

- (UIScrollView *)keymanScrollView {
    if ([[self class] isWKWebView:webView])
        return [(WKWebView *)webView scrollView];
    else
        return [(UIWebView *)webView scrollView];
}

- (BOOL)canAccessSharedContainer {
    if ([self sharedKeymanDirectory] == nil)
        return NO;
    
    if (!kIsKeymanSystemKeyboard)
        return YES;
    
    NSFileManager *fileMan = [NSFileManager defaultManager];
    NSString *sharedKeymanDir = [NSString stringWithString:[self sharedKeymanDirectory]];
    NSString *path = [sharedKeymanDir stringByAppendingPathComponent:kKeymanFullFileName];
    BOOL canAccess = [fileMan fileExistsAtPath:path];
    
    return canAccess;
}

- (void)copyUserDefaultsToSharedContainer {
    NSUserDefaults *sharedUserData = nil;
    if ([[NSUserDefaults standardUserDefaults] respondsToSelector:@selector(initWithSuiteName:)])
        sharedUserData = [[NSUserDefaults alloc] initWithSuiteName:applicationGroupIdentifier];
    
    if (sharedUserData == nil)
        return;
    
    NSUserDefaults *defaultUserData = [NSUserDefaults standardUserDefaults];
    if ([sharedUserData objectForKey:kKeymanUserKeyboardsListKey] == nil)
        [sharedUserData setObject:[defaultUserData objectForKey:kKeymanUserKeyboardsListKey]
                           forKey:kKeymanUserKeyboardsListKey];
    
    if ([sharedUserData objectForKey:kKeymanUserCurrentKeyboardKey] == nil)
        [sharedUserData setObject:[defaultUserData objectForKey:kKeymanUserCurrentKeyboardKey]
                           forKey:kKeymanUserCurrentKeyboardKey];
    
    if ([sharedUserData objectForKey:kKeymanEngineVersion] == nil)
        [sharedUserData setObject:[defaultUserData objectForKey:kKeymanEngineVersion]
                           forKey:kKeymanEngineVersion];
    
    if ([sharedUserData objectForKey:kKeymanKeyboardPickerDisplayedKey] == nil)
        [sharedUserData setObject:[defaultUserData objectForKey:kKeymanKeyboardPickerDisplayedKey]
                           forKey:kKeymanKeyboardPickerDisplayedKey];
    
    [sharedUserData synchronize];
}

- (void)copyUserDefaultsFromSharedContainer {
    NSUserDefaults *sharedUserData = nil;
    if ([[NSUserDefaults standardUserDefaults] respondsToSelector:@selector(initWithSuiteName:)])
        sharedUserData = [[NSUserDefaults alloc] initWithSuiteName:applicationGroupIdentifier];
    
    if (sharedUserData == nil)
        return;
    
    NSUserDefaults *defaultUserData = [NSUserDefaults standardUserDefaults];
    if ([sharedUserData objectForKey:kKeymanUserKeyboardsListKey] != nil)
        [defaultUserData setObject:[sharedUserData objectForKey:kKeymanUserKeyboardsListKey]
                            forKey:kKeymanUserKeyboardsListKey];
    
    if ([sharedUserData objectForKey:kKeymanEngineVersion] != nil)
        [defaultUserData setObject:[sharedUserData objectForKey:kKeymanEngineVersion]
                            forKey:kKeymanEngineVersion];
    
    [defaultUserData synchronize];
}

- (BOOL)addSkipBackupAttributeToItemAtPath:(NSString *) filePathString
{
    NSURL* URL= [NSURL fileURLWithPath: filePathString];
    assert([[NSFileManager defaultManager] fileExistsAtPath: [URL path]]);
    
    NSError *error = nil;
    BOOL success = [URL setResourceValue: [NSNumber numberWithBool: YES]
                                  forKey: NSURLIsExcludedFromBackupKey error: &error];
    if(!success){
        NSLog(@"Error excluding %@ from backup %@", [URL lastPathComponent], error);
    }
    return success;
}

- (BOOL)copyAndExcludeFromBackup:(NSString*)pathToCopy destinationPath:(NSString*)destinationPath {
    NSFileManager *fileMan = [NSFileManager defaultManager];
    BOOL isDirectory;
    BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
    NSError *error;
    if (fileExists && !isDirectory) {
        // copy if destination does not exist or replace if source is newer
        if (![fileMan fileExistsAtPath:destinationPath])
            [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
        else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
            [fileMan removeItemAtPath:destinationPath error:&error];
            [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
        }
        else
            return false;
        [self addSkipBackupAttributeToItemAtPath:destinationPath];
        return true;
    }
    return false;
}

- (void)copyKeymanFilesToSharedContainer {
    if ([self sharedKeymanDirectory] == nil)
        return;
    
    NSFileManager *fileMan = [NSFileManager defaultManager];
    NSString *defaultKeymanDir = [NSString stringWithString:[self defaultKeymanDirectory]];
    NSString *sharedKeymanDir = [NSString stringWithString:[self sharedKeymanDirectory]];
    NSString *defaultLangDir = [NSString stringWithString:[self defaultLanguageDirectory]];
    NSString *sharedLangDir = [NSString stringWithString:[self sharedLanguageDirectory]];
    NSString *defaultFontDir = [NSString stringWithString:[self defaultFontDirectory]];
    NSString *sharedFontDir = [NSString stringWithString:[self sharedFontDirectory]];
    //NSError *error;
    
    //BOOL isDirectory;
    NSArray *dirContents = [fileMan contentsOfDirectoryAtPath:defaultKeymanDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [defaultKeymanDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [sharedKeymanDir stringByAppendingPathComponent:file];
        
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
/*        BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }
        }*/
    }
    
    dirContents = [fileMan contentsOfDirectoryAtPath:defaultLangDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [defaultLangDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [sharedLangDir stringByAppendingPathComponent:file];

        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
/*        BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }
        }*/
    }
    
    dirContents = [fileMan contentsOfDirectoryAtPath:defaultFontDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [defaultFontDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [sharedFontDir stringByAppendingPathComponent:file];
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
/*        BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }
        }*/
    }
}

- (void)copyKeymanFilesFromSharedContainer {
    if ([self sharedKeymanDirectory] == nil)
        return;
    
    NSFileManager *fileMan = [NSFileManager defaultManager];
    NSString *defaultKeymanDir = [NSString stringWithString:[self defaultKeymanDirectory]];
    NSString *sharedKeymanDir = [NSString stringWithString:[self sharedKeymanDirectory]];
    NSString *defaultLangDir = [NSString stringWithString:[self defaultLanguageDirectory]];
    NSString *sharedLangDir = [NSString stringWithString:[self sharedLanguageDirectory]];
    NSString *defaultFontDir = [NSString stringWithString:[self defaultFontDirectory]];
    NSString *sharedFontDir = [NSString stringWithString:[self sharedFontDirectory]];

    NSArray *dirContents = [fileMan contentsOfDirectoryAtPath:sharedKeymanDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [sharedKeymanDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [defaultKeymanDir stringByAppendingPathComponent:file];
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
        /*BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }
        }*/
    }
    
    dirContents = [fileMan contentsOfDirectoryAtPath:sharedLangDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [sharedLangDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [defaultLangDir stringByAppendingPathComponent:file];
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
/*        BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }
        }*/
    }
    
    dirContents = [fileMan contentsOfDirectoryAtPath:sharedFontDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [sharedFontDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [defaultFontDir stringByAppendingPathComponent:file];
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
        /*BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }
        }*/
    }
    
    [self registerCustomFonts];
}

- (void)copyWebFilesToLibrary {
  
    NSString *libraryDirectory = [self activeKeymanDirectory];
    if (![libraryDirectory length]) {
        [self KMLog:@"Could not locate library directory! Could not copy Keyman files." checkDebugPrinting:NO];
        return;
    }
    
    NSError *error = nil;
    NSBundle *keymanBundle = [NSBundle bundleWithPath:[[NSBundle bundleForClass:[self class]] pathForResource:@"Keyman" ofType:@"bundle"]];
    NSString *pathToCopy = [keymanBundle pathForResource:kKeymanFileName ofType:kKeymanFileExtension];
    NSString *destinationPath = nil;
    if ([pathToCopy length]) {
        destinationPath = [libraryDirectory stringByAppendingPathComponent:kKeymanFullFileName];
        // copy if destination does not exist or replace if source is newer
        
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
        
        /*if (![fileMan fileExistsAtPath:destinationPath]) {
            [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            NSURL *URL = [NSURL fileURLWithPath:destinationPath];
            [URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
        }
        else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
            [fileMan removeItemAtPath:destinationPath error:nil];
            [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            NSURL *URL = [NSURL fileURLWithPath:destinationPath];
            [URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
        }*/
    }
    else {
        error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"Could not locate Keyman html file for copying." forKey:NSLocalizedDescriptionKey]];
    }
    
    if (error == nil) {
        pathToCopy = [keymanBundle pathForResource:kKeymaniOSCodeFileName ofType:nil];
        if ([pathToCopy length]) {
            destinationPath = [libraryDirectory stringByAppendingPathComponent:[pathToCopy lastPathComponent]];
            
            [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
            /*// copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath]) {
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
                //NSURL *URL = [NSURL fileURLWithPath:destinationPath];
                //[URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
            }
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:nil];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
                //NSURL *URL = [NSURL fileURLWithPath:destinationPath];
                //[URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
            }*/
        }
        else {
            error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"Could not locate Keyman javascript for copying." forKey:NSLocalizedDescriptionKey]];
        }
    }
    
    if (error == nil) {
        pathToCopy = [keymanBundle pathForResource:[NSString stringWithFormat:@"%@-1.6.js", kKeymanDefaultKeyboardID] ofType:nil];
        if ([pathToCopy length]) {
            destinationPath = [[self activeLanguageDirectory] stringByAppendingPathComponent:[pathToCopy lastPathComponent]];
            [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
            /*// copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath]) {
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
                //NSURL *URL = [NSURL fileURLWithPath:destinationPath];
                //[URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
            }
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:nil];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
                //NSURL *URL = [NSURL fileURLWithPath:destinationPath];
                //[URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
            }*/
        }
        else {
            error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"Could not locate default Keyman keyboard for copying." forKey:NSLocalizedDescriptionKey]];
        }
    }
    
    if (error == nil) {
        pathToCopy = [keymanBundle pathForResource:@"DejaVuSans" ofType:@"ttf"];
        if ([pathToCopy length]) {
            destinationPath = [[self activeFontDirectory] stringByAppendingPathComponent:[pathToCopy lastPathComponent]];
            [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
            /*// copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath]) {
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
                //NSURL *URL = [NSURL fileURLWithPath:destinationPath];
                //[URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
            }
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:nil];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
                //NSURL *URL = [NSURL fileURLWithPath:destinationPath];
                //[URL setResourceValue:@(YES) forKey:NSURLIsExcludedFromBackupKey error:nil];
            }*/
        }
        else {
            error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"Could not locate DejaVuSans.ttf for copying." forKey:NSLocalizedDescriptionKey]];
        }
    }
    
    if (error == nil) {
        pathToCopy = [keymanBundle pathForResource:@"kmwosk" ofType:@"css"];
        if ([pathToCopy length]) {
            destinationPath = [libraryDirectory stringByAppendingPathComponent:[pathToCopy lastPathComponent]];
            [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
            /*// copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath]) {
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            }
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:nil];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }*/
        }
        else {
            error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"Could not locate kmwosk.css for copying." forKey:NSLocalizedDescriptionKey]];
        }
    }
    
    if (error == nil) {
        pathToCopy = [keymanBundle pathForResource:@"keymanweb-osk" ofType:@"ttf"];
        if ([pathToCopy length]) {
            destinationPath = [libraryDirectory stringByAppendingPathComponent:[pathToCopy lastPathComponent]];
            [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
            /*// copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:nil];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
            }*/
        }
        else {
            error = [NSError errorWithDomain:@"Keyman" code:0 userInfo:[NSDictionary dictionaryWithObject:@"Could not locate keymanweb-osk.ttf for copying." forKey:NSLocalizedDescriptionKey]];
        }
    }
    
    if (error != nil)
        [self KMLog:[NSString stringWithFormat:@"Error copying Keyman files: %@", error] checkDebugPrinting:NO];
}

- (NSInteger)compareFileModDates:(NSString *)srcPath destinationPath:(NSString *)desPath {
    // returns;
    // -2 on error
    // 0 if srcModDate = desModDate
    // -1 if srcModDate < desModDate
    // 1 if srcModDate > desModDate
    NSError *e;
    NSDictionary *srcAttrs = [[NSFileManager defaultManager] attributesOfItemAtPath:srcPath error:&e];
    if (e != nil) return -2;
    NSDictionary *desAttrs = [[NSFileManager defaultManager] attributesOfItemAtPath:desPath error:&e];
    if (e != nil) return -2;
    
    NSDate *srcModDate = [srcAttrs fileModificationDate];
    NSDate *desModDate = [desAttrs fileModificationDate];
    NSComparisonResult result = [srcModDate compare:desModDate];
    if (result == NSOrderedDescending)
        return 1;
    else if (result == NSOrderedAscending)
        return -1;
    else
        return 0;
}

- (void)renameOldKeyboardFiles {
    NSFileManager *fileManager = [NSFileManager defaultManager];
    NSString *langDir = [self activeLanguageDirectory];
    NSArray *dirContents = [fileManager contentsOfDirectoryAtPath:langDir error:nil];
    for (NSString *filename in dirContents) {
        if ([filename rangeOfString:@"-"].location == NSNotFound) {
            if ([filename isEqualToString:@"us.js"]) {
                NSString *filePath = [[self activeLanguageDirectory] stringByAppendingPathComponent:filename];
                [fileManager removeItemAtPath:filePath error:nil];
                
                NSUserDefaults *userData = [self activeUserDefaults];
                NSDictionary *curKB = [userData objectForKey:kKeymanUserCurrentKeyboardKey];
                if ([[curKB objectForKey:kKeymanKeyboardIdKey] isEqualToString:@"us"]) {
                    [userData removeObjectForKey:kKeymanUserCurrentKeyboardKey];
                    [userData synchronize];
                }
            }
            else {
                NSString *newFilename = [NSString stringWithFormat:@"%@-1.0.js", [filename substringToIndex:[filename length]-3]];
                NSString *filePath = [langDir stringByAppendingPathComponent:filename];
                NSString *newFilePath = [langDir stringByAppendingPathComponent:newFilename];
                [fileManager moveItemAtPath:filePath toPath:newFilePath error:nil];
            }
        }
        else {
            NSString *oldFilename = [NSString stringWithFormat:@"%@.js", [filename substringToIndex:[filename rangeOfString:@"-"].location]];
            NSString *oldFilePath = [langDir stringByAppendingPathComponent:oldFilename];
            if ([fileManager fileExistsAtPath:oldFilePath])
                [fileManager removeItemAtPath:oldFilePath error:nil];
        }
    }
}

- (NSString *)defaultKeymanDirectory {
    NSString *keymanDir = nil;
    NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, YES);
    if ([paths count]) {
        keymanDir = [[paths objectAtIndex:0] stringByAppendingPathComponent:@"keyman"];
        if ([keymanDir length]) {
            [[NSFileManager defaultManager] createDirectoryAtPath:keymanDir withIntermediateDirectories:YES attributes:nil error:nil];
            
        }
    }

    return keymanDir;
}

- (NSString *)defaultLanguageDirectory {
    NSString *langDir = nil;
    langDir = [[self defaultKeymanDirectory] stringByAppendingPathComponent:@"languages"];
    if ([langDir length]) {
        [[NSFileManager defaultManager] createDirectoryAtPath:langDir withIntermediateDirectories:YES attributes:nil error:nil];
    }
    
    return langDir;
}

- (NSString *)defaultFontDirectory {
    NSString *fontDir = nil;
    fontDir = [[self defaultKeymanDirectory] stringByAppendingPathComponent:@"fonts"];
    if ([fontDir length]) {
        [[NSFileManager defaultManager] createDirectoryAtPath:fontDir withIntermediateDirectories:YES attributes:nil error:nil];
    }
    
    return fontDir;
}

+ (void)setApplicationGroupIdentifier:(NSString *)groupID {
    applicationGroupIdentifier = [NSString stringWithString:groupID];
}

- (NSString *)sharedContainerPath {
    NSString *path = nil;
    NSFileManager *fileMan = [NSFileManager defaultManager];
    
    if ([fileMan respondsToSelector:@selector(containerURLForSecurityApplicationGroupIdentifier:)])
        path = [[fileMan containerURLForSecurityApplicationGroupIdentifier:applicationGroupIdentifier] path];
    
    return path;
}

- (NSString *)sharedKeymanDirectory {
    NSString *sharedKeymanDirectory = nil;
    NSString *sharedPath = [self sharedContainerPath];
    if (sharedPath != nil) {
        sharedKeymanDirectory = [sharedPath stringByAppendingPathComponent:@"keyman"];
        if ([sharedKeymanDirectory length]) {
            [[NSFileManager defaultManager] createDirectoryAtPath:sharedKeymanDirectory withIntermediateDirectories:YES attributes:nil error:nil];
        }
    }
    
    return sharedKeymanDirectory;
}

- (NSString *)sharedLanguageDirectory {
    NSString *sharedLanguageDirectory = nil;
    sharedLanguageDirectory = [[self sharedKeymanDirectory] stringByAppendingPathComponent:@"languages"];
    if ([sharedLanguageDirectory length]) {
        [[NSFileManager defaultManager] createDirectoryAtPath:sharedLanguageDirectory withIntermediateDirectories:YES attributes:nil error:nil];
    }
    
    return sharedLanguageDirectory;
}

- (NSString *)sharedFontDirectory {
    NSString *sharedFontDirectory = nil;
    sharedFontDirectory = [[self sharedKeymanDirectory] stringByAppendingPathComponent:@"fonts"];
    if ([sharedFontDirectory length]) {
        [[NSFileManager defaultManager] createDirectoryAtPath:sharedFontDirectory withIntermediateDirectories:YES attributes:nil error:nil];
    }
    
    return sharedFontDirectory;
}

- (NSString *)activeKeymanDirectory {
    NSString *activeKeymanDirectory = nil;
    if ([self canAccessSharedContainer])
        activeKeymanDirectory = [self sharedKeymanDirectory];
    else
        activeKeymanDirectory = [self defaultKeymanDirectory];
    
    return activeKeymanDirectory;
}

- (NSString *)activeLanguageDirectory {
    NSString *activeLanguageDirectory = nil;
    if ([self canAccessSharedContainer])
        activeLanguageDirectory = [self sharedLanguageDirectory];
    else
        activeLanguageDirectory = [self defaultLanguageDirectory];
    
    return activeLanguageDirectory;
}

- (NSString *)activeFontDirectory {
    NSString *activeFontDirectory = nil;
    if ([self canAccessSharedContainer])
        activeFontDirectory = [self sharedFontDirectory];
    else
        activeFontDirectory = [self defaultFontDirectory];
    
    return activeFontDirectory;
}

- (NSUserDefaults *)activeUserDefaults {
    NSUserDefaults *userDefaults = nil;
    if ([self canAccessSharedContainer])
        userDefaults = [[NSUserDefaults alloc] initWithSuiteName:applicationGroupIdentifier];
    else
        userDefaults = [NSUserDefaults standardUserDefaults];
    
    return userDefaults;
}

- (NSString *)keyboardPathForID:(NSString *)keyboardID keyboardVersion:(NSString *)kbVersion {
    NSString *retVal = nil;
    
    if ([keyboardID length]) {
        if (kbVersion == nil || ![kbVersion length]) {
            kbVersion = [self latestKeyboardFileVersionWithID:keyboardID];
            if (kbVersion == nil)
                return retVal;
        }
        
        retVal = [[[self activeLanguageDirectory] stringByAppendingPathComponent:keyboardID] stringByAppendingString:[NSString stringWithFormat:@"-%@%@", kbVersion, @".js"]];
    }
    
    return retVal;
}

- (NSString *)keyboardPathForFilename:(NSString *)filename keyboardVersion:(NSString *)kbVersion {
    NSString *retVal = nil;
    if ([filename length]) {
        if ([filename rangeOfString:@"-" options:NSBackwardsSearch].location == NSNotFound) {
            NSString *fn = [NSString stringWithFormat:@"%@-%@.js", [filename substringToIndex:filename.length-3], (kbVersion != nil)?kbVersion:@"1.0"];
            retVal = [[self activeLanguageDirectory] stringByAppendingPathComponent:fn];
        }
        else
            retVal = [[self activeLanguageDirectory] stringByAppendingPathComponent:filename];
    }
    
    return retVal;
}

- (NSString *)fontPathForFilename:(NSString *)filename {
    NSString *retVal = nil;
    if ([filename length]) {
        retVal = [[self activeFontDirectory] stringByAppendingPathComponent:filename];
    }
    return retVal;
}

- (void)createKeyboardsInfo {
    NSString *langId = nil;
    NSString *kbID = nil;
    NSString *kbVersion = nil;
    NSString *isRTL = nil;
    NSString *dictKey = nil;
    NSString *kbBaseUri = [self.options objectForKey:kKeymanKeyboardBaseURIKey];
    NSString *kbUri = nil;
    NSDictionary *langDict = nil;
    NSDictionary *kbDict = nil;
    NSArray *keyboards = nil;
    
    keyboardsInfo_ = [[NSMutableDictionary alloc] initWithCapacity:50];
    keyboardsDictionary_ = [[NSMutableDictionary alloc] initWithCapacity:50];
    for (langDict in self.languages) {
        keyboards = [langDict objectForKey:kKeymanLanguageKeyboardsKey];
        for (kbDict in keyboards) {
            langId = [langDict objectForKey:kKeymanIdKey];
            kbID = [kbDict objectForKey:kKeymanIdKey];
            kbVersion = [kbDict objectForKey:kKeymanKeyboardVersionKey];
            if (kbVersion == nil)
                kbVersion = @"1.0";
            
            isRTL = [kbDict objectForKey:kKeymanKeyboardRTLKey];
            isRTL = (isRTL == nil)?@"N":@"Y";
            
            dictKey = [NSString stringWithFormat:@"%@_%@",langId, kbID];
            
            if ([self.keyboardsInfo objectForKey:kbID] == nil) {
                kbUri = [NSString stringWithFormat:@"%@%@", kbBaseUri, [kbDict objectForKey:kKeymanKeyboardFilenameKey]];
                [self.keyboardsInfo setObject:[NSDictionary dictionaryWithObjectsAndKeys:
                                               [kbDict objectForKey:kKeymanNameKey], kKeymanKeyboardNameKey,
                                               kbVersion, kKeymanKeyboardVersionKey,
                                               isRTL, kKeymanKeyboardRTLKey,
                                               [kbDict objectForKey:kKeymanKeyboardModifiedKey], kKeymanKeyboardModifiedKey,
                                               [kbDict objectForKey:kKeymanKeyboardFileSizeKey], kKeymanKeyboardFileSizeKey,
                                               kbUri, kKeymanKeyboardURIKey,
                                               [kbDict objectForKey:kKeymanFontKey], kKeymanFontKey,
                                               nil]
                                       forKey:kbID];
            }
            
            if ([self.keyboardsDictionary objectForKey:dictKey] == nil) {
                NSDictionary *oskFont = [kbDict objectForKey:kKeymanOskFontKey];
                [keyboardsDictionary_ setObject:[NSDictionary dictionaryWithObjectsAndKeys:
                                                 [kbDict objectForKey:kKeymanIdKey], kKeymanKeyboardIdKey,
                                                 [langDict objectForKey:kKeymanIdKey], kKeymanLanguageIdKey,
                                                 [kbDict objectForKey:kKeymanNameKey], kKeymanKeyboardNameKey,
                                                 [langDict objectForKey:kKeymanNameKey], kKeymanLanguageNameKey,
                                                 kbVersion, kKeymanKeyboardVersionKey,
                                                 isRTL, kKeymanKeyboardRTLKey,
                                                 [self jsFontFromFontDictionary:[kbDict objectForKey:kKeymanFontKey]], kKeymanFontKey,
                                                 oskFont?[self jsFontFromFontDictionary:oskFont]:nil, kKeymanOskFontKey,
                                                 nil]
                                         forKey:dictKey];
            }
        }
    }
    
    [self updateUserKeyboardsList];
}

- (void)updateUserKeyboardsList {
    if (keyboardsDictionary_ == nil || keyboardsDictionary_.count == 0)
        return;
    
    NSUserDefaults *userData = [self activeUserDefaults];
    CGFloat lastVersion = [self floatValueForVersionNumber:[userData objectForKey:kKeymanEngineVersion]];
    CGFloat currentVersion = [self floatValueForVersionNumber:[self sdkVersion]];

    if (lastVersion == currentVersion)
        return;
    
    NSMutableArray *userKbList = [[userData arrayForKey:kKeymanUserKeyboardsListKey] mutableCopy];
    NSUInteger len = userKbList.count;
    for (int i = 1; i < len; i++) {
        NSString *kbID = [[userKbList objectAtIndex:i] objectForKey:kKeymanKeyboardIdKey];
        NSString *langID = [[userKbList objectAtIndex:i] objectForKey:kKeymanLanguageIdKey];
        NSMutableDictionary *kb = [[keyboardsDictionary_ objectForKey:[NSString stringWithFormat:@"%@_%@", langID, kbID]] mutableCopy];
        if (kb != nil) {
            [kb setObject:[self latestKeyboardFileVersionWithID:kbID] forKey:kKeymanKeyboardVersionKey];
            [kb setObject:@"N" forKey:kKeymanCustomKeyboardKey];
            [userKbList replaceObjectAtIndex:i withObject:kb];
        }
        else {
            kb = [[userKbList objectAtIndex:i] mutableCopy];
            [kb setObject:@"Y" forKey:kKeymanCustomKeyboardKey];
            [userKbList replaceObjectAtIndex:i withObject:kb];
        }
    }
    
    [userData setObject:userKbList forKey:kKeymanUserKeyboardsListKey];
    [userData setObject:[self sdkVersion] forKey:kKeymanEngineVersion];
    [userData synchronize];
}

- (CGFloat)floatValueForVersionNumber:(NSString *)versionNumber {
    if (versionNumber == nil)
        return 1.0f;
    
    NSArray *numbers = [versionNumber componentsSeparatedByString:@"."];
    NSMutableString *floatStr = [NSMutableString stringWithString:@""];
    NSUInteger length = numbers.count;
    for (int i = 0; i < length; i++) {
        if (i == 0) {
            [floatStr appendString:[NSString stringWithFormat:@"%@.", [numbers objectAtIndex:0]]];
            if (length == 1)
                [floatStr appendString:@"0"];
        }
        else {
            [floatStr appendString:[numbers objectAtIndex:i]];
        }
    }
    
    return [floatStr floatValue];
}

/* No longer used
 - (NSDate *)dateFromKeymanDateString:(NSString *)dateString {
 NSDate *retVal = nil;
 if ([dateString length]) {
 // format date
 retVal = [self.dateFormatter dateFromString:dateString];
 }
 return retVal;
 }
 */

- (void)reachabilityChanged:(NSNotification *)notification {
    if (self.debugPrintingOn) {
        NSString *reachStr = @"Not Reachable";
        NetworkStatus status = [self.reachability currentReachabilityStatus];
        if (status == ReachableViaWiFi) reachStr = @"Reachable Via WiFi";
        if (status == ReachableViaWWAN) reachStr = @"Reachable Via WWan";
        [self KMLog:[NSString stringWithFormat:@"Reachability changed to '%@'", reachStr] checkDebugPrinting:YES];
    }
}

#pragma mark - WKNavigationDelegate

- (void)userContentController:(WKUserContentController *)userContentController didReceiveScriptMessage:(WKScriptMessage *)message {
    NSString *fragment = message.body;
    if ([fragment hasPrefix:@"ios-log:"]) {
        NSString *requestString = [fragment stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
        NSString *logString = [[requestString componentsSeparatedByString:@":#iOS#"] objectAtIndex:1];
        [self KMLog:[NSString stringWithFormat:@"WebView: %@", logString] checkDebugPrinting:NO];
    }
    else {
        [self.webDelegate updatedFragment:fragment];
        [self.inputDelegate updatedFragment:fragment];
        [self updatedFragment:fragment];
    }
}

- (void)webView:(WKWebView *)webView didFinishNavigation:(WKNavigation *)navigation {
    NSString *lastComponent = [[webView URL] lastPathComponent];
    NSString *fragment = [[webView URL] fragment];
    if ([lastComponent isEqualToString:kKeymanFullFileName] && [fragment length] == 0) {
        [self KMLog:@"Loaded keyboard." checkDebugPrinting:YES];
        
        [self resizeKeyboard];
        
        NSString *deviceType = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?@"AppleMobile":@"AppleTablet";
        [webView evaluateJavaScript:[NSString stringWithFormat:@"setDeviceType('%@');", deviceType] completionHandler:nil];
        if ((self.keyboardID == nil || self.languageID == nil) && !shouldReloadKeyboard) {
            NSString *kbID = kKeymanDefaultKeyboardID;
            NSString *langID = kKeymanDefaultLanguageID;
            NSString *kbName = kKeymanDefaultKeyboardName;
            NSString *langName = kKeymanDefaultLanguageName;
            NSString *font = kKeymanDefaultKeyboardFont;
            NSString *oskFont = nil;
            
            NSUserDefaults *userData = nil;
            if (kIsKeymanSystemKeyboard)
                userData = [NSUserDefaults standardUserDefaults];
            else
                userData = [self activeUserDefaults];
            
            NSArray *userKeyboards = [[self activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey];
            
            if (userKeyboards != nil && [userKeyboards count] > 0) {
                kbID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
                langID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageIdKey];
                kbName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardNameKey];
                langName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageNameKey];
                font = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanFontKey];
                oskFont = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanOskFontKey];
            }
            
            NSDictionary *currentKB = [userData dictionaryForKey:kKeymanUserCurrentKeyboardKey];
            //if (kIsKeymanSystemKeyboard && ![self canAccessSharedContainer])
            //currentKB = nil;
            
            if (currentKB != nil) {
                NSString *cKbId = [currentKB objectForKey:kKeymanKeyboardIdKey];
                NSString *cLangId = [currentKB objectForKey:kKeymanLanguageIdKey];
                if ([self indexForUserKeyboardWithID:cKbId languageID:cLangId] >= 0) {
                    kbID = [currentKB objectForKey:kKeymanKeyboardIdKey];
                    langID = [currentKB objectForKey:kKeymanLanguageIdKey];
                    kbName = [[currentKB objectForKey:kKeymanKeyboardNameKey] stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"];
                    langName = [[currentKB objectForKey:kKeymanLanguageNameKey] stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"];
                    font = [currentKB objectForKey:kKeymanFontKey];
                    oskFont = [currentKB objectForKey:kKeymanOskFontKey];
                }
            }
            
            [self setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
        }
        
        NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                                self.keyboardID, kKeymanKeyboardIdKey,
                                self.languageID, kKeymanLanguageIdKey,
                                nil];
        
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardLoadedNotification
                                                            object:self
                                                          userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbInfo, kKeymanKeyboardInfoKey, nil]];
        
        if (shouldReloadKeyboard) {
            [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(resetKeyboard) object:nil];
            [self performSelector:@selector(resetKeyboard) withObject:nil afterDelay:0.25];
            shouldReloadKeyboard = NO;
        }
    }
}

#pragma mark - UIWebViewDelegate

- (BOOL)webView:(UIWebView *)webView shouldStartLoadWithRequest:(NSURLRequest *)request navigationType:(UIWebViewNavigationType)navigationType {
    NSString *fragment = [[request URL] fragment];
    [self.webDelegate updatedFragment:fragment];
    [self.inputDelegate updatedFragment:fragment];
    [self updatedFragment:fragment];
    
    NSString *requestString = [[[request URL] absoluteString] stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
    if ([requestString hasPrefix:@"ios-log:"]) {
        NSString *logString = [[requestString componentsSeparatedByString:@":#iOS#"] objectAtIndex:1];
        [self KMLog:[NSString stringWithFormat:@"WebView: %@", logString] checkDebugPrinting:NO];
    }
    
    return YES;
}

- (void)webViewDidFinishLoad:(UIWebView *)webView {
    NSURLRequest *request = [webView request];
    NSString *lastComponent = [[request URL] lastPathComponent];
    NSString *fragment = [[request URL] fragment];
    if ([lastComponent isEqualToString:kKeymanFullFileName] && [fragment length] == 0) {
        [self KMLog:@"Loaded keyboard." checkDebugPrinting:YES];
        
        [self resizeKeyboard];
        
        NSString *deviceType = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone)?@"AppleMobile":@"AppleTablet";
        [webView stringByEvaluatingJavaScriptFromString:[NSString stringWithFormat:@"setDeviceType('%@');", deviceType]];
        if ((self.keyboardID == nil || self.languageID == nil) && !shouldReloadKeyboard) {
            NSString *kbID = kKeymanDefaultKeyboardID;
            NSString *langID = kKeymanDefaultLanguageID;
            NSString *kbName = kKeymanDefaultKeyboardName;
            NSString *langName = kKeymanDefaultLanguageName;
            NSString *font = kKeymanDefaultKeyboardFont;
            NSString *oskFont = nil;
            
            NSUserDefaults *userData = nil;
            if (kIsKeymanSystemKeyboard)
                userData = [NSUserDefaults standardUserDefaults];
            else
                userData = [self activeUserDefaults];
            
            NSArray *userKeyboards = [[self activeUserDefaults] arrayForKey:kKeymanUserKeyboardsListKey];
            
            if (userKeyboards != nil && [userKeyboards count] > 0) {
                kbID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardIdKey];
                langID = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageIdKey];
                kbName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanKeyboardNameKey];
                langName = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanLanguageNameKey];
                font = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanFontKey];
                oskFont = [[userKeyboards objectAtIndex:0] objectForKey:kKeymanOskFontKey];
            }
            
            NSDictionary *currentKB = [userData dictionaryForKey:kKeymanUserCurrentKeyboardKey];
            //if (kIsKeymanSystemKeyboard && ![self canAccessSharedContainer])
            //currentKB = nil;
            
            if (currentKB != nil) {
                NSString *cKbId = [currentKB objectForKey:kKeymanKeyboardIdKey];
                NSString *cLangId = [currentKB objectForKey:kKeymanLanguageIdKey];
                NSInteger cIndex = [self indexForUserKeyboardWithID:cKbId languageID:cLangId];
                if (cIndex >= 0) {
                    kbID = [[userKeyboards objectAtIndex:cIndex] objectForKey:kKeymanKeyboardIdKey];
                    langID = [[userKeyboards objectAtIndex:cIndex] objectForKey:kKeymanLanguageIdKey];
                    kbName = [[userKeyboards objectAtIndex:cIndex] objectForKey:kKeymanKeyboardNameKey];
                    langName = [[userKeyboards objectAtIndex:cIndex] objectForKey:kKeymanLanguageNameKey];
                    font = [[userKeyboards objectAtIndex:cIndex] objectForKey:kKeymanFontKey];
                    oskFont = [[userKeyboards objectAtIndex:cIndex] objectForKey:kKeymanOskFontKey];
                    /*
                    kbID = [currentKB objectForKey:kKeymanKeyboardIdKey];
                    langID = [currentKB objectForKey:kKeymanLanguageIdKey];
                    kbName = [[currentKB objectForKey:kKeymanKeyboardNameKey] stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"];
                    langName = [[currentKB objectForKey:kKeymanLanguageNameKey] stringByReplacingOccurrencesOfString:@"\\'" withString:@"'"];
                    font = [currentKB objectForKey:kKeymanFontKey];
                    oskFont = [currentKB objectForKey:kKeymanOskFontKey];
                    */
                }
            }
            
            [self setKeyboardWithID:kbID languageID:langID keyboardName:kbName languageName:langName font:font oskFont:oskFont];
        }
        
        NSDictionary *kbInfo = [NSDictionary dictionaryWithObjectsAndKeys:
                                self.keyboardID, kKeymanKeyboardIdKey,
                                self.languageID, kKeymanLanguageIdKey,
                                nil];
        
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardLoadedNotification
                                                            object:self
                                                          userInfo:[NSDictionary dictionaryWithObjectsAndKeys:kbInfo, kKeymanKeyboardInfoKey, nil]];
        
        if (shouldReloadKeyboard) {
            [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(resetKeyboard) object:nil];
            [self performSelector:@selector(resetKeyboard) withObject:nil afterDelay:0.25];
            shouldReloadKeyboard = NO;
        }
    }
}

- (CGRect)getKeyFrameWithX:(CGFloat)x Y:(CGFloat)y W:(CGFloat)w H:(CGFloat)h {
    BOOL isPad = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad;
    CGFloat adjY = isPad?-0.5:-1.0;
    
    CGRect frame = CGRectMake(x-w/2.0f, y-adjY, w, h);
    return frame;
}

- (void)updatedFragment:(NSString *)fragment {
    if (fragment == nil || [fragment length] == 0)
        return;

    if ([fragment rangeOfString:@"showKeyPreview"].location != NSNotFound) {
        BOOL isPad = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad;
        if (isPad)
            return;
        
        if (kIsKeymanSystemKeyboard && !self.isSystemKeyboardTopBarEnabled)
            return;
        
        if (subKeysView != nil)
            return;

        [self dismissKeyPreview];
        [self clearSubKeyArrays];
        NSRange range1 = [fragment rangeOfString:@"+x="];
        NSRange range2 = [fragment rangeOfString:@"+y="];
        NSRange range3 = [fragment rangeOfString:@"+w="];
        NSRange range4 = [fragment rangeOfString:@"+h="];
        NSRange range5 = [fragment rangeOfString:@"+t="];
        range1.location += range1.length;
        range1.length = range2.location - range1.location;
        CGFloat x = [[fragment substringWithRange:range1] floatValue];
        range2.location += range2.length;
        range2.length = range3.location - range2.location;
        CGFloat y = [[fragment substringWithRange:range2] floatValue];
        range3.location += range3.length;
        range3.length = range4.location - range3.location;
        CGFloat w = [[fragment substringWithRange:range3] floatValue];
        range4.location += range4.length;
        range4.length = range5.location - range4.location;
        CGFloat h = [[fragment substringWithRange:range4] floatValue];
        range5.location += range5.length;
        NSString *t = [fragment substringFromIndex:range5.location];
        keyFrame = [self getKeyFrameWithX:x Y:y W:w H:h];
        keyPreviewView = [[KeyPreviewView alloc] initWithFrame:keyFrame];
        NSMutableString *text = nil;
        NSArray *unicodes = [t componentsSeparatedByString:@","];
        NSScanner *scanner = nil;
        for (int i = 0; i < [unicodes count]; i++) {
            scanner = [NSScanner scannerWithString:[unicodes objectAtIndex:i]];
            unsigned int unicode;
            [scanner scanHexInt:&unicode];
            if (text == nil)
                text = [NSMutableString stringWithFormat:@"%C", (unsigned short) unicode];
            else
                [text appendString:[NSString stringWithFormat:@"%C", (unsigned short) unicode]];
        }
        
        [keyPreviewView setLabelText:[NSString stringWithString:text]];
        text = nil;
        NSString *oskFontName = [self oskFontNameForKeyboardWithID:self.keyboardID languageID:self.languageID];
        if (oskFontName == nil)
            oskFontName = [self fontNameForKeyboardWithID:self.keyboardID languageID:self.languageID];
        [keyPreviewView setLabelFont:oskFontName];
        [[[self class] inputView] addSubview:keyPreviewView];
        //keyPreviewView = [[UIView alloc] initWithFrame:CGRectMake(x-w/2.0f, y+6, w, h)];
        //[keyPreviewView setBackgroundColor:[UIColor colorWithRed:1.0 green:0 blue:0 alpha:0.5]];
        //[[[self class] inputView] addSubview:keyPreviewView];
    }
    else if ([fragment rangeOfString:@"dismissKeyPreview"].location != NSNotFound) {
        BOOL isPad = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad;
        if (isPad || keyPreviewView == nil)
            return;
        
        [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(dismissKeyPreview) object:nil];
        [self performSelector:@selector(dismissKeyPreview) withObject:nil afterDelay:0.1];
        [self clearSubKeyArrays];
    }
    else if ([fragment rangeOfString:@"insertText"].location != NSNotFound) {
        [self dismissHelpBubble];
        self.keymanHelpOn = NO;
    }
    else if ([fragment rangeOfString:@"menuKeyUp"].location != NSNotFound) {
        [self dismissHelpBubble];
        self.keymanHelpOn = NO;
        
        if (kIsKeymanSystemKeyboard) {
            NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
            [userData setBool:YES forKey:kKeymanKeyboardPickerDisplayedKey];
            [userData synchronize];
        }
    }
    else if ([fragment rangeOfString:@"hideKeyboard"].location != NSNotFound) {
        [self dismissHelpBubble];
        [self dismissSubKeys];
        [self dismissKeyboardMenu];
    }
    else if ([fragment rangeOfString:@"showMore"].location != NSNotFound) {
        [self dismissHelpBubble];
        self.keymanHelpOn = NO;
        [self dismissSubKeys];
        [self dismissKeyboardMenu];
        
        NSRange range1 = [fragment rangeOfString:@"+baseFrame="];
        NSRange range2 = [fragment rangeOfString:@"+keys="];
        NSRange range3 = [fragment rangeOfString:@"+font="];
        range1.location += range1.length;
        range1.length = range2.location - range1.location;
        NSArray *frame = [[fragment substringWithRange:range1] componentsSeparatedByString:@","];
        CGFloat x = [[frame objectAtIndex:0] floatValue];
        CGFloat y = [[frame objectAtIndex:1] floatValue];
        CGFloat w = [[frame objectAtIndex:2] floatValue];
        CGFloat h = [[frame objectAtIndex:3] floatValue];
        keyFrame = [self getKeyFrameWithX:x Y:y W:w H:h];
        
        range2.location += range2.length;
        range2.length = [fragment length] - range2.location;
        if (range3.location != NSNotFound) {
            range2.length -= [fragment length] - range3.location;
            range3.location += range3.length;
            specialOSKFont = [NSString stringWithString:[fragment substringFromIndex:range3.location]];
        }
        else {
            specialOSKFont = nil;
        }
        
        NSString *keys = [fragment substringWithRange:range2];
        NSArray *keyArray = [keys componentsSeparatedByString: @";"];
        NSUInteger len = [keyArray count];
        subKeyIDs = [keyArray mutableCopy];
        subKeyTexts = [keyArray mutableCopy];
        
        for(int i=0; i < len; i++) {
            NSArray *values = [[keyArray objectAtIndex:i] componentsSeparatedByString: @":"];
            if([values count] == 2) {
                [subKeyIDs replaceObjectAtIndex:i withObject:[values objectAtIndex:0]];
                [subKeyTexts replaceObjectAtIndex:i withObject:[values objectAtIndex:1]];
            }
            else if ([values count] == 1){
                [subKeyIDs replaceObjectAtIndex:i withObject:[values objectAtIndex:0]];
                NSString *subkeyText = [values objectAtIndex:0];
                NSUInteger index = [subkeyText rangeOfString:@"-"].location;
                if (index != NSNotFound)
                    subkeyText = [subkeyText substringFromIndex:index+1];
                index = [subkeyText rangeOfString:@"_"].location;
                if (index != NSNotFound)
                    subkeyText = [subkeyText substringFromIndex:index+1];
                NSString *unicode = [NSString stringWithFormat:@"0x%@", subkeyText];
                [subKeyTexts replaceObjectAtIndex:i withObject:unicode];
            }
            else {
                [subKeyIDs replaceObjectAtIndex:i withObject:@""];
                [subKeyTexts replaceObjectAtIndex:i withObject:@""];
            }
        }
        
        //[NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(clearSubKeyArrays) object:nil];
        //[self performSelector:@selector(clearSubKeyArrays) withObject:nil afterDelay:0.525];
    }
}

- (void)clearSubKeyArrays {
    if (subKeysView == nil) {
        [subKeys removeAllObjects];
        [subKeyIDs removeAllObjects];
        [subKeyTexts removeAllObjects];
    }
}

- (void)dismissHelpBubble {
    if (helpBubbleView != nil) {
        [helpBubbleView removeFromSuperview];
        helpBubbleView = nil;
    }
}

- (void)dismissKeyPreview {
    if (keyPreviewView != nil) {
        [keyPreviewView removeFromSuperview];
        keyPreviewView = nil;
    }
}

- (CGSize)sizeFromFrame:(CGRect)frame {
    return CGSizeMake(frame.size.width - frame.origin.x, frame.size.height - frame.origin.y);
}

#pragma mark - UIGestureRecognizer

- (BOOL)gestureRecognizer:(UIGestureRecognizer *)gestureRecognizer shouldRecognizeSimultaneouslyWithGestureRecognizer:(UIGestureRecognizer *)otherGestureRecognizer {
    return YES;
}

// UILongPressGestureRecognizer implementation to show sub keys in a subview
- (void)holdAction:(UILongPressGestureRecognizer *)sender {
    if (sender.state == UIGestureRecognizerStateEnded) {
        // Touch Ended
        if (subKeysView != nil) {
            [subKeysView removeFromSuperview];
            [subKeysView.subviews makeObjectsPerformSelector:@selector(removeFromSuperview)];
            subKeysView = nil;
            
            //[self performSelector:@selector(setPopupVisible:) withObject:[NSNumber numberWithBool:NO] afterDelay:1.0];
            [self setPopupVisible:[NSNumber numberWithBool:NO]];
            
            [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanSubKeysMenuDismissedNotification object:self
                                                              userInfo:nil];
        }
        
        UIButton *button;
        BOOL buttonClicked = NO;
        for (int i = 0; i < [subKeys count]; i++) {
            button = [subKeys objectAtIndex:i];
            if (button.isHighlighted) {
                button.highlighted = NO;
                button.backgroundColor = subKeyColor;
                [button setEnabled:NO];
                [button sendActionsForControlEvents:UIControlEventTouchUpInside];
                buttonClicked = YES;
                break;
            }
        }
        
        if (!buttonClicked) {
            [self clearSubKeyArrays];
        }
    }
    else if (sender.state == UIGestureRecognizerStateBegan) {
        // Touch & Hold Began
        CGPoint touchPoint = [sender locationInView:sender.view];
        if ([[self class] isWKWebView:webView]) {
            [(WKWebView *)webView evaluateJavaScript:@"langMenuPos();" completionHandler:^(id result, NSError *error) {
                NSString *keyFrame = (NSString *)result;
                [self setMenuKeyFrame:keyFrame];
                if (CGRectContainsPoint(menuKeyFrame, touchPoint)) {
                    [self.inputDelegate updatedFragment:@"showKeyboardMenu"];
                    return;
                }
            
                [self touchHoldBegan];
            }];
        }
        else {
            NSString *keyFrame = [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:@"langMenuPos();"];
            [self setMenuKeyFrame:keyFrame];
            if (CGRectContainsPoint(menuKeyFrame, touchPoint)) {
                [self.inputDelegate updatedFragment:@"showKeyboardMenu"];
                return;
            }
            
            [self touchHoldBegan];
        }
    }
    else {
        // Hold & Move
        CGPoint touchPoint = [sender locationInView:[subKeysView containerView]];
        UIButton *button;
        for (int i = 0; i < [subKeys count]; i++) {
            button = [subKeys objectAtIndex:i];
            button.highlighted = NO;
            button.backgroundColor = subKeyColor;
            [button setEnabled:NO];
            if (CGRectContainsPoint(button.frame, touchPoint)) {
                [button setEnabled:YES];
                button.highlighted = YES;
                button.backgroundColor = subKeyColorHighlighted;
            }
        }
    }
}

- (void)touchHoldBegan {
    NSInteger subKeysCount = [subKeyIDs count];
    CGFloat fontSize;
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad)
        fontSize = [UIFont buttonFontSize]*2;
    else
        fontSize = [UIFont buttonFontSize];
    
    NSString *oskFontName = [self oskFontNameForKeyboardWithID:self.keyboardID languageID:self.languageID];
    if (oskFontName == nil)
        oskFontName = [self fontNameForKeyboardWithID:self.keyboardID languageID:self.languageID];
    subKeys = [[NSMutableArray alloc] init];
    if (subKeyIDs != nil && subKeysCount) {
        for (int i = 0; i < subKeysCount; i++) {
            UIButton *button = [UIButton buttonWithType:UIButtonTypeCustom];
            [button setTag:i];
            [button setBackgroundColor:subKeyColor];
            [button setRoundedBorderWithRadius:4.0
                                   borderWidth:1.0
                                         color:[UIColor grayColor]];
            [button setTitleColor:[UIColor blackColor] forState:UIControlStateDisabled];
            [button setTitleColor:[UIColor blackColor] forState:UIControlStateHighlighted];
            
            if (oskFontName != nil)
                [[button titleLabel] setFont:[UIFont fontWithName:oskFontName size:fontSize]];
            else
                [[button titleLabel] setFont:[UIFont systemFontOfSize:fontSize]];
            
            if(specialOSKFont != nil) {
                [[button titleLabel] setFont:[UIFont fontWithName:@"KeymanwebOsk" size:fontSize]];
                [button setTitleColor:[UIColor grayColor] forState:UIControlStateDisabled];
            }
            
            NSMutableString *buttonTitle = nil;
            NSString *subKeyText = [subKeyTexts objectAtIndex:i];
            if([subKeyText rangeOfString:@"0x"].location != NSNotFound){
                NSArray *unicodes = [subKeyText componentsSeparatedByString: @","];
                NSScanner *s = nil;
                for(int i=0;i<[unicodes count];i++) {
                    s = [NSScanner scannerWithString:[unicodes objectAtIndex:i]];
                    unsigned int unicode;
                    [s scanHexInt:&unicode];
                    if(buttonTitle == nil)
                        buttonTitle = [NSMutableString stringWithFormat:@"%C", (unsigned short) unicode];
                    else
                        [buttonTitle appendString:[NSString stringWithFormat:@"%C", (unsigned short) unicode]];
                }
            }
            else {
                buttonTitle = [NSMutableString stringWithString:subKeyText];
            }
            
            [button addTarget:self action:@selector(buttonClick:) forControlEvents:UIControlEventTouchUpInside];
            [button setTitle:[NSString stringWithString:buttonTitle] forState:UIControlStateNormal];
            buttonTitle = nil;
            [button setTintColor:[UIColor colorWithRed:181.0/255.0 green:181.0/255.0 blue:181.0/255.0 alpha:1.0]];
            [button setEnabled:NO];
            
            [subKeys addObject:button];
        }
        
        [self dismissKeyPreview];
        subKeysView = [[SubKeysView alloc] initWithKeyFrame:keyFrame subKeys:subKeys];
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanSubKeysMenuWillShowNotification object:self
                                                          userInfo:nil];
        [[[self class] inputView] addSubview:subKeysView];
        [self setPopupVisible:[NSNumber numberWithBool:YES]];
    }
}

- (void)buttonClick:(id)sender {
    NSInteger keyIndex = [sender tag];
    if (keyIndex < [subKeyIDs count] && keyIndex < [subKeyTexts count]) {
        NSString *subKeyID = [subKeyIDs objectAtIndex:keyIndex];
        NSString *subKeyText = [subKeyTexts objectAtIndex:keyIndex];
        NSString *jsString = [NSString stringWithFormat:@"executePopupKey('%@','%@');", subKeyID, subKeyText];
        if ([[self class] isWKWebView:webView])
            [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
        else
            [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
    }
    
    [subKeys removeAllObjects];
    [subKeyIDs removeAllObjects];
    [subKeyTexts removeAllObjects];
}

- (BOOL)isSubKeysMenuVisible {
    return subKeysView == nil?NO:YES;
}

- (void)dismissSubKeys {
    if (subKeysView != nil) {
        [subKeysView removeFromSuperview];
        [subKeysView.subviews makeObjectsPerformSelector:@selector(removeFromSuperview)];
        subKeysView = nil;
        
        //[self performSelector:@selector(setPopupVisible:) withObject:[NSNumber numberWithBool:NO] afterDelay:1.0];
        [self setPopupVisible:[NSNumber numberWithBool:NO]];
        
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanSubKeysMenuDismissedNotification object:self
                                                          userInfo:nil];
    }
    
    [subKeys removeAllObjects];
    [subKeyIDs removeAllObjects];
    [subKeyTexts removeAllObjects];
}

- (void)setPopupVisible:(NSNumber *)value {
    BOOL visible = [value boolValue];
    NSString *jsString = [NSString stringWithFormat:@"popupVisible(%@);", visible?@"1":@"0"];
    if ([[self class] isWKWebView:webView])
        [(WKWebView *)webView evaluateJavaScript:jsString completionHandler:nil];
    else
        [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:jsString];
}

- (void)registerCustomFonts {
    NSArray *directoryContents = [[NSArray alloc] initWithArray:[[NSFileManager defaultManager] contentsOfDirectoryAtPath:[self activeFontDirectory] error:nil]];
    NSMutableDictionary *fontInfo = nil;
    NSString *fontFilename = nil;
    
    if (keymanFonts_ == nil)
        keymanFonts_ = [[NSMutableDictionary alloc] init];
    
    for (fontFilename in directoryContents) {
        if ([fontFilename rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [fontFilename rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
            fontInfo = [self.keymanFonts objectForKey:fontFilename];
            if (fontInfo == nil) {
                fontInfo = [[self registerFontWithFilename:fontFilename] mutableCopy];
                if (fontInfo != nil)
                    [keymanFonts_ setObject:fontInfo forKey:fontFilename];
            }
            else if (![[fontInfo objectForKey:kKeymanFontRegisteredKey] boolValue]) {
                fontInfo = [[self registerFontWithFilename:fontFilename] mutableCopy];
                if (fontInfo != nil)
                    [keymanFonts_ setObject:fontInfo forKey:fontFilename];
            }
        }
    }
}

- (void)unregisterCustomFonts {
    NSArray *directoryContents = [[NSArray alloc] initWithArray:[[NSFileManager defaultManager] contentsOfDirectoryAtPath:[self activeFontDirectory] error:nil]];
    NSString *fontFilename = nil;
    NSMutableDictionary *fontInfo = nil;
    
    if (keymanFonts_ == nil)
        return;
    
    for (fontFilename in directoryContents) {
        if ([fontFilename rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [fontFilename rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
            fontInfo = [self.keymanFonts objectForKey:fontFilename];
            if (fontInfo != nil && [[fontInfo objectForKey:kKeymanFontRegisteredKey] boolValue]) {
                if ([self unregisterFontWithFilename:fontFilename]) {
                    [fontInfo setObject:[NSNumber numberWithBool:NO] forKey:kKeymanFontRegisteredKey];
                    [keymanFonts_ setObject:fontInfo forKey:fontFilename];
                }
            }
        }
    }
}

- (NSDictionary *)registerFontWithFilename:(NSString *)fontFilename {
    NSString *fontPath = [[self activeFontDirectory] stringByAppendingPathComponent:fontFilename];
    if (![[NSFileManager defaultManager] fileExistsAtPath:fontPath])
        return nil;
    
    BOOL didRegister = NO;
    NSString *fontName = nil;
    NSURL *fontUrl = [NSURL fileURLWithPath:fontPath];
    CGDataProviderRef provider = CGDataProviderCreateWithURL((__bridge CFURLRef)fontUrl);
    CGFontRef font = CGFontCreateWithDataProvider(provider);
    fontName = (NSString *)CFBridgingRelease(CGFontCopyPostScriptName(font));
    CGDataProviderRelease(provider);
    CGFontRelease(font);
    
    if (![self fontExists:fontName]) {
        CFErrorRef error;
        didRegister = CTFontManagerRegisterFontsForURL((__bridge CFURLRef)fontUrl, kCTFontManagerScopeNone, &error);
        if (!didRegister) {
            CFStringRef errorDescription = CFErrorCopyDescription(error);
            [self KMLog:[NSString stringWithFormat:@"Failed to register font: %@ reason: %@", [fontPath lastPathComponent], errorDescription] checkDebugPrinting:NO];
            CFRelease(errorDescription);
            error = nil;
        }
        else
            [self KMLog:[NSString stringWithFormat:@"Registered font: %@", fontFilename] checkDebugPrinting:YES];
    }
    
    return [NSDictionary dictionaryWithObjectsAndKeys:fontName, kKeymanFontNameKey, [NSNumber numberWithBool:didRegister], kKeymanFontRegisteredKey, nil];
}

- (BOOL)unregisterFontWithFilename:(NSString *)fontFilename {
    NSString *fontPath = [[self activeFontDirectory] stringByAppendingPathComponent:fontFilename];
    if (![[NSFileManager defaultManager] fileExistsAtPath:fontPath])
        return NO;
    
    CFErrorRef error;
    BOOL didUnregister = CTFontManagerUnregisterFontsForURL((__bridge CFURLRef)[NSURL fileURLWithPath:fontPath], kCTFontManagerScopeNone, &error);
    if (!didUnregister) {
        CFStringRef errorDescription = CFErrorCopyDescription(error);
        [self KMLog:[NSString stringWithFormat:@"Failed to unregister font: %@ reason: %@", [fontPath lastPathComponent], errorDescription] checkDebugPrinting:NO];
        CFRelease(errorDescription);
        error = nil;
    }
    else
        [self KMLog:[NSString stringWithFormat:@"Unregistered font: %@", fontFilename] checkDebugPrinting:YES];
    
    return didUnregister;
}

- (BOOL)fontExists:(NSString *)fontName {
    BOOL exists = NO;
    NSArray *fontFamilyNames = [UIFont familyNames];
    for (NSString *familyName in fontFamilyNames)
    {
        NSArray *fontNames = [UIFont fontNamesForFamilyName:familyName];
        for (NSString *name in fontNames)
        {
            if ([fontName isEqualToString:name]) {
                exists = YES;
                break;
            }
        }
        
        if (exists)
            break;
    }
    
    return exists;
}

- (NSString *)fontNameForKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID {
    NSUserDefaults *userData = [self activeUserDefaults];
    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    NSString *fontName = nil;
    
    if (userKeyboards != nil) {
        NSString *kbId = nil;
        NSString *langId = nil;
        
        for (NSDictionary *kb in userKeyboards) {
            kbId = [kb objectForKey:kKeymanKeyboardIdKey];
            langId = [kb objectForKey:kKeymanLanguageIdKey];
            if ([kbId isEqualToString:keyboardID] && [langId isEqualToString:languageID]) {
                NSString *font = [kb objectForKey:kKeymanFontKey];
                NSString *fontFilename = [self fontFilenameFromJSONFont:font];
                fontName = [[self.keymanFonts objectForKey:fontFilename] objectForKey:kKeymanFontNameKey];
                break;
            }
        }
    }
    
    if (fontName == nil && self.keyboardsDictionary != nil) {
        NSString *kbKey = [NSString stringWithFormat:@"%@_%@", languageID, keyboardID];
        NSString *fontFilename = [self fontFilenameFromJSONFont:[[self.keyboardsDictionary objectForKey:kbKey] objectForKey:kKeymanFontKey]];
        fontName = [[self.keymanFonts objectForKey:fontFilename] objectForKey:kKeymanFontNameKey];
    }

    return fontName;
}

- (NSString *)oskFontNameForKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID {
    NSUserDefaults *userData = [self activeUserDefaults];
    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    NSString *oskFontName = nil;
    
    if (userKeyboards != nil) {
        NSString *kbId = nil;
        NSString *langId = nil;
        
        for (NSDictionary *kb in userKeyboards) {
            kbId = [kb objectForKey:kKeymanKeyboardIdKey];
            langId = [kb objectForKey:kKeymanLanguageIdKey];
            if ([kbId isEqualToString:keyboardID] && [langId isEqualToString:languageID]) {
                NSString *oskFont = [kb objectForKey:kKeymanOskFontKey];
                NSString *oskFontFilename = [self fontFilenameFromJSONFont:oskFont];
                oskFontName = [[self.keymanFonts objectForKey:oskFontFilename] objectForKey:kKeymanFontNameKey];
                break;
            }
        }
    }
    
    if (oskFontName == nil && self.keyboardsDictionary != nil) {
        NSString *kbKey = [NSString stringWithFormat:@"%@_%@", languageID, keyboardID];
        NSString *oskFontFilename = [self fontFilenameFromJSONFont:[[self.keyboardsDictionary objectForKey:kbKey] objectForKey:kKeymanOskFontKey]];
        oskFontName = [[self.keymanFonts objectForKey:oskFontFilename] objectForKey:kKeymanFontNameKey];
    }
    
    return oskFontName;
}

- (BOOL)isRTLKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID {
    NSUserDefaults *userData = [self activeUserDefaults];
    NSArray *userKeyboards = [userData arrayForKey:kKeymanUserKeyboardsListKey];
    NSString *isRTL = nil;
    
    if (userKeyboards != nil) {
        NSString *kbId = nil;
        NSString *langId = nil;
        
        for (NSDictionary *kb in userKeyboards) {
            kbId = [kb objectForKey:kKeymanKeyboardIdKey];
            langId = [kb objectForKey:kKeymanLanguageIdKey];
            if([kbId isEqualToString:keyboardID] && [langId isEqualToString:languageID]) {
                isRTL = [kb objectForKey:kKeymanKeyboardRTLKey];
                break;
            }
        }
    }
    
    if (isRTL == nil && self.keyboardsDictionary != nil) {
        NSString *kbKey = [NSString stringWithFormat:@"%@_%@", languageID, keyboardID];
        isRTL = [[self.keyboardsDictionary objectForKey:kbKey] objectForKey:kKeymanKeyboardRTLKey];
    }
    
    if (isRTL != nil && [isRTL isEqualToString:@"Y"])
        return YES;
    else
        return NO;
}

- (NSString *)fontFilenameFromJSONFont:(NSString *)jsonFont {
    if (jsonFont == nil)
        return nil;
    
    NSString *fontFilename = nil;
    NSError *e;
    
    if ([jsonFont rangeOfString:@".ttf"].location != NSNotFound || [jsonFont rangeOfString:@".otf"].location != NSNotFound) {
        if ([[jsonFont substringFromIndex:jsonFont.length-4] isEqualToString:@".ttf"] || [[jsonFont substringFromIndex:jsonFont.length-4] isEqualToString:@".otf"])
            return jsonFont;
    }

    NSDictionary *fontDict = [NSJSONSerialization JSONObjectWithData:[jsonFont dataUsingEncoding:NSUTF8StringEncoding] options:NSJSONReadingMutableContainers error:&e];
    if (fontDict != nil) {
        id fontFiles = [fontDict objectForKey:kKeymanFontFilesKey];
        if ([fontFiles isKindOfClass:[NSArray class]]) {
            for (NSString *file in fontFiles) {
                if ([file rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [file rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound) {
                    fontFilename = [NSString stringWithString:file];
                    break;
                }
            }
        }
        else if ([fontFiles isKindOfClass:[NSString class]]) {
            NSString *file = (NSString *)fontFiles;
            if ([file rangeOfString:@".ttf" options:NSBackwardsSearch].location != NSNotFound || [file rangeOfString:@".otf" options:NSBackwardsSearch].location != NSNotFound)
                fontFilename = [NSString stringWithString:file];
        }
    }
    
    return fontFilename;
}

- (NSString *)jsFontForKeyboardID:(NSString *)kbID languageID:(NSString *)langID {
    NSString *kbKey = [NSString stringWithFormat:@"%@_%@", langID, kbID];
    NSDictionary *kbDict = [self.keyboardsDictionary objectForKey:kbKey];
    NSString *jsFont = [kbDict objectForKey:kKeymanFontKey];
    return jsFont?jsFont:@"''";
}

- (NSString *)jsOskFontForKeyboardID:(NSString *)kbID languageID:(NSString *)langID {
    NSString *kbKey = [NSString stringWithFormat:@"%@_%@", langID, kbID];
    NSDictionary *kbDict = [self.keyboardsDictionary objectForKey:kbKey];
    NSString *jsOskFont = [kbDict objectForKey:kKeymanOskFontKey];
    return jsOskFont?jsOskFont:@"''";
}

- (NSString *)jsFontFromFontDictionary:(NSDictionary *)fontDict {
    NSMutableString *str = nil;
    if (fontDict != nil && fontDict.count) {
        NSError *e;
        NSData *data = [NSJSONSerialization dataWithJSONObject:fontDict options:0 error:&e];
        str = [[NSMutableString alloc] initWithData:data encoding:NSASCIIStringEncoding];
        [str replaceOccurrencesOfString:kKeymanFontFilenameKey withString:kKeymanFontFilesKey options:0 range:NSMakeRange(0, str.length)]; // Font filename is deprecated
        [str replaceOccurrencesOfString:kKeymanFontSourceKey withString:kKeymanFontFilesKey options:0 range:NSMakeRange(0, str.length)];
    }
    else
        return @"''";
    
    return [NSString stringWithString:str];
}

+ (NSString *)md5FromString:(NSString *)str {
    const char *cStr = [str UTF8String];
    unsigned char digest[16];
    CC_MD5(cStr, (CC_LONG)strlen(cStr), digest);
    
    NSMutableString *md5String = [NSMutableString stringWithCapacity:CC_MD5_DIGEST_LENGTH * 2];
    
    for(int i = 0; i < CC_MD5_DIGEST_LENGTH; i++)
        [md5String appendFormat:@"%02x", digest[i]];
    
    return [NSString stringWithString:md5String];
}

+ (NSString *)sha1FromString:(NSString *)str {
    const char *cStr = [str cStringUsingEncoding:NSUTF8StringEncoding];
    NSData *data = [NSData dataWithBytes:cStr length:str.length];
    
    uint8_t digest[CC_SHA1_DIGEST_LENGTH];
    
    CC_SHA1(data.bytes, (CC_LONG)data.length, digest);
    
    NSMutableString *sha1String = [NSMutableString stringWithCapacity:CC_SHA1_DIGEST_LENGTH * 2];
    
    for(int i = 0; i < CC_SHA1_DIGEST_LENGTH; i++)
        [sha1String appendFormat:@"%02x", digest[i]];
    
    return [NSString stringWithString:sha1String];
}

- (void)KMLog:(NSString *)logStr checkDebugPrinting:(BOOL)checkDebugPrinting {
    if (!checkDebugPrinting || (checkDebugPrinting && self.debugPrintingOn)) {
        NSLog(@"%@", logStr);
        [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanDebugLogNotification object:self
                                                          userInfo:[NSDictionary dictionaryWithObjectsAndKeys:logStr, @"log", nil]];
        //[self.debugLog appendString:[NSString stringWithFormat:@"[%@] %@\n", [NSDate date], logStr]];
    }
}
/*
- (NSMutableString *)debugLog {
    if (_debugLog == nil)
        _debugLog = [NSMutableString stringWithString:@""];
    
    return _debugLog;
}*/

#pragma mark - InputViewController methods

- (void)updateViewConstraints {
    [self dismissSubKeys];
    [self dismissKeyPreview];
    [self dismissKeyboardMenu];
    [self resizeKeyboardIfNeeded];
}

- (void)inputViewDidLoad {
    [self dismissSubKeys];
    [self dismissKeyPreview];
    [self dismissKeyboardMenu];
    [self resizeKeyboard];
    
    NSUserDefaults *aUserDef = [self activeUserDefaults];
    NSUserDefaults *sUserDef = [NSUserDefaults standardUserDefaults];
    NSDate *xDate = [[sUserDef objectForKey:kKeymanSynchronizeSWKeyboardKey] objectAtIndex:0];
    NSDate *nDate = [[aUserDef objectForKey:kKeymanSynchronizeSWKeyboardKey] objectAtIndex:0];
    BOOL shouldSynchronize = YES;
    if (xDate != nil && nDate != nil)
        shouldSynchronize = ([nDate compare:xDate] != NSOrderedSame)?YES:NO;
    else if (nDate == nil)
        shouldSynchronize = NO;

    if ((!didSynchronize || shouldSynchronize) && [self canAccessSharedContainer]) {
        [self synchronizeSWKeyboard];
        if (keyboardID_ != nil && languageID_ != nil) {
            shouldReloadKeyboard = YES;
            [self reloadKeyboard];
        }
        didSynchronize = YES;
        [sUserDef setObject:[aUserDef objectForKey:kKeymanSynchronizeSWKeyboardKey] forKey:kKeymanSynchronizeSWKeyboardKey];
        [sUserDef synchronize];
    }
}

- (void)inputViewWillRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration {
    [self dismissSubKeys];
    [self dismissKeyPreview];
    [self dismissKeyboardMenu];
    [self resizeKeyboardWithOrientation:toInterfaceOrientation];
    if (self.keymanHelpOn) {
        [helpBubbleView removeFromSuperview];
        [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(showHelpBubble) object:nil];
        [self performSelector:@selector(showHelpBubble) withObject:nil afterDelay:1.5];
    }
    
    didResizeToOrientation = YES;
}

- (void)synchronizeSWKeyboard {
    [self copyUserDefaultsFromSharedContainer];
    [self copyKeymanFilesFromSharedContainer];
}

- (void)setMenuKeyFrame:(NSString *)frameStr {
    CGRect frame = CGRectZero;
    if ([frameStr length]) {
        NSArray *values = [frameStr componentsSeparatedByString: @","];
        CGFloat x = [[values objectAtIndex:0] floatValue];
        CGFloat y = [[values objectAtIndex:1] floatValue];
        CGFloat w = [[values objectAtIndex:2] floatValue];
        CGFloat h = [[values objectAtIndex:3] floatValue];
        
        BOOL isPad = [[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad;
        CGFloat adjX = 0.0;
        CGFloat adjY = isPad?-0.5:-1.0;
        
        frame = CGRectMake(x+adjX -w/2.0f, y -adjY, w, h);
    }

    menuKeyFrame = frame;
}

- (void)showKeyboardMenu:(KeymanInputViewController *)ic closeButtonTitle:(NSString *)closeButtonTitle {
    UIView *parentView = ic.view;
    if (parentView == nil)
        parentView = webView;
    
    if ([[self class] isWKWebView:webView]) {
        [(WKWebView *)webView evaluateJavaScript:@"langMenuPos();" completionHandler:^(id result, NSError *error) {
            NSString *keyFrame = (NSString *)result;
            [self setMenuKeyFrame:keyFrame];
            if (!CGRectEqualToRect(menuKeyFrame, CGRectZero)) {
                [keyboardMenuView removeFromSuperview];
                keyboardMenuView = [[KeyboardMenuView alloc] initWithKeyFrame:menuKeyFrame inputViewController:ic closeButtonTitle:closeButtonTitle];
                [parentView addSubview:keyboardMenuView];
            }
        }];
    }
    else {
        NSString *keyFrame = [(UIWebView *)webView stringByEvaluatingJavaScriptFromString:@"langMenuPos();"];
        [self setMenuKeyFrame:keyFrame];
        if (!CGRectEqualToRect(menuKeyFrame, CGRectZero)) {
            [keyboardMenuView removeFromSuperview];
            keyboardMenuView = [[KeyboardMenuView alloc] initWithKeyFrame:menuKeyFrame inputViewController:ic closeButtonTitle:closeButtonTitle];
            [parentView addSubview:keyboardMenuView];
        }
    }
}

- (void)dismissKeyboardMenu {
    if (keyboardMenuView != nil) {
        [keyboardMenuView removeFromSuperview];
        keyboardMenuView = nil;
    }
}

- (BOOL)isKeyboardMenuVisible {
    return keyboardMenuView == nil?NO:YES;
}

- (BOOL)isSystemKeyboardTopBarEnabled {
    return YES;
}

- (BOOL)copyKeymanFilesToTemp {
    if (NSTemporaryDirectory() == nil)
        return NO;
    
    NSFileManager *fileMan = [NSFileManager defaultManager];
    NSString *activeKeymanDir = [NSString stringWithString:[self activeKeymanDirectory]];
    NSString *tempKeymanDir = [NSString stringWithString:[NSTemporaryDirectory() stringByAppendingPathComponent:@"keyman"]];
    [[NSFileManager defaultManager] createDirectoryAtPath:tempKeymanDir withIntermediateDirectories:YES attributes:nil error:nil];
    NSString *activeLangDir = [NSString stringWithString:[self activeLanguageDirectory]];
    NSString *tempLangDir = [NSString stringWithString:[tempKeymanDir stringByAppendingPathComponent:@"languages"]];
    [[NSFileManager defaultManager] createDirectoryAtPath:tempLangDir withIntermediateDirectories:YES attributes:nil error:nil];
    NSString *activeFontDir = [NSString stringWithString:[self activeFontDirectory]];
    NSString *tempFontDir = [NSString stringWithString:[tempKeymanDir stringByAppendingPathComponent:@"fonts"]];
    [[NSFileManager defaultManager] createDirectoryAtPath:tempFontDir withIntermediateDirectories:YES attributes:nil error:nil];

    NSArray *dirContents = [fileMan contentsOfDirectoryAtPath:activeKeymanDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [activeKeymanDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [tempKeymanDir stringByAppendingPathComponent:file];
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
/*        BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                if (error)
                    [self KMLog:[NSString stringWithFormat:@"%@", error.description] checkDebugPrinting:NO];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
                if (error)
                    [self KMLog:[NSString stringWithFormat:@"%@", error.description] checkDebugPrinting:NO];
            }
        }*/
    }
    
    dirContents = [fileMan contentsOfDirectoryAtPath:activeLangDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [activeLangDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [tempLangDir stringByAppendingPathComponent:file];
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
/*        BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                if (error)
                    [self KMLog:[NSString stringWithFormat:@"%@", error.description] checkDebugPrinting:NO];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
                if (error)
                    [self KMLog:[NSString stringWithFormat:@"%@", error.description] checkDebugPrinting:NO];
            }
        }*/
    }
    
    dirContents = [fileMan contentsOfDirectoryAtPath:activeFontDir error:nil];
    for (NSString *file in dirContents){
        NSString *pathToCopy = [activeFontDir stringByAppendingPathComponent:file];
        NSString *destinationPath = [tempFontDir stringByAppendingPathComponent:file];
        [self copyAndExcludeFromBackup:pathToCopy destinationPath:destinationPath];
/*        BOOL fileExists = [fileMan fileExistsAtPath:pathToCopy isDirectory:&isDirectory];
        if (fileExists && !isDirectory) {
            // copy if destination does not exist or replace if source is newer
            if (![fileMan fileExistsAtPath:destinationPath])
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:nil];
            else if ([self compareFileModDates:pathToCopy destinationPath:destinationPath] > 0) {
                [fileMan removeItemAtPath:destinationPath error:&error];
                if (error)
                    [self KMLog:[NSString stringWithFormat:@"%@", error.description] checkDebugPrinting:NO];
                [fileMan copyItemAtPath:pathToCopy toPath:destinationPath error:&error];
                if (error)
                    [self KMLog:[NSString stringWithFormat:@"%@", error.description] checkDebugPrinting:NO];
            }
        }*/
    }
    
    return YES;
}

@end
