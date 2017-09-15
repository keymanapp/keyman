//
//  KMManager+Internal.h
//  Keyman Engine
//
//  Created by Patrick Weekes on 2012-05-16.
//  Updated by Serkan Kurt on 17/05/2013.
//  Copyright (c) 2017 SIL International. All rights reserved.
//
//  This file contains methods which the Keyman Engine SDK uses internally, but which should not be available to developers using the SDK

#import "KMManager.h"
#import "Reachability.h"
#import "KMInputViewController.h"
#import "KMHTTPDownloader.h"

extern NSString *const kKeymanApiBaseURL;
extern NSString *const kKeymanOptionsKey;
extern NSString *const kKeymanLanguageKey;
extern NSString *const kKeymanKeyboardCopyrightKey;

@protocol KMWebViewDelegate;

typedef void (^KMFetchKeyboardsBlock)(NSDictionary*);

@interface KMManager ()

// The protocol is not strictly enforced in order to hide it from the publically-viewable classes which implement it
@property (nonatomic, weak) id/*<KMWebViewDelegate>*/ webDelegate;
@property (nonatomic, weak) id/*<KMWebViewDelegate>*/ inputDelegate;
@property (nonatomic, strong) NSArray *languages;
@property (nonatomic, strong) NSDictionary *options; // dictionary of Keyman options i.e. base uri for keyboards and fonts
@property (nonatomic, copy, readonly) NSString *keyboardID;
@property (nonatomic, copy, readonly) NSString *languageID;
@property (nonatomic, strong) NSMutableDictionary *keyboardsInfo;
@property (nonatomic, strong) NSMutableDictionary *keyboardsDictionary;
@property (nonatomic, strong) NSMutableDictionary *keymanFonts;
//@property (nonatomic, strong) NSDateFormatter *dateFormatter;
@property (nonatomic, strong) Reachability *reachability;
@property (nonatomic, strong) KMHTTPDownloader *downloadQueue;
@property (nonatomic, strong) KMHTTPDownloader *sharedQueue;
@property (nonatomic, strong) HTTPDownloadRequest *currentRequest;
@property (assign) BOOL shouldReloadKeyboard;
@property (assign) BOOL didSynchronize;
@property (assign) BOOL blockInsertText;
@property (nonatomic, assign) BOOL isSystemKeyboardTopBarEnabled;

+ (UIView *)inputView;
+ (void)setKMSelectionRange:(NSRange)range manually:(BOOL)manually;
+ (void)clearText;
+ (void)setKMText:(NSString *)text;
+ (BOOL)retinaScreen;
+ (void)fetchKeyboardsWithCompletionBlock:(KMFetchKeyboardsBlock)completionBlock;
+ (BOOL)isKeymanSystemKeyboard;
+ (BOOL)isWebKitEnabled;

// TODO: take all this language stuff and formalize it
//			- some should be public
//			- some should possibly be in a protocol
- (NSUserDefaults *)activeUserDefaults;
- (NSArray *)keyboardsForIndex:(NSInteger)index;
- (NSDictionary *)keyboardInfoForLanguageIndex:(NSInteger)languageIndex keyboardIndex:(NSInteger)keyboardIndex;
- (void)downloadKeyboardForLanguageIndex:(NSInteger)languageIndex keyboardIndex:(NSInteger)keyboardIndex isUpdate:(BOOL)isUpdate;
- (NSString *)keyboardIdForCurrentRequest;
- (NSString *)jsFontFromFontDictionary:(NSDictionary *)fontDict;
- (NSInteger)switchToNextKeyboard;
- (NSInteger)currentKeyboardIndex;
- (void)updateKeyboardVersionForID:(NSString *)kbID newKeyboardVersion:(NSString *)kbVersion;
- (NSString *)latestKeyboardFileVersionWithID:(NSString *)keyboardID;
- (BOOL)isRTLKeyboardWithID:(NSString *)keyboardID languageID:(NSString *)languageID;
- (BOOL)canAccessSharedContainer;
- (BOOL)isSubKeysMenuVisible;
- (void)KMLog:(NSString *)logStr checkDebugPrinting:(BOOL)checkDebugPrinting;

// InputViewController methods
- (void)updateViewConstraints;
- (void)inputViewDidLoad;
- (void)inputViewWillRotateToInterfaceOrientation:(UIInterfaceOrientation)toInterfaceOrientation duration:(NSTimeInterval)duration;
- (void)showKeyboardMenu:(KMInputViewController *)ic closeButtonTitle:(NSString *)closeButtonTitle;
- (void)dismissKeyboardMenu;
- (BOOL)isKeyboardMenuVisible;
- (void)reloadKeyboard;

@end

@protocol KMWebViewDelegate <NSObject>
@required
- (void)updatedFragment:(NSString *)fragment;
@end
