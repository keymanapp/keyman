//
//  KMInputMethodAppDelegate.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 12/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#ifndef KMInputMethodAppDelegate_h
#define KMInputMethodAppDelegate_h

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>
#import "KMModifierMapping.h"
#import "KMPackageReader.h"
#import "KMInputController.h"
#import "KMAboutWindowController.h"
#import "KMInfoWindowController.h"
#import "KMKeyboardHelpWindowController.h"
#import "OSKWindowController.h"
#import "NSWindow+SuppMethods.h"
#import "NSString+SuppMethods.h"

typedef void(^PostEventCallback)(CGEventRef eventToPost);

extern NSString *const kKeymanKeyboardDownloadCompletedNotification;

typedef struct {
    NSString *sentryEnvironment;
    NSString *versionRelease;
    NSString *versionWithTag;
    NSString *versionGitTag;
    NSString *tier;
    NSString *keymanCom;
    NSString *helpKeymanCom;
    NSString *apiKeymanCom;
} KeymanVersionInfo;

// tags for default menu items, displayed whether keyboards are active or not
static const int DIVIDER_MENUITEM_TAG = -4;
static const int CONFIG_MENUITEM_TAG = -3;
static const int OSK_MENUITEM_TAG = -2;
static const int ABOUT_MENUITEM_TAG = -1;

// the number of menu items that do not represent active keyboards
static const int DEFAULT_KEYMAN_MENU_ITEM_COUNT = 4;

// the tag for the first keyboard dynamically added to the menu
static const int KEYMAN_FIRST_KEYBOARD_MENUITEM_TAG = 0;

// the menu index for the first keyboard dynamically added to the menu
static const int KEYMAN_FIRST_KEYBOARD_MENUITEM_INDEX = 0;

@interface KMInputMethodAppDelegate : NSObject
#define USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE 1
#ifdef USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE
    <NSAlertDelegate>
#endif
{
    IBOutlet NSMenu *_menu;
}

@property (nonatomic, strong) KMEngine *kme;
// TODO: refactor and encapsulate below properties with current keyboard
@property (nonatomic, strong, readonly) KMXFile *kmx;
@property (nonatomic, strong, readonly) KMModifierMapping *modifierMapping;
@property (nonatomic, strong) KVKFile *kvk;
@property (nonatomic, strong) NSString *keyboardName;
@property (nonatomic, strong) NSString *selectedKeyboard;
@property (nonatomic, strong) NSImage *keyboardIcon;
// TODO: refactor above properties
@property (nonatomic, strong) NSString *keyboardsPath;
@property (nonatomic, strong) NSString *fontsPath;
@property (nonatomic, strong) NSMutableArray *kmxFileList;
@property (nonatomic, strong) NSMutableArray *activeKeyboards;
@property (assign) int numberOfKeyboardMenuItems;
@property (nonatomic, strong) NSMutableString *contextBuffer;
@property (nonatomic, assign) NSEventModifierFlags currentModifiers;
@property (nonatomic, assign) CFMachPortRef lowLevelEventTap;
@property (nonatomic, assign) CFRunLoopSourceRef runLoopEventSrc;
@property (nonatomic, assign) BOOL contextChangedByLowLevelEvent;
@property (nonatomic, strong) OSKWindowController *oskWindow;
@property (nonatomic, strong) NSAlert *downloadInfoView;
@property (nonatomic, strong) NSProgressIndicator *progressIndicator;
@property (nonatomic, weak) KMInputController *inputController;
@property (nonatomic, strong) NSWindowController *configWindow;
@property (nonatomic, strong) NSWindowController *downloadKBWindow;
@property (nonatomic, strong) KMAboutWindowController *aboutWindow;
@property (nonatomic, strong) KMInfoWindowController *infoWindow;
@property (nonatomic, strong) KMKeyboardHelpWindowController *kbHelpWindow;
@property (nonatomic, strong) NSURLConnection *connection;
@property (nonatomic, strong) NSString *downloadFilename;
@property (nonatomic, strong) NSMutableData *receivedData;
@property (nonatomic, assign) NSUInteger expectedBytes;

- (NSMenu *)menu;
- (void)saveActiveKeyboards;
- (void)applyPersistedOptions;
- (void)showAboutWindow;
- (void)showOSK;
- (void)showConfigurationWindow;
- (void)selectKeyboardFromMenu:(NSInteger)tag;
- (void)handleKeyEvent:(NSEvent *)event;
- (void)loadKeyboardFromKmxFile:(KMXFile *)kmx;
- (void)resetKmx;
- (NSEventModifierFlags) determineModifiers;
- (BOOL)unzipFile:(NSString *)filePath;
- (NSWindowController *)downloadKBWindow_;
- (NSWindowController *)aboutWindow_;
- (NSWindowController *)infoWindow_;
- (NSWindowController *)kbHelpWindow_;
- (void)processURL:(NSString*)rawUrl;
- (void)downloadKeyboardFromKeyboardId:(NSString *)keyboardId;
- (NSString *)kmxFilePathAtIndex:(NSUInteger)index;
- (NSString *)packagePathAtIndex:(NSUInteger)index;
- (NSInteger)indexForPackageFolder:(NSString *)packageFolder;
- (NSString *)packageFolderFromPath:(NSString *)path;
- (KMPackageInfo *)loadPackageInfo:(NSString *)path;
- (NSString *)packageNameFromPackageInfo:(NSString *)packageFolder;
- (NSArray *)keyboardNamesFromFolder:(NSString *)packageFolder;
- (NSString *)kvkFilePathFromFilename:(NSString *)kvkFilename;
- (NSString *)oskWindowTitle;
- (void)postKeyboardEventWithSource: (CGEventSourceRef)source code:(CGKeyCode) virtualKey postCallback:(PostEventCallback)postEvent;
- (KeymanVersionInfo)versionInfo;
- (void)registerConfigurationWindow:(NSWindowController *)window;
@end

#endif /* KMInputMethodAppDelegate_h */
