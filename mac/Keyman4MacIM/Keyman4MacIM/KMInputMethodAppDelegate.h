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
#import "KMInputController.h"
#import "KMAboutWindowController.h"
#import "KMInfoWindowController.h"
#import "KMKeyboardHelpWindowController.h"
#import "OSKWindowController.h"
#import "NSWindow+SuppMethods.h"
#import "NSString+SuppMethods.h"

extern NSString *const kKMSelectedKeyboardKey;
extern NSString *const kKMActiveKeyboardsKey;
extern NSString *const kKeymanKeyboardDownloadCompletedNotification;

extern NSString *const kPackage;
extern NSString *const kButtons;
extern NSString *const kStartMenu;
extern NSString *const kStartMenuEntries;
extern NSString *const kInfo;
extern NSString *const kFiles;

extern NSString *const kAuthor;
extern NSString *const kCopyright;
extern NSString *const kFile;
extern NSString *const kFont;
extern NSString *const kGraphicFile;
extern NSString *const kKeyboard;
extern NSString *const kName;
extern NSString *const kReadMeFile;
extern NSString *const kVersion;
extern NSString *const kWebSite;

@interface KMInputMethodAppDelegate : NSObject <NSAlertDelegate> {
    IBOutlet NSMenu *_menu;
}

@property (nonatomic, strong) KMEngine *kme;
@property (nonatomic, strong) KMXFile *kmx;
@property (nonatomic, strong) KVKFile *kvk;
@property (nonatomic, strong) NSString *keyboardsPath;
@property (nonatomic, strong) NSString *fontsPath;
@property (nonatomic, strong) NSMutableArray *kmxFileList;
@property (nonatomic, strong) NSString *selectedKeyboard;
@property (nonatomic, strong) NSMutableArray *activeKeyboards;
@property (nonatomic, strong) NSMutableString *contextBuffer;
@property (nonatomic, assign) NSEventModifierFlags currentModifierFlags;
@property (nonatomic, assign) BOOL contextChangingEventDetected;
@property (nonatomic, strong) OSKWindowController *oskWindow;
@property (nonatomic, strong) NSString *keyboardName;
@property (nonatomic, strong) NSImage *keyboardIcon;
@property (nonatomic, strong) NSAlert *downloadInfoView;
@property (nonatomic, strong) NSProgressIndicator *progressIndicator;
@property (nonatomic, weak) IMKInputController *inputController;
@property (nonatomic, strong) NSWindowController *configWindow;
@property (nonatomic, strong) NSWindowController *downloadKBWindow;
@property (nonatomic, strong) KMAboutWindowController *aboutWindow;
@property (nonatomic, strong) KMInfoWindowController *infoWindow;
@property (nonatomic, strong) KMKeyboardHelpWindowController *kbHelpWindow;
@property (nonatomic, strong) NSURLConnection *connection;
@property (nonatomic, strong) NSString *downloadFilename;
@property (nonatomic, strong) NSMutableData *receivedData;
@property (nonatomic, assign) NSUInteger expectedBytes;
@property (nonatomic, assign) BOOL alwaysShowOSK;
@property (nonatomic, assign) BOOL useVerboseLogging;
@property (nonatomic, assign) BOOL useNullChar;
@property (nonatomic, assign) BOOL debugMode;

- (NSMenu *)menu;
- (void)saveActiveKeyboards;
- (void)showAboutWindow;
- (void)showOSK;
- (void)showConfigurationWindow;
- (void)handleKeyEvent:(NSEvent *)event;
- (BOOL)unzipFile:(NSString *)filePath;
- (NSWindowController *)downloadKBWindow_;
- (NSWindowController *)aboutWindow_;
- (NSWindowController *)infoWindow_;
- (NSWindowController *)kbHelpWindow_;
- (void)processURL:(NSString*)rawUrl;
- (NSString *)kmxFilePathAtIndex:(NSUInteger)index;
- (NSString *)packagePathAtIndex:(NSUInteger)index;
- (NSInteger)indexForPackageFolder:(NSString *)packageFolder;
- (NSString *)packageFolderFromPath:(NSString *)path;
- (NSString *)packageNameFromFolder:(NSString *)packageFolder;
- (NSDictionary *)infoDictionaryFromFile:(NSString *)infoFile;
- (NSString *)kvkFilePathFromFilename:(NSString *)kvkFilename;
- (NSString *)oskWindowTitle;

@end

#endif /* KMInputMethodAppDelegate_h */
