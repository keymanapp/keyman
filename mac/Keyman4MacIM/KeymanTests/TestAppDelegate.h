//
//  TestAppDelegate.h
//  Keyman4MacIM
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef TestAppDelegate_h
#define TestAppDelegate_h
#import <Foundation/Foundation.h>
#import <AppKit/AppKit.h>
#import <KeymanEngine4Mac/KeymanEngine4Mac.h>

@interface TestAppDelegate : NSObject<NSApplicationDelegate>

@property (nonatomic, strong) KMEngine *kme;
@property (nonatomic, strong) KMXFile *kmx;
//@property (nonatomic, strong) KVKFile *kvk;
//@property (nonatomic, strong) NSString *keyboardsPath;
//@property (nonatomic, strong) NSString *fontsPath;
//@property (nonatomic, strong) NSMutableArray *kmxFileList;
//@property (nonatomic, strong) NSString *selectedKeyboard;
//@property (nonatomic, strong) NSMutableArray *activeKeyboards;
@property (nonatomic, strong) NSMutableString *contextBuffer;
//@property (nonatomic, assign) NSEventModifierFlags currentModifierFlags;
@property (nonatomic, assign) CFMachPortRef lowLevelEventTap; // Always nil for tests
//@property (nonatomic, assign) CFRunLoopSourceRef runLoopEventSrc;
//@property (nonatomic, assign) BOOL sleeping;
@property (nonatomic, assign) BOOL contextChangingEventDetected;
//@property (nonatomic, strong) OSKWindowController *oskWindow;
//@property (nonatomic, strong) NSString *keyboardName;
//@property (nonatomic, strong) NSImage *keyboardIcon;
//@property (nonatomic, strong) NSAlert *downloadInfoView;
//@property (nonatomic, strong) NSProgressIndicator *progressIndicator;
//@property (nonatomic, weak) IMKInputController *inputController;
//@property (nonatomic, strong) NSWindowController *configWindow;
//@property (nonatomic, strong) NSWindowController *downloadKBWindow;
//@property (nonatomic, strong) KMAboutWindowController *aboutWindow;
//@property (nonatomic, strong) KMInfoWindowController *infoWindow;
//@property (nonatomic, strong) KMKeyboardHelpWindowController *kbHelpWindow;
//@property (nonatomic, strong) NSURLConnection *connection;
//@property (nonatomic, strong) NSString *downloadFilename;
//@property (nonatomic, strong) NSMutableData *receivedData;
//@property (nonatomic, assign) NSUInteger expectedBytes;
//@property (nonatomic, assign) BOOL alwaysShowOSK;
//@property (nonatomic, assign) BOOL useVerboseLogging;
@property (nonatomic, assign) BOOL useNullChar;
@property (nonatomic, assign) BOOL debugMode;

@end


#endif /* TestAppDelegate_h */
