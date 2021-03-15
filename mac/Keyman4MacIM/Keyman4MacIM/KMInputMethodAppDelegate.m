//
//  KMInputMethodAppDelegate.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 12/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

// *** TO INVESTIGATE ***
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (activateServerWithReply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (deactivateServerWithReply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (menusDictionaryWithClientAsync:reply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (modesWithClientAsync:reply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (commitCompositionWithReply:) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (hidePalettes) block performed very slowly (0.00 secs)
// Keyman4MacIM[6245]: IMK Stall detected, *please Report* your user scenario in <rdar://problem/16792073> - (sessionFinished) block performed very slowly (0.00 secs)

#import "KMInputMethodAppDelegate.h"
#import "KMConfigurationWindowController.h"
#import "KMDownloadKBWindowController.h"
#import "ZipArchive.h"
@import Sentry;

/** NSUserDefaults keys */
NSString *const kKMSelectedKeyboardKey = @"KMSelectedKeyboardKey";
NSString *const kKMActiveKeyboardsKey = @"KMActiveKeyboardsKey";
NSString *const kKMSavedStoresKey = @"KMSavedStoresKey";
NSString *const kKMAlwaysShowOSKKey = @"KMAlwaysShowOSKKey";
NSString *const kKMUseVerboseLogging = @"KMUseVerboseLogging";
NSString *const kKMLegacyApps = @"KMLegacyApps";

NSString *const kKeymanKeyboardDownloadCompletedNotification = @"kKeymanKeyboardDownloadCompletedNotification";

NSString *const kPackage = @"[Package]";
NSString *const kButtons = @"[Buttons]";
NSString *const kStartMenu = @"[StartMenu]";
NSString *const kStartMenuEntries = @"[StartMenuEntries]";
NSString *const kInfo = @"[Info]";
NSString *const kFiles = @"[Files]";

NSString *const kAuthor = @"Author";
NSString *const kCopyright = @"Copyright";
NSString *const kFile = @"File";
NSString *const kFont = @"Font";
NSString *const kGraphicFile = @"GraphicFile";
NSString *const kKeyboard = @"Keyboard";
NSString *const kName = @"Name";
NSString *const kReadMeFile = @"ReadMeFile";
NSString *const kVersion = @"Version";
NSString *const kWebSite = @"WebSite";
NSString *const kWelcome = @"Welcome";

typedef enum {
    ctPackage, ctButtons, ctStartMenu, ctStartMenuEntries, ctInfo, ctFiles, ctUnknown
} ContentType;

@implementation KMInputMethodAppDelegate
@synthesize kme = _kme;
@synthesize kmx = _kmx;
@synthesize kvk = _kvk;
@synthesize keyboardName = _keyboardName;
@synthesize keyboardsPath = _keyboardsPath;
@synthesize kmxFileList = _kmxFileList;
@synthesize selectedKeyboard = _selectedKeyboard;
@synthesize activeKeyboards = _activeKeyboards;
@synthesize contextBuffer = _contextBuffer;
@synthesize alwaysShowOSK = _alwaysShowOSK;

id _lastServerWithOSKShowing = nil;
NSString* _keymanDataPath = nil;

- (id)init {
    self = [super init];
    if (self) {
#ifdef DEBUG
        // If debugging, we'll turn it on by default, regardless of setting. If developer
        // really wants it off, they can either change this line of code temporarily or
        // go to the config screen and turn it off explicitly.
        _debugMode = YES;
#else
        _debugMode = self.useVerboseLogging;
#endif
        [[NSAppleEventManager sharedAppleEventManager] setEventHandler:self
                                                           andSelector:@selector(handleURLEvent:withReplyEvent:)
                                                         forEventClass:kInternetEventClass
                                                            andEventID:kAEGetURL];

        self.lowLevelEventTap = CGEventTapCreate(kCGAnnotatedSessionEventTap,
                                                 kCGHeadInsertEventTap,
                                                 kCGEventTapOptionListenOnly,
                                                 CGEventMaskBit(kCGEventFlagsChanged) |
                                                 CGEventMaskBit(kCGEventLeftMouseDown) |
                                                 CGEventMaskBit(kCGEventLeftMouseUp) |
                                                 CGEventMaskBit(kCGEventKeyDown),
                                                 (CGEventTapCallBack)eventTapFunction,
                                                 nil);

        if (!self.lowLevelEventTap) {
            NSLog(@"Can't tap into low level events!");
        }
        else {
            CFRelease(self.lowLevelEventTap);
        }

        self.runLoopEventSrc = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, self.lowLevelEventTap, 0);

        CFRunLoopRef runLoop = CFRunLoopGetCurrent();

        if (self.runLoopEventSrc && runLoop) {
            CFRunLoopAddSource(runLoop,  self.runLoopEventSrc, kCFRunLoopDefaultMode);
        }
    }

    return self;
}

- (KeymanVersionInfo)versionInfo {
    KeymanVersionInfo result;
    // Get version information from Info.plist, which is filled in 
    // by build.sh.
    NSDictionary *keymanInfo =[[[NSBundle mainBundle] infoDictionary] objectForKey:@"Keyman"];
    result.sentryEnvironment = [keymanInfo objectForKey:@"SentryEnvironment"];
    result.tier = [keymanInfo objectForKey:@"Tier"];
    result.versionRelease = [keymanInfo objectForKey:@"VersionRelease"];
    result.versionWithTag = [keymanInfo objectForKey:@"VersionWithTag"];
    if([result.tier isEqualToString:@"stable"]) {
        result.keymanCom = @"keyman.com";
        result.helpKeymanCom = @"help.keyman.com";
        result.apiKeymanCom = @"api.keyman.com";
    } 
    else {
        result.keymanCom = @"keyman-staging.com";
        result.helpKeymanCom = @"help.keyman-staging.com";
        result.apiKeymanCom = @"api.keyman-staging.com";
    }
    return result;
}

-(void)applicationDidFinishLaunching:(NSNotification *)aNotification {
    [[NSUserDefaults standardUserDefaults] registerDefaults:@{ @"NSApplicationCrashOnExceptions": @YES }];

    KeymanVersionInfo keymanVersionInfo = [self versionInfo];
    NSString *releaseName = [NSString stringWithFormat:@"release-%@", keymanVersionInfo.versionWithTag];
    
    [SentrySDK startWithConfigureOptions:^(SentryOptions *options) {
        options.dsn = @"https://960f8b8e574c46e3be385d60ce8e1fea@sentry.keyman.com/9";
        options.releaseName = releaseName;
        options.environment = keymanVersionInfo.sentryEnvironment;
        // options.debug = @YES; 
    }];
    
    // [SentrySDK captureMessage:@"Starting Keyman [test message]"];
}

#ifdef USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE
- (BOOL)alertShowHelp:(NSAlert *)alert {
    NSLog(@"Sentry - KME: Got call to force crash from engine");
    [SentrySDK crash];
    NSLog(@"Sentry - KME: should not have gotten this far!");
    return NO;
}
#endif

- (void)handleURLEvent:(NSAppleEventDescriptor*)event withReplyEvent:(NSAppleEventDescriptor*)replyEvent {

    [self processURL:[[event paramDescriptorForKeyword:keyDirectObject] stringValue]];
}

- (void)processURL:(NSString*)rawUrl {
    NSMutableString *urlStr = [NSMutableString stringWithString:rawUrl];
    [urlStr replaceOccurrencesOfString:@"keyman:" withString:@"keyman/" options:0 range:NSMakeRange(0, 7)];
    NSURL *url = [NSURL URLWithString:urlStr];
    if (self.debugMode)
        NSLog(@"url = %@", url);

    if ([url.lastPathComponent isEqualToString:@"download"]) {
        if (_connection != nil) {
            if (self.debugMode)
                NSLog(@"Already downloading a keyboard.");
            return;
        }

        NSURL *downloadUrl;
        NSArray *params = [[url query] componentsSeparatedByString:@"&"];
        for (NSString *value in params) {
            NSUInteger index = NSNotFound;
            if ((index = [value rangeOfString:@"filename="].location) != NSNotFound)
                _downloadFilename = [NSString stringWithString:[value substringFromIndex:index+9]];
            else if ((index = [value rangeOfString:@"url="].location) != NSNotFound) {
                NSString *urlString = [NSString stringWithString:[value substringFromIndex:index+4]];
                if ([urlString respondsToSelector:@selector(stringByRemovingPercentEncoding)])
                    urlString = [urlString stringByRemovingPercentEncoding];
                else if ([urlString respondsToSelector:@selector(stringByReplacingPercentEscapesUsingEncoding:)]) {
                    // OS version prior to 10.9 - use this (now deprecated) method instead:
                    urlString = [urlString stringByReplacingPercentEscapesUsingEncoding:NSUTF8StringEncoding];
                }
                downloadUrl = [NSURL URLWithString:urlString];
            }
        }
    }
}

+ (KMInputMethodAppDelegate *)AppDelegate {
    return (KMInputMethodAppDelegate *)[NSApp delegate];
}

-(void) sleepFollowingDeactivationOfServer:(id)lastServer {
    if ([self debugMode]) {
        NSLog(@"Keyman no longer active IM.");
    }
    self.sleeping = YES;
    if ([self.oskWindow.window isVisible]) {
        if ([self debugMode]) {
            NSLog(@"Hiding OSK.");
        }
        // Storing this ensures that if the deactivation is temporary, resulting from dropping down a menu,
        // the OSK will re-display when that client application re-activates.
        _lastServerWithOSKShowing = lastServer;
        [self.oskWindow.window setIsVisible:NO];
    }
    if (self.lowLevelEventTap) {
        if ([self debugMode]) {
            NSLog(@"Disabling event tap...");
        }
        CGEventTapEnable(self.lowLevelEventTap, NO);
    }
}

-(void) wakeUpWith:(id)newServer {
    self.sleeping = NO;
    if (self.lowLevelEventTap && !CGEventTapIsEnabled(self.lowLevelEventTap)) {
        if ([self debugMode]) {
            NSLog(@"Keyman is now the active IM. Re-enabling event tap...");
        }
        CGEventTapEnable(self.lowLevelEventTap, YES);
    }
    // See note in sleepFollowingDeactivationOfServer.
    if (_kvk != nil && (_alwaysShowOSK || _lastServerWithOSKShowing == newServer)) {
        [self showOSK];
    }

    _lastServerWithOSKShowing = nil;
}

CGEventRef eventTapFunction(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *refcon) {
    KMInputMethodAppDelegate *appDelegate = [KMInputMethodAppDelegate AppDelegate];
    if (appDelegate != nil) {
        if (type == kCGEventTapDisabledByTimeout || type == kCGEventTapDisabledByUserInput) {
            // kCGEventTapDisabledByUserInput most likely means we're "sleeping", in which case we want it to stay
            // diabled until we get the wake-up call.
            if (!appDelegate.sleeping) {
                // REVIEW: We might need to consider putting in some kind of counter/flag to ensure that the very next
                // event is not another disable so we don't end up in an endless cycle.
                NSLog(@"Event tap disabled by %@! Attempting to restart...", (type == kCGEventTapDisabledByTimeout ? @"timeout" : @"user"));
                CGEventTapEnable(appDelegate.lowLevelEventTap, YES);
                if (!CGEventTapIsEnabled(appDelegate.lowLevelEventTap)) {
                    if (appDelegate.runLoopEventSrc) { // This should always be true
                        CFRunLoopRef runLoop = CFRunLoopGetCurrent();
                        if (runLoop) {
                            CFRunLoopRemoveSource(runLoop, appDelegate.runLoopEventSrc, kCFRunLoopDefaultMode);
                        }
                        appDelegate.runLoopEventSrc = nil;
                    }
                    appDelegate.lowLevelEventTap = nil;
                }
            }
            return event;
        }

        NSEvent* sysEvent = [NSEvent eventWithCGEvent:event];
        // Too many of these to be useful for most debugging sessions, but we'll keep this around to be
        // un-commented when needed.
        //if (appDelegate.debugMode)
        //    NSLog(@"System Event: %@", sysEvent);

        switch (type) {
            case kCGEventFlagsChanged:
                if (appDelegate.debugMode)
                    NSLog(@"System Event: flags changed: %x", (int) sysEvent.modifierFlags);
                appDelegate.currentModifierFlags = sysEvent.modifierFlags;
                if (appDelegate.currentModifierFlags & NSEventModifierFlagCommand) {
                    appDelegate.contextChangingEventDetected = YES;
                }
                break;

            case kCGEventLeftMouseUp:
            case kCGEventLeftMouseDown:
            case kCGEventOtherMouseUp:
            case kCGEventOtherMouseDown:
                appDelegate.contextChangingEventDetected = YES;
                break;

            case kCGEventKeyDown:
                // Pass back delete events through to the input method event handler
                // because some 'legacy' apps don't allow us to see back delete events
                // that we have synthesized (and we need to see them, for serialization
                // of events)
                if(sysEvent.keyCode == kVK_Delete && appDelegate.inputController != nil) {
                    [appDelegate.inputController handleDeleteBackLowLevel:sysEvent];
                }

                switch(sysEvent.keyCode) {
                    case kVK_Home:
                    case kVK_PageUp:
                    case kVK_End:
                    case kVK_PageDown:
                        // While these four may not actually move the cursor, this is application-dependent,
                        // so we'll treat them all as a context change. This means we lose deadkeys if these
                        // are pressed, but that's actually probably a good thing anyway!
                    case kVK_LeftArrow:
                    case kVK_RightArrow:
                    case kVK_DownArrow:
                    case kVK_UpArrow:
                        // Cursor movement means that we need to recheck the context
                    case kVK_Escape:
                        // Escape key should be treated as a context reset
                    case kVK_F1:
                    case kVK_F2:
                    case kVK_F3:
                    case kVK_F4:
                    case kVK_F5:
                    case kVK_F6:
                    case kVK_F7:
                    case kVK_F8:
                    case kVK_F9:
                    case kVK_F10:
                    case kVK_F11:
                    case kVK_F12:
                    case kVK_F13:
                    case kVK_F14:
                    case kVK_F15:
                    case kVK_F16:
                    case kVK_F17:
                    case kVK_F18:
                    case kVK_F19:
                    case kVK_F20:
                        // Function keys are command keys, so context is invalid
                    case kVK_Tab:
                        // A tab between cells in a Word document, for instance, is a context change
                    case kVK_ForwardDelete:
                        // Text modification by the forward delete key should reset context, but
                        // note that normal Delete (aka Bksp) is already managed by Keyman correctly.
                        appDelegate.contextChangingEventDetected = YES;
                        break;
                }
                break;

            default:
                break;
        }
    }
    return event;
}

- (NSMenu *)menu {
    return _menu;
}

- (KMEngine *)kme {
    if (_kme == nil) {
        _kme = [[KMEngine alloc] initWithKMX:nil contextBuffer:self.contextBuffer];
        [_kme setDebugMode:self.debugMode];
    }

    return _kme;
}

- (void)setKmx:(KMXFile *)kmx {
    _kmx = kmx;
    [self.kme setKmx:_kmx];
}

- (void)setKvk:(KVKFile *)kvk {
    _kvk = kvk;
    if (_oskWindow != nil)
        [_oskWindow resetOSK];
}

- (void)setKeyboardName:(NSString *)keyboardName {
    _keyboardName = keyboardName;
    if (_oskWindow != nil)
        [_oskWindow.window setTitle:self.oskWindowTitle];
}

- (void)loadSavedStores {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    NSDictionary *allSavedStores = [userData dictionaryForKey:kKMSavedStoresKey];
    if (!allSavedStores) return;
    NSDictionary *savedStores = [allSavedStores objectForKey:_selectedKeyboard];
    if (!savedStores) return;

    NSNumberFormatter *fmt = [[NSNumberFormatter alloc] init];
    fmt.numberStyle = NSNumberFormatterDecimalStyle;

    for (NSString *key in savedStores) {
        NSString *value = [savedStores objectForKey:key];
        NSUInteger storeID = [[fmt numberFromString:key] unsignedIntegerValue];
        [self.kme setStore:(DWORD)storeID withValue:value];
    }
}

- (void)saveStore:(NSNumber *)storeKey withValue:(NSString* )value {
    NSString *storeKeyStr = [storeKey stringValue];
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    NSDictionary *allSavedStores = [userData dictionaryForKey:kKMSavedStoresKey];
    NSDictionary *savedStores;

    if (allSavedStores) {
        savedStores = [allSavedStores objectForKey:_selectedKeyboard];
    }

    if (savedStores) {
        NSMutableDictionary *newSavedStores = [savedStores mutableCopy];
        [newSavedStores setObject:value forKey:storeKeyStr];
        savedStores = newSavedStores;
    } else {
        savedStores = [[NSDictionary alloc] initWithObjectsAndKeys:value, storeKeyStr, nil];
    }

    if (allSavedStores) {
        NSMutableDictionary *newAllSavedStores = [allSavedStores mutableCopy];
        [newAllSavedStores setObject:savedStores forKey:_selectedKeyboard];
        allSavedStores = newAllSavedStores;
    } else {
        allSavedStores = [[NSDictionary alloc] initWithObjectsAndKeys:savedStores, _selectedKeyboard, nil];
    }

    [userData setObject:allSavedStores forKey:kKMSavedStoresKey];
    [userData synchronize];
}

- (NSString *)oskWindowTitle {
    if (_keyboardName == nil || !_keyboardName.length)
        return [NSString stringWithFormat:@"Keyman"];
    else
        return [NSString stringWithFormat:@"%@ - Keyman", _keyboardName];
}

- (void)setAlwaysShowOSK:(BOOL)alwaysShowOSK {
    _alwaysShowOSK = alwaysShowOSK;
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setBool:alwaysShowOSK forKey:kKMAlwaysShowOSKKey];
    [userData synchronize];
}

- (void)setUseVerboseLogging:(BOOL)useVerboseLogging {
    NSLog(@"Turning verbose logging %@", useVerboseLogging ? @"on." : @"off.");
    _debugMode = useVerboseLogging;
    if (_kme != nil)
        [_kme setUseVerboseLogging:useVerboseLogging];
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setBool:useVerboseLogging forKey:kKMUseVerboseLogging];
    [userData synchronize];
}

- (BOOL)alwaysShowOSK {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    _alwaysShowOSK = [userData boolForKey:kKMAlwaysShowOSKKey];
    return _alwaysShowOSK;
}

- (BOOL)useVerboseLogging {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    return [userData boolForKey:kKMUseVerboseLogging];
}

/**
 * Locate and create the Keyman data path; currently in ~/Documents/Keyman-Keyboards
 */
- (NSString *)keymanDataPath {
    if(_keymanDataPath == nil) {
        NSString *documentDirPath = [NSSearchPathForDirectoriesInDomains(NSDocumentDirectory, NSUserDomainMask, YES) objectAtIndex:0];
        _keymanDataPath = [documentDirPath stringByAppendingPathComponent:@"Keyman-Keyboards"];

        NSFileManager *fm = [NSFileManager defaultManager];
        if (![fm fileExistsAtPath:_keymanDataPath]) {
            [fm createDirectoryAtPath:_keymanDataPath withIntermediateDirectories:YES attributes:nil error:nil];
        }
    }
    return _keymanDataPath;
}

/**
 * Returns the list of user-default legacy apps
 */
- (NSArray *)legacyAppsUserDefaults { 
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    return [userData arrayForKey:kKMLegacyApps];
}

/**
 * Returns the root folder where keyboards are stored; currently the same
 * as the keymanDataPath, but may diverge in future versions (possibly a sub-folder)
 */
- (NSString *)keyboardsPath {
    if (_keyboardsPath == nil) {
        _keyboardsPath = [self keymanDataPath];
    }

    return _keyboardsPath;
}

- (NSArray *)kmxFileList {
    if (_kmxFileList == nil) {
        NSArray *kmxFiles = [self KMXFiles];
        _kmxFileList = [[NSMutableArray alloc] initWithCapacity:0];
        NSMutableArray *others = nil;
        for (NSString *filePath in kmxFiles) {
            NSString *packageFolder = [self packageFolderFromPath:filePath];
            NSInteger index = [self indexForPackageFolder:packageFolder];
            if ([packageFolder isEqualToString:@"Others"]) {
                if (others == nil)
                    others = [NSMutableArray arrayWithCapacity:0];
                [others addObject:filePath];
            }
            else {
                if (index >= 0) {
                    [[_kmxFileList objectAtIndex:index] addObject:filePath];
                }
                else {
                    NSMutableArray *pArray = [[NSMutableArray alloc] initWithObjects:filePath, nil];
                    [_kmxFileList addObject:pArray];
                }
            }
        }

        if (others != nil)
            [_kmxFileList addObject:others];
    }

    return _kmxFileList;
}

- (NSString *)kmxFilePathAtIndex:(NSUInteger)index {
    NSUInteger x = 0;
    NSUInteger len = _kmxFileList.count;
    for (int i = 0; i < len; i++) {
        x++;
        NSArray *pArray = (NSArray *)[_kmxFileList objectAtIndex:i];
        for (NSString *path in pArray) {
            if (index == x)
                return path;
            x++;
        }
    }

    return nil;
}

- (NSString *)packagePathAtIndex:(NSUInteger)index {
    NSString *packagePath = nil;
    NSUInteger x = 0;
    NSUInteger len = _kmxFileList.count;
    for (int i = 0; i < len; i++) {
        NSArray *pArray = (NSArray *)[_kmxFileList objectAtIndex:i];
        if (!pArray.count) {
            i++;
            x++;
            continue;
        }

        if (index >= x && index <= (x+pArray.count)) {
            packagePath = [[pArray objectAtIndex:0] stringByDeletingLastPathComponent];
            break;
        }

        x += (pArray.count+1);
    }

    return packagePath;
}

- (NSInteger)indexForPackageFolder:(NSString *)packageFolder {
    NSInteger index = -1;
    NSUInteger len = _kmxFileList.count;
    for (int i = 0; i < len; i++) {
        NSArray *pArray = (NSArray *)[_kmxFileList objectAtIndex:i];
        if (pArray.count > 0) {
            NSString *packageFolder2 = [self packageFolderFromPath:[pArray objectAtIndex:0]];
            if ([packageFolder isEqualToString:packageFolder2]) {
                index = i;
                break;
            }
        }
    }

    return index;
}

- (NSString *)packageFolderFromPath:(NSString *)path {
    NSString *packageFolder = nil;
    NSString *sourcePath = [self keyboardsPath];
    NSString *mPath = [NSString stringWithString:[path stringByDeletingLastPathComponent]];
    if ([mPath isEqualToString:sourcePath])
        return @"Others";

    while (![mPath isEqualToString:sourcePath]) {
        packageFolder = [mPath lastPathComponent];
        mPath = [mPath stringByDeletingLastPathComponent];
    }

    return packageFolder;
}

- (NSString *)packageNameFromFolder:(NSString *)packageFolder {
    NSString *packageName = nil;
    NSString *path = [[self keyboardsPath] stringByAppendingPathComponent:packageFolder];
    NSString *fileContents = [NSString stringWithContentsOfFile:[path stringByAppendingPathComponent:@"kmp.inf"] encoding:NSUTF8StringEncoding error:NULL];
    NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
    BOOL hasInfo = NO;
    for (NSString *line in lines) {
        if ([line startsWith:@"[Info]"]) {
            hasInfo = YES;
            continue;
        }

        if (hasInfo && [line startsWith:@"Name="]) {
            NSString *value = [[[line substringFromIndex:5] componentsSeparatedByString:@","] objectAtIndex:0];
            packageName = [NSString stringWithString:[value stringByReplacingOccurrencesOfString:@"\"" withString:@""]];
            break;
        }
    }

    if (packageName == nil)
        packageName = packageFolder;

    return packageName;
}

- (NSArray *)keyboardNamesFromFolder:(NSString *)packageFolder {
    NSMutableArray *kbNames = [[NSMutableArray alloc] initWithCapacity:0];;
    for (NSString *kmxFile in [self KMXFilesAtPath:packageFolder]) {
        NSDictionary * infoDict = [KMXFile infoDictionaryFromFilePath:kmxFile];
        if (infoDict != nil) {
            NSString *name = [infoDict objectForKey:kKMKeyboardNameKey];
            if (name != nil && [name length]) {
                if (self.debugMode)
                    NSLog(@"Adding keyboard name: %@", name);
                [kbNames addObject:name];
            }
        }
    }
    return kbNames;
}

- (NSDictionary *)infoDictionaryFromFile:(NSString *)infoFile {
    NSMutableDictionary *infoDict = [NSMutableDictionary dictionaryWithCapacity:0];
    NSMutableArray *files = [NSMutableArray arrayWithCapacity:0];
    NSMutableArray *fonts = [NSMutableArray arrayWithCapacity:0];
    NSMutableArray *kbs = [NSMutableArray arrayWithCapacity:0];

    @try {
        NSString *fileContents = [[NSString stringWithContentsOfFile:infoFile encoding:NSUTF8StringEncoding error:NULL] stringByReplacingOccurrencesOfString:@"\r" withString:@""];
        NSArray *lines = [fileContents componentsSeparatedByString:@"\n"];
        ContentType contentType = ctUnknown;
        for (NSString *line in lines) {
            if (!line.length)
                continue;

            if ([line startsWith:kPackage]) {
                contentType = ctPackage;
                continue;
            }
            else if ([line startsWith:kButtons]) {
                contentType = ctButtons;
                continue;
            }
            else if ([line startsWith:kStartMenu]) {
                contentType = ctStartMenu;
                continue;
            }
            else if ([line startsWith:kStartMenuEntries]) {
                contentType = ctStartMenuEntries;
                continue;
            }
            else if ([line startsWith:kInfo]) {
                contentType = ctInfo;
                continue;
            }
            else if ([line startsWith:kFiles]) {
                contentType = ctFiles;
                continue;
            }

            switch (contentType) {
                case ctPackage: {
                    if ([line startsWith:kReadMeFile])
                        [infoDict setObject:[line substringFromIndex:kReadMeFile.length+1] forKey:kReadMeFile];
                    else if ([line startsWith:kGraphicFile])
                        [infoDict setObject:[line substringFromIndex:kGraphicFile.length+1] forKey:kGraphicFile];

                    break;
                }
                case ctButtons:
                    break;
                case ctStartMenu:
                    break;
                case ctStartMenuEntries: {
                    if ([line startsWith:kWelcome]) {
                        NSString *s = [line substringFromIndex:kWelcome.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kWelcome];
                    }

                    break;
                }
                case ctInfo: {
                    if ([line startsWith:kName]) {
                        NSString *s = [line substringFromIndex:kName.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kName];
                    }
                    else if ([line startsWith:kVersion]) {
                        NSString *s = [line substringFromIndex:kVersion.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kVersion];
                    }
                    else if ([line startsWith:kAuthor]) {
                        NSString *s = [line substringFromIndex:kAuthor.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kAuthor];
                    }
                    else if ([line startsWith:kCopyright]) {
                        NSString *s = [line substringFromIndex:kCopyright.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kCopyright];
                    }
                    else if ([line startsWith:kWebSite]) {
                        NSString *s = [line substringFromIndex:kWebSite.length+1];
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[vs objectAtIndex:0] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [infoDict setObject:@[v1, v2] forKey:kWebSite];
                    }

                    break;
                }
                case ctFiles: {
                    NSUInteger x = [line rangeOfString:@"="].location;
                    if (x == NSNotFound)
                        continue;

                    NSString *s = [line substringFromIndex:x+2];
                    if ([s startsWith:kFile]) {
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[[vs objectAtIndex:0] substringFromIndex:kFile.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [files addObject:@[v1, v2]];
                    }
                    else if ([s startsWith:kFont]) {
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[[vs objectAtIndex:0] substringFromIndex:kFont.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [fonts addObject:@[v1, v2]];
                    }
                    else if ([s startsWith:kKeyboard]) {
                        NSArray *vs = [s componentsSeparatedByString:@"\","];
                        NSString *v1 = [[[vs objectAtIndex:0] substringFromIndex:kKeyboard.length+1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        NSString *v2 = [[vs objectAtIndex:1] stringByReplacingOccurrencesOfString:@"\"" withString:@""];
                        [kbs addObject:@[v1, v2]];
                    }

                    break;
                }
                default:
                    break;
            }
        }
    }
    @catch (NSException *e) {
        NSLog(@"Error = %@", e.description);
        return nil;
    }

    if (files.count)
        [infoDict setValue:files forKey:kFile];
    if (fonts.count)
        [infoDict setValue:fonts forKey:kFont];
    if (kbs.count)
        [infoDict setValue:kbs forKey:kKeyboard];

    return infoDict.count?[NSDictionary dictionaryWithDictionary:infoDict]:nil;
}

- (NSString *)selectedKeyboard {
    if (_selectedKeyboard == nil) {
        NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
        _selectedKeyboard = [userData objectForKey:kKMSelectedKeyboardKey];
    }

    return _selectedKeyboard;
}

- (void)setSelectedKeyboard:(NSString *)selectedKeyboard {
    _selectedKeyboard = selectedKeyboard;
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setObject:_selectedKeyboard forKey:kKMSelectedKeyboardKey];
    [userData synchronize];
}

- (NSMutableArray *)activeKeyboards {
    if (!_activeKeyboards) {
        NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
        _activeKeyboards = [[userData arrayForKey:kKMActiveKeyboardsKey] mutableCopy];
        if (!_activeKeyboards)
            _activeKeyboards = [[NSMutableArray alloc] initWithCapacity:0];
    }

    return _activeKeyboards;
}

- (void)saveActiveKeyboards {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setObject:_activeKeyboards forKey:kKMActiveKeyboardsKey];
    [userData synchronize];
    [self resetActiveKeyboards];
    [self setKeyboardsSubMenu];
}

- (void)clearActiveKeyboards {
    NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
    [userData setObject:nil forKey:kKMActiveKeyboardsKey];
    [userData synchronize];
    [self setKeyboardsSubMenu];
}

- (void)resetActiveKeyboards {
    NSMutableArray *pathsToRemove = [[NSMutableArray alloc] initWithCapacity:0];
    for (NSString *path in self.activeKeyboards) {
        if (![[NSFileManager defaultManager] fileExistsAtPath:path])
            [pathsToRemove addObject:path];
    }

    if (pathsToRemove.count > 0) {
        [self.activeKeyboards removeObjectsInArray:pathsToRemove];
        NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
        [userData setObject:_activeKeyboards forKey:kKMActiveKeyboardsKey];
        [userData synchronize];
    }
}

- (NSMutableString *)contextBuffer {
    if (_contextBuffer == nil) {
        _contextBuffer = [[NSMutableString alloc] initWithString:@""];
    }

    return _contextBuffer;
}

- (void)setContextBuffer:(NSMutableString *)contextBuffer {
    _contextBuffer = [contextBuffer mutableCopy];
    if (_contextBuffer.length)
        [_contextBuffer replaceOccurrencesOfString:@"\0" withString:[NSString nullChar] options:0 range:NSMakeRange(0, 1)];
    [self.kme setContextBuffer:self.contextBuffer];
}

- (void)awakeFromNib {
    [self setKeyboardsSubMenu];

    NSMenuItem *config = [self.menu itemWithTag:2];
    if (config)
        [config setAction:@selector(menuAction:)];

    NSMenuItem *osk = [self.menu itemWithTag:3];
    if (osk)
        [osk setAction:@selector(menuAction:)];

    NSMenuItem *about = [self.menu itemWithTag:4];
    if (about)
        [about setAction:@selector(menuAction:)];
}

- (void)setKeyboardsSubMenu {
    NSMenuItem *keyboards = [self.menu itemWithTag:1];
    if (keyboards) {
        KVKFile *kvk = nil;
        BOOL didSetKeyboard = NO;
        NSInteger itag = 1000;
        [keyboards.submenu removeAllItems];
        for (NSString *path in self.activeKeyboards) {
            NSDictionary *infoDict = [KMXFile infoDictionaryFromFilePath:path];
            if (!infoDict)
                continue;
            //NSString *str = [NSString stringWithFormat:@"%@ (%@)", [infoDict objectForKey:kKMKeyboardNameKey], [infoDict objectForKey:kKMKeyboardVersionKey]];
            NSString *str = [infoDict objectForKey:kKMKeyboardNameKey];
            NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:str action:@selector(menuAction:) keyEquivalent:@""];
            [item setTag:itag++];
            if ([path isEqualToString:self.selectedKeyboard]) {
                [item setState:NSOnState];
                KMXFile *kmx = [[KMXFile alloc] initWithFilePath:path];
                [self setKmx:kmx];
                NSDictionary *kmxInfo = [KMXFile infoDictionaryFromFilePath:path];
                NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
                if (kvkFilename != nil) {
                    NSString *kvkFilePath = [self kvkFilePathFromFilename:kvkFilename];
                    if (kvkFilePath != nil)
                        kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
                }
                [self setKvk:kvk];
                [self setKeyboardName:[kmxInfo objectForKey:kKMKeyboardNameKey]];
                [self setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
                [self loadSavedStores];

                didSetKeyboard = YES;
            }
            else
                [item setState:NSOffState];

            [keyboards.submenu addItem:item];
        }

        if (keyboards.submenu.numberOfItems == 0) {
            NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:@"(None)" action:NULL keyEquivalent:@""];
            [keyboards.submenu addItem:item];
            [self setKmx:nil];
            [self setKvk:nil];
            [self setKeyboardName:nil];
            [self setKeyboardIcon:nil];
            [self setContextBuffer:nil];
            [self setSelectedKeyboard:nil];
        }
        else if (!didSetKeyboard) {
            [keyboards.submenu itemAtIndex:0].state = NSOnState;
            NSString *path = [self.activeKeyboards objectAtIndex:0];
            KMXFile *kmx = [[KMXFile alloc] initWithFilePath:path];
            [self setKmx:kmx];
            NSDictionary *kmxInfo = [KMXFile infoDictionaryFromFilePath:path];
            NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
            if (kvkFilename != nil) {
                NSString *kvkFilePath = [self kvkFilePathFromFilename:kvkFilename];
                if (kvkFilePath != nil)
                    kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
            }
            [self setKvk:kvk];
            [self setKeyboardName:[kmxInfo objectForKey:kKMKeyboardNameKey]];
            [self setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
            [self setContextBuffer:nil];
            [self loadSavedStores];
            [self setSelectedKeyboard:path];
        }
    }
}

- (NSArray *)KMXFiles {
    return [self KMXFilesAtPath:self.keyboardsPath];
}

- (NSArray *)KMXFilesAtPath:(NSString *)path {
    NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:path];
    NSMutableArray *kmxFiles = [[NSMutableArray alloc] initWithCapacity:0];
    NSString *filePath;
    while (filePath = (NSString *)[dirEnum nextObject]) {
        NSString *extension = [[filePath pathExtension] lowercaseString];
        if ([extension isEqualToString:@"kmx"])
            [kmxFiles addObject:[path stringByAppendingPathComponent:filePath]];
    }

    return kmxFiles;
}

- (NSArray *)KVKFiles {
    NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:self.keyboardsPath];
    NSMutableArray *kvkFiles = [[NSMutableArray alloc] initWithCapacity:0];
    NSString *filePath;
    while (filePath = (NSString *)[dirEnum nextObject]) {
        NSString *extension = [[filePath pathExtension] lowercaseString];
        if ([extension isEqualToString:@"kvk"])
            [kvkFiles addObject:[self.keyboardsPath stringByAppendingPathComponent:filePath]];
    }

    return kvkFiles;
}

- (NSString *)kvkFilePathFromFilename:(NSString *)kvkFilename {
    NSString *kvkFilePath = nil;
    NSArray *kvkFiles = [self KVKFiles];
    for (NSString *filePath in kvkFiles) {
        if ([[filePath lastPathComponent] isEqualToString:kvkFilename]) {
            kvkFilePath = filePath;
            break;
        }
    }

    return kvkFilePath;
}

- (NSWindowController *)oskWindow {
    if (!_oskWindow)
        _oskWindow = [[OSKWindowController alloc] initWithWindowNibName:@"OSKWindowController"];

    return _oskWindow;
}

- (void)showConfigurationWindow {
    if (self.debugMode)
        NSLog(@"Showing config window...");
    [self.configWindow.window centerInParent];
    [self.configWindow.window makeKeyAndOrderFront:nil];
    [self.configWindow.window setLevel:NSFloatingWindowLevel];
}

- (void)registerConfigurationWindow:(NSWindowController *)window {
    _configWindow = window;
}

- (void)showOSK {
    [[self.oskWindow window] makeKeyAndOrderFront:nil];
    [[self.oskWindow window] setLevel:NSStatusWindowLevel];
    [[self.oskWindow window] setTitle:self.oskWindowTitle];
}

- (void)showAboutWindow {
    [self.aboutWindow.window centerInParent];
    [self.aboutWindow.window makeKeyAndOrderFront:nil];
    [self.aboutWindow.window setLevel:NSFloatingWindowLevel];
}

/*
 * Wrappers for creating and destroying windows
 * TODO: the side-effect of creating the window and its controller should
 *       probably be managed better.
 */

- (NSWindowController *)configWindow {
    if (_configWindow.window == nil) {
        if (self.debugMode)
            NSLog(@"Creating config window...");
        _configWindow = [[KMConfigurationWindowController alloc] initWithWindowNibName:@"preferences"];
        [self observeCloseFor:_configWindow.window];
    }

    return _configWindow;
}

- (NSWindowController *)aboutWindow_ {
    return _aboutWindow;
}

- (NSWindowController *)aboutWindow {
    if (_aboutWindow.window == nil) {
        _aboutWindow = [[KMAboutWindowController alloc] initWithWindowNibName:@"KMAboutWindowController"];
    }

    return _aboutWindow;
}

- (NSWindowController *)infoWindow_ {
    return _infoWindow;
}

- (NSWindowController *)infoWindow {
    if (_infoWindow.window == nil) {
        _infoWindow = [[KMInfoWindowController alloc] initWithWindowNibName:@"KMInfoWindowController"];
        [self observeCloseFor:_infoWindow.window];
    }

    return _infoWindow;
}

- (NSWindowController *)kbHelpWindow_ {
    return _kbHelpWindow;
}

- (NSWindowController *)kbHelpWindow {
    if (_kbHelpWindow.window == nil) {
        _kbHelpWindow = [[KMKeyboardHelpWindowController alloc] initWithWindowNibName:@"KMKeyboardHelpWindowController"];
        [self observeCloseFor:_kbHelpWindow.window];
    }

    return _kbHelpWindow;
}

- (NSWindowController *)downloadKBWindow_ {
    return _downloadKBWindow;
}

- (NSWindowController *)downloadKBWindow {
    if (_downloadKBWindow.window == nil) {
        _downloadKBWindow = [[KMDownloadKBWindowController alloc] initWithWindowNibName:@"KMDownloadKBWindowController"];
        [self observeCloseFor:_downloadKBWindow.window];
    }

    return _downloadKBWindow;
}

/* 
 * Release windows after closing -- no need to keep them hanging about
 */

- (void)observeCloseFor:(NSWindow *)window {
    [[NSNotificationCenter defaultCenter] addObserver:self 
                                          selector:@selector(windowWillClose:)
                                          name:NSWindowWillCloseNotification
                                          object:window];
}

- (void)windowWillClose:(NSNotification *)notification {
    NSWindow* window = notification.object;
    if (_downloadKBWindow != nil && window == _downloadKBWindow.window) {
        _downloadKBWindow = nil;
    } else if(_kbHelpWindow != nil && window == _kbHelpWindow.window) {
        _kbHelpWindow = nil;
    } else if(_infoWindow != nil && window == _infoWindow.window) {
        _infoWindow = nil;
    } else if(_configWindow != nil && window == _configWindow.window) {
        _configWindow = nil;
    }
    [[NSNotificationCenter defaultCenter] removeObserver:self name:NSWindowWillCloseNotification object:window];
}

/*
 * Endpoints for download process
 * TODO: this should really be refactored
 */

- (void)alertDidEnd:(NSAlert *)alert returnCode:(NSInteger)returnCode contextInfo:(void *)contextInfo {
    NSButton *button = (NSButton *)[alert.buttons objectAtIndex:0];
    if (button.tag == -1) {
        [_connection cancel];
    }
    else if (button.tag == 1) {
        [_downloadKBWindow close];
        if (self.configWindow.window != nil) {
            [self.configWindow.window makeKeyAndOrderFront:nil];
            if (![[self.configWindow.window childWindows] containsObject:self.infoWindow.window]) {
                [self.configWindow.window addChildWindow:self.infoWindow.window ordered:NSWindowAbove];
            }
            [self.infoWindow.window centerInParent];
            [self.infoWindow.window makeKeyAndOrderFront:nil];
        }
        else {
            [self.infoWindow.window centerInParent];
            [self.infoWindow.window makeKeyAndOrderFront:nil];
            [self.infoWindow.window setLevel:NSFloatingWindowLevel];
        }

        NSString *packagePath = [self.keyboardsPath stringByAppendingPathComponent:[self.downloadFilename stringByDeletingPathExtension]];
        [self.infoWindow setPackagePath:packagePath];
    }

    _downloadInfoView = nil;
    _connection = nil;
    _downloadFilename = nil;
    _receivedData = nil;
    _expectedBytes = 0;
}

- (NSAlert *)downloadInfoView {
    if (_downloadInfoView == nil) {
        _downloadInfoView = [[NSAlert alloc] init];
        [_downloadInfoView setMessageText:@"Downloading..."];
        [_downloadInfoView setInformativeText:@""];
        [_downloadInfoView addButtonWithTitle:@"Cancel"];
        [_downloadInfoView setAlertStyle:NSInformationalAlertStyle];
        [_downloadInfoView setAccessoryView:self.progressIndicator];
    }

    return _downloadInfoView;
}

- (NSProgressIndicator *)progressIndicator {
    if (_progressIndicator == nil) {
        _progressIndicator = [[NSProgressIndicator alloc] initWithFrame:NSMakeRect(0, 0, 300, 20)];
        [_progressIndicator setIndeterminate:NO];
        [_progressIndicator setMinValue:0];
        [_progressIndicator setMaxValue:100];
        [_progressIndicator setDoubleValue:0];
    }

    return _progressIndicator;
}

- (void)downloadKeyboardFromKeyboardId:(NSString *)keyboardId {
    KeymanVersionInfo keymanVersionInfo = [self versionInfo];
    NSURL* url = [NSURL URLWithString:[NSString stringWithFormat:@"https://%@/go/package/download/%@?platform=macos&tier=%@",
        keymanVersionInfo.keymanCom, keyboardId, keymanVersionInfo.tier]];  //&bcp47=%@&update=0
    _downloadFilename = [NSString stringWithFormat:@"%@.kmp", keyboardId];
    [self downloadKeyboardFromURL:url];
}

- (void)downloadKeyboardFromURL:(NSURL *)url {
    NSURL* downloadUrl = url;

    if (downloadUrl && _downloadFilename) {
        if (_infoWindow.window != nil)
            [_infoWindow close];

        [self.downloadInfoView setInformativeText:self.downloadFilename];
        if (self.configWindow.window != nil) {
            [self.configWindow.window makeKeyAndOrderFront:nil];
            if (![[self.configWindow.window childWindows] containsObject:self.downloadKBWindow.window]) {
                [self.configWindow.window addChildWindow:self.downloadKBWindow.window ordered:NSWindowAbove];
            }
            [self.downloadKBWindow.window centerInParent];
            [self.downloadKBWindow.window makeKeyAndOrderFront:nil];
            [self.downloadInfoView beginSheetModalForWindow:self.downloadKBWindow.window
                                                modalDelegate:self
                                                didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                                                contextInfo:nil];
        }
        else {
            [self.downloadKBWindow.window centerInParent];
            [self.downloadKBWindow.window makeKeyAndOrderFront:nil];
            [self.downloadKBWindow.window setLevel:NSFloatingWindowLevel];
            [self.downloadInfoView beginSheetModalForWindow:self.downloadKBWindow.window
                                                modalDelegate:self
                                                didEndSelector:@selector(alertDidEnd:returnCode:contextInfo:)
                                                contextInfo:nil];
        }

        if (_connection == nil) {
            [_downloadInfoView setMessageText:@"Downloading..."];
            NSButton *button = (NSButton *)[_downloadInfoView.buttons objectAtIndex:0];
            [button setTitle:@"Cancel"];
            [button setTag:-1];
            [self.progressIndicator setDoubleValue:0];
            NSURLRequest *request = [NSURLRequest requestWithURL:url cachePolicy:NSURLRequestReloadIgnoringCacheData timeoutInterval:60];
            _receivedData = [[NSMutableData alloc] initWithLength:0];
            _connection = [[NSURLConnection alloc] initWithRequest:request delegate:self startImmediately:YES];
        }

    }
}

- (void)connection:(NSURLConnection *)connection didReceiveResponse:(NSURLResponse *)response {
    [self.receivedData setLength:0];
    self.expectedBytes = [response expectedContentLength];
}

- (void)connection:(NSURLConnection *)connection didReceiveData:(NSData *)data {
    [self.receivedData appendData:data];
    float progress = ((float)[self.receivedData length]/(float)self.expectedBytes)*100;
    [self.progressIndicator setDoubleValue:progress];
}

- (void)connection:(NSURLConnection *)connection didFailWithError:(NSError *)error {
    _connection = nil;
    _downloadFilename = nil;
    _receivedData = nil;
    _expectedBytes = 0;
}

- (NSURLRequest *)connection:(NSURLConnection *)connection
             willSendRequest:(nonnull NSURLRequest *)request
            redirectResponse:(nullable NSURLResponse *)response {
    return request;
}

- (NSCachedURLResponse *)connection:(NSURLConnection *)connection willCacheResponse:(NSCachedURLResponse *)cachedResponse {
    return nil;
}

- (void)connectionDidFinishLoading:(NSURLConnection *)connection {
    NSString *filePath = [self.keyboardsPath stringByAppendingPathComponent:self.downloadFilename];
    [self.receivedData writeToFile:filePath atomically:YES];
    [self unzipFile:filePath];
    [[NSFileManager defaultManager] removeItemAtPath:filePath error:nil];
    [_downloadInfoView setMessageText:@"Download Complete"];
    NSButton *button = (NSButton *)[_downloadInfoView.buttons objectAtIndex:0];
    [button setTitle:@"Done"];
    [button setTag:1];
    [[NSNotificationCenter defaultCenter] postNotificationName:kKeymanKeyboardDownloadCompletedNotification
                                                        object:self
                                                      userInfo:nil];
    _connection = nil;
    _receivedData = nil;
    _expectedBytes = 0;
}

- (void)handleKeyEvent:(NSEvent *)event {
    if (_oskWindow == nil)
        return;

    [_oskWindow.oskView handleKeyEvent:event];
}

extern const CGKeyCode kProcessPendingBuffer;

// This could more easily and logically be done in the input method, but this
// allows us to override the norma behavior for unit testing, where there is no
// active event loop to post to.
- (void)postKeyboardEventWithSource: (CGEventSourceRef)source code:(CGKeyCode) virtualKey postCallback:(PostEventCallback)postEvent{

    CGEventRef ev = CGEventCreateKeyboardEvent (source, virtualKey, true); //down
    if (postEvent)
        postEvent(ev);
    CFRelease(ev);
    if (virtualKey != kProcessPendingBuffer) { // special 0xFF code is not a real key-press, so no "up" is needed
        ev = CGEventCreateKeyboardEvent (source, virtualKey, false); //up
        if (postEvent)
            postEvent(ev);
        CFRelease(ev);
    }
}

- (BOOL)unzipFile:(NSString *)filePath {
    BOOL didUnzip = NO;
    NSString *fileName = filePath.lastPathComponent;
    NSString *folderName = [fileName stringByDeletingPathExtension];
    ZipArchive *za = [[ZipArchive alloc] init];
    if ([za UnzipOpenFile:filePath]) {
        NSString *destFolder = [self.keyboardsPath stringByAppendingPathComponent:folderName];
        if (self.debugMode) {
            NSLog(@"Unzipping %@ to %@", filePath, destFolder);
            if ([[NSFileManager defaultManager] fileExistsAtPath:destFolder])
                NSLog(@"The destiination folder already exists. Overwriting...");
        }
        didUnzip = [za UnzipFileTo:destFolder overWrite:YES];
        [za UnzipCloseFile];
    }

    if (didUnzip) {
        if (self.debugMode)
            NSLog(@"Unzipped file: %@", filePath);
        NSString * keyboardFolderPath = [self.keyboardsPath stringByAppendingPathComponent:folderName];
        [self installFontsAtPath:keyboardFolderPath];
        for (NSString *kmxFile in [self KMXFilesAtPath:keyboardFolderPath]) {
            if (self.debugMode)
                NSLog(@"Adding keyboard to list of active keyboards: %@", kmxFile);
            [self.activeKeyboards addObject:kmxFile];
        }
        [self saveActiveKeyboards];
    }
    else {
        if (self.debugMode) {
            NSLog(@"Failed to unzip file: %@", filePath);
        }
    }

    return didUnzip;
}

- (NSString *)fontsPath {
    if (_fontsPath == nil) {
        BOOL isDir;
        NSArray *paths = NSSearchPathForDirectoriesInDomains(NSLibraryDirectory, NSUserDomainMask, YES);

        if (paths.count == 1) {
            NSString *path = [[paths objectAtIndex:0] stringByAppendingPathComponent:@"Fonts"];
            if ([[NSFileManager defaultManager] fileExistsAtPath:path isDirectory:&isDir] && isDir)
                _fontsPath = [NSString stringWithString:path];
        }
    }

    return _fontsPath;
}

- (void)installFontsAtPath:(NSString *)path {
    NSString *fontsPath = self.fontsPath;
    if (fontsPath == nil)
        return;

    NSArray *fonts = [self FontFilesAtPath:path];
    for (NSString *srcPath in fonts) {
        NSString *destPath = [fontsPath stringByAppendingPathComponent:[srcPath lastPathComponent]];
        NSError *error;
        if ([[NSFileManager defaultManager] fileExistsAtPath:destPath])
            [[NSFileManager defaultManager] removeItemAtPath:destPath error:&error];

        if (error == nil)
            [[NSFileManager defaultManager] copyItemAtPath:srcPath toPath:destPath error:&error];

        if (error != nil)
            NSLog(@"Error = %@", error);
    }
}

- (NSArray *)FontFilesAtPath:(NSString *)path {
    NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:path];
    NSMutableArray *fontFiles = [[NSMutableArray alloc] initWithCapacity:0];
    NSString *filePath;
    while (filePath = (NSString *)[dirEnum nextObject]) {
        NSString *extension = [[filePath pathExtension] lowercaseString];
        if ([extension isEqualToString:@"ttf"] || [extension isEqualToString:@"otf"])
            [fontFiles addObject:[path stringByAppendingPathComponent:filePath]];
    }

    return fontFiles;
}

@end
