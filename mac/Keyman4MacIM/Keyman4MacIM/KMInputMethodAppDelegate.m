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
#import "KMSettingsRepository.h"
#import "KMDataRepository.h"
#import "KMConfigurationWindowController.h"
#import "KMDownloadKBWindowController.h"
#import "ZipArchive.h"
#import "KMPackageReader.h"
#import "KMPackageInfo.h"
#import "KMKeyboardInfo.h"
#import "PrivacyConsent.h"
#import "KMLogs.h"
@import Sentry;

/** NSUserDefaults keys */
NSString *const kKMSelectedKeyboardKey = @"KMSelectedKeyboardKey";
NSString *const kKMActiveKeyboardsKey = @"KMActiveKeyboardsKey";
/**
 The following constant "KMSavedStoresKey" is left here for documentation
 though we have abandoned stores written to UserDefaults with this key because
 they used a less-reliable numeric key prior to integration with Keyman Core.
 It is replaced by the renamed "KMPersistedOptionsKey" which directly
 represents what it is saving.
 */
NSString *const kKMDeprecatedPersistedOptionsKey = @"KMSavedStoresKey";
NSString *const kKMPersistedOptionsKey = @"KMPersistedOptionsKey";
NSString *const kKMAlwaysShowOSKKey = @"KMAlwaysShowOSKKey";
NSString *const kKMUseVerboseLogging = @"KMUseVerboseLogging";

NSString *const kKeymanKeyboardDownloadCompletedNotification = @"kKeymanKeyboardDownloadCompletedNotification";

@implementation NSString (VersionNumbers)
/**
 * Returns a minimal version number by removing all '.0' from end of string,
 * e.g. '9.0.0' -> '9', '9.1.0' -> '9.1'.
 * Use to prepare version numbers for comparison with a `NSNumericSearch`, for example:
 * ```
 *   NSString* version1 = [fileVersion minimalVersionNumberString];
 *   NSString* version2 = [programVersion minimalVersionNumberString];
 *   [version1 compare:version2 options:NSNumericSearch]
 * ```
 */
- (NSString *)minimalVersionNumberString {
  static NSString *const unnecessaryVersionSuffix = @".0";
  NSString *minimalVersionNumber = self;
  
  while ([minimalVersionNumber hasSuffix:unnecessaryVersionSuffix]) {
    minimalVersionNumber = [minimalVersionNumber substringToIndex:minimalVersionNumber.length - unnecessaryVersionSuffix.length];
  }
  
  return minimalVersionNumber;
}
@end

@interface KMInputMethodAppDelegate ()
@property (nonatomic, strong) KMPackageReader *packageReader;
@end

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
    
    // first notify user and request access to Accessibility/PostEvent permissions
    // pass block as completion handler to complete init with initCompletion
    [PrivacyConsent.shared requestPrivacyAccess:^void (void){
      [self initCompletion];
    }];
  }
  
  return self;
}

- (void)initCompletion {
  os_log_info([KMLogs startupLog], "initCompletionHandler method invoked");
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
    os_log_error([KMLogs startupLog], "Unable to create lowLevelEventTap!");
  }
  else {
    os_log([KMLogs startupLog], "Successfully created lowLevelEventTap with CGEventTapCreate.");
    CFRelease(self.lowLevelEventTap);
  }
  
  self.runLoopEventSrc = CFMachPortCreateRunLoopSource(kCFAllocatorDefault, self.lowLevelEventTap, 0);
  
  CFRunLoopRef runLoop = CFRunLoopGetCurrent();
  
  if (self.runLoopEventSrc && runLoop) {
    CFRunLoopAddSource(runLoop,  self.runLoopEventSrc, kCFRunLoopDefaultMode);
  }
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
  result.versionGitTag = [keymanInfo objectForKey:@"VersionGitTag"];
  // if([result.tier isEqualToString:@"stable"]) {  // #7227 disabling:
  result.keymanCom = @"keyman.com";
  result.helpKeymanCom = @"help.keyman.com";
  result.apiKeymanCom = @"api.keyman.com";
  /*}
   else {
   result.keymanCom = @"keyman-staging.com";
   result.helpKeymanCom = @"help.keyman-staging.com";
   result.apiKeymanCom = @"api.keyman-staging.com";
   }*/
  return result;
}

-(void)applicationDidFinishLaunching:(NSNotification *)aNotification {
  [[NSUserDefaults standardUserDefaults] registerDefaults:@{ @"NSApplicationCrashOnExceptions": @YES }];
  
  [KMLogs reportLogStatus];
  
  [self startSentry];

  [self setDefaultKeymanMenuItems];
  [self updateKeyboardMenuItems];
  
  [self setPostLaunchKeymanSentryTags];
  // [SentrySDK captureMessage:@"Starting Keyman [test message]"];
}

- (void)startSentry {
  KeymanVersionInfo keymanVersionInfo = [self versionInfo];
  NSString *releaseName = [NSString stringWithFormat:@"%@", keymanVersionInfo.versionGitTag];
  
  [SentrySDK startWithConfigureOptions:^(SentryOptions *options) {
    options.dsn = @"https://960f8b8e574c46e3be385d60ce8e1fea@o1005580.ingest.sentry.io/5983522";
    options.releaseName = releaseName;
    options.environment = keymanVersionInfo.sentryEnvironment;
    //options.debug = YES;
  }];
}

- (void)setPostLaunchKeymanSentryTags {
  NSString *keyboardFileName = [self.kmx.filePath lastPathComponent];
   os_log_info([KMLogs keyboardLog], "initial kmx set to %{public}@", keyboardFileName);

  NSString *hasAccessibility = [PrivacyConsent.shared checkAccessibility]?@"Yes":@"No";

   // assign custom keyboard tag in Sentry to initial keyboard
   [SentrySDK configureScope:^(SentryScope * _Nonnull scope) {
      [scope setTagValue:hasAccessibility forKey:@"accessibilityEnabled"];
      [scope setTagValue:keyboardFileName forKey:@"keyboard"];
   }];
}

#ifdef USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE
- (BOOL)alertShowHelp:(NSAlert *)alert {
  os_log_error([KMLogs startupLog], "Sentry - KME: Got call to force crash from engine");
  [SentrySDK crash];
  os_log_error([KMLogs startupLog], "Sentry - KME: should not have gotten this far!");
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
  os_log_debug([KMLogs keyboardLog], "processURL, url = %{public}@", url);
  
  if ([url.lastPathComponent isEqualToString:@"download"]) {
    if (_connection != nil) {
      os_log_debug([KMLogs keyboardLog], "Already downloading a keyboard.");
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
        urlString = [urlString stringByRemovingPercentEncoding];
        downloadUrl = [NSURL URLWithString:urlString];
      }
    }
  }
}

+ (KMInputMethodAppDelegate *)AppDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

-(void) sleepFollowingDeactivationOfServer:(id)lastServer {
  os_log_debug([KMLogs lifecycleLog], "Keyman no longer active IM.");
  self.sleeping = YES;
  if ([self.oskWindow.window isVisible]) {
    os_log_debug([KMLogs oskLog], "sleepFollowingDeactivationOfServer, Hiding OSK.");
    // Storing this ensures that if the deactivation is temporary, resulting from dropping down a menu,
    // the OSK will re-display when that client application re-activates.
    _lastServerWithOSKShowing = lastServer;
    [self.oskWindow.window setIsVisible:NO];
  }
  if (self.lowLevelEventTap) {
    os_log_debug([KMLogs lifecycleLog], "sleepFollowingDeactivationOfServer, disabling event tap...");
    CGEventTapEnable(self.lowLevelEventTap, NO);
  }
}

-(void) wakeUpWith:(id)newServer {
  self.sleeping = NO;
  if (self.lowLevelEventTap && !CGEventTapIsEnabled(self.lowLevelEventTap)) {
    os_log_debug([KMLogs lifecycleLog], "wakeUpWith, Keyman is now the active IM. Re-enabling event tap...");
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
      // disabled until we get the wake-up call.
      if (!appDelegate.sleeping) {
        // REVIEW: We might need to consider putting in some kind of counter/flag to ensure that the very next
        // event is not another disable so we don't end up in an endless cycle.
        os_log([KMLogs eventsLog], "Event tap disabled by %{public}@! Attempting to restart...", (type == kCGEventTapDisabledByTimeout ? @"timeout" : @"user"));
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
    //    os_log_debug([KMLogs eventsLog], "System Event: %@", sysEvent);
    
    switch (type) {
      case kCGEventFlagsChanged:
        os_log_debug([KMLogs eventsLog], "eventTapFunction: system event kCGEventFlagsChanged to: %x", (int) sysEvent.modifierFlags);
        appDelegate.currentModifierFlags = sysEvent.modifierFlags;
        if (appDelegate.currentModifierFlags & NSEventModifierFlagCommand) {
          appDelegate.contextChangedByLowLevelEvent = YES;
        }
        break;
        
      case kCGEventLeftMouseUp:
      case kCGEventLeftMouseDown:
      case kCGEventOtherMouseUp:
      case kCGEventOtherMouseDown:
        os_log_debug([KMLogs eventsLog], "Event tap context invalidation flagged due to event of type: %u", type);
        appDelegate.contextChangedByLowLevelEvent = YES;
        break;
        
      case kCGEventKeyDown:
        os_log_debug([KMLogs eventsLog], "Event tap keydown event, keyCode: %hu", sysEvent.keyCode);
        // Pass back low-level backspace events to the input method event handler
        // because some non-compliant apps do not allow us to see backspace events
        // that we have generated (and we need to see them, for serialization
        // of events)
        if(sysEvent.keyCode == kVK_Delete && appDelegate.inputController != nil) {
          os_log_debug([KMLogs eventsLog], "Event tap handling kVK_Delete.");
          [appDelegate.inputController handleBackspace:sysEvent];
        } else if(sysEvent.keyCode == kVK_Delete) {
          os_log_debug([KMLogs eventsLog], "Event tap not handling kVK_Delete, appDelegate.inputController = %{public}@", appDelegate.inputController);
        }
        if(sysEvent.keyCode == 255) {
          os_log_debug([KMLogs eventsLog], "*** kKeymanEventKeyCode = 0xFF");
        } else {
          os_log_debug([KMLogs eventsLog], "*** other: %d(%x)", (char) sysEvent.keyCode, sysEvent.keyCode);
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
            appDelegate.contextChangedByLowLevelEvent = YES;
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
    _kme = [[KMEngine alloc] initWithKMX:nil context:self.contextBuffer verboseLogging:self.debugMode];
  }
  
  return _kme;
}

- (KMPackageReader *)packageReader {
  if (_packageReader == nil) {
    _packageReader = [[KMPackageReader alloc] init];
    [_packageReader setDebugMode:self.debugMode];
  }
  
  return _packageReader;
}

- (void)setKmx:(KMXFile *)kmx {
  _kmx = kmx;
  [self.kme setKmx:_kmx];
  
  NSString *keyboardFileName = [kmx.filePath lastPathComponent];
   os_log_info([KMLogs keyboardLog], "setKmx to %{public}@", keyboardFileName);

   // assign custom keyboard tag in Sentry to default keyboard
   [SentrySDK configureScope:^(SentryScope * _Nonnull scope) {
       [scope setTagValue:keyboardFileName forKey:@"keyboard"];
   }];
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

- (void)readPersistedOptions {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  NSDictionary *allPersistedOptions = [userData dictionaryForKey:kKMPersistedOptionsKey];
  if (!allPersistedOptions) {
    return;
  }
  NSDictionary *persistedOptionsForSelectedKeyboard = [allPersistedOptions objectForKey:_selectedKeyboard];
  if (!persistedOptionsForSelectedKeyboard) {
    os_log_info([KMLogs configLog], "no persisted options found in UserDefaults for keyboard %{public}@ ", _selectedKeyboard);
    return;
  }
  
  // TODO: pass array instead of making repeated calls
  for (NSString *key in persistedOptionsForSelectedKeyboard) {
    NSString *value = [persistedOptionsForSelectedKeyboard objectForKey:key];
    os_log_info([KMLogs configLog], "persisted options found in UserDefaults for keyboard %{public}@, key: %{public}@, value: %{public}@", _selectedKeyboard, key, value);
    [self.kme setCoreOptions:key withValue:value];
  }
}

- (void)writePersistedOptions:(NSString *)storeKey withValue:(NSString* )value {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  NSDictionary *allPersistedOptions = [userData dictionaryForKey:kKMPersistedOptionsKey];
  NSDictionary *persistedOptionsForSelectedKeyboard;
  
  if (allPersistedOptions) {
    persistedOptionsForSelectedKeyboard = [allPersistedOptions objectForKey:_selectedKeyboard];
  }
  
  if (persistedOptionsForSelectedKeyboard) {
    NSMutableDictionary *newSavedStores = [persistedOptionsForSelectedKeyboard mutableCopy];
    [newSavedStores setObject:value forKey:storeKey];
    persistedOptionsForSelectedKeyboard = newSavedStores;
  } else {
    persistedOptionsForSelectedKeyboard = [[NSDictionary alloc] initWithObjectsAndKeys:value, storeKey, nil];
  }
  
  if (allPersistedOptions) {
    NSMutableDictionary *newAllSavedStores = [allPersistedOptions mutableCopy];
    [newAllSavedStores setObject:persistedOptionsForSelectedKeyboard forKey:_selectedKeyboard];
    allPersistedOptions = newAllSavedStores;
  } else {
    allPersistedOptions = [[NSDictionary alloc] initWithObjectsAndKeys:persistedOptionsForSelectedKeyboard, _selectedKeyboard, nil];
  }
  
  [userData setObject:allPersistedOptions forKey:kKMPersistedOptionsKey];
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
  os_log_debug([KMLogs configLog], "Turning verbose logging %{public}@", useVerboseLogging ? @"on." : @"off.");
  _debugMode = useVerboseLogging;
  if (_kme != nil)
    [_kme setUseVerboseLogging:useVerboseLogging];
  if (_packageReader != nil) {
    [_packageReader setDebugMode:useVerboseLogging];
  }
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

#pragma mark - Keyman Data

/**
 * Returns the root folder where keyboards are stored
 */
- (NSString *)keyboardsPath {
  if (_keyboardsPath == nil) {
    _keyboardsPath = [KMDataRepository shared].keymanKeyboardsDirectory.path;
  }
  
  return _keyboardsPath;
}

- (NSArray *)kmxFileList {
  os_log_debug([KMLogs dataLog], "kmxFileList");
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

- (KMPackageInfo *)loadPackageInfo:(NSString *)path {
  return [self.packageReader loadPackageInfo:path];
}

- (NSString *)packageNameFromPackageInfo:(NSString *)packageFolder {
  NSString *packageName = nil;
  
  NSString *path = [[self keyboardsPath] stringByAppendingPathComponent:packageFolder];
  KMPackageInfo *packageInfo = [self.packageReader loadPackageInfo:path];
  
  if (packageInfo) {
    packageName = packageInfo.packageName;
  }
  
  return packageName;
}

- (NSArray *)keyboardNamesFromFolder:(NSString *)packageFolder {
  os_log_debug([KMLogs dataLog], "keyboardNamesFromFolder, folder = %{public}@", packageFolder);
  NSMutableArray *kbNames = [[NSMutableArray alloc] initWithCapacity:0];;
  for (NSString *kmxFile in [self KMXFilesAtPath:packageFolder]) {
    NSDictionary * infoDict = [KMXFile keyboardInfoFromKmxFile:kmxFile];
    if (infoDict != nil) {
      NSString *name = [infoDict objectForKey:kKMKeyboardNameKey];
      if (name != nil && [name length]) {
        os_log_debug([KMLogs configLog], "Adding keyboard name: %{public}@", name);
        [kbNames addObject:name];
      }
    }
  }
  return kbNames;
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
  [self updateKeyboardMenuItems];
}

- (void)clearActiveKeyboards {
  NSUserDefaults *userData = [NSUserDefaults standardUserDefaults];
  [userData setObject:nil forKey:kKMActiveKeyboardsKey];
  [userData synchronize];
  [self updateKeyboardMenuItems];
}

- (void)resetActiveKeyboards {
  // Remove entries with missing files
  NSMutableArray *pathsToRemove = [[NSMutableArray alloc] initWithCapacity:0];
  for (NSString *path in self.activeKeyboards) {
    if (![[NSFileManager defaultManager] fileExistsAtPath:path])
      [pathsToRemove addObject:path];
  }
  
  BOOL found = FALSE;
  if (pathsToRemove.count > 0) {
    [self.activeKeyboards removeObjectsInArray:pathsToRemove];
    found = TRUE;
  }
  
  // Remove duplicate entries
  NSUInteger i = 0;
  while(i < [self.activeKeyboards count]) {
    NSString *item = self.activeKeyboards[i];
    NSUInteger n = [self.activeKeyboards indexOfObject:item inRange: NSMakeRange(i+1, [self.activeKeyboards count]-i-1)];
    if(n != NSNotFound) {
      [self.activeKeyboards removeObjectAtIndex:n];
      found = TRUE;
    } else {
      i++;
    }
  }
  
  if (found) {
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
  [self.kme setCoreContextIfNeeded:self.contextBuffer];
}

- (void)awakeFromNib {
  [self prepareStorage];
}

/**
 * Prepare the app environment for all the things that need to be persisted:
 * namely, the keyboard data on disk and the settings in UserDefaults
 */
- (void)prepareStorage {
  [KMDataRepository.shared createDataDirectoryIfNecessary];
  
  if ([KMSettingsRepository.shared dataMigrationNeeded]) {
    BOOL movedData = [KMDataRepository.shared migrateData];
    [KMSettingsRepository.shared convertSettingsForMigration];
  }
  
  [KMDataRepository.shared createKeyboardsDirectoryIfNecessary];
  [KMSettingsRepository.shared setDataModelVersionIfNecessary];
}

- (void)setDefaultKeymanMenuItems {
  NSMenuItem *config = [self.menu itemWithTag:CONFIG_MENUITEM_TAG];
  if (config) {
    [config setAction:@selector(menuAction:)];
  }
  
  NSMenuItem *osk = [self.menu itemWithTag:OSK_MENUITEM_TAG];
  if (osk) {
    [osk setAction:@selector(menuAction:)];
  }
  
  NSMenuItem *about = [self.menu itemWithTag:ABOUT_MENUITEM_TAG];
  if (about) {
    [about setAction:@selector(menuAction:)];
  }
}

- (void)updateKeyboardMenuItems {
  self.numberOfKeyboardMenuItems = [self calculateNumberOfKeyboardMenuItems];
  [self removeDynamicKeyboardMenuItems];
  [self addDynamicKeyboardMenuItems];
}

- (int)calculateNumberOfKeyboardMenuItems {
  if (self.activeKeyboards.count == 0) {
    // if there are no active keyboards, then we will insert one placeholder menu item 'No Active Keyboards'
    return 1;
  } else {
    return (int) self.activeKeyboards.count;
  }
}

- (void)removeDynamicKeyboardMenuItems {
  int numberToRemove = (int) self.menu.numberOfItems - DEFAULT_KEYMAN_MENU_ITEM_COUNT;
  
  os_log_info([KMLogs keyboardLog], "*** removeDynamicKeyboardMenuItems, self.menu.numberOfItems = %ld, number of items to remove = %d", (long)self.menu.numberOfItems, numberToRemove);
  
  if (numberToRemove > 0) {
    for (int i = 0; i < numberToRemove; i++) {
      [self.menu removeItemAtIndex:KEYMAN_FIRST_KEYBOARD_MENUITEM_INDEX];
    }
  }
}

- (void)addDynamicKeyboardMenuItems {
  BOOL didSetSelectedKeyboard = NO;
  NSInteger itag = KEYMAN_FIRST_KEYBOARD_MENUITEM_TAG;
  NSString *keyboardMenuName = @"";
  int menuItemIndex = KEYMAN_FIRST_KEYBOARD_MENUITEM_INDEX;
  
  if (self.debugMode) {
    os_log_info([KMLogs configLog], "*** populateKeyboardMenuItems, number of active keyboards=%lu", self.activeKeyboards.count);
  }
  
  // loop through the active keyboards list and add them to the menu
  for (NSString *path in self.activeKeyboards) {
    NSDictionary *infoDict = [KMXFile keyboardInfoFromKmxFile:path];
    if (!infoDict) {
      continue;
    }
    keyboardMenuName = [infoDict objectForKey:kKMKeyboardNameKey];
    NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:keyboardMenuName action:@selector(menuAction:) keyEquivalent:@""];
    [item setTag:itag++];
    
    // if this is the selected keyboard, then configure it as selected
    if ([path isEqualToString:self.selectedKeyboard]) {
      [self setSelectedKeyboard:path inMenuItem:item];
      didSetSelectedKeyboard = YES;
    }
    else {
      [item setState:NSOffState];
    }
    
    [self.menu insertItem:item atIndex:menuItemIndex++];
  }
  
  if (self.activeKeyboards.count == 0) {
    [self addKeyboardPlaceholderMenuItem];
  } else if (!didSetSelectedKeyboard) {
    [self setDefaultSelectedKeyboard];
  }
}

- (void) setSelectedKeyboard:(NSString*)keyboardName inMenuItem:(NSMenuItem*) menuItem {
  KVKFile *kvk = nil;
  
  [menuItem setState:NSOnState];
  KMXFile *kmx = [[KMXFile alloc] initWithFilePath:keyboardName];
  [self setKmx:kmx];
  NSDictionary *kmxInfo = [KMXFile keyboardInfoFromKmxFile:keyboardName];
  NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
  if (kvkFilename != nil) {
    NSString *kvkFilePath = [self kvkFilePathFromFilename:kvkFilename];
    if (kvkFilePath != nil) {
      kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
    }
  }
  [self setKvk:kvk];
  [self setKeyboardName:[kmxInfo objectForKey:kKMKeyboardNameKey]];
  [self setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
  [self readPersistedOptions];
}

// defaults to the whatever keyboard happens to be first in the list
- (void) setDefaultSelectedKeyboard {
  NSMenuItem* menuItem = [self.menu itemAtIndex:KEYMAN_FIRST_KEYBOARD_MENUITEM_INDEX];
  NSString *keyboardName = [self.activeKeyboards objectAtIndex:0];
  [self setSelectedKeyboard:keyboardName inMenuItem:menuItem];
  [self setSelectedKeyboard:keyboardName];
  [self setContextBuffer:nil];
}

- (void) addKeyboardPlaceholderMenuItem {
  NSString* placeholder = NSLocalizedString(@"no-keyboard-configured-menu-placeholder", nil);
  NSMenuItem *item = [[NSMenuItem alloc] initWithTitle:placeholder action:NULL keyEquivalent:@""];
  [self.menu insertItem:item atIndex:KEYMAN_FIRST_KEYBOARD_MENUITEM_INDEX];
  [self setKmx:nil];
  [self setKvk:nil];
  [self setKeyboardName:nil];
  [self setKeyboardIcon:nil];
  [self setContextBuffer:nil];
  [self setSelectedKeyboard:nil];
}

- (void)selectKeyboardFromMenu:(NSInteger)tag {
  NSMenuItem *menuItem = [self.menu itemWithTag:tag];
  NSString *title = menuItem.title;
  os_log_info([KMLogs keyboardLog], "Input Menu, selected Keyboards menu, itag: %lu, title: %{public}@", tag, title);
  for (NSMenuItem *item in self.menu.itemArray) {
    // set the state of the keyboard items in the Keyman menu based on the new selection
    if (item.tag >= KEYMAN_FIRST_KEYBOARD_MENUITEM_TAG) {
      if (item.tag == tag) {
        [item setState:NSOnState];
      }
      else {
        [item setState:NSOffState];
      }
    }
  }
  
  NSString *path = [self.activeKeyboards objectAtIndex:tag-KEYMAN_FIRST_KEYBOARD_MENUITEM_TAG];
  KMXFile *kmx = [[KMXFile alloc] initWithFilePath:path];
  [self setKmx:kmx];
  KVKFile *kvk = nil;
  NSDictionary *kmxInfo = [KMXFile keyboardInfoFromKmxFile:path];
  NSString *kvkFilename = [kmxInfo objectForKey:kKMVisualKeyboardKey];
  if (kvkFilename != nil) {
    NSString *kvkFilePath = [self kvkFilePathFromFilename:kvkFilename];
    if (kvkFilePath != nil)
      kvk = [[KVKFile alloc] initWithFilePath:kvkFilePath];
  }
  [self setKvk:kvk];
  NSString *keyboardName = [kmxInfo objectForKey:kKMKeyboardNameKey];
  os_log_info([KMLogs keyboardLog], "Selected keyboard from menu: %{public}@", keyboardName);
  [self setKeyboardName:keyboardName];
  [self setKeyboardIcon:[kmxInfo objectForKey:kKMKeyboardIconKey]];
  [self setContextBuffer:nil];
  [self setSelectedKeyboard:path];
  [self readPersistedOptions];
  if (kvk != nil && self.alwaysShowOSK)
    [self showOSK];
}

- (NSArray *)KMXFiles {
  return [self KMXFilesAtPath:self.keyboardsPath];
}

- (NSArray *)KMXFilesAtPath:(NSString *)path {
  os_log_debug([KMLogs dataLog], "Reading KMXFiles at path %{public}@", path);
  NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:path];
  NSMutableArray *kmxFiles = [[NSMutableArray alloc] initWithCapacity:0];
  NSString *filePath;
  while (filePath = (NSString *)[dirEnum nextObject]) {
    NSString *extension = [[filePath pathExtension] lowercaseString];
    if ([extension isEqualToString:@"kmx"]) {
      [kmxFiles addObject:[path stringByAppendingPathComponent:filePath]];
      os_log_debug([KMLogs dataLog], "file = %{public}@", filePath);
    }
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
  os_log_debug([KMLogs uiLog], "Showing config window...");
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
    os_log_debug([KMLogs uiLog], "Creating config window...");
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
    [self observeCloseFor:_aboutWindow.window];
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
  if (window == _downloadKBWindow.window) {
    _downloadKBWindow.window = nil;
  } else if(window == _kbHelpWindow.window) {
    _kbHelpWindow.window = nil;
  } else if(window == _infoWindow.window) {
    _infoWindow.window = nil;
  } else if(window == _configWindow.window) {
    _configWindow.window = nil;
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
    [_downloadInfoView setMessageText:NSLocalizedString(@"message-keyboard-downloading", nil)];
    [_downloadInfoView setInformativeText:@""];
    [_downloadInfoView addButtonWithTitle:NSLocalizedString(@"button-cancel-downloading", nil)];
    [_downloadInfoView setAlertStyle:NSAlertStyleInformational];
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
      [_downloadInfoView setMessageText:NSLocalizedString(@"message-keyboard-downloading", nil)];
      NSButton *button = (NSButton *)[_downloadInfoView.buttons objectAtIndex:0];
      [button setTitle:NSLocalizedString(@"button-cancel-downloading", nil)];
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
  
  [_downloadInfoView setMessageText:NSLocalizedString(@"message-keyboard-download-complete", nil)];
  NSButton *button = (NSButton *)[_downloadInfoView.buttons objectAtIndex:0];
  [button setTitle:NSLocalizedString(@"button-download-complete", nil)];
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
  if (postEvent) {
    os_log_info([KMLogs eventsLog], "postKeyboardEventWithSource, keycode: %d", virtualKey);
    postEvent(ev);
  }
  CFRelease(ev);
  if (virtualKey != kProcessPendingBuffer) { // special 0xFF code is not a real key-press, so no "up" is needed
    ev = CGEventCreateKeyboardEvent (source, virtualKey, false); //up
    if (postEvent)
      postEvent(ev);
    CFRelease(ev);
  }
}

// Check the package info to ensure that we support this version
// (e.g. Keyman 15 does not support a 16.0 version package)
- (BOOL)verifyPackageVersionInTempFolder: (NSString *)tempDestFolder filePath:(NSString *)filePath {
  KMPackageInfo *packageInfo = [self loadPackageInfo:tempDestFolder];
  if(packageInfo == nil) {
    os_log_info([KMLogs eventsLog], "Could not find kmp.json in %{public}@", filePath);
  } else {
    NSString* requiredVersion = [packageInfo.fileVersion minimalVersionNumberString];
    KeymanVersionInfo keymanVersionInfo = [self versionInfo];
    NSString *currentVersion = [keymanVersionInfo.versionRelease minimalVersionNumberString];
    
    if ([requiredVersion compare:currentVersion options:NSNumericSearch] == NSOrderedDescending) {
      // currentVersion is lower than the requiredVersion
      os_log_error([KMLogs keyboardLog], "Package %{public}@ requires a newer version of Keyman: %{public}@", filePath, requiredVersion);
    } else {
      return YES;
    }
  }
  return NO;
}

// TODO: This seriously needs to be refactored out of the app delegate and into
//       a keyboard install module
- (BOOL)unzipFile:(NSString *)filePath {
  BOOL didUnzip = NO;
  NSError *error = nil;
  NSString *fileName = filePath.lastPathComponent;
  NSString *folderName = [fileName stringByDeletingPathExtension];
  
  // First we unzip into a temp folder, and check kmp.json for the fileVersion
  // before we continue installation. We don't want to overwrite existing
  // package if it is there if the files are not compatible with the installed
  // version of Keyman.
  
  NSString *tempFolderName = [folderName stringByAppendingString:@".tmp.install"];
  NSString *tempDestFolder = [self.keyboardsPath stringByAppendingPathComponent:tempFolderName];
  
  ZipArchive *za = [[ZipArchive alloc] init];
  if ([za UnzipOpenFile:filePath]) {
    os_log_debug([KMLogs keyboardLog], "Unzipping %{public}@ to %{public}@", filePath, tempDestFolder);
    if ([[NSFileManager defaultManager] fileExistsAtPath:tempDestFolder]) {
      os_log_debug([KMLogs keyboardLog], "The temp destination folder already exists. Overwriting...");
    }
    
    didUnzip = [za UnzipFileTo:tempDestFolder overWrite:YES];
    [za UnzipCloseFile];
  }
  
  if (!didUnzip) {
    os_log_error([KMLogs keyboardLog], "Failed to unzip file: %{public}@", filePath);
    return NO;
  }
  
  os_log_debug([KMLogs keyboardLog], "Unzipped file: %{public}@", filePath);
  
  BOOL didInstall = [self verifyPackageVersionInTempFolder:tempDestFolder filePath:filePath];
  
  NSString *destFolder = [self.keyboardsPath stringByAppendingPathComponent:folderName];

  // Remove existing package if it exists
  if (didInstall && [[NSFileManager defaultManager] fileExistsAtPath:destFolder]) {
    os_log_debug([KMLogs keyboardLog], "The destination folder already exists. Overwriting...");
    [[NSFileManager defaultManager] removeItemAtPath:destFolder error:&error];
    if (error != nil) {
      os_log_error([KMLogs keyboardLog], "Unable to remove destination folder %{public}@", destFolder);
      didInstall = NO;
    }
  }
  
  //
  // We believe this package is valid, let's go ahead and install it
  //
  
  // Rename the temp folder to the desired dest folder. removing existing folder first
  if(didInstall) {
    [[NSFileManager defaultManager] moveItemAtPath:tempDestFolder toPath:destFolder error:&error];
    if (error != nil) {
      os_log_error([KMLogs keyboardLog], "Unable to move temp folder %{public}@ to dest folder %{public}@", tempDestFolder, destFolder);
      didInstall = NO;
    }
  }
  
  if(!didInstall) {
    [[NSFileManager defaultManager] removeItemAtPath:tempDestFolder error:&error];
    if (error != nil) {
      os_log_error([KMLogs keyboardLog], "Unable to remove temp folder %{public}@", tempDestFolder);
    }
    
    return NO;
  }
  
  // Package has installed, now scan for keyboards and fonts
  // TODO: we need to be reading the kmp.json data to determine keyboards to install
  NSString * keyboardFolderPath = [self.keyboardsPath stringByAppendingPathComponent:folderName];
  [self installFontsAtPath:keyboardFolderPath];
  for (NSString *kmxFile in [self KMXFilesAtPath:keyboardFolderPath]) {
    os_log_debug([KMLogs keyboardLog], "Adding keyboard to list of active keyboards: %{public}@", kmxFile);
    if (![self.activeKeyboards containsObject:kmxFile])
      [self.activeKeyboards addObject:kmxFile];
  }
  [self saveActiveKeyboards];
  
  return YES;
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
      os_log_error([KMLogs keyboardLog], "installFontsAtPath error = %{public}@", error);
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
