//
//  KMInputMethodAppDelegate.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 12/02/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMInputMethodAppDelegate.h"
#import "KMInputMethodLifecycle.h"
#import "KMSettingsRepository.h"
#import "KMDataRepository.h"
#import "KMConfigurationWindowController.h"
#import "KMDownloadKBWindowController.h"
#import "ZipArchive.h"
#import "KMPackageReader.h"
#import "KMPackageInfo.h"
#import "PrivacyConsent.h"
#import "KMLogs.h"
@import Sentry;

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
@property BOOL receivedKeyDownFromOsk;
@property NSEventModifierFlags oskEventModifiers;
@end

@implementation KMInputMethodAppDelegate
@synthesize kme = _kme;
@synthesize kmx = _kmx;
@synthesize modifierMapping = _modifierMapping;
@synthesize kvk = _kvk;
@synthesize keyboardName = _keyboardName;
@synthesize keyboardsPath = _keyboardsPath;
@synthesize kmxFileList = _kmxFileList;
@synthesize selectedKeyboard = _selectedKeyboard;
@synthesize activeKeyboards = _activeKeyboards;
@synthesize contextBuffer = _contextBuffer;

id _lastServerWithOSKShowing = nil;

- (id)init {
  self = [super init];
  if (self) {    
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
  
  // register to receive notifications generated from KMInputMethodLifecycle
  [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(inputMethodActivated:) name:kInputMethodActivatedNotification object:nil];
  [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(inputMethodDeactivated:) name:kInputMethodDeactivatedNotification object:nil];

  // start Input Method lifecycle
  [KMInputMethodLifecycle.shared startLifecycle];
}

/**
 * When the input method is deactivated, hide the OSK and disable the low-level event tap
 */
- (void)inputMethodDeactivated:(NSNotification *)notification {
  if ([self.oskWindow.window isVisible]) {
    os_log_debug([KMLogs oskLog], "***KMInputMethodAppDelegate inputMethodDeactivated, hiding OSK");
    [self.oskWindow.window setIsVisible:NO];
  } else {
    os_log_debug([KMLogs oskLog], "***KMInputMethodAppDelegate inputMethodDeactivated, OSK already hidden");
  }

  if (self.lowLevelEventTap) {
    os_log_debug([KMLogs lifecycleLog], "***inputMethodDeactivated, disabling event tap");
    CGEventTapEnable(self.lowLevelEventTap, NO);
  }
}

/**
 * When the input method is activated, show the OSK and enable the low-level event tap
 */
- (void)inputMethodActivated:(NSNotification *)notification {
  if (self.lowLevelEventTap && !CGEventTapIsEnabled(self.lowLevelEventTap)) {
    os_log_debug([KMLogs lifecycleLog], "***KMInputMethodAppDelegate inputMethodActivated, re-enabling event tap...");
    CGEventTapEnable(self.lowLevelEventTap, YES);
  }

  if (_kvk != nil && ([KMInputMethodLifecycle.shared shouldShowOskOnActivate])) {
    os_log_debug([KMLogs oskLog], "***KMInputMethodAppDelegate inputMethodActivated, showing OSK");
    [self showOSK];
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

CGEventRef eventTapFunction(CGEventTapProxy proxy, CGEventType type, CGEventRef event, void *refcon) {
  KMInputMethodAppDelegate *appDelegate = [KMInputMethodAppDelegate AppDelegate];
  if (appDelegate != nil) {
    if (type == kCGEventTapDisabledByTimeout || type == kCGEventTapDisabledByUserInput) {
      // kCGEventTapDisabledByUserInput most likely means we're "sleeping", in which case we want it to stay disabled until we get the wake-up call.
      if ([KMInputMethodLifecycle.shared shouldEnableEventTap]) {
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
        os_log_debug([KMLogs eventsLog], "eventTapFunction: system event kCGEventFlagsChanged to: 0x%X", (int) sysEvent.modifierFlags);
        appDelegate.currentModifiers = sysEvent.modifierFlags;
        if (appDelegate.currentModifiers & NSEventModifierFlagCommand) {
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
        appDelegate.receivedKeyDownFromOsk = NO;
        os_log_debug([KMLogs eventsLog], "Event tap keydown event, keyCode: 0x%X (%d)", sysEvent.keyCode, sysEvent.keyCode);
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
          if ([OSKView isOskKeyDownEvent:event]) {
            NSEventModifierFlags oskEventModifiers = [OSKView extractModifierFlagsFromOskEvent:event];
            appDelegate.receivedKeyDownFromOsk = YES;
            appDelegate.oskEventModifiers = oskEventModifiers;
            os_log_debug([KMLogs eventsLog], "*** keydown event received from OSK, modifiers: 0x%lX",(unsigned long)oskEventModifiers);
          }
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
    _kme = [[KMEngine alloc] initWithKMX:nil context:self.contextBuffer];
  }
  
  return _kme;
}

- (KMPackageReader *)packageReader {
  if (_packageReader == nil) {
    _packageReader = [[KMPackageReader alloc] init];
  }
  
  return _packageReader;
}

- (void)resetKmx {
  _kmx = nil;
  _modifierMapping = nil;
}

- (void)loadKeyboardFromKmxFile:(KMXFile *)kmx {
  _kmx = kmx;
  CoreKeyboardInfo *keyboardInfo = [self.kme loadKeyboardFromKmxFile:kmx];

  os_log_info([KMLogs keyboardLog], "setKmx loaded keyboard, keyboard info: %{public}@", keyboardInfo);
  
  _modifierMapping = [[KMModifierMapping alloc] init:keyboardInfo];

  os_log_info([KMLogs keyboardLog], "modifierMapping bothOptionKeysGenerateRightAlt: %d", self.modifierMapping.bothOptionKeysGenerateRightAlt);

   // assign custom keyboard tag in Sentry to default keyboard
   [SentrySDK configureScope:^(SentryScope * _Nonnull scope) {
       [scope setTagValue:keyboardInfo.keyboardId forKey:@"keyboard"];
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

- (void)applyPersistedOptions {
  NSDictionary *selectedPersistedOptions = [[KMSettingsRepository shared] readOptionsForSelectedKeyboard];
  
  // TODO: pass array instead of making repeated calls
  for (NSString *key in selectedPersistedOptions) {
    NSString *value = [selectedPersistedOptions objectForKey:key];
    os_log_info([KMLogs configLog], "persisted options found in UserDefaults for keyboard %{public}@, key: %{public}@, value: %{public}@", _selectedKeyboard, key, value);
    [self.kme setCoreOptions:key withValue:value];
  }
}

- (NSString *)oskWindowTitle {
  if (_keyboardName == nil || !_keyboardName.length)
    return [NSString stringWithFormat:@"Keyman"];
  else
    return [NSString stringWithFormat:@"%@ - Keyman", _keyboardName];
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
  if (_kmxFileList == nil) {
    os_log_debug([KMLogs dataLog], "creating kmxFileList");
    NSArray *kmxFiles = [self getKmxFilesInKeyboardsDirectory];
    _kmxFileList = [[NSMutableArray alloc] initWithCapacity:0];
    NSMutableArray *others = nil;
    for (NSString *filePath in kmxFiles) {
      os_log_debug([KMLogs dataLog], "kmxFileList, filePath: %{public}@", filePath);
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
  for (NSString *kmxFile in [self getKmxFilesAtPath:packageFolder]) {
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
    _selectedKeyboard = [[KMSettingsRepository shared] readSelectedKeyboard];
  }
  os_log_debug([KMLogs dataLog], "selectedKeyboard = %{public}@", _selectedKeyboard);

  return _selectedKeyboard;
}

- (void)setSelectedKeyboard:(NSString *)selectedKeyboard {
  [[KMSettingsRepository shared] writeSelectedKeyboard:selectedKeyboard];
  _selectedKeyboard = selectedKeyboard;
}

- (NSMutableArray *)activeKeyboards {
  if (!_activeKeyboards) {
    os_log_debug([KMLogs dataLog], "initializing activeKeyboards");
    _activeKeyboards = [[KMSettingsRepository.shared readActiveKeyboards] mutableCopy];
  }

  return _activeKeyboards;
}

- (void)saveActiveKeyboards {
  os_log_debug([KMLogs dataLog], "saveActiveKeyboards");
  [KMSettingsRepository.shared writeActiveKeyboards:_activeKeyboards];
  [self resetActiveKeyboards];
  [self updateKeyboardMenuItems];
}

- (void)clearActiveKeyboards {
  [KMSettingsRepository.shared clearActiveKeyboards];
  [self updateKeyboardMenuItems];
}

- (void)addActiveKeyboard:(NSString *) partialPath {
  if (![self.activeKeyboards containsObject:partialPath]) {
    os_log_debug([KMLogs keyboardLog], "addActiveKeyboard, adding '%{public}@' to list of active keyboards: ", partialPath);
    [self.activeKeyboards addObject:partialPath];
  }
}

- (void)resetActiveKeyboards {
  // Remove entries with missing files
  NSMutableArray *pathsToRemove = [[NSMutableArray alloc] initWithCapacity:0];
  for (NSString *path in self.activeKeyboards) {
    NSString *fullPath = [KMDataRepository.shared buildFullPath:path];
    os_log_debug([KMLogs dataLog], "resetActiveKeyboards, checking fullPath: '%{public}@' for path: '%{public}@'", fullPath, path);
    if (![[NSFileManager defaultManager] fileExistsAtPath:fullPath]) {
      os_log_debug([KMLogs dataLog], "resetActiveKeyboards, need to remove non-existent path: %{public}@", fullPath);
      [pathsToRemove addObject:path];
    }
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
    [KMSettingsRepository.shared writeActiveKeyboards:_activeKeyboards];
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
    [KMDataRepository.shared migrateData];
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
  os_log_debug([KMLogs uiLog], "calculateNumberOfKeyboardMenuItems, entered");
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
  os_log_debug([KMLogs startupLog], "addDynamicKeyboardMenuItems, entered");
  BOOL didSetSelectedKeyboard = NO;
  NSInteger itag = KEYMAN_FIRST_KEYBOARD_MENUITEM_TAG;
  NSString *keyboardMenuName = @"";
  int menuItemIndex = KEYMAN_FIRST_KEYBOARD_MENUITEM_INDEX;
  
  os_log_debug([KMLogs configLog], "*** populateKeyboardMenuItems, number of active keyboards=%lu", self.activeKeyboards.count);
  
  // loop through the active keyboards list and add them to the menu
  for (NSString *path in self.activeKeyboards) {
    NSString *fullPath = [KMDataRepository.shared buildFullPath:path];
    os_log_debug([KMLogs dataLog], "addDynamicKeyboardMenuItems, path = '%{public}@', full path = '%{public}@'", path, fullPath);
    NSDictionary *infoDict = [KMXFile keyboardInfoFromKmxFile:fullPath];
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
  
  NSString *fullPath = [KMDataRepository.shared buildFullPath:keyboardName];
  os_log_debug([KMLogs dataLog], "setSelectedKeyboard, keyboardName = '%{public}@', full path = '%{public}@'", keyboardName, fullPath);
  [menuItem setState:NSOnState];
  KMXFile *kmx = [[KMXFile alloc] initWithFilePath:fullPath];
  [self loadKeyboardFromKmxFile:kmx];
  NSDictionary *kmxInfo = [KMXFile keyboardInfoFromKmxFile:fullPath];
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
  [self applyPersistedOptions];
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
  [self resetKmx];
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
  NSString *fullPath = [KMDataRepository.shared buildFullPath:path];
  os_log_debug([KMLogs dataLog], "setSelectedKeyboard, keyboardName = '%{public}@', full path = '%{public}@'", path, fullPath);
  
  KMXFile *kmx = [[KMXFile alloc] initWithFilePath:fullPath];
  [self loadKeyboardFromKmxFile:kmx];
  KVKFile *kvk = nil;
  NSDictionary *kmxInfo = [KMXFile keyboardInfoFromKmxFile:fullPath];
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
  [self applyPersistedOptions];
}

- (NSArray *)getKmxFilesInKeyboardsDirectory {
  return [self getKmxFilesAtPath:self.keyboardsPath];
}

- (NSArray *)getKmxFilesAtPath:(NSString *)path {
  os_log_debug([KMLogs dataLog], "getKmxFilesAtPath, path: '%{public}@'", path);
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

- (NSArray *)getKvkFilesArray {
  NSDirectoryEnumerator *dirEnum = [[NSFileManager defaultManager] enumeratorAtPath:self.keyboardsPath];
  NSMutableArray *kvkFilesArray = [[NSMutableArray alloc] initWithCapacity:0];
  NSString *filePath;
  while (filePath = (NSString *)[dirEnum nextObject]) {
    NSString *extension = [[filePath pathExtension] lowercaseString];
    if ([extension isEqualToString:@"kvk"])
      [kvkFilesArray addObject:[self.keyboardsPath stringByAppendingPathComponent:filePath]];
  }
  
  return kvkFilesArray;
}

- (NSString *)kvkFilePathFromFilename:(NSString *)kvkFilename {
  NSString *kvkFilePath = nil;
  NSArray *kvkFilesArray = [self getKvkFilesArray];
  for (NSString *filePath in kvkFilesArray) {
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
  [self.oskWindow prepareToShowOsk];
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

// TODO: rewrite confusing pattern, multiple methods differing only by underscore
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

- (void)downloadComplete:(NSModalResponse) returnCode {
  os_log_debug([KMLogs uiLog], "downloadComplete, NSModalResponse returnCode: %ld", (long)returnCode);
  if (returnCode == NSModalResponseCancel) {
    os_log_debug([KMLogs uiLog], "downloadComplete, returnCode == NSModalResponseCancel");
    [_connection cancel];
  }
  else if (returnCode == NSModalResponseOK) {
    os_log_debug([KMLogs uiLog], "downloadComplete, returnCode == NSModalResponseOK");

    [_downloadKBWindow close];

    if (self.configWindow.window != nil) {
      os_log_debug([KMLogs uiLog], "downloadComplete, self.configWindow.window != nil");
      [self.configWindow.window makeKeyAndOrderFront:nil];
      if (![[self.configWindow.window childWindows] containsObject:self.infoWindow.window]) {
        [self.configWindow.window addChildWindow:self.infoWindow.window ordered:NSWindowAbove];
      }
      [self.infoWindow.window centerInParent];
      [self.infoWindow.window makeKeyAndOrderFront:nil];
    }
    else {
      os_log_debug([KMLogs uiLog], "downloadComplete, self.configWindow.window == nil");
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
  os_log_debug([KMLogs uiLog], "downloadInfoView");
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
  os_log_debug([KMLogs uiLog], "progressIndicator");
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
  os_log_debug([KMLogs uiLog], "downloadKeyboardFromURL, url.path: %{public}@", url.path);

  if (downloadUrl && _downloadFilename) {
    if (_infoWindow.window != nil)
      [_infoWindow close];
    
    [self.downloadInfoView setInformativeText:self.downloadFilename];
    
    if (self.configWindow.window != nil) {
      os_log_debug([KMLogs uiLog], "downloadKeyboardFromURL, self.configWindow.window != nil");
      [self.configWindow.window makeKeyAndOrderFront:nil];
      if (![[self.configWindow.window childWindows] containsObject:self.downloadKBWindow.window]) {
        [self.configWindow.window addChildWindow:self.downloadKBWindow.window ordered:NSWindowAbove];
      }
      [self.downloadKBWindow.window centerInParent];
      [self.downloadKBWindow.window makeKeyAndOrderFront:nil];
      
      /*
       Open sheet off of config window, not the download window.
       This is because, if the download is successful,
       the download window will be closed by the sheet.
       */
      [self.downloadInfoView beginSheetModalForWindow:self.configWindow.window completionHandler:^(NSModalResponse returnCode)  {
        [self downloadComplete:returnCode];
      }];
    }
    else {
      os_log_debug([KMLogs uiLog], "downloadKeyboardFromURL, self.configWindow.window == nil");
      [self.downloadKBWindow.window centerInParent];
      [self.downloadKBWindow.window makeKeyAndOrderFront:nil];
      [self.downloadKBWindow.window setLevel:NSFloatingWindowLevel];
      /*
       Open sheet off of config window, same as above.
       */
      [self.downloadInfoView beginSheetModalForWindow:self.configWindow.window completionHandler:^(NSModalResponse returnCode)  {
        [self downloadComplete:returnCode];
      }];
    }
    
    if (_connection == nil) {
      os_log_debug([KMLogs uiLog], "downloadKeyboardFromURL, _connection == nil, set button title cancel downloading, tag = -1");
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
  os_log_debug([KMLogs uiLog], "connectionDidFinishLoading");
  NSString *filePath = [self.keyboardsPath stringByAppendingPathComponent:self.downloadFilename];
  [self.receivedData writeToFile:filePath atomically:YES];
  [self unzipFile:filePath];
  [[NSFileManager defaultManager] removeItemAtPath:filePath error:nil];
  
  [_downloadInfoView setMessageText:NSLocalizedString(@"message-keyboard-download-complete", nil)];
  NSButton *button = (NSButton *)[_downloadInfoView.buttons objectAtIndex:0];
  os_log_debug([KMLogs uiLog], "connectionDidFinishLoading, set button title download complete, tag = 1");
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

- (NSEventModifierFlags) determineModifiers {
  NSEventModifierFlags modifierFlags = 0;
  
  if (self.receivedKeyDownFromOsk) {
    /**
     * the event was generated from the OSK, so use the oskEventModifiers in effect at time of event generation
     */
    modifierFlags = self.oskEventModifiers;
    os_log_debug([KMLogs eventsLog], "--- use modifiers from OSK, oskEventModifiers: 0x%lX", (unsigned long)modifierFlags);
    self.oskEventModifiers = 0;
  } else { 
    /**
     * the event originated from the physical keyboard, use the modifiers on kCGEventFlagsChanged events
     */
    NSEventModifierFlags originalModifiers = self.currentModifiers;
    modifierFlags = [self.modifierMapping adjustModifiers:originalModifiers];
    os_log_debug([KMLogs eventsLog], "--- use adjusted modifiers from current state: 0x%lX", (unsigned long)modifierFlags);
    
    /**
     * If the OSK is open, then we also need to apply its modifiers, if any, to this event originating from the physical keyboard.
     * If the OSK is closed, its modifiers will be zero.
     */
    NSEventModifierFlags oskModifiers = [self.oskWindow getOskEventModifierFlags];
    if (oskModifiers != 0) {
      os_log_debug([KMLogs eventsLog], "--- modifiers from OSK to apply to physical keyboard event: 0x%lX", (unsigned long)oskModifiers);
      
      // combine osk modifiers with adjusted modifiers
      modifierFlags = oskModifiers | modifierFlags;
      os_log_debug([KMLogs eventsLog], "--- combined modifiers to apply to physical keyboard event: 0x%lX", (unsigned long)modifierFlags);
    }
  }
  
  return modifierFlags;
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

  os_log_debug([KMLogs keyboardLog], "unzipFile, folderName: %{public}@, fileName: %{public}@", folderName, fileName);

  // First we unzip into a temp folder, and check kmp.json for the fileVersion
  // before we continue installation. We don't want to overwrite existing
  // package if it is there if the files are not compatible with the installed
  // version of Keyman.
  
  NSString *tempFolderName = [folderName stringByAppendingString:@".tmp.install"];
  NSString *tempDestFolder = [self.keyboardsPath stringByAppendingPathComponent:tempFolderName];
  
  ZipArchive *za = [[ZipArchive alloc] init];
  if ([za UnzipOpenFile:filePath]) {
    os_log_debug([KMLogs keyboardLog], "unzipFile, Unzipping %{public}@ to %{public}@", filePath, tempDestFolder);
    if ([[NSFileManager defaultManager] fileExistsAtPath:tempDestFolder]) {
      os_log_debug([KMLogs keyboardLog], "unzipFile, The temp destination folder already exists. Overwriting...");
    }
    
    didUnzip = [za UnzipFileTo:tempDestFolder overWrite:YES];
    [za UnzipCloseFile];
  }
  
  if (!didUnzip) {
    os_log_error([KMLogs keyboardLog], "unzipFile, Failed to unzip file: %{public}@", filePath);
    return NO;
  }
  
  os_log_debug([KMLogs keyboardLog], "unzipFile, Unzipped file: %{public}@", filePath);
  
  BOOL didInstall = [self verifyPackageVersionInTempFolder:tempDestFolder filePath:filePath];
  
  NSString *destFolder = [self.keyboardsPath stringByAppendingPathComponent:folderName];

  // Remove existing package if it exists
  if (didInstall && [[NSFileManager defaultManager] fileExistsAtPath:destFolder]) {
    os_log_debug([KMLogs keyboardLog], "unzipFile, The destination folder already exists. Overwriting...");
    [[NSFileManager defaultManager] removeItemAtPath:destFolder error:&error];
    if (error != nil) {
      os_log_error([KMLogs keyboardLog], "unzipFile, Unable to remove destination folder %{public}@", destFolder);
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
      os_log_error([KMLogs keyboardLog], "unzipFile, Unable to move temp folder %{public}@ to dest folder %{public}@", tempDestFolder, destFolder);
      didInstall = NO;
    }
  }
  
  if(!didInstall) {
    [[NSFileManager defaultManager] removeItemAtPath:tempDestFolder error:&error];
    if (error != nil) {
      os_log_error([KMLogs keyboardLog], "unzipFile, Unable to remove temp folder %{public}@", tempDestFolder);
    }
    
    return NO;
  }
  
  // Package has installed, now scan for keyboards and fonts
  // TODO: we need to be reading the kmp.json data to determine keyboards to install
  NSString * keyboardFolderPath = [self.keyboardsPath stringByAppendingPathComponent:folderName];
  os_log_debug([KMLogs keyboardLog], "unzipFile, folderName: %{public}@, keyboardFolderPath: %{public}@", folderName, keyboardFolderPath);
  [self installFontsAtPath:keyboardFolderPath];
  
  for (NSString *kmxFile in [self getKmxFilesAtPath:keyboardFolderPath]) {
    NSString *partialPath = [KMDataRepository.shared buildPartialPathFrom:folderName keyboardFile:[kmxFile lastPathComponent]];
    [self addActiveKeyboard:partialPath];
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
