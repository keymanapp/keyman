//
//  KMEngine.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/12/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMEngine.h"
#import "KMBinaryFileFormat.h"
#import "KMCompStore.h"
#import "KMCompGroup.h"
#import "KMCompKey.h"
#import "NSString+XString.h"
#import "WindowsVKCodes.h"
#import "MacVKCodes.h"
#import "CoreWrapper.h"
#import "KMELogs.h"
@import Carbon;

@interface KMEngine ()
@property (readonly) CoreHelper *coreHelper;
@property (nonatomic, retain) CoreWrapper * coreWrapper;
@end

@implementation KMEngine

NSMutableString* _easterEggForSentry = nil;
const NSString* kEasterEggText = @"Sentrycrash#KME";
const NSString* kEasterEggKmxName = @"EnglishSpanish.kmx";

- (id)initWithKMX:(KMXFile *)kmx context:(NSString *)contextString verboseLogging:(BOOL)enableDebugLogging {
  self = [super init];
  if (self) {
    self.debugMode = enableDebugLogging;
    _kmx = kmx;
    _coreHelper = [[CoreHelper alloc] initWithDebugMode:enableDebugLogging];
    
    if (kmx) {
      [self loadCoreWrapperFromKmxFile:self.kmx.filePath];
      [self.coreWrapper setContextIfNeeded:contextString];
    }
  }
  
  return self;
}

-(void)loadCoreWrapperFromKmxFile:(NSString *)kmxFilePath {
  @try {
    _coreWrapper = [[CoreWrapper alloc] initWithHelper:_coreHelper kmxFilePath:kmxFilePath];
  }
  @catch (NSException *exception) {
    os_log_error([KMELogs coreLog], "loadCoreWrapperFromKmxFile, failed to create keyboard for path '%{public}@' with exception: %{public}@", kmxFilePath, exception.description);
  }

  /**
   * Get the list of Keys supported by the keyboard
   */
  NSArray* keyList = [_coreWrapper getKeyList];
  os_log_debug([KMELogs coreLog], "loadCoreWrapperFromKmxFile, keyList: %{public}@", keyList);
}

-(void)setKmx:(KMXFile*) kmxFile {
  if (_kmx!=kmxFile) {
    _kmx = kmxFile;
    if (kmxFile != nil) {
      [self loadCoreWrapperFromKmxFile:kmxFile.filePath];
    }
  }
}

- (void)setUseVerboseLogging:(BOOL)useVerboseLogging {
  self.debugMode = useVerboseLogging;
  
  if (self.coreHelper) {
    self.coreHelper.debugMode = useVerboseLogging;
  }
  
  if (useVerboseLogging) {
    os_log_debug([KMELogs testLog], "KMEngine - Turning verbose logging on");
    // In Keyman Engine if "debugMode" is turned on (explicitly) with "English plus Spanish" as the current keyboard and you type "Sentrycrash#KME",
    // it will force a simulated crash to test reporting to sentry.keyman.com.
    NSString * kmxName = [[_kmx filePath] lastPathComponent];
    os_log_debug([KMELogs testLog], "Sentry - KME: _kmx name = %{public}@", kmxName);
    if ([kEasterEggKmxName isEqualToString:kmxName]) {
      os_log_debug([KMELogs testLog], "Sentry - KME: Preparing to detect Easter egg.");
      _easterEggForSentry = [[NSMutableString alloc] init];
    }
    else
      _easterEggForSentry = nil;
  }
  else {
    os_log_debug([KMELogs testLog], "KMEngine - Turning verbose logging off");
  }
}

- (NSString *)getCoreContextDebug {
  return self.coreWrapper.contextDebug;
}

- (void)clearCoreContext {
  [self.coreWrapper clearCoreContext];
}

- (void)setCoreContextIfNeeded:(NSString *)context {
  [self.coreWrapper setContextIfNeeded:context];
}

- (void)setCoreOptions:(NSString *)key withValue:(NSString *)value {
  BOOL success = [self.coreWrapper setOptionsForCore:key value:value];
  os_log_debug([KMELogs coreLog], "setCoreOptions for key: %{public}@, value: %{public}@ succeeded = %{public}@", key, value, success ? @"YES" : @"NO");
}

/*
 * Returns a CoreKeyOutput object detailing the results of the keystroke
 */
- (CoreKeyOutput *)processEvent:(NSEvent *)event {
  if (!self.kmx)
    return nil;
  
  return [self.coreWrapper processEvent:event];
}

- (void) processPossibleEasterEggCharacterFrom:(NSString *)characters {
  NSUInteger len = [_easterEggForSentry length];
  os_log_debug([KMELogs keyLog], "Sentry - KME: Processing character(s): %{public}@", characters);
  if ([characters length] == 1 && [characters characterAtIndex:0] == [kEasterEggText characterAtIndex:len]) {
    NSString *characterToAdd = [kEasterEggText substringWithRange:NSMakeRange(len, 1)];
    os_log_debug([KMELogs keyLog], "Sentry - KME: Adding character to Easter Egg code string: %{public}@", characterToAdd);
    [_easterEggForSentry appendString:characterToAdd];
    if ([kEasterEggText isEqualToString:_easterEggForSentry]) {
      os_log_debug([KMELogs keyLog], "Sentry - KME: Forcing crash now!");
      // Both of the following approaches do throw an exception that causes control to exit this method,
      // but at least in my debug builds locally, neither one seems to get picked up by Sentry in a
      // way that results in a new report on sentry.keyman.com
      
#ifndef USE_ALERT_SHOW_HELP_TO_FORCE_EASTER_EGG_CRASH_FROM_ENGINE
      //#1
      @throw ([NSException exceptionWithName:@"SentryForce" reason:@"Easter egg hit" userInfo:nil]);
      
      //#2
      //    NSDecimalNumber *i = [NSDecimalNumber decimalNumberWithDecimal:[@(1) decimalValue]];
      //    NSDecimalNumber *o = [NSDecimalNumber decimalNumberWithDecimal:[@(0) decimalValue]];
      //    // Divide by 0 to throw an exception
      //    NSDecimalNumber *x = [i decimalNumberByDividingBy:o];
      
#else
      //#3 The following DOES work, but it's really lame because the crash actually gets forced in the IM
      // via this bogus call to a protocol method implemented in the IM's App Delegate just for the
      // purpose of enabling the engine to force a crash.
      [(NSObject <NSAlertDelegate> *)[NSApp delegate] alertShowHelp:[NSAlert alertWithMessageText:@"Forcing an error" defaultButton:nil alternateButton:nil otherButton:nil informativeTextWithFormat:@"Forcing an Easter egg error from KME!"]];
#endif
    }
  }
  else if (len > 0) {
    os_log_debug([KMELogs keyLog], "Sentry - KME: Clearing Easter Egg code string.");
    [_easterEggForSentry setString:@""];
  }
}

- (NSString *)getCharsFromKeyCode:(UInt16)keyCode {
  TISInputSourceRef currentKeyboard = TISCopyCurrentKeyboardLayoutInputSource();
  CFDataRef uchr = (CFDataRef)TISGetInputSourceProperty(currentKeyboard, kTISPropertyUnicodeKeyLayoutData);
  const UCKeyboardLayout *keyboardLayout = (const UCKeyboardLayout*)CFDataGetBytePtr(uchr);
  
  if (keyboardLayout) {
    UInt32 deadKeyState = 0;
    UniCharCount maxStringLength = 255;
    UniCharCount actualStringLength = 0;
    UniChar unicodeString[maxStringLength];
    
    OSStatus status = UCKeyTranslate(keyboardLayout,
                                     keyCode, kUCKeyActionDown, 0,
                                     LMGetKbdType(), 0,
                                     &deadKeyState,
                                     maxStringLength,
                                     &actualStringLength, unicodeString);
    
    if (actualStringLength == 0 && deadKeyState) {
      status = UCKeyTranslate(keyboardLayout,
                              kVK_Space, kUCKeyActionDown, 0,
                              LMGetKbdType(), 0,
                              &deadKeyState,
                              maxStringLength,
                              &actualStringLength, unicodeString);
    }
    
    if (actualStringLength > 0 && status == noErr)
      return [NSString stringWithCharacters:unicodeString length:(NSUInteger)actualStringLength];
  }
  
  return nil;
}

@end
