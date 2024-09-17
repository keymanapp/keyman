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

- (id)initWithKMX:(KMXFile *)kmx context:(NSString *)contextString {
  self = [super init];
  if (self) {
    _kmx = kmx;
    _coreHelper = [[CoreHelper alloc] init];
    
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
}

-(void)setKmx:(KMXFile*) kmxFile {
  if (_kmx!=kmxFile) {
    _kmx = kmxFile;
    if (kmxFile != nil) {
      [self loadCoreWrapperFromKmxFile:kmxFile.filePath];
    }
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
