//
//  TestAppDelegate.m
//  KeymanTests
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import "TestAppDelegate.h"

@implementation TestAppDelegate

@synthesize kme = _kme;
@synthesize kmx = _kmx;
@synthesize contextBuffer = _contextBuffer;

- (KMEngine *)kme {
  if (_kme == nil) {
    _kme = [[KMEngine alloc] initWithKMX:nil context:self.contextBuffer verboseLogging:self.debugMode];
  }
  
  return _kme;
}

- (void)setKmx:(KMXFile *)kmx {
  _kmx = kmx;
  [self.kme setKmx:_kmx];
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

- (BOOL)debugMode {
  return YES;
}

-(NSEvent *)keyStrokeEventForCharacter:(NSString *)character keyCode:(unsigned short) keyCode {
  return [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSZeroPoint modifierFlags:0 timestamp:NSTimeIntervalSince1970 windowNumber:0 context:nil characters:character charactersIgnoringModifiers:character isARepeat:NO keyCode:keyCode];
}

- (void)postKeyboardEventWithSource: (CGEventSourceRef)source code:(CGKeyCode) virtualKey postCallback:(PostEventCallback)postEvent {
  _virtualKeyPosted = virtualKey;
}
@end
