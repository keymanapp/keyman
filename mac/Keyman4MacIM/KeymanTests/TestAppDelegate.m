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
    _kme = [[KMEngine alloc] initWithKMX:nil context:self.contextBuffer];
  }
  
  return _kme;
}

- (void)setKmx:(KMXFile *)kmx {
  _kmx = kmx;
  [self.kme loadKeyboardFromKmxFile:_kmx];
}

- (NSMutableString *)contextBuffer {
  if (_contextBuffer == nil) {
    _contextBuffer = [[NSMutableString alloc] initWithString:@""];
  }
  
  return _contextBuffer;
}

-(NSEvent *)keyStrokeEventForCharacter:(NSString *)character keyCode:(unsigned short) keyCode {
  return [NSEvent keyEventWithType:NSEventTypeKeyDown location:NSZeroPoint modifierFlags:0 timestamp:NSTimeIntervalSince1970 windowNumber:0 context:nil characters:character charactersIgnoringModifiers:character isARepeat:NO keyCode:keyCode];
}

- (void)postKeyboardEventWithSource: (CGEventSourceRef)source code:(CGKeyCode) virtualKey postCallback:(PostEventCallback)postEvent {
  _virtualKeyPosted = virtualKey;
}
@end
