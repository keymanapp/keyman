/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KeySender.m
 * KeyTest
 * 
 * Created by Shawn Schantz on 2023-04-17.
 * 
 * Sends keydown events for the provided keycode to the frontmost application.
 */

#import <InputMethodKit/InputMethodKit.h>
#import "KeySender.h"

@interface KeySender ()

@property (readonly) CGEventSourceRef eventSource;

@end

@implementation KeySender

const CGKeyCode kProcessBuffer = 0xFF;

-(instancetype)init  {
  self = [super init];
  if (self) {
    _eventSource = CGEventSourceCreate(kCGEventSourceStatePrivate);
    long sourceID = CGEventSourceGetSourceStateID(_eventSource);
    NSLog(@"***SGS KeySender init, eventSource sourceID = %ld", sourceID);
  }
  return self;
}

- (void)sendKeyDown:(NSUInteger)keyCode forSourceEvent:(NSEvent *)event includeKeyUp:(BOOL)includeKeyUpEvent {
  // Returns the frontmost app, which is the app that receives key events.
  NSRunningApplication *app = NSWorkspace.sharedWorkspace.frontmostApplication;
  pid_t processId = app.processIdentifier;
  NSString *appName = app.localizedName;

  NSLog(@"***SGS sendKeyDown keyCode %lu to app %@ with pid %d", (unsigned long)keyCode, appName, processId);

  CGEventFlags KMEventModifierKeyman = 1 << 24;

  if (keyCode < 0x100) {
    CGEventRef cgevent = [event CGEvent];
    //CGEventSourceRef source = CGEventCreateSourceFromEvent(cgevent);
    //CGEventSourceRef source = CGEventSourceCreate(kCGEventSourceStatePrivate);
    //CGEventSourceStateID eventSourceStateID = CGEventSourceGetSourceStateID(source);
    //NSLog(@"***SGS send keyCode CGEventSourceStateID = %d", eventSourceStateID);

//    void CGEventSetFlags(CGEventRef event, CGEventFlags flags);
//    void CGEventSetSource(CGEventRef event, CGEventSourceRef source);

    // use source from event to generate new event
    CGEventSourceRef source = CGEventCreateSourceFromEvent(cgevent);
    CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(source, (CGKeyCode)keyCode, true);
    long sourceID = CGEventGetIntegerValueField(keyDownEvent, kCGEventSourceStateID);
    NSLog(@"***SGS KeySender sendKeyDown, keyDownEvent kCGEventSourceStateID = %ld", sourceID);

    CGEventSetIntegerValueField(keyDownEvent, kCGEventSourceUserData, 0xDEADC0DE);
    long userData = CGEventGetIntegerValueField(keyDownEvent, kCGEventSourceUserData);
    NSLog(@"***SGS KeySender sendKeyDown, keyDownEvent kCGEventSourceUserData = %lx", userData);

    CGEventFlags modifierFlags = CGEventGetFlags(keyDownEvent);
    NSLog(@"***SGS sendKeyDown modifier before = %llx", modifierFlags);
    modifierFlags = modifierFlags | KMEventModifierKeyman;
    CGEventSetFlags(keyDownEvent, modifierFlags);
    modifierFlags = CGEventGetFlags(keyDownEvent);
    NSLog(@"***SGS sendKeyDown modifier after update = %llx", modifierFlags);

    //int64_t value = CGEventGetIntegerValueField(cgevent, kCGEventSourceUserData);
    //NSLog(@"***SGS send keyCode CGEventGetIntegerValueField = %lld", value);
    //value = 1234;
    //CGEventSetIntegerValueField(keyDownEvent, kCGEventSourceUserData, value);
    //NSLog(@"***SGS send keyCode new CGEventGetIntegerValueField = %lld", value);
    CGEventPostToPid(processId, keyDownEvent);
    CFRelease(keyDownEvent);

    if (includeKeyUpEvent) {
      CGEventRef keyUpEvent = CGEventCreateKeyboardEvent(source, (CGKeyCode)keyCode, false);
      CGEventPostToPid(processId, keyUpEvent);
      CFRelease(keyUpEvent);
    }
  }
}

-(void) dealloc {
  CFRelease(self.eventSource);
}

- (void)sendBackspaceforSourceEvent:(NSEvent *)event {
  NSLog(@"***SGS sendBackspace");
  [self sendKeyDown:kProcessBuffer forSourceEvent:event includeKeyUp:NO];
  //[self sendKeyDown:kVK_Delete forSourceEvent:event includeKeyUp:YES];
}

- (void)sendProcessBufferEvent:(NSEvent *)event {
  NSLog(@"***SGS sendEmptyBufferEvent");
  // this is not a real keycode, so we do not need a key up event
  // kProcessBuffer is used to indicate that all the backspaces have been processed
  // and we can now proceed to inserting the buffered, transformed text to the client
  [self sendKeyDown:kProcessBuffer forSourceEvent:event includeKeyUp:NO];
}
@end
