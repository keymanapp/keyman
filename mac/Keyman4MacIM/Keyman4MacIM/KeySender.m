/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KeySender.m
 * Keyman
 * 
 * Created by Shawn Schantz on 2023-04-17.
 * 
 * Sends keydown events for the provided keycode to the frontmost application.
 */

#import <InputMethodKit/InputMethodKit.h>
#import "KeySender.h"
#import "KMInputMethodAppDelegate.h"

const CGKeyCode kKeymanEventKeyCode = 0xFF;

@interface KeySender ()

@property (readonly) CGEventSourceRef eventSource;

@end

@implementation KeySender

-(KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

-(instancetype)init  {
  self = [super init];
  if (self) {
    _eventSource = CGEventSourceCreate(kCGEventSourceStatePrivate);
  }
  return self;
}

- (void)sendKeyDown:(NSUInteger)keyCode forSourceEvent:(NSEvent *)event includeKeyUp:(BOOL)includeKeyUpEvent {
  // Returns the frontmost app, which is the app that receives key events.
  NSRunningApplication *app = NSWorkspace.sharedWorkspace.frontmostApplication;
  pid_t processId = app.processIdentifier;
  NSString *appName = app.localizedName;

  [self.appDelegate logDebugMessage:@"sendKeyDown keyCode %lu to app %@ with pid %d", (unsigned long)keyCode, appName, processId];

  if (keyCode < 0x100) {
    CGEventRef cgevent = [event CGEvent];

    // use source from event to generate new event
    CGEventSourceRef source = CGEventCreateSourceFromEvent(cgevent);
    CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(source, (CGKeyCode)keyCode, true);

    // TODO: add version check
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
  [self.appDelegate logDebugMessage:@"KeySender sendBackspaceforSourceEvent"];
  [self sendKeyDown:kVK_Delete forSourceEvent:event includeKeyUp:YES];
}

- (void)sendKeymanKeyCodeForEvent:(NSEvent *)event {
  [self.appDelegate logDebugMessage:@"KeySender sendKeymanKeyCodeForEvent"];
  // this is not a real keycode, so we do not need a key up event
  // kKeymanEventKeyCode is used to indicate that all the backspaces have been processed
  // and we can insert the queuedText to the client
  [self sendKeyDown:kKeymanEventKeyCode forSourceEvent:event includeKeyUp:NO];
}
@end
