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
@end

@implementation KeySender

-(KMInputMethodAppDelegate *)appDelegate {
  return (KMInputMethodAppDelegate *)[NSApp delegate];
}

-(instancetype)init  {
  self = [super init];
  return self;
}

- (void)sendKeyDown:(NSUInteger)keyCode forSourceEvent:(NSEvent *)event includeKeyUp:(BOOL)includeKeyUpEvent {
  // Returns the frontmost app, which is the app that receives key events.
  NSRunningApplication *app = NSWorkspace.sharedWorkspace.frontmostApplication;
  pid_t processId = app.processIdentifier;
  NSString *bundleId = app.bundleIdentifier;

  [self.appDelegate logDebugMessage:@"sendKeyDown keyCode %lu to app %@ with pid %d", (unsigned long)keyCode, bundleId, processId];

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

- (void)sendBackspaceforEventSource:(CGEventSourceRef)eventSource {
  [self.appDelegate logDebugMessage:@"KeySender sendBackspaceforEventSource"];

  [self postKeyboardEventWithSource:eventSource code:kVK_Delete postCallback:^(CGEventRef eventToPost) {
      CGEventPost(kCGHIDEventTap, eventToPost);
  }];
}

- (void)postKeyboardEventWithSource: (CGEventSourceRef)source code:(CGKeyCode) virtualKey postCallback:(PostEventCallback)postEvent{
    
  if (!postEvent) {
    [self.appDelegate logDebugMessage:@"KeySender postKeyboardEventWithSource callback not specified", virtualKey];
    return;
  }
  
  [self.appDelegate logDebugMessage:@"KeySender postKeyboardEventWithSource for virtualKey: @%", virtualKey];

  CGEventRef ev = CGEventCreateKeyboardEvent (source, virtualKey, true); //down
  postEvent(ev);
  CFRelease(ev);
  ev = CGEventCreateKeyboardEvent (source, virtualKey, false); //up
  postEvent(ev);
  CFRelease(ev);
}

/**
 sendKeymanKeyCodeForEvent sends the kKeymanEventKeyCode to the
 frontmost application to indicate that all the backspaces have been processed
 and we can insert the queuedText to the client
 */

- (void)sendKeymanKeyCodeForEvent:(NSEvent *)event {
  [self.appDelegate logDebugMessage:@"KeySender sendKeymanKeyCodeForEvent"];
  
  [self sendKeyDown:kKeymanEventKeyCode forSourceEvent:event includeKeyUp:NO];
  
  // Returns the frontmost app, which is the app that receives key events.
  NSRunningApplication *app = NSWorkspace.sharedWorkspace.frontmostApplication;
  pid_t processId = app.processIdentifier;
  NSString *bundleId = app.bundleIdentifier;
  
  [self.appDelegate logDebugMessage:@"sendKeymanKeyCodeForEvent keyCode %lu to app %@ with pid %d", (unsigned long)kKeymanEventKeyCode, bundleId, processId];
  
  CGEventRef cgevent = [event CGEvent];
  
  // use source from event to generate new event
  CGEventSourceRef source = CGEventCreateSourceFromEvent(cgevent);
  CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(source, kKeymanEventKeyCode, true);
  
  // TODO: add version check
  CGEventPostToPid(processId, keyDownEvent);
  CFRelease(keyDownEvent);
  
  // this is not a real keycode, so we do not need a key up event
}
@end
