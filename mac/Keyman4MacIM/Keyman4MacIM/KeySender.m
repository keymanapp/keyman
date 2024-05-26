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
  
  ProcessSerialNumber psn;
  
  // Returns the frontmost app, which is the app that receives key events.
  NSRunningApplication *app = NSWorkspace.sharedWorkspace.frontmostApplication;
  pid_t processId = app.processIdentifier;
  NSString *bundleId = app.bundleIdentifier;
  
  [self.appDelegate logDebugMessage:@"sendKeymanKeyCodeForEvent keyCode %lu to app %@ with pid %d", (unsigned long)kKeymanEventKeyCode, bundleId, processId];
  
  // use nil as source, as this generated event is not directly tied to the originating event
  CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(nil, kKeymanEventKeyCode, true);
  
  CGEventPostToPid(processId, keyDownEvent);
  CFRelease(keyDownEvent);
  
  // this is not a real keycode, so we do not need a key up event
}
@end
