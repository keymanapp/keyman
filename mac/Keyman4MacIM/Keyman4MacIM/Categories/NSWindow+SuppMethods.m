//
//  NSWindow+SuppMethods.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NSWindow+SuppMethods.h"

@implementation NSWindow (SuppMethods)

- (void)centerInParent {
  if (self.parentWindow == nil) {
    [self center];
    return;
  }
  
  NSRect cFrame = self.frame;
  NSRect pFrame = self.parentWindow.frame;
  CGFloat x = pFrame.origin.x + (NSWidth(pFrame) - NSWidth(cFrame))/2;
  CGFloat y = pFrame.origin.y + (NSHeight(pFrame) - NSHeight(cFrame))/2;
  [self setFrameOrigin:NSMakePoint(x, y)];
}

- (void)centerInWindow:(NSWindow *)window {
  if (window == nil) {
    [self center];
    return;
  }
  
  NSRect sFrame = self.frame;
  NSRect wFrame = window.frame;
  CGFloat x = wFrame.origin.x + (NSWidth(wFrame) - NSWidth(sFrame))/2;
  CGFloat y = wFrame.origin.y + (NSHeight(wFrame) - NSHeight(sFrame))/2;
  [self setFrameOrigin:NSMakePoint(x, y)];
}

- (void)addViewToTitleBar:(NSView *)viewToAdd positionX:(CGFloat)x {
  viewToAdd.frame = NSMakeRect(x, NSHeight([self.contentView frame]), NSWidth(viewToAdd.frame), self.titleBarHeight);
  
  NSUInteger mask = 0;
  if(x >  NSWidth(self.frame)/2.0) {
    mask |= NSViewMinXMargin;
  }
  else {
    mask |= NSViewMaxXMargin;
  }
  
  [viewToAdd setAutoresizingMask:mask|NSViewMinYMargin];
  [[self.contentView superview] addSubview:viewToAdd];
}

- (CGFloat)titleBarHeight {
  NSRect sFrame = [self.contentView superview].frame;
  NSRect cFrame = [self.contentView frame];
  return NSHeight(sFrame) - NSHeight(cFrame);
}

@end
