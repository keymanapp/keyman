//
//  NSWindow+SuppMethods.h
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>

@interface NSWindow (SuppMethods)

- (void)centerInParent;
- (void)centerInWindow:(NSWindow *)window;
- (void)addViewToTitleBar:(NSView *)viewToAdd positionX:(CGFloat)x;

@end
