//
//  KMBarView.m
//  Keyman4Mac
//
//  Created by Serkan Kurt on 9/11/2015.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import "KMBarView.h"

@implementation KMBarView

- (void)drawRect:(NSRect)rect {
  CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] CGContext];
  
  NSRect rect1 = NSMakeRect(0, 0, rect.size.width*0.56, rect.size.height);
  NSRect rect2 = NSMakeRect(rect1.size.width, 0, rect.size.width*0.23, rect.size.height);
  NSRect rect3 = NSMakeRect(rect2.origin.x + rect2.size.width, 0, rect.size.width*0.21, rect.size.height);
  
  CGContextSetFillColorWithColor(context, [NSColor colorWithRed:1.0 green:104.0/255.0 blue:38.0/255.0 alpha:1.0].CGColor);
  CGContextBeginPath(context);
  CGContextAddRect(context, rect1);
  CGContextClosePath(context);
  CGContextDrawPath(context, kCGPathFill);
  
  CGContextSetFillColorWithColor(context, [NSColor colorWithRed:200.0/255.0 green:0.0 blue:54.0/255.0 alpha:1.0].CGColor);
  CGContextBeginPath(context);
  CGContextAddRect(context, rect2);
  CGContextClosePath(context);
  CGContextDrawPath(context, kCGPathFill);
  
  CGContextSetFillColorWithColor(context, [NSColor colorWithRed:74.0/255.0 green:186.0/255.0 blue:208.0/255.0 alpha:1.0].CGColor);
  CGContextBeginPath(context);
  CGContextAddRect(context, rect3);
  CGContextClosePath(context);
  CGContextDrawPath(context, kCGPathFill);
}

@end
