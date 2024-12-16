//
//  KMBarView.m
//  Keyman4Mac
//
//  Created by Serkan Kurt on 9/11/2015.
//  Copyright © 2017 SIL International. All rights reserved.
//

#import "KMBarView.h"
#import "KMLogs.h"

@implementation KMBarView

/**
 * draws stripe in three colors as part of Keyman branding
 * over half is orange, followed by red and blue segments
 */
- (void)drawRect:(NSRect)rect {
  CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] CGContext];
  
  NSRect barRect = self.frame;
  os_log_debug([KMLogs uiLog], "KMBarView drawRect: %{public}@ frame: %{public}@", NSStringFromRect(rect), NSStringFromRect(barRect));

  NSRect orangeRect = NSMakeRect(0, 0, barRect.size.width*0.56, barRect.size.height);
  NSRect redRect = NSMakeRect(orangeRect.size.width, 0, barRect.size.width*0.23, barRect.size.height);
  NSRect blueRect = NSMakeRect(redRect.origin.x + redRect.size.width, 0, barRect.size.width*0.21, barRect.size.height);
  
  CGContextSetFillColorWithColor(context, [NSColor colorWithSRGBRed:246.0/255.0 green:137.0/255.0 blue:36.0/255.0 alpha:1.0].CGColor);
  CGContextBeginPath(context);
  CGContextAddRect(context, orangeRect);
  CGContextClosePath(context);
  CGContextDrawPath(context, kCGPathFill);
  
  CGContextSetFillColorWithColor(context, [NSColor colorWithSRGBRed:204.0/255.0 green:56.0/255.0 blue:70.0/255.0 alpha:1.0].CGColor);
  CGContextBeginPath(context);
  CGContextAddRect(context, redRect);
  CGContextClosePath(context);
  CGContextDrawPath(context, kCGPathFill);
  
  CGContextSetFillColorWithColor(context, [NSColor colorWithSRGBRed:121.0/255.0 green:195.0/255.0 blue:218.0/255.0 alpha:1.0].CGColor);
  CGContextBeginPath(context);
  CGContextAddRect(context, blueRect);
  CGContextClosePath(context);
  CGContextDrawPath(context, kCGPathFill);
}

@end
