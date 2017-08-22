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
    CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] graphicsPort];
    
    NSRect rect1 = NSMakeRect(0, 0, rect.size.width*0.56, rect.size.height);
    NSRect rect2 = NSMakeRect(rect1.size.width, 0, rect.size.width*0.23, rect.size.height);
    NSRect rect3 = NSMakeRect(rect2.origin.x + rect2.size.width, 0, rect.size.width*0.21, rect.size.height);
    
    CGContextSetFillColorWithColor(context, [NSColor colorWithRed:246.0/255.0 green:137.0/255.0 blue:36.0/255.0 alpha:1.0].CGColor);
    CGContextBeginPath(context);
    CGContextAddRect(context, rect1);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathFill);
    
    CGContextSetFillColorWithColor(context, [NSColor colorWithRed:204.0/255.0 green:56.0/255.0 blue:70.0/255.0 alpha:1.0].CGColor);
    CGContextBeginPath(context);
    CGContextAddRect(context, rect2);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathFill);
    
    CGContextSetFillColorWithColor(context, [NSColor colorWithRed:121.0/255.0 green:195.0/255.0 blue:218.0/255.0 alpha:1.0].CGColor);
    CGContextBeginPath(context);
    CGContextAddRect(context, rect3);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathFill);
}

@end
