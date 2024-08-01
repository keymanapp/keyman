//
//  KeyLabelCell.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 7/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyLabelCell.h"

@implementation KeyLabelCell

- (void)drawInteriorWithFrame:(NSRect)cellFrame inView:(NSView *)controlView {
  NSRect titleRect = [self titleRectForBounds:cellFrame];
  if (self.drawsBackground && self.backgroundColor != nil) {
    if (self.backgroundColor.alphaComponent > 0) {
      [self.backgroundColor set];
      NSRectFill(titleRect);
    }
  }
  
  NSAttributedString *attrString = self.attributedStringValue;
  if (self.isHighlighted && self.backgroundStyle == NSBackgroundStyleDark) {
    NSMutableAttributedString *whiteString = attrString.mutableCopy;
    [whiteString addAttribute:NSForegroundColorAttributeName
                        value:[NSColor whiteColor]
                        range:NSMakeRange(0, whiteString.length)];
    attrString = whiteString;
  }
  
  [attrString drawWithRect:titleRect options:NSStringDrawingTruncatesLastVisibleLine|NSStringDrawingUsesLineFragmentOrigin];
}

- (NSRect)titleRectForBounds:(NSRect)theRect {
  NSRect titleRect = [super titleRectForBounds:theRect];
  NSAttributedString *attrString = self.attributedStringValue;
  NSRect textRect = [attrString boundingRectWithSize:titleRect.size options:NSStringDrawingTruncatesLastVisibleLine|NSStringDrawingUsesLineFragmentOrigin];
  if (NSHeight(textRect) < NSHeight(titleRect)) {
    titleRect.origin.y = theRect.origin.y + (NSHeight(theRect) - NSHeight(textRect))*0.5f;
    titleRect.size.height = NSHeight(textRect);
  }
  
  return titleRect;
}

@end
