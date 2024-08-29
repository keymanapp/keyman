//
//  KeyView.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 7/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KeyView.h"
#import "KeyLabel.h"
#import "MacVKCodes.h"
#import "TimerTarget.h"
#import "KMELogs.h"

CGFloat lw = 1.0;
CGFloat r = 7.0;
const NSTimeInterval delayBeforeRepeating = 0.5f;
const NSTimeInterval repeatInterval = 0.05f;

static CGFloat const kRelativeCharacterLabelHeight = 0.45f;
static CGFloat const kRelativeModifierLabelHeight = 0.30f;

@interface KeyView ()
@property (nonatomic, assign) NSUInteger keyCode;
@property (nonatomic, assign) BOOL hasKeyCaption;
@property (nonatomic, assign) BOOL isModifierKey;
@property (nonatomic, assign) BOOL isSpecialKey;
@property (nonatomic, assign) BOOL isCharacterKey;
@property (nonatomic, strong) KeyLabel *label;
@property (nonatomic, strong) NSTextField *caption;
@property (nonatomic, strong) NSColor *bgColorRegularKey;
@property (nonatomic, strong) NSColor *bgColorSpecialKey;
@property (nonatomic, strong) NSTimer *keyEventTimer;
@property (readwrite) NSInteger tag;
@end

@implementation KeyView
@synthesize tag;
@synthesize bgColorRegularKey, bgColorSpecialKey;

- (id)initWithFrame:(NSRect)frame {
  self = [super initWithFrame:frame];
  if (self) {
    /*
     // usually too much, but uncomment for debugging
     os_log_debug([KMELogs oskLog], "KeyView initWithFrame: %{public}@, bounds: %{public}@, default clipsToBounds %{public}@", NSStringFromRect(frame), NSStringFromRect(self.bounds), self.clipsToBounds?@"YES":@"NO");
    */
    self.clipsToBounds = true;
    CGSize size = frame.size;
    CGFloat x = size.width*0.05;
    NSRect labelFrame = NSMakeRect(x, 0, size.width -lw -2*x, size.height);
    _label = [[KeyLabel alloc] initWithFrame:labelFrame];
    [_label setEditable:NO];
    [_label setBordered:NO];
    [_label setDrawsBackground:NO];
    [_label setAlignment:NSTextAlignmentCenter];
    if ([_caption respondsToSelector:@selector(setLineBreakMode:)]) {
      [_label setLineBreakMode:NSLineBreakByClipping];
    } // There might be some problem not calling this, but it seems to be okay as far as I can tell
    
    CGFloat fontSize = labelFrame.size.height*kRelativeModifierLabelHeight;
    [_label setFont:[NSFont systemFontOfSize:fontSize]];
    [_label setTextColor:[NSColor blackColor]];
    [_label setStringValue:@""];
    [_label setLineBreakMode:NSLineBreakByClipping];
    [self addSubview:_label];
    
    bgColorRegularKey = [self getOpaqueColorWithRed:209 green:211 blue:212];
    bgColorSpecialKey = [self getOpaqueColorWithRed:166 green:169 blue:172];
  }
  
  return self;
}

- (void)drawRect:(NSRect)rect {
  /*
   // usually too much, but uncomment for debugging
  os_log_debug([KMELogs uiLog], "KeyView drawRect: %{public}@, bounds: %{public}@, keyCode: 0x%lx, caption: %{public}@, label: %{public}@", NSStringFromRect(rect), NSStringFromRect(self.bounds), self.keyCode, self.caption.stringValue, self.label.stringValue);
  */
  
  [[self getOpaqueColorWithRed:241 green:242 blue:242] setFill];
  NSRectFillUsingOperation(rect, NSCompositingOperationSourceOver);
  
  // Drawing code here.
  CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] CGContext];
  
  CGContextSetLineJoin(context, kCGLineJoinRound);
  CGContextSetLineWidth(context, lw);
  CGContextSetStrokeColorWithColor(context, bgColorRegularKey.CGColor);
  
  CGFloat x = rect.origin.x + lw;
  CGFloat y = rect.origin.y + lw;
  CGFloat w = rect.size.width - 2*lw;
  CGFloat h = rect.size.height - 2*lw;
  
  CGContextBeginPath(context);
  CGContextMoveToPoint(context, w/2.0, y);
  CGContextAddArcToPoint(context, w, y, w, y+h, r);
  CGContextAddArcToPoint(context, w, y+h, x, y+h, r);
  CGContextAddArcToPoint(context, x, y+h, x, y, r);
  CGContextAddArcToPoint(context, x, y, w/2.0, y, r);
  CGContextClosePath(context);
  CGContextDrawPath(context, kCGPathStroke);
  
  CGContextBeginPath(context);
  CGContextMoveToPoint(context, w/2.0, y);
  CGContextAddArcToPoint(context, w, y, w, y+h, r);
  CGContextAddArcToPoint(context, w, y+h, x, y+h, r);
  CGContextAddArcToPoint(context, x, y+h, x, y, r);
  CGContextAddArcToPoint(context, x, y, w/2.0, y, r);
  CGContextClosePath(context);
  CGContextClip(context);
  
  NSColor *bgColor = bgColorRegularKey;
  
  if ([self isSpecialKey])
    bgColor = bgColorSpecialKey;
  
  CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
  CGFloat gradientLocations[] = {0, 1};
  NSArray *gradientColors = [NSArray arrayWithObjects:(id)bgColor.CGColor, bgColor.CGColor, nil];
  CGGradientRef gradient = CGGradientCreateWithColors(colorSpace, (__bridge CFArrayRef)gradientColors, gradientLocations);
  CGPoint startPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMinY(rect));
  CGPoint endPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMaxY(rect));
  CGContextDrawLinearGradient(context, gradient, startPoint, endPoint, 0);
  CGGradientRelease(gradient);
  CGColorSpaceRelease(colorSpace);
}

- (BOOL)isFlipped {
  return YES;
}

- (void)setKey:(OSKKey *)key {
  _key = key;
  [self setKeyCode:key.keyCode];
  [self setTag:key.keyCode|0x1000];
  if ([self isSpecialKey])
    [self setLabelText:key.caption];
  else
    [self setLabelText:@""];
  if (key.scale == 1.0)
    [self setCaptionText:key.caption];
}

- (void)setKeyCode:(NSUInteger)keyCode {
  _keyCode = keyCode;
  if (keyCode == MVK_LEFT_SHIFT || keyCode == MVK_RIGHT_SHIFT)
    _isModifierKey = YES;
  else if (keyCode == MVK_LEFT_ALT || keyCode == MVK_RIGHT_ALT)
    _isModifierKey = YES;
  else if (keyCode == MVK_LEFT_CTRL || keyCode == MVK_RIGHT_CTRL)
    _isModifierKey = YES;
  
  if (keyCode >= MVK_CAPS_LOCK && keyCode <= MVK_RIGHT_ALT)
    _isSpecialKey = YES;
  else if (self.keyCode == MVK_ENTER || self.keyCode == MVK_TAB || self.keyCode == MVK_BACKSPACE)
    _isSpecialKey = YES;
  else
    _isSpecialKey = NO;
  
  if (!_isSpecialKey && !_isModifierKey) {
    _isCharacterKey = YES;
  } else {
    _isCharacterKey = NO;
  }
  
  // having changed the keycode, adjust the font size of the key label to optimize for available space
  [self adjustLabelFontSize];
}


/**
 * Set the size of the font relative to the key height, depending on what type of key it is.
 * This allows larger more readable character key text.
 */
- (void)adjustLabelFontSize {
  CGFloat relativeFontSize = kRelativeCharacterLabelHeight;
  
  if (!self.isCharacterKey) {
    relativeFontSize = kRelativeModifierLabelHeight;
  }
  
  CGFloat fontSize = self.label.bounds.size.height*relativeFontSize;
  [self.label setFont:[NSFont systemFontOfSize:fontSize]];
}

- (void)setLabelText:(NSString *)text {
  [_label setStringValue:text];
}

- (void)setLabelFont:(NSString *)fontName {
  CGFloat fontSize = [_label.font pointSize];
  if (fontName != nil) {
    NSFont *font = [NSFont fontWithName:fontName size:fontSize];
    if (font != nil) {
      [_label setFont:font];
    }
  }
}

- (void)setCaptionText:(NSString *)text {
  if (_caption == nil)
    [self setHasKeyCaption:YES];
  [_caption setStringValue:text];
}

- (void)setCaptionFont:(NSString *)fontName {
  if (_caption == nil)
    [self setHasKeyCaption:YES];
  
  CGFloat fontSize = [_caption.font pointSize];
  if (fontName != nil) {
    NSFont *font = [NSFont fontWithName:fontName size:fontSize];
    if (font != nil) {
      [_caption setFont:font];
    }
  }
}

- (void)setBitmap:(NSImage *)bitmap {
  // Bitmap is disabled: Per Marc, this is super low priority. Bitmaps are ugly and scale badly. We
  // supported them a long time ago in Windows but you'd need to add support for Windows' BMP format
  // to show them. I think we'd be better off not supporting them for now (just document as a platform
  // difference).
  _bitmap = nil;
  /*
   _bitmap = bitmap;
   [_bitmapView removeFromSuperview];
   if (_bitmap != nil) {
   NSTextFieldCell *cell = (NSTextFieldCell *)self.label.cell;
   NSRect frame = cell.controlView.frame;
   frame = NSInsetRect(frame, frame.size.width*0.15, frame.size.height*0.15);
   _bitmapView = [[NSImageView alloc] initWithFrame:frame];
   [_bitmapView setImageScaling:NSImageScaleProportionallyUpOrDown];
   [_bitmapView setImage:_bitmap];
   [self.label setStringValue:@"[    ]"];
   [self addSubview:_bitmapView];
   }*/
}

- (void)setHasKeyCaption:(BOOL)hasKeyCaption {
  _hasKeyCaption = hasKeyCaption;
  if (hasKeyCaption && _caption == nil) {
    NSRect captionFrame = NSMakeRect(1.5, 3, self.frame.size.width*0.3, self.frame.size.height*0.3);
    CGFloat fontSize = captionFrame.size.height*0.7;
    _caption = [[NSTextField alloc] initWithFrame:captionFrame];
    [_caption setEditable:NO];
    [_caption setBordered:NO];
    [_caption setDrawsBackground:NO];
    //[_caption setBackgroundColor:[NSColor yellowColor]];
    [_caption setAlignment:NSTextAlignmentLeft];
    if ([_caption respondsToSelector:@selector(setLineBreakMode:)]) {
      [_caption setLineBreakMode:NSLineBreakByClipping];
    } // There might be some problem not calling this, but it seems to be okay as far as I can tell
    [_caption setFont:[NSFont systemFontOfSize:fontSize]];
    [_caption setTextColor:[NSColor darkGrayColor]];
    [_caption setStringValue:@""];
    [self addSubview:_caption];
  }
  else if (!hasKeyCaption) {
    [_caption removeFromSuperview];
    _caption = nil;
  }
}

- (void)mouseDown:(NSEvent *)theEvent {
  @synchronized(self.target) {
    if (!self.isModifierKey) {
      // All KeyViews are about to get blown away and recreated, so we don't want the timer to fire
      // again until some other key gets pressed.
      [self stopTimer];
    }
    else {
      [self setKeyPressed:YES];
    }
    [self processKeyClick];
    if (!self.isModifierKey)
      [self startTimerWithTimeInterval:delayBeforeRepeating];
  }
}

- (void)mouseUp:(NSEvent *)theEvent {
  if (!self.isModifierKey) {
    [self setKeyPressed:NO];
    [self stopTimer];
  }
}

-(void)processKeyClick {
  [self.target keyAction:self];
}

- (void)startTimerWithTimeInterval:(NSTimeInterval)interval {
  @synchronized(self.target) {
    if (_keyEventTimer == nil) {
      // The TimerTarget class and the following two lines allow the timer to hold a *weak*
      // reference to this KeyView object, so it can be disposed even if there is a timer waiting
      // to fire that refers to it.
      TimerTarget *timerTarget = [[TimerTarget alloc] init];
      timerTarget.target = self;
      _keyEventTimer = [NSTimer scheduledTimerWithTimeInterval:interval
                                                        target:timerTarget
                                                      selector:@selector(timerAction:)
                                                      userInfo:nil
                                                       repeats:YES];
    }
  }
}

- (void)stopTimer {
  @synchronized(self.target) {
    //os_log_debug([KMELogs uiLog], "KeyView TIMER - stopping");
    if (_keyEventTimer != nil) {
      [_keyEventTimer invalidate];
      _keyEventTimer = nil;
    }
  }
}

- (void)timerAction:(NSTimer *)timer {
  @synchronized(self.target) {
    //os_log_debug([KMELogs uiLog], "KeyView TIMER - Fired for key %lu", [self keyCode]);
    [self processKeyClick];
    
    if ([timer timeInterval] == delayBeforeRepeating) {
      // Fired following a normal (non-modifier key press). As long as user continues to hold
      // down that key, it will now begin to repeat every 0.05 seconds. All we really want to
      // do is change the time interval, but NSTimer doesn't support that.
      [self stopTimer];
      [self startTimerWithTimeInterval:repeatInterval];
    }
  }
}

- (void)setKeyPressed:(BOOL)keyPressed {
  _keyPressed = keyPressed;
  if (keyPressed) {
    bgColorRegularKey = [self getOpaqueColorWithRed:109 green:111 blue:112];
    bgColorSpecialKey = [self getOpaqueColorWithRed:236 green:239 blue:242];
    [self setNeedsDisplay:YES];
  }
  else {
    bgColorRegularKey = [self getOpaqueColorWithRed:209 green:211 blue:212];
    bgColorSpecialKey = [self getOpaqueColorWithRed:166 green:169 blue:172];
    [self setNeedsDisplay:YES];
  }
}

- (NSColor *)getOpaqueColorWithRed:(NSUInteger) red green: (NSUInteger) green blue: (NSUInteger) blue {
  return [NSColor colorWithSRGBRed:red/255.0 green:green/255.0 blue:blue/255.0 alpha:1.0];
}
@end
