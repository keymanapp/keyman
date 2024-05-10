//
//  OSKView.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 7/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "OSKView.h"
#import "OSKKey.h"
#import "KeyView.h"
#import "MacVKCodes.h"
#import "WindowsVKCodes.h"
#import "NKey.h"
//#import "KMEngine.h"
#import "CoreHelper.h"

#include <Carbon/Carbon.h>
#import <os/log.h>

@interface OSKView()
@property (nonatomic, strong) NSArray *oskLayout;
@property (nonatomic, strong) NSArray *oskDefaultNKeys;
@property (nonatomic, assign) BOOL shiftState;
@property (nonatomic, assign) BOOL oskShiftState;
@property (nonatomic, assign) BOOL altState;
@property (nonatomic, assign) BOOL oskAltState;
@property (nonatomic, assign) BOOL ctrlState;
@property (nonatomic, assign) BOOL oskCtrlState;
@end

@implementation OSKView
@synthesize tag;

- (id)initWithFrame:(NSRect)frame {
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "OSKView initWithFrame: %{public}@", NSStringFromRect(frame));
    self = [super initWithFrame:frame];
    if (self) {
        // Custom initialization
        [self initOSKKeys];
    }
    
    return self;
}

- (void)drawRect:(NSRect)rect {
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "OSKView drawRect: %{public}@", NSStringFromRect(rect));

    CGContextRef context = (CGContextRef)[[NSGraphicsContext currentContext] CGContext];
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, 1.0);
    CGColorRef cgClearColor = CGColorGetConstantColor(kCGColorClear);
    CGContextSetStrokeColorWithColor(context, cgClearColor);
    CGContextSetFillColorWithColor(context, cgClearColor);
    
    CGContextBeginPath(context);
    CGContextAddRect(context, CGRectMake(1.0, 1.0, rect.size.width-1.0, rect.size.height-1.0));
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextAddRect(context, CGRectMake(1.0, 1.0, rect.size.width-1.0, rect.size.height-1.0));
    CGContextClip(context);
    
    //TODO: gradient from clear to clear -- what does this do?
    NSColor *bgColor = [NSColor clearColor]; //[NSColor colorWithWhite:0.7 alpha:1.0];
    NSColor *bgColor2 = [NSColor clearColor]; //[NSColor colorWithWhite:0.5 alpha:1.0];

    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    NSArray *gradientColors = [NSArray arrayWithObjects:(id)bgColor.CGColor, bgColor2.CGColor, nil];
    CGFloat gradientLocations[] = {0, 1};
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

// Returns TRUE for ISO keyboards or when the .kvk file requests the 102nd key
// to right of the Left Shift key.
- (BOOL)use102ndKey {
    /*
     Note: LMGetKbdType() does not appear to work reliably; it seems to always
     return ANSI even when there is a European keyboard plugged in to my Macbook.
     We'll use it anyway in case better results are found for Macs with native
     ISO keyboards.
    */
    UInt8 kbdType = LMGetKbdType();
    PhysicalKeyboardLayoutType kbdLayoutType = KBGetLayoutType(kbdType);
    switch(kbdLayoutType) {
        case kKeyboardANSI: return _kvk != nil && _kvk.flags & KVKH_102;
        case kKeyboardJIS:
        case kKeyboardISO: return YES;
    }
    // We don't know what the keyboard is, so let's err on the side of
    // more keys, not fewer
    return YES;
}

- (void)setKvk:(KVKFile *)kvk {
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "OSKView setKvk, forces keyboard to re-layout");
    _kvk = kvk;

    // Force the keyboard to re-layout
    _oskLayout = nil;
    _oskDefaultNKeys = nil;
    
    [self setKeyLabels:self.oskShiftState alt:self.oskAltState ctrl:self.oskCtrlState];
}

- (void)initOSKKeys {
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "OSKView initOSKKeys");
    CGFloat viewWidth = self.frame.size.width;
    CGFloat viewHeight = self.frame.size.height;
    CGFloat margin = 2.0;
    CGFloat keyHeight = (viewHeight - margin*2)/self.oskLayout.count;
    CGFloat keyWidth = (viewWidth - margin*2)/14.5;
    CGFloat px;
    CGFloat py = margin;
    for (NSArray *row in self.oskLayout) {
        px = margin;
        NSUInteger len = [row count];
        for (int i = 0; i < len; i++) {
            OSKKey *key = (OSKKey *)[row objectAtIndex:i];
            CGFloat width = keyWidth * key.scale;
            if ((i+1) == len && (px + width + margin) < viewWidth)
                width += (viewWidth - (px + width + margin));
            NSRect rect = NSInsetRect(NSMakeRect(px, py, width, keyHeight), 2, 2);
            KeyView *keyView = [[KeyView alloc] initWithFrame:rect];;
            [keyView setKey:key];
            [keyView setTarget:self];
            if (self.oskShiftState && (keyView.key.keyCode == MVK_LEFT_SHIFT || keyView.key.keyCode == MVK_RIGHT_SHIFT))
                [keyView setKeyPressed:YES];
            else if (self.oskAltState && (keyView.key.keyCode == MVK_LEFT_ALT || keyView.key.keyCode == MVK_RIGHT_ALT))
                [keyView setKeyPressed:YES];
            [self addSubview:keyView];
            px += width;
        }
        py += keyHeight;
    }
    
    [self setKeyLabels:self.oskShiftState alt:self.oskAltState ctrl:self.oskCtrlState];
}

- (NSArray *)oskLayout {
    if (_oskLayout == nil) {
        os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
        os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "oskLayout -> creating new arrays of OSKKey objects");
        NSArray *row1 = [NSArray arrayWithObjects:
                         [[OSKKey alloc] initWithKeyCode:MVK_GRAVE caption:@"`" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_1 caption:@"1" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_2 caption:@"2" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_3 caption:@"3" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_4 caption:@"4" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_5 caption:@"5" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_6 caption:@"6" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_7 caption:@"7" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_8 caption:@"8" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_9 caption:@"9" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_0 caption:@"0" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_MINUS caption:@"-" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_EQUAL caption:@"=" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_BACKSPACE caption:@"⬅︎" scale:1.5], nil];
        
        NSArray *row2 = [NSArray arrayWithObjects:
                         [[OSKKey alloc] initWithKeyCode:MVK_TAB caption:@"↹" scale:1.5],
                         [[OSKKey alloc] initWithKeyCode:MVK_Q caption:@"Q" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_W caption:@"W" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_E caption:@"E" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_R caption:@"R" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_T caption:@"T" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_Y caption:@"Y" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_U caption:@"U" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_I caption:@"I" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_O caption:@"O" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_P caption:@"P" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_LEFT_BRACKET caption:@"[" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_RIGHT_BRACKET caption:@"]" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_BACKSLASH caption:@"\\" scale:1.0], nil];
        
        NSArray *row3 = [NSArray arrayWithObjects:
                         [[OSKKey alloc] initWithKeyCode:MVK_CAPS_LOCK caption:@"Caps Lock" scale:1.75],
                         [[OSKKey alloc] initWithKeyCode:MVK_A caption:@"A" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_S caption:@"S" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_D caption:@"D" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_F caption:@"F" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_G caption:@"G" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_H caption:@"H" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_J caption:@"J" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_K caption:@"K" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_L caption:@"L" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_SEMICOLON caption:@";" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_QUOTE caption:@"'" scale:1.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_ENTER caption:@"↵" scale:1.75], nil];
        
        NSArray *row4a = [self use102ndKey] ?
                         [NSArray arrayWithObjects:
                          [[OSKKey alloc] initWithKeyCode:MVK_LEFT_SHIFT caption:@"⇧" scale:1.25],
                          [[OSKKey alloc] initWithKeyCode:MVK_OEM102 caption:@"\\" scale:1.0], nil] :
                         [NSArray arrayWithObjects:
                          [[OSKKey alloc] initWithKeyCode:MVK_LEFT_SHIFT caption:@"⇧" scale:2.25], nil];

        NSArray *row4b = [NSArray arrayWithObjects:
                          [[OSKKey alloc] initWithKeyCode:MVK_Z caption:@"Z" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_X caption:@"X" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_C caption:@"C" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_V caption:@"V" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_B caption:@"B" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_N caption:@"N" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_M caption:@"M" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_COMMA caption:@"," scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_PERIOD caption:@"." scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_SLASH caption:@"/" scale:1.0],
                          [[OSKKey alloc] initWithKeyCode:MVK_RIGHT_SHIFT caption:@"⇧" scale:2.25], nil];
                
        NSArray *row4 = [row4a arrayByAddingObjectsFromArray:row4b];
        
        NSArray *row5 = [NSArray arrayWithObjects:
                         [[OSKKey alloc] initWithKeyCode:MVK_LEFT_CTRL caption:@"Ctrl" scale:1.75],
                         [[OSKKey alloc] initWithKeyCode:MVK_LEFT_ALT caption:@"Alt" scale:1.5],
                         [[OSKKey alloc] initWithKeyCode:MVK_SPACE caption:@"" scale:8.0],
                         [[OSKKey alloc] initWithKeyCode:MVK_RIGHT_ALT caption:@"Alt" scale:1.5],
                         [[OSKKey alloc] initWithKeyCode:MVK_RIGHT_CTRL caption:@"Ctrl" scale:1.75], nil];
        
        _oskLayout = [NSArray arrayWithObjects:row1, row2, row3, row4, row5, nil];
    }
    
    return _oskLayout;
}

- (NSArray *)oskDefaultNKeys {
    if (_oskDefaultNKeys == nil) {
        os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
        os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "oskDefaultNKeys -> creating new arrays of default number OSKKey objects");
        NSMutableArray *defNKeys = [[NSMutableArray alloc] initWithCapacity:0];
        
        // row 1
        NKey *nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_GRAVE; nkey1.text = @"`"; nkey1.bitmap = nil;
        NKey *nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_GRAVE; nkey2.text = @"~"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_1; nkey1.text = @"1"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_1; nkey2.text = @"!"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_2; nkey1.text = @"2"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_2; nkey2.text = @"@"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_3; nkey1.text = @"3"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_3; nkey2.text = @"#"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_4; nkey1.text = @"4"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_4; nkey2.text = @"$"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_5; nkey1.text = @"5"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_5; nkey2.text = @"%"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_6; nkey1.text = @"6"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_6; nkey2.text = @"^"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_7; nkey1.text = @"7"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_7; nkey2.text = @"&"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_8; nkey1.text = @"8"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_8; nkey2.text = @"*"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_9; nkey1.text = @"9"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_9; nkey2.text = @"("; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_0; nkey1.text = @"0"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_0; nkey2.text = @")"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_MINUS; nkey1.text = @"-"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_MINUS; nkey2.text = @"_"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_EQUAL; nkey1.text = @"="; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_EQUAL; nkey2.text = @"+"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        
        // row 2
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_Q; nkey1.text = @"q"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_Q; nkey2.text = @"Q"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_W; nkey1.text = @"w"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_W; nkey2.text = @"W"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_E; nkey1.text = @"e"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_E; nkey2.text = @"E"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_R; nkey1.text = @"r"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_R; nkey2.text = @"R"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_T; nkey1.text = @"t"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_T; nkey2.text = @"T"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_Y; nkey1.text = @"y"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_Y; nkey2.text = @"Y"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_U; nkey1.text = @"u"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_U; nkey2.text = @"U"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_I; nkey1.text = @"i"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_I; nkey2.text = @"I"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_O; nkey1.text = @"o"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_O; nkey2.text = @"O"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_P; nkey1.text = @"p"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_P; nkey2.text = @"P"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_LEFT_BRACKET; nkey1.text = @"["; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_LEFT_BRACKET; nkey2.text = @"{"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_RIGHT_BRACKET; nkey1.text = @"]"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_RIGHT_BRACKET; nkey2.text = @"}"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_BACKSLASH; nkey1.text = @"\\"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_BACKSLASH; nkey2.text = @"|"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        
        // row 3
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_A; nkey1.text = @"a"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_A; nkey2.text = @"A"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_S; nkey1.text = @"s"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_S; nkey2.text = @"S"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_D; nkey1.text = @"d"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_D; nkey2.text = @"D"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_F; nkey1.text = @"f"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_F; nkey2.text = @"F"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_G; nkey1.text = @"g"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_G; nkey2.text = @"G"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_H; nkey1.text = @"h"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_H; nkey2.text = @"H"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_J; nkey1.text = @"j"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_J; nkey2.text = @"J"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_K; nkey1.text = @"k"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_K; nkey2.text = @"K"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_L; nkey1.text = @"l"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_L; nkey2.text = @"L"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_SEMICOLON; nkey1.text = @";"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_SEMICOLON; nkey2.text = @":"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_QUOTE; nkey1.text = @"'"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_QUOTE; nkey2.text = @"\""; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        
        if([self use102ndKey]) {
            nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_OEM_102; nkey1.text = @"\\"; nkey1.bitmap = nil;
            nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_OEM_102; nkey2.text = @"|"; nkey2.bitmap = nil;
            [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        }
        // row 4
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_Z; nkey1.text = @"z"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_Z; nkey2.text = @"Z"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_X; nkey1.text = @"x"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_X; nkey2.text = @"X"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_C; nkey1.text = @"c"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_C; nkey2.text = @"C"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_V; nkey1.text = @"v"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_V; nkey2.text = @"V"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_B; nkey1.text = @"b"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_B; nkey2.text = @"B"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_N; nkey1.text = @"n"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_N; nkey2.text = @"N"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_KEY_M; nkey1.text = @"m"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_KEY_M; nkey2.text = @"M"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_COMMA; nkey1.text = @","; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_COMMA; nkey2.text = @"<"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_PERIOD; nkey1.text = @"."; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_PERIOD; nkey2.text = @">"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        nkey1 = [[NKey alloc] init]; nkey1.flags = 2; nkey1.shift = 0; nkey1.vkey = VK_SLASH; nkey1.text = @"/"; nkey1.bitmap = nil;
        nkey2 = [[NKey alloc] init]; nkey2.flags = 2; nkey2.shift = 1; nkey2.vkey = VK_SLASH; nkey2.text = @"?"; nkey2.bitmap = nil;
        [defNKeys addObjectsFromArray:@[nkey1, nkey2]];
        
        _oskDefaultNKeys = [[NSArray alloc] initWithArray:defNKeys];
    }
    
    return _oskDefaultNKeys;
}

- (void)resetOSK {
    [self setOskShiftState:NO];
    [self setOskAltState:NO];
    [self.subviews makeObjectsPerformSelector:@selector(removeFromSuperview)];
    [self initOSKKeys];
}

- (void)resizeOSKLayout {
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "OSKView resizeOSKLayout, removing all superviews");
    [self.subviews makeObjectsPerformSelector:@selector(removeFromSuperview)];
    [self initOSKKeys];
}

- (void)keyAction:(id)sender {
    KeyView *keyView = (KeyView *)sender;
    NSUInteger keyCode = [keyView.key keyCode];
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "OSKView keyAction keyCode: 0x%lx", keyCode);
    if (keyCode < 0x100) {
        NSRunningApplication *app = NSWorkspace.sharedWorkspace.frontmostApplication;
        pid_t processId = app.processIdentifier;
        CGEventSourceRef source = CGEventSourceCreate(kCGEventSourceStatePrivate);
        CGEventRef keyDownEvent = CGEventCreateKeyboardEvent(source, (CGKeyCode)keyCode, true);
        CGEventRef keyUpEvent = CGEventCreateKeyboardEvent(source, (CGKeyCode)keyCode, false);
        if (self.shiftState || self.oskShiftState) {
            CGEventSetFlags(keyDownEvent, CGEventGetFlags(keyDownEvent) | kCGEventFlagMaskShift);
            CGEventSetFlags(keyUpEvent, CGEventGetFlags(keyUpEvent) | kCGEventFlagMaskShift);
        }
        if (self.altState || self.oskAltState) {
            CGEventSetFlags(keyDownEvent, CGEventGetFlags(keyDownEvent) | kCGEventFlagMaskAlternate);
            CGEventSetFlags(keyUpEvent, CGEventGetFlags(keyUpEvent) | kCGEventFlagMaskAlternate);
        }
        if (self.ctrlState || self.oskCtrlState) {
            CGEventSetFlags(keyDownEvent, CGEventGetFlags(keyDownEvent) | kCGEventFlagMaskControl);
            CGEventSetFlags(keyUpEvent, CGEventGetFlags(keyUpEvent) | kCGEventFlagMaskControl);
        }
        CGEventPostToPid(processId, keyDownEvent);
        CGEventPostToPid(processId, keyUpEvent);
        CFRelease(source);
        CFRelease(keyDownEvent);
        CFRelease(keyUpEvent);
    }
    else {
        if (keyCode == MVK_LEFT_SHIFT || keyCode == MVK_RIGHT_SHIFT) {
            [self setOskShiftState:!self.oskShiftState];
        }
        else if (keyCode == MVK_LEFT_ALT || keyCode == MVK_RIGHT_ALT) {
            [self setOskAltState:!self.oskAltState];
        }
        else if (keyCode == MVK_LEFT_CTRL || keyCode == MVK_RIGHT_CTRL) {
            [self setOskCtrlState:!self.oskCtrlState];
        }
    }
}

- (void)handleKeyEvent:(NSEvent *)event {
    os_log_t oskLog = os_log_create("org.sil.keyman", "osk");
    os_log_with_type(oskLog, OS_LOG_TYPE_DEBUG, "OSKView handleKeyEvent event.type: %lu", event.type);
    NSView *view = [self viewWithTag:event.keyCode|0x1000];
    if (view == nil || ![view isKindOfClass:[KeyView class]])
        return;
    
    KeyView *keyView = (KeyView *)view;
    if (event.type == NSEventTypeKeyDown)
        [keyView setKeyPressed:YES];
    [NSObject cancelPreviousPerformRequestsWithTarget:self selector:@selector(setKeyPressedOff:) object:keyView];
    [self performSelector:@selector(setKeyPressedOff:) withObject:keyView afterDelay:0.1];
}

- (void)setKeyPressedOff:(KeyView *)keyView {
    [keyView setKeyPressed:NO];
}

- (void)setKeyLabels:(BOOL)shift alt:(BOOL)alt ctrl:(BOOL)ctrl {
    [self resetKeyLabels];
    NSMutableArray *mKeys = [[self keyTags] mutableCopy];
    NSArray *nkeys = [self.kvk keys];
    if (nkeys == nil)
        nkeys = self.oskDefaultNKeys;
    
    WORD flags = 0;
    if (shift)
        flags |= KVKS_SHIFT;
    if (alt)
        flags |= KVKS_RALT;
    if (ctrl)
        flags |= KVKS_RCTRL;
    
    NSString *ansiFont = [self ansiFont];
    NSString *unicodeFont = [self unicodeFont];
    unsigned short keyCode;
    for (NKey *nkey in nkeys) {
        if (nkey.shift == flags) {
            keyCode = [self MacKeyCode:nkey.vkey];
            if (keyCode < USHRT_MAX) {
                NSView *view = [self viewWithTag:keyCode|0x1000];
                if (view == nil || ![view isKindOfClass:[KeyView class]])
                    continue;
                
                KeyView *keyView = (KeyView *)view;
                [keyView setLabelFont:ansiFont];
                if (nkey.flags & KVKK_UNICODE) {
                    [keyView setLabelText:nkey.text];
                    [keyView setLabelFont:unicodeFont];
                    [mKeys removeObject:[NSNumber numberWithInteger:(keyCode|0x1000)]];
                }
                
                if (nkey.flags & KVKK_BITMAP) {
                    [keyView setBitmap:nkey.bitmap];
                }
                
                [keyView setNeedsDisplay:YES];
            }
        }
    }
    /*
    for (NSNumber *t in mKeys) {
        NSView *view = [self viewWithTag:[t integerValue]|0x1000];
        if (view == nil || ![view isKindOfClass:[KeyView class]])
            continue;
        
        KeyView *keyView = (KeyView *)view;
        [keyView setLabelText:@""];
    }*/
}

- (NSArray *)keyTags {
    NSMutableArray *keyTags = [NSMutableArray arrayWithCapacity:0];
    NSArray *views = [self subviews];
    for (NSView *view in views) {
        if (![view isKindOfClass:[KeyView class]])
            continue;
        
        [keyTags addObject:[NSNumber numberWithInteger:[view tag]]];
    }
    
    return keyTags;
}

- (void)resetKeyLabels {
    NSArray *views = [self subviews];
    for (NSView *view in views) {
        if (![view isKindOfClass:[KeyView class]])
            continue;
        
        [(KeyView *)view setKey:[(KeyView *)view key]];
        [(KeyView *)view setBitmap:nil];
        [(KeyView *)view setNeedsDisplay:YES];
    }
}

- (void)setShiftState:(BOOL)shiftState {
    if (_shiftState != shiftState) {
        _shiftState = shiftState;
        KeyView *shiftKeyL = (KeyView *)[self viewWithTag:MVK_LEFT_SHIFT|0x1000];
        KeyView *shiftKeyR = (KeyView *)[self viewWithTag:MVK_RIGHT_SHIFT|0x1000];
        if (shiftState) {
            [self setKeyLabels:YES alt:self.oskAltState ctrl:self.oskCtrlState];
            [shiftKeyL setKeyPressed:YES];
            [shiftKeyR setKeyPressed:YES];
        }
        else {
            [self setKeyLabels:NO alt:self.oskAltState ctrl:self.oskCtrlState];
            [shiftKeyL setKeyPressed:NO];
            [shiftKeyR setKeyPressed:NO];
        }
    }
}

- (void)setOskShiftState:(BOOL)oskShiftState {
    if (_oskShiftState != oskShiftState && !self.shiftState) {
        _oskShiftState = oskShiftState;
        KeyView *shiftKeyL = (KeyView *)[self viewWithTag:MVK_LEFT_SHIFT|0x1000];
        KeyView *shiftKeyR = (KeyView *)[self viewWithTag:MVK_RIGHT_SHIFT|0x1000];
        if (oskShiftState) {
            [self setKeyLabels:YES alt:self.oskAltState ctrl:self.oskCtrlState];
            [shiftKeyL setKeyPressed:YES];
            [shiftKeyR setKeyPressed:YES];
        }
        else if (!self.shiftState) {
            [self setKeyLabels:NO alt:self.oskAltState ctrl:self.oskCtrlState];
            [shiftKeyL setKeyPressed:NO];
            [shiftKeyR setKeyPressed:NO];
        }
    }
}

- (void)setAltState:(BOOL)altState {
    if (_altState != altState) {
        _altState = altState;
        KeyView *altKeyL = (KeyView *)[self viewWithTag:MVK_LEFT_ALT|0x1000];
        KeyView *altKeyR = (KeyView *)[self viewWithTag:MVK_RIGHT_ALT|0x1000];
        if (altState) {
            [self setKeyLabels:self.oskShiftState alt:YES ctrl:self.oskCtrlState];
            [altKeyL setKeyPressed:YES];
            [altKeyR setKeyPressed:YES];
        }
        else {
            [self setKeyLabels:self.oskShiftState alt:NO ctrl:self.oskCtrlState];
            [altKeyL setKeyPressed:NO];
            [altKeyR setKeyPressed:NO];
        }
    }
}

- (void)setOskAltState:(BOOL)oskAltState {
    if (_oskAltState != oskAltState && !self.altState) {
        _oskAltState = oskAltState;
        KeyView *altKeyL = (KeyView *)[self viewWithTag:MVK_LEFT_ALT|0x1000];
        KeyView *altKeyR = (KeyView *)[self viewWithTag:MVK_RIGHT_ALT|0x1000];
        if (oskAltState) {
            [self setKeyLabels:self.oskShiftState alt:YES ctrl:self.oskCtrlState];
            [altKeyL setKeyPressed:YES];
            [altKeyR setKeyPressed:YES];
        }
        else if (!self.altState) {
            [self setKeyLabels:self.oskShiftState alt:NO ctrl:self.oskCtrlState];
            [altKeyL setKeyPressed:NO];
            [altKeyR setKeyPressed:NO];
        }
    }
}

- (void)setCtrlState:(BOOL)ctrlState {
    if (_ctrlState != ctrlState) {
        _ctrlState = ctrlState;
        KeyView *ctrlKeyL = (KeyView *)[self viewWithTag:MVK_LEFT_CTRL|0x1000];
        KeyView *ctrlKeyR = (KeyView *)[self viewWithTag:MVK_RIGHT_CTRL|0x1000];
        if (ctrlState) {
            [self setKeyLabels:self.oskShiftState alt:self.oskAltState ctrl:YES];
            [ctrlKeyL setKeyPressed:YES];
            [ctrlKeyR setKeyPressed:YES];
        }
        else {
            [self setKeyLabels:self.oskShiftState alt:self.oskAltState ctrl:NO];
            [ctrlKeyL setKeyPressed:NO];
            [ctrlKeyR setKeyPressed:NO];
        }
    }
}

- (void)setOskCtrlState:(BOOL)oskCtrlState {
   if (_oskCtrlState != oskCtrlState && !self.ctrlState) {
        _oskCtrlState = oskCtrlState;
        KeyView *ctrlKeyL = (KeyView *)[self viewWithTag:MVK_LEFT_CTRL|0x1000];
        KeyView *ctrlKeyR = (KeyView *)[self viewWithTag:MVK_RIGHT_CTRL|0x1000];
        if (oskCtrlState) {
            [self setKeyLabels:self.oskShiftState alt:self.oskAltState ctrl:YES];
            [ctrlKeyL setKeyPressed:YES];
            [ctrlKeyR setKeyPressed:YES];
        }
        else if (!self.ctrlState) {
            [self setKeyLabels:self.oskShiftState alt:self.oskAltState ctrl:NO];
            [ctrlKeyL setKeyPressed:NO];
            [ctrlKeyR setKeyPressed:NO];
        }
    }
}

- (NSString *)ansiFont {
    return [self.kvk ansiFont].name;
}

- (NSString *)unicodeFont {
    return [self.kvk unicodeFont].name;
}

// Converts a Windows VK code to Mac VK code
- (unsigned short)MacKeyCode:(unsigned short)vkCode {
    unsigned short keyCode = USHRT_MAX;
    for (unsigned short i = 0; i < 0x80; i++) {
        if (VirtualKeyMap[i] == vkCode) {
            keyCode = i;
            break;
        }
    }
    
    return keyCode;
}

@end
