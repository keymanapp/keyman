//
//  KMKeyPreviewView.m
//  KeymanEngine
//
//  Created by Serkan Kurt on 2/06/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMKeyPreviewView.h"
#import "KMManager+Internal.h"
#import "KMInputViewController.h"

@interface KMKeyPreviewView ()
@property (assign) CGRect keyFrame;
@property (assign) CGFloat strokeWidth;
@property (assign) CGFloat borderRadius;
@property (assign) CGFloat adjX, adjY;
@property (assign) NSInteger direction; // -1 left, 1 right, 0 up
@property (nonatomic, strong) UIColor *borderColor;
@property (nonatomic, strong) UIColor *bgColor;
@property (nonatomic, strong) UIColor *bgColor2;
@property (nonatomic, strong) UILabel *label;
@end

@implementation KMKeyPreviewView
@synthesize keyFrame, strokeWidth, adjX, adjY, direction;
@synthesize borderRadius, bgColor, bgColor2, borderColor;

- (id)initWithFrame:(CGRect)frame {
    BOOL isPortrait = YES;
    if ([KMManager isKeymanSystemKeyboard])
        isPortrait = [KMInputViewController isPortrait];
    else {
        UIInterfaceOrientation orientation = [[UIApplication sharedApplication] statusBarOrientation];
        isPortrait = UIInterfaceOrientationIsPortrait(orientation);
    }
    
    keyFrame = CGRectInset(frame, 0, 0); // CGRectInset(frame, 2, 2);
    CGFloat viewWidth = keyFrame.size.width*(isPortrait?1.6:1.3);
    //CGFloat viewHeight = keyFrame.size.height*([KMManager isKeymanSystemKeyboard]?2.1:2.2);
    CGFloat viewHeight = keyFrame.size.height*2.1; //*1.95; // 2.1;
    CGFloat viewPosX = keyFrame.origin.x - (viewWidth - keyFrame.size.width)/2.0;
    CGFloat viewPosY = keyFrame.origin.y - (viewHeight - keyFrame.size.height);
    
    adjY = 0;
    NSInteger tbHeight = [[KMManager sharedInstance] isSystemKeyboardTopBarEnabled]?[KMInputViewController topBarHeight]:0;
    if ([KMManager isKeymanSystemKeyboard] && (viewPosY < -tbHeight)) {
        adjY = viewPosY + tbHeight;
        viewPosY = -tbHeight;
        viewHeight = viewHeight + adjY;
    }
    
    CGFloat screenWidth;
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion >= 8)
        screenWidth = screenRect.size.width;
    else {
        if (isPortrait)
            screenWidth = screenRect.size.width;
        else
            screenWidth = screenRect.size.height;
    }
    
    adjX = 0;
    direction = 0;
    if (viewPosX < 0) {
        adjX = keyFrame.origin.x - viewPosX;
        viewPosX = keyFrame.origin.x;
    }
    else if ((viewPosX + viewWidth) > screenWidth) {
        adjX = viewPosX - keyFrame.origin.x;
        viewPosX = keyFrame.origin.x + 2*adjX;
    }
    
    /*
    if ([KMManager isKeymanSystemKeyboard] && (viewPosY < 0)) {
        viewWidth = keyFrame.size.width*(UIInterfaceOrientationIsPortrait(orientation)?4.0:3.0);
        viewHeight = keyFrame.size.height*1.2;
        if (viewPosX < ((screenWidth - (int)screenWidth%100)/2)) {
            direction = 1;
            viewPosX = keyFrame.origin.x;
            viewPosY = keyFrame.origin.y;
        }
        else {
            direction = -1;
            viewPosX = keyFrame.origin.x - (viewWidth - keyFrame.size.width);
            viewPosY = keyFrame.origin.y;
        }
    }
    else {
        direction = 0;
        if (viewPosX < 0) {
            adjX = keyFrame.origin.x - viewPosX;
            viewPosX = keyFrame.origin.x;
        }
        else if ((viewPosX + viewWidth) > screenWidth) {
            adjX = viewPosX - keyFrame.origin.x;
            viewPosX = keyFrame.origin.x + 2*adjX;
        }
    }*/
    
    self = [super initWithFrame:CGRectMake(viewPosX, viewPosY, viewWidth, viewHeight)];
    if (self) {
        super.backgroundColor = [UIColor clearColor];
        self.userInteractionEnabled = NO;
        strokeWidth = 1.0;
        borderRadius = 5.0; // 4.0;
        /*
        bgColor = [[UIColor alloc] initWithRed:225.0/255.0
                                         green:225.0/255.0
                                          blue:225.0/255.0
                                         alpha:1.0];
        bgColor2 = [[UIColor alloc] initWithRed:175.0/255.0
                                          green:175.0/255.0
                                           blue:175.0/255.0
                                          alpha:1.0];
        borderColor = [[UIColor alloc] initWithRed:85.0/255.0 green:87.0/255.0 blue:89.0/255.0 alpha:1.0];
        */
        bgColor = [UIColor whiteColor];
        bgColor2 = [UIColor whiteColor];
        borderColor = [[UIColor alloc] initWithRed:145.0/255.0 green:148.0/255.0 blue:152.0/255.0 alpha:1.0];
        
        CGFloat m = strokeWidth*2;
        CGFloat fontSize = viewHeight*0.35;
        CGRect labelFrame;
        if (direction == 1) {
            labelFrame = CGRectMake(viewWidth - viewWidth*0.35 + m, viewHeight*0.1 - m, viewWidth*0.35 - 2*m, viewHeight*0.9 - m);
            fontSize = viewHeight*0.7;
        }
        else if (direction == -1) {
            labelFrame = CGRectMake(m, viewHeight*0.1 - m, viewWidth*0.35 - 2*m, viewHeight*0.9 - m);
            fontSize = viewHeight*0.7;
        }
        else
            labelFrame = CGRectMake(m, m, viewWidth - 2*m, viewHeight*0.5 - m);
        
        self.label = [[UILabel alloc] initWithFrame:labelFrame];
        [self.label setNumberOfLines:1];
        [self.label setMinimumScaleFactor:0.25];
        [self.label setAdjustsFontSizeToFitWidth:YES];
        [self.label setTextAlignment:NSTextAlignmentCenter];
        [self.label setFont:[UIFont systemFontOfSize:fontSize]];
        [self.label setShadowColor:[UIColor lightTextColor]];
        [self.label setShadowOffset:CGSizeMake(1.0, 1.0)];
        [self.label setBackgroundColor:[UIColor clearColor]];
        [self.label setTextColor:[UIColor darkTextColor]];
        [self.label setText:@""];
        [self addSubview:self.label];
    }
    
    return self;
}

- (void)drawRect:(CGRect)rect {
    [self drawKeyPreviewUp:rect];
    /*
    if (direction == 1)
        [self drawKeyPreviewRight:rect];
    else if (direction == -1)
        [self drawKeyPreviewLeft:rect];
    else
        [self drawKeyPreviewUp:rect];
    */
}

- (void)drawKeyPreviewUp:(CGRect)rect {
    CGFloat keyWidth = keyFrame.size.width;
    CGFloat viewWidth = rect.size.width;
    CGFloat viewHeight = rect.size.height;
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, strokeWidth*2);
    CGContextSetStrokeColorWithColor(context, borderColor.CGColor);
    //CGContextSetStrokeColorWithColor(context, [UIColor colorWithRed:1.0 green:0.0 blue:0.0 alpha:0.5].CGColor);
    CGContextSetFillColorWithColor(context, bgColor.CGColor);
    
    CGFloat keyLeft = (viewWidth - keyWidth)/2.0 + strokeWidth - adjX;
    CGFloat keyRight = (viewWidth - keyWidth)/2.0 + keyWidth - strokeWidth - adjX;
    CGFloat midY1 = viewHeight*3.0/5.0;
    CGFloat midY2 = viewHeight/2.0;
    CGFloat viewLeft = strokeWidth;
    CGFloat viewRight = viewWidth - strokeWidth;
    CGFloat viewTop = strokeWidth;
    CGFloat viewBottom = viewHeight - strokeWidth*1.25; /* */
    CGFloat viewMid = viewWidth/2.0;
    CGFloat r0 = borderRadius - strokeWidth;
    CGFloat r1 = borderRadius - strokeWidth;
    CGFloat r2 = borderRadius*1.25 - strokeWidth;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, midY2, r1);
    CGContextAddArcToPoint(context, viewLeft, midY2, keyLeft, midY1, r2);
    CGContextAddArcToPoint(context, keyLeft, midY1, keyLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, keyLeft, viewBottom, keyRight, viewBottom, r0);
    CGContextAddArcToPoint(context, keyRight, viewBottom, keyRight, midY1, r0);
    CGContextAddArcToPoint(context, keyRight, midY1, viewRight, midY2, r2);
    CGContextAddArcToPoint(context, viewRight, midY2, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewMid, viewTop, r1);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, midY2, r1);
    CGContextAddArcToPoint(context, viewLeft, midY2, keyLeft, midY1, r2);
    CGContextAddArcToPoint(context, keyLeft, midY1, keyLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, keyLeft, viewBottom, keyRight, viewBottom, r0);
    CGContextAddArcToPoint(context, keyRight, viewBottom, keyRight, midY1, r0);
    CGContextAddArcToPoint(context, keyRight, midY1, viewRight, midY2, r2);
    CGContextAddArcToPoint(context, viewRight, midY2, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewMid, viewTop, r1);
    CGContextClosePath(context);
    CGContextClip(context);
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    NSArray *gradientColors = [NSArray arrayWithObjects:(id)bgColor.CGColor, bgColor2.CGColor, nil];
    CGFloat gradientLocations[] = {0, 1};
    CGGradientRef gradient = CGGradientCreateWithColors(colorSpace, (__bridge CFArrayRef) gradientColors, gradientLocations);
    
    CGPoint startPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMinY(rect));
    CGPoint endPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMaxY(rect));
    
    CGContextDrawLinearGradient(context, gradient, startPoint, endPoint, 0);
    CGGradientRelease(gradient);
    CGColorSpaceRelease(colorSpace);
    UIGraphicsPopContext();
}

/*
- (void)drawKeyPreviewRight:(CGRect)rect {
    CGFloat keyWidth = keyFrame.size.width;
    CGFloat keyHeight = keyFrame.size.height;
    CGFloat viewWidth = rect.size.width;
    CGFloat viewHeight = rect.size.height;
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, strokeWidth*2.0);
    CGContextSetStrokeColorWithColor(context, borderColor.CGColor);
    CGContextSetFillColorWithColor(context, bgColor.CGColor);
    
    CGFloat keyBottom = keyHeight - strokeWidth;
    CGFloat midX1 = viewWidth*3.0/4.5;
    CGFloat midX2 = viewWidth*3.0/5.0;
    CGFloat midY1 = viewHeight*0.15;
    CGFloat midY2 = viewHeight*0.7;
    CGFloat viewLeft = strokeWidth;
    CGFloat viewRight = viewWidth - strokeWidth;
    CGFloat viewTop = strokeWidth;
    CGFloat viewBottom = viewHeight - strokeWidth;
    CGFloat keyMid = keyWidth/2.0;
    CGFloat keyRight = keyWidth - strokeWidth;
    CGFloat r = borderRadius - strokeWidth;
    CGFloat r2 = borderRadius*2 - strokeWidth;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, keyBottom, r);
    CGContextAddArcToPoint(context, viewLeft, keyBottom, keyRight, keyBottom, r);
    CGContextAddArcToPoint(context, keyRight, keyBottom, keyRight, midY2, r);
    CGContextAddArcToPoint(context, keyRight, midY2, midX2, midY2, r);
    CGContextAddArcToPoint(context, midX2, midY2, midX1, viewBottom, r2);
    CGContextAddArcToPoint(context, midX1, viewBottom, viewRight, viewBottom, r2);
    CGContextAddArcToPoint(context, viewRight, viewBottom, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, midX1, viewTop, r2);
    CGContextAddArcToPoint(context, midX1, viewTop, midX2, midY1, r2);
    CGContextAddArcToPoint(context, midX2, midY1, keyRight, midY1, r2);
    CGContextAddArcToPoint(context, keyRight, midY1, keyRight, viewTop, r);
    CGContextAddArcToPoint(context, keyRight, viewTop, keyMid, viewTop, r);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, keyBottom, r);
    CGContextAddArcToPoint(context, viewLeft, keyBottom, keyRight, keyBottom, r);
    CGContextAddArcToPoint(context, keyRight, keyBottom, keyRight, midY2, r);
    CGContextAddArcToPoint(context, keyRight, midY2, midX2, midY2, r);
    CGContextAddArcToPoint(context, midX2, midY2, midX1, viewBottom, r2);
    CGContextAddArcToPoint(context, midX1, viewBottom, viewRight, viewBottom, r2);
    CGContextAddArcToPoint(context, viewRight, viewBottom, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, midX1, viewTop, r2);
    CGContextAddArcToPoint(context, midX1, viewTop, midX2, midY1, r2);
    CGContextAddArcToPoint(context, midX2, midY1, keyRight, midY1, r2);
    CGContextAddArcToPoint(context, keyRight, midY1, keyRight, viewTop, r);
    CGContextAddArcToPoint(context, keyRight, viewTop, keyMid, viewTop, r);
    CGContextClosePath(context);
    CGContextClip(context);
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    NSArray *gradientColors = [NSArray arrayWithObjects:(id)bgColor.CGColor, bgColor2.CGColor, nil];
    CGFloat gradientLocations[] = {0, 1};
    CGGradientRef gradient = CGGradientCreateWithColors(colorSpace, (__bridge CFArrayRef) gradientColors, gradientLocations);
    
    CGPoint startPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMinY(rect));
    CGPoint endPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMaxY(rect));
    
    CGContextDrawLinearGradient(context, gradient, startPoint, endPoint, 0);
    CGGradientRelease(gradient);
    CGColorSpaceRelease(colorSpace);
    UIGraphicsPopContext();
}

- (void)drawKeyPreviewLeft:(CGRect)rect {
    CGFloat keyWidth = keyFrame.size.width;
    CGFloat keyHeight = keyFrame.size.height;
    CGFloat viewWidth = rect.size.width;
    CGFloat viewHeight = rect.size.height;
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, strokeWidth*2.0);
    CGContextSetStrokeColorWithColor(context, borderColor.CGColor);
    CGContextSetFillColorWithColor(context, bgColor.CGColor);
    
    CGFloat keyBottom = keyHeight - strokeWidth;
    CGFloat midX1 = viewWidth - viewWidth*3.0/4.5;
    CGFloat midX2 = viewWidth - viewWidth*3.0/5.0;
    CGFloat midY1 = viewHeight*0.15;
    CGFloat midY2 = viewHeight*0.7;
    CGFloat viewLeft = strokeWidth;
    CGFloat viewRight = viewWidth - strokeWidth;
    CGFloat viewTop = strokeWidth;
    CGFloat viewBottom = viewHeight - strokeWidth;
    CGFloat keyMid = viewWidth - keyWidth/2.0;
    CGFloat keyLeft = viewWidth - keyWidth + strokeWidth;
    CGFloat r = borderRadius - strokeWidth;
    CGFloat r2 = borderRadius*2 - strokeWidth;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, viewTop);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewRight, keyBottom, r);
    CGContextAddArcToPoint(context, viewRight, keyBottom, keyLeft, keyBottom, r);
    CGContextAddArcToPoint(context, keyLeft, keyBottom, keyLeft, midY2, r);
    CGContextAddArcToPoint(context, keyLeft, midY2, midX2, midY2, r);
    CGContextAddArcToPoint(context, midX2, midY2, midX1, viewBottom, r2);
    CGContextAddArcToPoint(context, midX1, viewBottom, viewLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, viewLeft, viewBottom, viewLeft, viewTop, r2);
    CGContextAddArcToPoint(context, viewLeft, viewTop, midX1, viewTop, r2);
    CGContextAddArcToPoint(context, midX1, viewTop, midX2, midY1, r2);
    CGContextAddArcToPoint(context, midX2, midY1, keyLeft, midY1, r2);
    CGContextAddArcToPoint(context, keyLeft, midY1, keyLeft, viewTop, r);
    CGContextAddArcToPoint(context, keyLeft, viewTop, keyMid, viewTop, r);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, viewTop);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewRight, keyBottom, r);
    CGContextAddArcToPoint(context, viewRight, keyBottom, keyLeft, keyBottom, r);
    CGContextAddArcToPoint(context, keyLeft, keyBottom, keyLeft, midY2, r);
    CGContextAddArcToPoint(context, keyLeft, midY2, midX2, midY2, r);
    CGContextAddArcToPoint(context, midX2, midY2, midX1, viewBottom, r2);
    CGContextAddArcToPoint(context, midX1, viewBottom, viewLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, viewLeft, viewBottom, viewLeft, viewTop, r2);
    CGContextAddArcToPoint(context, viewLeft, viewTop, midX1, viewTop, r2);
    CGContextAddArcToPoint(context, midX1, viewTop, midX2, midY1, r2);
    CGContextAddArcToPoint(context, midX2, midY1, keyLeft, midY1, r2);
    CGContextAddArcToPoint(context, keyLeft, midY1, keyLeft, viewTop, r);
    CGContextAddArcToPoint(context, keyLeft, viewTop, keyMid, viewTop, r);
    CGContextClosePath(context);
    CGContextClip(context);
    
    CGColorSpaceRef colorSpace = CGColorSpaceCreateDeviceRGB();
    NSArray *gradientColors = [NSArray arrayWithObjects:(id)bgColor.CGColor, bgColor2.CGColor, nil];
    CGFloat gradientLocations[] = {0, 1};
    CGGradientRef gradient = CGGradientCreateWithColors(colorSpace, (__bridge CFArrayRef) gradientColors, gradientLocations);
    
    CGPoint startPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMinY(rect));
    CGPoint endPoint = CGPointMake(CGRectGetMidX(rect), CGRectGetMaxY(rect));
    
    CGContextDrawLinearGradient(context, gradient, startPoint, endPoint, 0);
    CGGradientRelease(gradient);
    CGColorSpaceRelease(colorSpace);
    UIGraphicsPopContext();
}
*/

- (void)setLabelText:(NSString *)text {
    [self.label setText:text];
}

- (void)setLabelFont:(NSString *)fontName {
    CGFloat fontSize = [self.label.font pointSize];
    if (fontName != nil)
        [self.label setFont:[UIFont fontWithName:fontName size:fontSize]];
    else
        [self.label setFont:[UIFont systemFontOfSize:fontSize]];
}

- (void)setBackgroundColor:(UIColor *)backgroundColor {
    super.backgroundColor = [UIColor clearColor];
    self.bgColor = [UIColor colorWithCGColor:backgroundColor.CGColor];
}

@end
