//
//  KMSubKeysView.m
//  KeymanEngine
//
//  Created by Serkan Kurt on 15/08/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMSubKeysView.h"
#import "KMManager+Internal.h"
#import "KMInputViewController.h"

static CGFloat borderRadius = 5.0; // 4.0;
static CGFloat strokeWidth = 1.0;

@interface KMSubKeysView ()
@property (assign) CGRect keyFrame;
@property (assign) CGFloat adjX;
@property (assign) CGFloat adjY;
@property (assign) CGFloat scale;
@property (assign) NSInteger direction; // -1 left, 1 right, 0 up
@property (assign) NSInteger rows;
@property (assign) BOOL isPad;
@property (nonatomic, strong) UIColor *borderColor;
@property (nonatomic, strong) UIColor *bgColor;
@property (nonatomic, strong) UIColor *bgColor2;
@end

@implementation KMSubKeysView
@synthesize containerView;
@synthesize keyFrame, adjX, adjY, scale, rows, direction;
@synthesize bgColor, bgColor2, borderColor;

- (id)initWithKeyFrame:(CGRect)frame subKeys:(NSArray *)subKeys {
    BOOL isSystemKeyboard = [KMManager isKeymanSystemKeyboard];
    BOOL isPortrait = YES;
    if (isSystemKeyboard)
        isPortrait = [KMInputViewController isPortrait];
    else
        isPortrait = UIInterfaceOrientationIsPortrait([[UIApplication sharedApplication] statusBarOrientation]);
    
    scale = [[UIScreen mainScreen] scale];
    CGFloat screenWidth, screenHeight;
    CGRect screenRect = [[UIScreen mainScreen] bounds];
    int iosVersion = [[[UIDevice currentDevice] systemVersion] intValue];
    if (iosVersion >= 8 || isPortrait) {
        screenWidth = screenRect.size.width;
        screenHeight = screenRect.size.height;
    }
    else {
        screenWidth = screenRect.size.height;
        screenHeight = screenRect.size.width;
    }
    
    keyFrame = CGRectInset(frame, 0, 0); // CGRectInset(frame, 2, 2);
    CGFloat buttonWidth, buttonHeight;
    CGFloat marginX = 4.0;
    CGFloat marginY = 3.0;
    CGSize maxButtonSize = CGSizeMake(keyFrame.size.width*0.9, keyFrame.size.height*0.9);
    CGSize minButtonSize;
    self.isPad = ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPad)?YES:NO;
    if (self.isPad) {
        if (isPortrait)
            minButtonSize = CGSizeMake(keyFrame.size.width*0.65, keyFrame.size.height*0.65);
        else
            minButtonSize = CGSizeMake(keyFrame.size.width*0.55, keyFrame.size.height*0.55);
    }
    else {
        if (isPortrait)
            minButtonSize = CGSizeMake(keyFrame.size.width*0.75, keyFrame.size.height*0.75);
        else
            minButtonSize = CGSizeMake(keyFrame.size.width*0.675, keyFrame.size.height*0.675);
    }
    
    CGFloat kbHeight = [KMManager inputView].frame.size.height;
    NSInteger tbHeight = [[KMManager sharedInstance] isSystemKeyboardTopBarEnabled]?[KMInputViewController topBarHeight]:0;
    CGFloat maxContainerHeight = (screenHeight - kbHeight) + keyFrame.origin.y - strokeWidth;
    if (isSystemKeyboard)
        maxContainerHeight = keyFrame.origin.y + tbHeight - strokeWidth;
    
    CGFloat maxWidth = screenWidth;
    NSUInteger subKeysCount = subKeys.count;
    NSUInteger columns = (maxWidth - marginX)/(maxButtonSize.width + marginX);
    rows = subKeysCount/columns;
    if (subKeysCount%columns)
        rows++;
    
    if (subKeysCount%rows == 0)
        columns = subKeysCount/rows;
    else {
        NSUInteger s = (columns*rows - subKeysCount)/2;
        columns -= s/(rows-1);
    }
    
    buttonWidth = maxButtonSize.width;
    buttonHeight = (maxContainerHeight - (rows+1)*marginY)/rows;
    if (buttonHeight > maxButtonSize.height)
        buttonHeight = maxButtonSize.height;
    else if (buttonHeight < minButtonSize.height)
        buttonHeight = minButtonSize.height;
    
    //CGFloat containerWidth =  ((columns<2?1.25:columns) * (buttonWidth + marginX)) + marginX;
    //CGFloat cx = self.isPad?1.0:1.5;
    CGFloat cx = 1.5;
    CGFloat containerWidth =  ((columns<2?cx:columns) * (buttonWidth + marginX)) + marginX;
    CGFloat containerHeight = (rows * (buttonHeight + marginY)) + marginY;
    CGRect containerFrame = CGRectMake(strokeWidth, strokeWidth, containerWidth, containerHeight);
    
    CGFloat viewWidth = containerWidth + 2*strokeWidth;
    CGFloat baseHeight = keyFrame.size.height + marginY;
    CGFloat viewHeight = baseHeight + containerHeight - marginY + strokeWidth;
    CGFloat viewPosX = keyFrame.origin.x - (viewWidth - keyFrame.size.width)/2.0;
    CGFloat viewPosY = keyFrame.origin.y - (viewHeight - keyFrame.size.height);

    adjX = 0;
    adjY = 0;
    if (isSystemKeyboard && (viewPosY < -tbHeight)) {
        adjY = viewPosY + tbHeight;
        viewPosY = -tbHeight;
        viewHeight = viewHeight + adjY;
    }
    
    if (viewPosX < 0) {
        if ((keyFrame.origin.x - borderRadius*1.25 /*2.0*/) < 0) {
            adjX = keyFrame.origin.x - viewPosX;
            viewPosX = keyFrame.origin.x;
        }
        else {
            adjX = -viewPosX;
            viewPosX = 0;
        }
    }
    else if ((viewPosX + viewWidth) > screenWidth) {
        if (((keyFrame.origin.x + keyFrame.size.width) + borderRadius*1.25 /*2.0*/) > screenWidth) {
            adjX = viewPosX - keyFrame.origin.x;
            viewPosX = keyFrame.origin.x + 2*adjX;
        }
        else {
            adjX = (screenWidth - viewWidth) - viewPosX;
            viewPosX = adjX + viewPosX;
        }
    }
    
    /*
    if (isSystemKeyboard && (viewPosY < -tbHeight)) {
        CGFloat pX = keyFrame.origin.x + keyFrame.size.width + xLength;
        CGFloat widthLeft = screenWidth - (screenWidth - keyFrame.origin.x + xLength);
        CGFloat widthRight = screenWidth - pX;
        if (widthRight > widthLeft) {
            direction = 1;
            maxWidth = widthRight;
        }
        else {
            direction = -1;
            maxWidth = widthLeft;
        }
        
        viewWidth = keyFrame.size.width + xLength + containerWidth;
        viewHeight = containerHeight;
        if (direction == 1)
            viewPosX = keyFrame.origin.x;
        else
            viewPosX = (keyFrame.origin.x + keyFrame.size.width) - viewWidth;
        
        viewPosY = (keyFrame.origin.y + keyFrame.size.height) - viewHeight;
        if (viewPosY < -tbHeight)
            viewPosY = -tbHeight + 2;
        else
            adjY = viewPosY;
        
        CGFloat cX = (direction == 1)?(viewWidth - containerWidth):0;
        CGFloat cY = viewPosY - adjY;
        containerFrame = CGRectMake(cX, cY, containerWidth, containerHeight);
    }
    else {
        direction = 0;
        if (viewPosX < 0) {
            if ((keyFrame.origin.x - borderRadius*2) < 0) {
                adjX = keyFrame.origin.x - viewPosX;
                viewPosX = keyFrame.origin.x;
            }
            else {
                adjX = -viewPosX;
                viewPosX = 0;
            }
        }
        else if ((viewPosX + viewWidth) > screenWidth) {
            if (((keyFrame.origin.x + keyFrame.size.width) + borderRadius*2) > screenWidth) {
                adjX = viewPosX - keyFrame.origin.x;
                viewPosX = keyFrame.origin.x + 2*adjX;
            }
            else {
                adjX = (screenWidth - viewWidth) - viewPosX;
                viewPosX = adjX + viewPosX;
            }
        }
    }*/
    
    self = [super initWithFrame:CGRectMake(viewPosX, viewPosY, viewWidth, viewHeight)];
    if (self) {
        super.backgroundColor = [UIColor clearColor];
        self.userInteractionEnabled = NO;
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
        
        containerView = [[UIView alloc] initWithFrame:containerFrame];
        [containerView setBackgroundColor:[UIColor clearColor]];
        [self addSubview:containerView];
        CGFloat fontSize;
        if (self.isPad)
            fontSize = buttonHeight*0.4;
        else
            fontSize = buttonHeight*0.5;
        
        for (UIButton *button in subKeys) {
            NSInteger i = button.tag;
            NSInteger buttonRow = i/columns;
            CGRect buttonFrame = CGRectMake(marginX+((i%columns)*(buttonWidth+marginX)), (buttonRow*buttonHeight)+(buttonRow+1)*marginY, buttonWidth, buttonHeight);
            [button setFrame:buttonFrame];
            if (subKeysCount == 1) {
                [button setCenter:containerView.center];
                buttonFrame.origin.x = button.frame.origin.x;
                [button setFrame:buttonFrame];
            }
            
            [button.titleLabel setMinimumScaleFactor:0.25];
            [button.titleLabel setAdjustsFontSizeToFitWidth:YES];
            [button.titleLabel setFont:[button.titleLabel.font fontWithSize:fontSize]];

            [containerView addSubview:button];
        }
    }
    
    return self;
}

- (void)drawRect:(CGRect)rect {
    //if (self.isPad)
        //[self drawKeyPreviewUpForPad:rect];
    //else
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
    CGFloat keyHeight = keyFrame.size.height + adjY;
    CGFloat viewWidth = rect.size.width;
    CGFloat viewHeight = rect.size.height;
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, strokeWidth*2);
    CGContextSetStrokeColorWithColor(context, borderColor.CGColor);
    CGContextSetFillColorWithColor(context, bgColor.CGColor);
    //CGContextSetStrokeColorWithColor(context, [UIColor colorWithRed:1.0 green:0.0 blue:0.0 alpha:0.5].CGColor);
    CGFloat keyLeft = (viewWidth - keyWidth)/2.0 + strokeWidth - adjX;
    CGFloat keyRight = (viewWidth - keyWidth)/2.0 + keyWidth - strokeWidth - adjX;
    keyLeft = ceilf(keyLeft*10)/10;
    keyRight = floorf(keyRight*10)/10;
    
    CGFloat midY = viewHeight - keyHeight;
    CGFloat viewLeft = strokeWidth;
    CGFloat viewRight = viewWidth - strokeWidth;
    CGFloat viewTop = strokeWidth;
    CGFloat viewBottom = viewHeight - strokeWidth*0.75 - ((rows - 1)*0.25) - (scale==3.0?((3 - rows)*0.4):0) - (scale==2.0?((3 - rows)*0.1):0);
    CGFloat viewMid = viewWidth/2.0;
    CGFloat r0 = borderRadius - strokeWidth;
    CGFloat r1 = borderRadius - strokeWidth;
    CGFloat r2 = borderRadius*1.25 /*2.0*/ - strokeWidth;
    
    if (roundf(viewLeft) == roundf(keyLeft) || floorf(viewLeft) == floorf(keyLeft))
        viewLeft = keyLeft = viewLeft<keyLeft?viewLeft:keyLeft;

    if (roundf(viewRight) == roundf(keyRight) || floorf(viewRight) == floorf(keyRight))
        viewRight = keyRight = viewRight>keyRight?viewRight:keyRight;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, midY, r1);
    CGContextAddArcToPoint(context, viewLeft, midY, keyLeft, midY, r1);
    CGContextAddArcToPoint(context, keyLeft, midY, keyLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, keyLeft, viewBottom, keyRight, viewBottom, r0);
    CGContextAddArcToPoint(context, keyRight, viewBottom, keyRight, midY, r0);
    CGContextAddArcToPoint(context, keyRight, midY, viewRight, midY, r2);
    CGContextAddArcToPoint(context, viewRight, midY, viewRight, viewTop, r1);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewMid, viewTop, r1);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, midY, r1);
    CGContextAddArcToPoint(context, viewLeft, midY, keyLeft, midY, r1);
    CGContextAddArcToPoint(context, keyLeft, midY, keyLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, keyLeft, viewBottom, keyRight, viewBottom, r0);
    CGContextAddArcToPoint(context, keyRight, viewBottom, keyRight, midY, r0);
    CGContextAddArcToPoint(context, keyRight, midY, viewRight, midY, r2);
    CGContextAddArcToPoint(context, viewRight, midY, viewRight, viewTop, r1);
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

- (void)drawKeyPreviewUpForPad:(CGRect)rect {
    CGFloat keyWidth = keyFrame.size.width;
    CGFloat keyHeight = keyFrame.size.height + adjY;
    CGFloat viewWidth = rect.size.width;
    CGFloat viewHeight = rect.size.height;
    
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, strokeWidth*2);
    CGContextSetStrokeColorWithColor(context, borderColor.CGColor);
    CGContextSetFillColorWithColor(context, bgColor.CGColor);
    //CGContextSetStrokeColorWithColor(context, [UIColor colorWithRed:1.0 green:0.0 blue:0.0 alpha:0.5].CGColor);
    CGFloat keyLeft = (viewWidth - keyWidth)/2.0 + strokeWidth - adjX;
    CGFloat keyRight = (viewWidth - keyWidth)/2.0 + keyWidth - strokeWidth - adjX;
    keyLeft = ceilf(keyLeft*10)/10;
    keyRight = floorf(keyRight*10)/10;
    
    CGFloat viewLeft = strokeWidth;
    CGFloat viewRight = viewWidth - strokeWidth;
    CGFloat viewTop = strokeWidth;
    CGFloat viewBottom = viewHeight - keyHeight - strokeWidth;
    CGFloat viewMid = viewWidth/2.0;
    CGFloat r = borderRadius - strokeWidth;
    
    if (roundf(viewLeft) == roundf(keyLeft) || floorf(viewLeft) == floorf(keyLeft))
        viewLeft = keyLeft = viewLeft<keyLeft?viewLeft:keyLeft;
    
    if (roundf(viewRight) == roundf(keyRight) || floorf(viewRight) == floorf(keyRight))
        viewRight = keyRight = viewRight>keyRight?viewRight:keyRight;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, viewBottom, r);
    CGContextAddArcToPoint(context, viewLeft, viewBottom, viewRight, viewBottom, r);
    CGContextAddArcToPoint(context, viewRight, viewBottom, viewRight, viewTop, r);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewMid, viewTop, r);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, viewMid, viewTop);
    CGContextAddArcToPoint(context, viewLeft, viewTop, viewLeft, viewBottom, r);
    CGContextAddArcToPoint(context, viewLeft, viewBottom, viewRight, viewBottom, r);
    CGContextAddArcToPoint(context, viewRight, viewBottom, viewRight, viewTop, r);
    CGContextAddArcToPoint(context, viewRight, viewTop, viewMid, viewTop, r);
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
    
    CGFloat keyTop = keyFrame.origin.y + strokeWidth - adjY;
    CGFloat keyBottom = keyTop + keyHeight - strokeWidth*2;
    CGFloat midX = keyWidth + xLength;
    CGFloat midY1 = keyTop + keyHeight*0.3;
    CGFloat midY2 = keyTop + keyHeight*0.7 - strokeWidth;
    CGFloat viewLeft = strokeWidth;
    CGFloat viewRight = viewWidth - strokeWidth;
    CGFloat viewTop = strokeWidth;
    CGFloat viewBottom = viewHeight - strokeWidth;
    CGFloat keyMid = keyWidth/2.0;
    CGFloat keyRight = keyWidth - strokeWidth;
    CGFloat r = borderRadius - strokeWidth;
    CGFloat r2 = borderRadius*1.0 - strokeWidth;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, keyTop);
    CGContextAddArcToPoint(context, viewLeft, keyTop, viewLeft, keyBottom, r);
    CGContextAddArcToPoint(context, viewLeft, keyBottom, keyRight, keyBottom, r);
    CGContextAddArcToPoint(context, keyRight, keyBottom, keyRight, midY2, r);
    CGContextAddArcToPoint(context, keyRight, midY2, midX, midY2, r);
    CGContextAddArcToPoint(context, midX, midY2, midX, viewBottom, r2);
    CGContextAddArcToPoint(context, midX, viewBottom, viewRight, viewBottom, r2);
    CGContextAddArcToPoint(context, viewRight, viewBottom, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, midX, viewTop, r2);
    CGContextAddArcToPoint(context, midX, viewTop, midX, midY1, r2);
    CGContextAddArcToPoint(context, midX, midY1, keyRight, midY1, r2);
    CGContextAddArcToPoint(context, keyRight, midY1, keyRight, keyTop, r);
    CGContextAddArcToPoint(context, keyRight, keyTop, keyMid, keyTop, r);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, keyTop);
    CGContextAddArcToPoint(context, viewLeft, keyTop, viewLeft, keyBottom, r);
    CGContextAddArcToPoint(context, viewLeft, keyBottom, keyRight, keyBottom, r);
    CGContextAddArcToPoint(context, keyRight, keyBottom, keyRight, midY2, r);
    CGContextAddArcToPoint(context, keyRight, midY2, midX, midY2, r);
    CGContextAddArcToPoint(context, midX, midY2, midX, viewBottom, r2);
    CGContextAddArcToPoint(context, midX, viewBottom, viewRight, viewBottom, r2);
    CGContextAddArcToPoint(context, viewRight, viewBottom, viewRight, viewTop, r2);
    CGContextAddArcToPoint(context, viewRight, viewTop, midX, viewTop, r2);
    CGContextAddArcToPoint(context, midX, viewTop, midX, midY1, r2);
    CGContextAddArcToPoint(context, midX, midY1, keyRight, midY1, r2);
    CGContextAddArcToPoint(context, keyRight, midY1, keyRight, keyTop, r);
    CGContextAddArcToPoint(context, keyRight, keyTop, keyMid, keyTop, r);
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
    
    CGFloat keyTop = keyFrame.origin.y + strokeWidth - adjY;
    CGFloat keyBottom = keyTop + keyHeight - strokeWidth*2;
    CGFloat midX = viewWidth - (keyWidth + xLength);
    CGFloat midY1 = keyTop + keyHeight*0.3;
    CGFloat midY2 = keyTop + keyHeight*0.7 - strokeWidth;
    CGFloat viewLeft = strokeWidth;
    CGFloat viewRight = viewWidth - strokeWidth;
    CGFloat viewTop = strokeWidth;
    CGFloat viewBottom = viewHeight - strokeWidth;
    CGFloat keyMid = viewWidth - keyWidth/2.0;
    CGFloat keyLeft = viewWidth - keyWidth + strokeWidth;
    CGFloat r = borderRadius - strokeWidth;
    CGFloat r2 = borderRadius*1.0 - strokeWidth;
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, keyTop);
    CGContextAddArcToPoint(context, viewRight, keyTop, viewRight, keyBottom, r);
    CGContextAddArcToPoint(context, viewRight, keyBottom, keyLeft, keyBottom, r);
    CGContextAddArcToPoint(context, keyLeft, keyBottom, keyLeft, midY2, r);
    CGContextAddArcToPoint(context, keyLeft, midY2, midX, midY2, r);
    CGContextAddArcToPoint(context, midX, midY2, midX, viewBottom, r2);
    CGContextAddArcToPoint(context, midX, viewBottom, viewLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, viewLeft, viewBottom, viewLeft, viewTop, r2);
    CGContextAddArcToPoint(context, viewLeft, viewTop, midX, viewTop, r2);
    CGContextAddArcToPoint(context, midX, viewTop, midX, midY1, r2);
    CGContextAddArcToPoint(context, midX, midY1, keyLeft, midY1, r2);
    CGContextAddArcToPoint(context, keyLeft, midY1, keyLeft, keyTop, r);
    CGContextAddArcToPoint(context, keyLeft, keyTop, keyMid, keyTop, r);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, keyMid, keyTop);
    CGContextAddArcToPoint(context, viewRight, keyTop, viewRight, keyBottom, r);
    CGContextAddArcToPoint(context, viewRight, keyBottom, keyLeft, keyBottom, r);
    CGContextAddArcToPoint(context, keyLeft, keyBottom, keyLeft, midY2, r);
    CGContextAddArcToPoint(context, keyLeft, midY2, midX, midY2, r);
    CGContextAddArcToPoint(context, midX, midY2, midX, viewBottom, r2);
    CGContextAddArcToPoint(context, midX, viewBottom, viewLeft, viewBottom, r2);
    CGContextAddArcToPoint(context, viewLeft, viewBottom, viewLeft, viewTop, r2);
    CGContextAddArcToPoint(context, viewLeft, viewTop, midX, viewTop, r2);
    CGContextAddArcToPoint(context, midX, viewTop, midX, midY1, r2);
    CGContextAddArcToPoint(context, midX, midY1, keyLeft, midY1, r2);
    CGContextAddArcToPoint(context, keyLeft, midY1, keyLeft, keyTop, r);
    CGContextAddArcToPoint(context, keyLeft, keyTop, keyMid, keyTop, r);
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
}*/

@end
