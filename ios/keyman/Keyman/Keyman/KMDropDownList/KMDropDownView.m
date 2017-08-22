//
//  KMDropDownView.m
//  Keyman
//
//  Created by Serkan Kurt on 12/12/14.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMDropDownView.h"

@interface KMDropDownView ()

@property (nonatomic, strong) UIColor *bgColor;
@property (nonatomic, strong) UIColor *bgColor2;

@end

@implementation KMDropDownView
@synthesize strokeWidth, borderRadius, arrowWidth, arrowHeight, bgColor, bgColor2, borderColor;

- (id)initWithFrame:(CGRect)frame {
    self = [super initWithFrame:frame];
    if (self) {
        strokeWidth = 2.0;        
        borderRadius = 5.0;
        arrowWidth = 21.0;
        arrowHeight = 7.0;
        arrowPosX = self.bounds.size.width/2.0;
        super.backgroundColor = [UIColor clearColor];
        bgColor = [[UIColor alloc] initWithWhite:1.0 alpha:1.0];
        bgColor2 = [[UIColor alloc] initWithWhite:1.0 alpha:1.0];
        borderColor = [UIColor lightGrayColor];
    }
    
    return self;
}

- (void)drawRect:(CGRect)rect {
    CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
    CGRect currentFrame = self.bounds;

    CGContextSetLineJoin(context, kCGLineJoinRound);
    CGContextSetLineWidth(context, strokeWidth);
    CGContextSetStrokeColorWithColor(context, borderColor.CGColor);
    CGContextSetFillColorWithColor(context, bgColor.CGColor);
    
    CGContextBeginPath(context);
    CGFloat arrowPosY = arrowHeight + strokeWidth;
    CGContextMoveToPoint(context, borderRadius + strokeWidth, arrowPosY);
    CGContextAddLineToPoint(context, arrowPosX - arrowWidth/2.0, arrowPosY);
    CGContextAddLineToPoint(context, arrowPosX, strokeWidth);
    CGContextAddLineToPoint(context, arrowPosX + arrowWidth/2.0, arrowPosY);
    CGContextAddArcToPoint(context, currentFrame.size.width - strokeWidth, arrowPosY, currentFrame.size.width - strokeWidth, currentFrame.size.height - strokeWidth, borderRadius - strokeWidth);
    CGContextAddArcToPoint(context, currentFrame.size.width - strokeWidth, currentFrame.size.height - strokeWidth, round(currentFrame.size.width/2.0 + arrowWidth/2.0) - strokeWidth, currentFrame.size.height - strokeWidth, borderRadius - strokeWidth);
    CGContextAddArcToPoint(context, strokeWidth, currentFrame.size.height - strokeWidth, strokeWidth, arrowPosY, borderRadius - strokeWidth);
    CGContextAddArcToPoint(context, strokeWidth, arrowPosY, currentFrame.size.width - strokeWidth, arrowPosY, borderRadius - strokeWidth);
    CGContextClosePath(context);
    CGContextDrawPath(context, kCGPathFillStroke);
    
    CGContextBeginPath(context);
    CGContextMoveToPoint(context, borderRadius + strokeWidth, arrowPosY);
    CGContextAddLineToPoint(context, arrowPosX - arrowWidth/2.0, arrowPosY);
    CGContextAddLineToPoint(context, arrowPosX, strokeWidth);
    CGContextAddLineToPoint(context, arrowPosX + arrowWidth/2.0, arrowPosY);
    CGContextAddArcToPoint(context, currentFrame.size.width - strokeWidth, arrowPosY, currentFrame.size.width - strokeWidth, currentFrame.size.height - strokeWidth, borderRadius - strokeWidth);
    CGContextAddArcToPoint(context, currentFrame.size.width - strokeWidth, currentFrame.size.height - strokeWidth, round(currentFrame.size.width/2.0 + arrowWidth/2.0) - strokeWidth, currentFrame.size.height - strokeWidth, borderRadius - strokeWidth);
    CGContextAddArcToPoint(context, strokeWidth, currentFrame.size.height - strokeWidth, strokeWidth, arrowPosY, borderRadius - strokeWidth);
    CGContextAddArcToPoint(context, strokeWidth, arrowPosY, currentFrame.size.width - strokeWidth, arrowPosY, borderRadius - strokeWidth);
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

- (void)setBackgroundColor:(UIColor *)backgroundColor {
    super.backgroundColor = [UIColor clearColor];
    self.bgColor = [UIColor colorWithCGColor:backgroundColor.CGColor];
}

- (void)setBackgroundColor2:(UIColor *)backgroundColor2 {
    self.bgColor2 = [UIColor colorWithCGColor:backgroundColor2.CGColor];
}

- (void)setArrowPosX:(CGFloat)posX {
    CGRect currentFrame = self.bounds;
    if (posX < (arrowWidth/2.0 + borderRadius + strokeWidth)) {
        arrowPosX = arrowWidth/2.0 + borderRadius + strokeWidth;
    }
    else if (posX > (currentFrame.size.width - arrowWidth/2.0 - borderRadius - strokeWidth)) {
        arrowPosX = currentFrame.size.width - arrowWidth/2.0 - borderRadius - strokeWidth;
    }
    else {
        arrowPosX = posX;
    }
    [self setNeedsDisplay];
}

@end
