//
//  UIButton+Helpers.m
//  KeymanEngine
//
//  Created by Serkan Kurt on 5/03/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "UIButton+Helpers.h"
#import <QuartzCore/QuartzCore.h>

@implementation UIButton (Helpers)

- (void)setRoundedBorderWithRadius:(float)radius borderWidth:(float)borderWidth color:(UIColor*)color {
    CALayer *layer = [self layer];
    [layer setCornerRadius:radius];
    [layer setBorderWidth:borderWidth];
    [layer setBorderColor:[color CGColor]];
    [layer setMasksToBounds:YES];
}

@end
