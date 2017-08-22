//
//  KMPopoverView.h
//  Keyman Engine
//
//  Created by Serkan Kurt on 27/06/13.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface KMPopoverView : UIView {
    CGFloat arrowPosX;
}

@property CGFloat strokeWidth;
@property CGFloat arrowWidth;
@property CGFloat arrowHeight;
@property CGFloat borderRadius;
@property (nonatomic, strong) UIColor *borderColor;

- (void)setArrowPosX:(CGFloat)posX;
- (void)setBackgroundColor2:(UIColor *)backgroundColor2;

@end
