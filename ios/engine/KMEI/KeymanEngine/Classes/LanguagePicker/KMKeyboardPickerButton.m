//
//  KMKeyboardPickerButton.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-07-03.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMKeyboardPickerButton.h"
#import "KMManager+Internal.h"

#import "QuartzCore/QuartzCore.h"

@interface KMKeyboardPickerButton ()
@property (nonatomic, weak) UIViewController* presentingVC;
@end

@implementation KMKeyboardPickerButton

@synthesize presentingVC = presentingVC_;

- (void)dealloc {
	self.presentingVC = nil;
}

+ (KMKeyboardPickerButton *)buttonWithPresentingViewController:(UIViewController *)presentingVC {
	
	KMKeyboardPickerButton* retVal = [self buttonWithType:UIButtonTypeCustom];
	retVal.presentingVC = presentingVC;
	retVal.layer.cornerRadius = 8.0f;
	retVal.clipsToBounds = YES;
	
	[retVal setColor:[UIColor colorWithRed:0.62f green:0.68f blue:0.76f alpha:1.0f]];
	[retVal addTarget:retVal action:@selector(showKeyboardPicker) forControlEvents:UIControlEventTouchUpInside];
	
	NSBundle* keymanBundle = [NSBundle bundleWithPath:[[NSBundle mainBundle] pathForResource:@"Keyman" ofType:@"bundle"]];
	NSString* retinaSuffix = [KMManager retinaScreen] ? @"@2x" : @"";
	NSString* imagePath = [keymanBundle pathForResource:[NSString stringWithFormat:@"keyboard_icon%@", retinaSuffix] ofType:@"png"];
	UIImage* img = [UIImage imageWithContentsOfFile:imagePath];
	[retVal setImage:img forState:UIControlStateNormal];
	[retVal sizeToFit];
	retVal.frame = CGRectInset(retVal.frame, -15.0f, -3.0f);

	return retVal;
}

- (void)showKeyboardPicker {
	if (self.presentingVC) {
		[[KMManager sharedInstance] showKeyboardPickerInViewController:self.presentingVC shouldAddKeyboard:NO];
	}
}

// Clear images if the developer sets a title
- (void)setTitle:(NSString *)title forState:(UIControlState)state {
	[self setImage:nil forState:state];
	
	[super setTitle:title forState:state];
}

- (void)setColor:(UIColor *)color {
	CGRect rect = CGRectMake(0, 0, 1, 1);
	UIGraphicsBeginImageContext(rect.size);
	CGContextRef context = UIGraphicsGetCurrentContext();
    UIGraphicsPushContext(context);
	CGContextSetFillColorWithColor(context, [color CGColor]);
	CGContextFillRect(context, rect);
	UIImage *bgImg = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsPopContext();
	UIGraphicsEndImageContext();
	[self setBackgroundImage:bgImg forState:UIControlStateNormal];
}

@end
