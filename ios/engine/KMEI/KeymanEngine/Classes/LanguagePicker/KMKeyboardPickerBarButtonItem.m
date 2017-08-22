//
//  KMKeyboardPickerBarButtonItem.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-07-03.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMKeyboardPickerBarButtonItem.h"
#import "KMManager+Internal.h"

@interface KMKeyboardPickerBarButtonItem ()
@property (nonatomic, weak) UIViewController* presentingVC;
@end

@implementation KMKeyboardPickerBarButtonItem

@synthesize presentingVC = presentingVC_;


- (void)dealloc {
	self.presentingVC = nil;
}

+ (KMKeyboardPickerBarButtonItem *)buttonWithPresentingViewController:(UIViewController *)presentingVC {
	NSBundle* keymanBundle = [NSBundle bundleWithPath:[[NSBundle mainBundle] pathForResource:@"Keyman" ofType:@"bundle"]];
	NSString* retinaSuffix = [KMManager retinaScreen] ? @"@2x" : @"";
	
	NSString* imagePath = [keymanBundle pathForResource:[NSString stringWithFormat:@"keyboard_icon%@", retinaSuffix] ofType:@"png"];
	UIImage* img = [UIImage imageWithContentsOfFile:imagePath];
	
	KMKeyboardPickerBarButtonItem *retVal = [[self alloc] initWithImage:img style:UIBarButtonItemStylePlain target:nil action:@selector(showKeyboardPicker)];
	[retVal setTarget:retVal];
	retVal.presentingVC = presentingVC;
	
	if (([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) &&
		[retVal respondsToSelector:@selector(setLandscapeImagePhone:)])
	{
		
		NSString* imagePath = [keymanBundle pathForResource:[NSString stringWithFormat:@"keyboard_icon_landscape%@", retinaSuffix] ofType:@"png"];
		UIImage* img = [UIImage imageWithContentsOfFile:imagePath];
		[retVal setLandscapeImagePhone:img];
	}
	return retVal;
}

- (void)showKeyboardPicker {
	if (self.presentingVC) {
		[[KMManager sharedInstance] showKeyboardPickerInViewController:self.presentingVC shouldAddKeyboard:NO];
	}
}

// Clear images if the developer sets a title
- (void)setTitle:(NSString *)title {
	[self setImage:nil];
	if ([self respondsToSelector:@selector(setLandscapeImagePhone:)]) {
		[self setLandscapeImagePhone:nil];
	}
	[super setTitle:title];
}

@end
