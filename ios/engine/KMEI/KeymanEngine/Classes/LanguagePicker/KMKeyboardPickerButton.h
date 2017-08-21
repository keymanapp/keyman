//
//  KMKeyboardPickerButton.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface KMKeyboardPickerButton : UIButton {
	UIViewController* __weak presentingVC_;
}

// Returns a UIButton that will display the keyboard picker when tapped
//   - since the keyboard picker is modal, a UIViewController must be supplied to display it
//   - the button has a default image and background color
//      - these can be overridden with other images or a title
+ (KMKeyboardPickerButton *)buttonWithPresentingViewController:(UIViewController *)presentingVC;

// Sets the background color of the button
- (void)setColor:(UIColor *)color;

@end
