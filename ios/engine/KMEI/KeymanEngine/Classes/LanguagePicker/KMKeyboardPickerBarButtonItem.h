//
//  KMKeyboardPickerBarButtonItem.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface KMKeyboardPickerBarButtonItem : UIBarButtonItem {
	UIViewController* __weak presentingVC_;
}

// Returns a UIBarButtonItem that will display the keyboard picker when tapped
//   - since the keyboard picker is modal, a UIViewController must be supplied to display it
//   - the button has default images for normal and landscape orientations
//      - these can be overridden with other images or a title
+ (KMKeyboardPickerBarButtonItem *)buttonWithPresentingViewController:(UIViewController *)presentingVC;

@end
