//
//  KMTextField.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//
//  KMTextField requires iOS 6.0 or greater due to use of the UITextInput protocol

#import <UIKit/UIKit.h>
#import "KMTextFieldDelegateProxy.h"

@interface KMTextField : UITextField <UITextFieldDelegate> {
	KMTextFieldDelegateProxy *delegateProxy_;
	NSTimer *selectionPollingTimer_;
	NSInteger lastSelectionPosition_;
	NSInteger lastSelectionLength_;
    UIViewController *viewController; // viewController should be set to main view controller to enable keyboard picker.
    BOOL shouldSetCustomFontOnKeyboardChange_; // if YES, sets Keyman custom font (if any) to KMTextField instance on keyboard change event, defaults to YES.
    BOOL isInputClickSoundEnabled_; // if YES, plays keyboard click sound whenever input received, defaults to YES.
}

@property (nonatomic, strong) UIViewController *viewController;
@property (nonatomic, assign) BOOL shouldSetCustomFontOnKeyboardChange;
@property (nonatomic, assign) BOOL isInputClickSoundEnabled;

// Use this KMTextFieldDelegate instead of the normal UITextFieldDelegate.
//   - All of the normal UITextFieldDelegate methods are supported.
- (void)setKeymanDelegate:(id<KMTextFieldDelegate>)keymanDelegate;

// Dismisses the keyboard if this textview is the first responder.
//   - Use this instead of [resignFirstResponder] as it also resigns the Keyman keyboard's responders.
- (void)dismissKeyboard;

@end
