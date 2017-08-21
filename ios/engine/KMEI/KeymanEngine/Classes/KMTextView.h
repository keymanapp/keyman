//
//  KMTextView.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//
//  KMTextView requires iOS 6.0 or greater

#import <UIKit/UIKit.h>
#import "KMTextViewDelegateProxy.h"

@interface KMTextView : UITextView <UITextViewDelegate, UIInputViewAudioFeedback> {
	KMTextViewDelegateProxy* delegateProxy_;
    UIViewController *viewController; // viewController should be set to main view controller to enable keyboard picker.
    BOOL shouldSetCustomFontOnKeyboardChange_; // if YES, sets Keyman custom font (if any) to KMTextView instance on keyboard change event, defaults to YES.
    BOOL isInputClickSoundEnabled_; // if YES, plays keyboard click sound whenever input received, defaults to YES.
}

@property (nonatomic, strong) UIViewController *viewController;
@property (nonatomic, assign) BOOL shouldSetCustomFontOnKeyboardChange;
@property (nonatomic, assign) BOOL isInputClickSoundEnabled;

// Use this KMTextViewDelegate instead of the normal UITextViewDelegate.
//   - All of the normal UITextViewDelegate methods are supported.
- (void)setKeymanDelegate:(id<KMTextViewDelegate>)keymanDelegate;

// Dismisses the keyboard if this textview is the first responder.
//   - Use this instead of [resignFirstResponder] as it also resigns the Keyman keyboard's responders.
- (void)dismissKeyboard;

@end
