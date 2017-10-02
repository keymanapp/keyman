//
//  KMTextFieldDelegateProxy.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//
//
//  Proxies delegate messages for a KMTextField.
//		This allows the KMTextField to hook into these calls while allowing a developer to still
//		use the delegate as normal (albeit with a different name: 'keymanDelegate')
//
//	This class is required because at the time of writing, setting a UITextField as it's
//	own delegate caused an infinite loop in Apple's code, repeatedly calling the selectionDidChange callback
//		See: http://lists.apple.com/archives/cocoa-dev/2010/Feb/msg01391.html

#import <UIKit/UIKit.h>

@protocol KMTextFieldDelegate;

@interface KMTextFieldDelegateProxy : NSObject <UITextFieldDelegate> {
	id<KMTextFieldDelegate> __weak keymanDelegate_;
}

@property (nonatomic, weak) id<KMTextFieldDelegate> keymanDelegate;

@end

@protocol KMTextFieldDelegate <UITextFieldDelegate>
	// Setting this protocol space aside in case we later want to add additional delegate methods for KMTextField
@end
