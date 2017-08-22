//
//  KMTextViewDelegateProxy.h
//  Keyman Engine
//
//  Copyright (c) 2017 SIL International. All rights reserved.
//
//
//  Proxies delegate messages for a KMTextView.
//		This allows the KMTextView to hook into these calls while allowing a developer to still
//		use the delegate as normal (albeit with a different name: 'keymanDelegate')
//
//	This class is required because at the time of writing, setting a UITextView as it's
//	own delegate caused an infinite loop in Apple's code, repeatedly calling the selectionDidChange callback
//		See: http://lists.apple.com/archives/cocoa-dev/2010/Feb/msg01391.html

#import <Foundation/Foundation.h>

@protocol KMTextViewDelegate;

@interface KMTextViewDelegateProxy : NSObject <UITextViewDelegate> {
    id<KMTextViewDelegate> __weak keymanDelegate_;
}

@property (nonatomic, weak) id<KMTextViewDelegate> keymanDelegate;

@end

@protocol KMTextViewDelegate <UITextViewDelegate>
	// Setting this protocol space aside in case we later want to add additional delegate methods for KMTextView
@end
