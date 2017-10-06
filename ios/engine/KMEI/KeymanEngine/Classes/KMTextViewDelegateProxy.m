//
//  KMTextViewDelegateProxy.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 25/03/12.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMTextViewDelegateProxy.h"
#import "KeymanEngine/KeymanEngine-Swift.h"

@implementation KMTextViewDelegateProxy

@synthesize keymanDelegate = keymanDelegate_;

#pragma mark - Object Admin

- (void)dealloc {
	self.keymanDelegate = nil;
}

#pragma mark - UITextViewDelegate

// NOTE: the return value from the textView's hook is ignored
- (BOOL)textViewShouldBeginEditing:(KeymanTextView *)textView {
	if ([textView respondsToSelector:@selector(textViewShouldBeginEditing:)]) {
		[textView textViewShouldBeginEditing:textView];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textViewShouldBeginEditing:)]) {
		return [self.keymanDelegate textViewShouldBeginEditing:textView];
	}
	return YES;
}

// NOTE: the return value from the textView's hook is ignored
- (BOOL)textViewShouldEndEditing:(KeymanTextView *)textView {
	if ([textView respondsToSelector:@selector(textViewShouldEndEditing:)]) {
		[textView textViewShouldEndEditing:textView];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textViewShouldEndEditing:)]) {
		return [self.keymanDelegate textViewShouldEndEditing:textView];
	}
	
	return YES;
}

- (void)textViewDidBeginEditing:(KeymanTextView *)textView {
	if ([textView respondsToSelector:@selector(textViewDidBeginEditing:)]) {
		[textView textViewDidBeginEditing:textView];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textViewDidBeginEditing:)]) {
		[self.keymanDelegate textViewDidBeginEditing:textView];
	}
}

- (void)textViewDidEndEditing:(KeymanTextView *)textView {
	if ([textView respondsToSelector:@selector(textViewDidEndEditing:)]) {
		[textView textViewDidEndEditing:textView];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textViewDidEndEditing:)]) {
		[self.keymanDelegate textViewDidEndEditing:textView];
	}
}

// NOTE: the return value from the textView's hook is ignored
- (BOOL)textView:(KeymanTextView *)textView shouldChangeTextInRange:(NSRange)range replacementText:(NSString *)text {
	if ([textView respondsToSelector:@selector(textView:shouldChangeTextInRange:replacementText:)]) {
		[textView textView:textView shouldChangeTextInRange:range replacementText:text];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textView:shouldChangeTextInRange:replacementText:)]) {
		return [self.keymanDelegate textView:textView shouldChangeTextInRange:range replacementText:text];
	}
	
	return YES;
}

- (void)textViewDidChange:(KeymanTextView *)textView {
	if ([textView respondsToSelector:@selector(textViewDidChange:)]) {
		[textView textViewDidChange:textView];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textViewDidChange:)]) {
		[self.keymanDelegate textViewDidChange:textView];
	}
}

- (void)textViewDidChangeSelection:(KeymanTextView *)textView {
	if ([textView respondsToSelector:@selector(textViewDidChangeSelection:)]) {
		[textView textViewDidChangeSelection:textView];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textViewDidChangeSelection:)]) {
		[self.keymanDelegate textViewDidChangeSelection:textView];
	}
}

@end
