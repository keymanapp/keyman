//
//  KMTextFieldDelegateProxy.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-05-16.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMTextFieldDelegateProxy.h"
#import "KMTextField.h"

@implementation KMTextFieldDelegateProxy

@synthesize keymanDelegate = keymanDelegate_;

#pragma mark - Object Admin

- (void)dealloc {
	self.keymanDelegate = nil;
}

#pragma mark - UITextFieldDelegate

// NOTE: the return value from the textField's hook is ignored
- (BOOL)textFieldShouldBeginEditing:(KMTextField *)textField {
	if ([textField respondsToSelector:@selector(textFieldShouldBeginEditing:)]) {
		[textField textFieldShouldBeginEditing:textField];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textFieldShouldBeginEditing:)]) {
		return [self.keymanDelegate textFieldShouldBeginEditing:textField];
	}
	return YES;
}

- (void)textFieldDidBeginEditing:(KMTextField *)textField {
	if ([textField respondsToSelector:@selector(textFieldDidBeginEditing:)]) {
		[textField textFieldDidBeginEditing:textField];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textFieldDidBeginEditing:)]) {
		[self.keymanDelegate textFieldDidBeginEditing:textField];
	}
}

// NOTE: the return value from the textField's hook is ignored
- (BOOL)textFieldShouldEndEditing:(KMTextField *)textField {
	if ([textField respondsToSelector:@selector(textFieldShouldEndEditing:)]) {
		[textField textFieldShouldEndEditing:textField];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textFieldShouldEndEditing:)]) {
		return [self.keymanDelegate textFieldShouldEndEditing:textField];
	}
	
	return YES;
}

- (void)textFieldDidEndEditing:(KMTextField *)textField {
	if ([textField respondsToSelector:@selector(textFieldDidEndEditing:)]) {
		[textField textFieldDidEndEditing:textField];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textFieldDidEndEditing:)]) {
		[self.keymanDelegate textFieldDidEndEditing:textField];
	}
}

// NOTE: the return value from the textField's hook is ignored
- (BOOL)textField:(KMTextField *)textField shouldChangeCharactersInRange:(NSRange)range replacementString:(NSString *)string {
	if ([textField respondsToSelector:@selector(textField:shouldChangeCharactersInRange:replacementString:)]) {
		[textField textField:textField shouldChangeCharactersInRange:range replacementString:string];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textField:shouldChangeCharactersInRange:replacementString:)]) {
		return [self.keymanDelegate textField:textField shouldChangeCharactersInRange:range replacementString:string];
	}
	
	return YES;
}

// NOTE: the return value from the textField's hook is ignored
- (BOOL)textFieldShouldClear:(KMTextField *)textField {
	if ([textField respondsToSelector:@selector(textFieldShouldClear:)]) {
		[textField textFieldShouldClear:textField];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textFieldShouldClear:)]) {
		return [self.keymanDelegate textFieldShouldClear:textField];
	}
	
	return YES;
}

// NOTE: the return value from the textField's hook is ignored
- (BOOL)textFieldShouldReturn:(KMTextField *)textField {
	if ([textField respondsToSelector:@selector(textFieldShouldReturn:)]) {
		[textField textFieldShouldReturn:textField];
	}
	if ([self.keymanDelegate respondsToSelector:@selector(textFieldShouldReturn:)]) {
		return [self.keymanDelegate textFieldShouldReturn:textField];
	}
	
	return YES;
}

@end
