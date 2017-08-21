//
//  UIView+Helpers.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-04-28.
//  Copyright (c) 2017 SIL International. All rights reserved.
//


#import "UIView+Helpers.h"
#import "NSString+Helpers.h"

@implementation UIView (Helpers)

- (NSString *)allSubviewsAsString {
	NSMutableString* output = [NSMutableString stringWithCapacity:50];
	[output appendFormat:@"%@", self];
	
	if ([self.subviews count] > 0) {
		[output appendString:@" {\n"];
		for (UIView* sub in self.subviews) {
			[output appendFormat:@"  %@\n", [[sub allSubviewsAsString] stringByReplacingOccurrencesOfString:@"\n" withString:@"\n  "]];
		}
		[output appendString:@"}"];
	}
	return output;
}

- (UIView *)findFirstResponder {
	UIView* retVal = nil;
	if ([self isFirstResponder]) {
		retVal = self;
	} else {
		for (UIView* subview in self.subviews) {
			retVal = [subview findFirstResponder];
			if (retVal) break;
		}
	}
	return retVal;
}

@end
