//
//  NSString+Helpers.m
//  Keyman Engine
//
//  Created by Patrick Weekes on 12-04-28.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NSString+Helpers.h"

@implementation NSString (Helpers)

+ (NSString *)stringWithCharacter:(char)character length:(NSUInteger)length {
    NSMutableString* retVal = [NSMutableString stringWithCapacity:length];
    for (int i=0; i<length; ++i) {
        [retVal appendFormat:@"%c", character];
    }
    
    return retVal;
}

@end
