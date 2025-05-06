//
//  NSString+XString.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 8/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NSString+XString.h"

@implementation NSString (XString)

+ (NSString *)nullChar {
    return [NSString stringWithFormat:@"%C%C%d", UC_SENTINEL, CODE_NULLCHAR, 1];
}

@end
