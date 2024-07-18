//
//  NSString+SuppMethods.m
//  Keyman4MacIM
//
//  Created by Serkan Kurt on 9/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NSString+SuppMethods.h"

@implementation NSString (SuppMethods)

- (BOOL)startsWith:(NSString *)str {
  return [[self lowercaseString] hasPrefix:[str lowercaseString]];
}

@end
