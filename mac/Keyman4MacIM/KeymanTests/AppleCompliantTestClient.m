//
//  AppleCompliantTestClient.m
//  KeymanTests
//
//  Created by tom on 6/25/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "AppleCompliantTestClient.h"

@implementation AppleCompliantTestClient

- (NSAttributedString*)attributedSubstringFromRange:(NSRange)range {
  return [[NSAttributedString alloc] initWithString:[super substringFromRangeProtected:range]];
}

@end
