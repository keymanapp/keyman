//
//  NoContextTestClient.m
//  KeymanTests
//
//  Created by tom on 6/25/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NoContextTestClient.h"

@implementation NoContextTestClient

- (void)insertText:(id)string replacementRange:(NSRange)replacementRange {
  [super insertTextProtected: string replacementRange:replacementRange];
}

- (NSAttributedString*)attributedSubstringFromRange:(NSRange)range {
  return nil;
}

@end
