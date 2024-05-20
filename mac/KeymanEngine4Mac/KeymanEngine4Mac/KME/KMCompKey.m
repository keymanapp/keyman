//
//  KMCompKey.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 28/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMCompKey.h"
#import "NSString+XString.h"

@implementation KMCompKey

- (id)init {
  self = [super init];
  if (self) {
    _output = @"";
    _context = @"";
  }
  
  return self;
}

- (NSString *)description {
  NSString *format = @"<%@:%p K:0x%X L:0x%X F:0x%X O:%@ C:%@>";
  NSString *str = [NSString stringWithFormat:format,
                   self.className, self, self.key, self.line, self.shiftFlags, [self.output codeString], [self.context codeString]];
  return str;
}

@end
