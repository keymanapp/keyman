//
//  NFont.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/06/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NFont.h"

@implementation NFont

- (id)init {
  self = [super init];
  if (self) {
    _name = @"";
  }
  
  return self;
}

- (NSString *)description {
  NSString *format = @"<%@:%p N:%@ S:%d C:%d>";
  NSString *str = [NSString stringWithFormat:format, self.className, self, self.name, self.size, self.color];
  return str;
}

@end
