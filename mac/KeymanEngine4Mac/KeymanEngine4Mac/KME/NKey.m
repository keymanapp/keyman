//
//  NKey.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 22/06/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "NKey.h"

@implementation NKey

- (id)init {
  self = [super init];
  if (self) {
    _text = @"";
  }
  
  return self;
}

- (NSString *)description {
  NSString *format = @"<%@:%p flags:%d shift:%d virtualkey:%d text:%@ utf8:%@ bmp:%@>";
  NSString *str = [NSString stringWithFormat:format, self.className, self, self.flags, self.shift, self.vkey, self.text, [self.text dataUsingEncoding:NSUTF8StringEncoding], self.bitmap];
  return str;
}

@end
