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
  NSString *format = @"<%@:%p flags:0x%hhX shift:0x%X virtualkey:0x%X text:%@ utf8:%@ bmp:%@>";
  NSString *str = [NSString stringWithFormat:format, self.className, self, self.typeFlags, self.modifierFlags, self.keyCode, self.text, [self.text dataUsingEncoding:NSUTF8StringEncoding], self.bitmap];
  return str;
}

@end
