//
//  KMCompGroup.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 28/11/2014.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "KMCompGroup.h"
#import "NSString+XString.h"

@implementation KMCompGroup

- (id)init {
  self = [super init];
  if (self) {
    _name = @"";
    _keys = [[NSMutableArray alloc] initWithCapacity:0];
    _match = @"";
    _noMatch = @"";
  }
  
  return self;
}

- (NSString *)description {
  NSString *format = @"<%@:%p, Name:%@ Match:%@ NoMatch:%@ fUsingKeys:%d Keys:%@>";
  NSString *str = [NSString stringWithFormat:format,
                   [self className], self, _name, [_match codeString], [_noMatch codeString], _fUsingKeys, _keys];
  return str;
}

@end
