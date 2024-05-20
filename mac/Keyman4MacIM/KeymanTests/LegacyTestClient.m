//
//  LegacyTestClient.m
//  KeymanTests
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import "LegacyTestClient.h"
#import <Foundation/Foundation.h>

@implementation LegacyTestClient

NSRange _selection;
NSMutableString * _contents;

- (instancetype)init{
  return [self initWithString:@""];
}

- (instancetype)initWithString:(NSString *)contents {
  self = [super init];
  if (self) {
    _contents = [[NSMutableString alloc] initWithString:contents];
    _selection = NSMakeRange(self.length, 0);
  }
  return self;
}

- (void)handleEventIfNotHandledBy: (KMInputMethodEventHandler *) im event:(NSEvent *)event {
  if (![im handleEvent:event client:self]) {
    if (event.keyCode == kVK_Delete) {
      if (_selection.length == 0 && _selection.location > 0)
        _selection = NSMakeRange(_selection.location - 1, 1);
      if (_selection.length) {
        [_contents deleteCharactersInRange:_selection];
        _selection = NSMakeRange(_selection.location, 0);
      }
    }
    else {
      [self insertTextProtected:event.characters replacementRange:_selection];
    }
  }
}

- (void)insertTextProtected:(id)string replacementRange:(NSRange)replacementRange {
  [_contents deleteCharactersInRange:replacementRange];
  [_contents insertString:string atIndex:replacementRange.location];
  _selection = NSMakeRange(replacementRange.location + [string length], 0);
}

- (NSString *)substringFromRangeProtected:(NSRange)range {
  assert(range.location >= 0);
  assert(range.location + range.length <= [self length]);
  return [_contents substringWithRange:range];
}

- (NSRange)selectedRange {
  return _selection;
}

- (NSInteger)length {
  return (NSInteger)_contents.length;
}

- (NSString *) getResult {
  return _contents;
}
@end
