//
//  SimpleTestClient.m
//  KeymanTests
//
//  Created by tom on 6/22/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import "SimpleTestClient.h"
#import <Foundation/Foundation.h>

@implementation SimpleTestClient

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

- (void)handeEventIfNotHandledBy: (KMInputMethodEventHandler *) im event:(NSEvent *)event {
    if (![im handleEvent:event client:self])
        [self insertText:event.characters replacementRange:_selection];
}

- (void)insertText:(id)string replacementRange:(NSRange)replacementRange {
    [_contents deleteCharactersInRange:replacementRange];
    [_contents insertString:string atIndex:replacementRange.location];
    _selection = NSMakeRange(replacementRange.location + [string length], 0);
}

- (NSRange)selectedRange {
    return _selection;
}

// TODO: Control implementation of this based on a vriable to be able to
// simulate lagacy and non-legacy clients (currently legacy).
- (NSAttributedString*)attributedSubstringFromRange:(NSRange)range {
    return nil;
}

- (NSInteger)length {
    return (NSInteger)_contents.length;
}

- (NSString *) getResult {
    return _contents;
}
@end
