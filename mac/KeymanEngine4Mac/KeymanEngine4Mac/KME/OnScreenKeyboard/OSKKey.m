//
//  OSKKey.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 1/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "OSKKey.h"

@implementation OSKKey

- (id)initWithKeyCode:(NSUInteger)keyCode caption:(NSString *)caption scale:(CGFloat)scale {
    self = [super init];
    if (self) {
        _keyCode = keyCode;
        
        if (caption == nil)
            _caption = @"";
        else
            _caption = [NSString stringWithString:caption];
        
        _scale = scale;
    }
    
    return self;
}

@end
