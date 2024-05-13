//
//  OSKKey.m
//  KeymanEngine4Mac
//
//  Created by Serkan Kurt on 1/07/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import "OSKKey.h"
#import <os/log.h>

@implementation OSKKey

- (id)initWithKeyCode:(NSUInteger)keyCode caption:(NSString *)caption scale:(CGFloat)scale {
    self = [super init];
    if (self) {
        os_log_t oskKeyLog = os_log_create("org.sil.keyman", "osk-key");
      os_log_with_type(oskKeyLog, OS_LOG_TYPE_DEBUG, "OSKKey initWithKeyCode: 0x%lx, caption: %{public}@, scale: %f", keyCode, caption, scale);
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
