//
//  FVShared.m
//  FirstVoices
//
//  Created by Serkan Kurt on 19/11/2015.
//  Copyright © 2015 FirstVoices. All rights reserved.
//

#import "FVShared.h"

NSString *const FVGroupID = @"group.FVKeyboards";

NSString *const kFVKeyboardList = @"FVKeyboardList";

NSString *const kFVKeyboardNameKey = @"FVKeyboardName";
NSString *const kFVKeyboardLanguageCodeKey = @"FVKeyboardLanguageCode";
NSString *const kFVKeyboardFilenameKey = @"FVKeyboardFilename";
NSString *const kFVKeyboardCheckStateKey = @"FVKeyboardCheckState";

@implementation FVShared

+ (NSUserDefaults *)userDefaults {
    NSUserDefaults *_userDefaults = nil;
    if ([[NSUserDefaults standardUserDefaults] respondsToSelector:@selector(initWithSuiteName:)])
        _userDefaults = [[NSUserDefaults alloc] initWithSuiteName:FVGroupID];
    
    if (_userDefaults == nil)
        _userDefaults = [NSUserDefaults standardUserDefaults];
    
    return _userDefaults;
}

@end
