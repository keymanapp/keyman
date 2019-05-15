//
//  FVShared.h
//  FirstVoices
//
//  Created by Serkan Kurt on 19/11/2015.
//  Copyright © 2015 FirstVoices. All rights reserved.
//

#import <Foundation/Foundation.h>

extern NSString *const FVGroupID;

extern NSString *const kFVKeyboardList;

extern NSString *const kFVKeyboardNameKey;
extern NSString *const kFVKeyboardLanguageCodeKey;
extern NSString *const kFVKeyboardFilenameKey;
extern NSString *const kFVKeyboardCheckStateKey;

@interface FVShared : NSObject

+ (NSUserDefaults *)userDefaults;

@end
