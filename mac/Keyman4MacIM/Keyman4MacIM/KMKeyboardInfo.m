//
//  KMKeyboardInfo.m
//  Keyman
//
//  Created by Shawn Schantz on 11/25/21.
//  Copyright © 2021 SIL International. All rights reserved.
//

#import "KMKeyboardInfo.h"

@implementation KMLanguageInfo

- (instancetype)initWithName:(NSString*)name
              identifier:(NSString*)identifier
{
    self = [super init];
    if (self) {
        _name = [name copy];
        _identifier = [identifier copy];
    }
    return self;
}

@end

@implementation KMKeyboardInfo

- (instancetype)initWithName:(NSString*)name
              identifier:(NSString*)identifier
                      oskFont:(NSString*)oskFont
                 displayFont:(NSString*)displayFont
                   languages:(NSArray*)languages
{
    self = [super init];
    if (self) {
        _name = [name copy];
        _identifier = [identifier copy];
        _oskFont = [oskFont copy];
        _displayFont = [displayFont copy];
        _languages = [languages copy];
    }
    return self;
}

@end
