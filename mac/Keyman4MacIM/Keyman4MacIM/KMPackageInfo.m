//
//  KMPackageInfo.m
//  Keyman
//
//  Created by Shawn Schantz on 11/22/21.
//  Copyright © 2021 SIL International. All rights reserved.
//

#import "KMPackageInfo.h"

@implementation KMPackageInfo

- (instancetype)initWithName:(NSString*)packageName
              packageVersion:(NSString*)packageVersion
                      readMe:(NSString*)readMe
                 fileVersion:(NSString*)fileVersion
      keymanDeveloperVersion:(NSString*)keymanDeveloperVersion
                   copyright:(NSString*)copyright
                  authorName:(NSString*)authorName
                  authorUrl:(NSString*)authorUrl
                     website:(NSString*)website
                   keyboards:(NSDictionary*)keyboards
                   files:(NSArray*)files
{
    self = [super init];
    if (self) {
        _packageName = [packageName copy];
        _packageVersion = [packageVersion copy];
        _readMe = [readMe copy];
        _fileVersion = [fileVersion copy];
        _keymanDeveloperVersion = [keymanDeveloperVersion copy];
        _copyright = [copyright copy];
        _authorName = [authorName copy];
        _authorUrl = [authorUrl copy];
        _website = [website copy];
        _keyboards = [keyboards copy];
        _files = [files copy];
    }
    return self;
}

@end
