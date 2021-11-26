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
              readmeFilename:(NSString*)readmeFilename
                  graphicFilename:(NSString*)graphicFilename
                 fileVersion:(NSString*)fileVersion
      keymanDeveloperVersion:(NSString*)keymanDeveloperVersion
                   copyright:(NSString*)copyright
                  authorName:(NSString*)authorName
                  authorUrl:(NSString*)authorUrl
                     website:(NSString*)website
                   keyboards:(NSDictionary*)keyboards
                       fonts:(NSArray*)fonts
                       files:(NSArray*)files
{
    self = [super init];
    if (self) {
        _packageName = [packageName copy];
        _packageVersion = [packageVersion copy];
        _readmeFilename = [readmeFilename copy];
        _graphicFilename = [graphicFilename copy];
        _fileVersion = [fileVersion copy];
        _keymanDeveloperVersion = [keymanDeveloperVersion copy];
        _copyright = [copyright copy];
        _authorName = [authorName copy];
        _authorUrl = [authorUrl copy];
        _website = [website copy];
        _keyboards = [keyboards copy];
        _fonts = [fonts copy];
        _files = [files copy];
    }
    return self;
}

- (void)debugReport
{
    NSLog(@"SGS2021 packageInfo.packageName = %@", _packageName);
    NSLog(@"SGS2021 packageInfo.packageVersion = %@", _packageVersion);
    NSLog(@"SGS2021 packageInfo.authorName = %@", _authorName);
    NSLog(@"SGS2021 packageInfo.authorUrl = %@", _authorUrl);
    NSLog(@"SGS2021 packageInfo.copyright = %@", _copyright);
    NSLog(@"SGS2021 packageInfo.readMe = %@", _readmeFilename);
    NSLog(@"SGS2021 packageInfo.fonts.count = %@", [_fonts count]);
}


@end
