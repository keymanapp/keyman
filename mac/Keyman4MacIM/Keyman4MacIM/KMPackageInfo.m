/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMPackageInfo.m
 * Keyman
 *
 * Created by Shawn Schantz on 2021/11/22.
 *
 * Value object describing a Keyman package.
 */

#import "KMPackageInfo.h"

@implementation KMPackageInfoBuilder
- (instancetype)init {
  self = [super init];
  if (self) {
    _packageName = nil;
    _packageVersion = nil;
    _readmeFilename = nil;
    _graphicFilename = nil;
    _fileVersion = nil;
    _keymanDeveloperVersion = nil;
    _copyright = nil;
    _authorName = nil;
    _authorUrl = nil;
    _website = nil;
    _keyboards = nil;
    _fonts = nil;
    _files = nil;
  }
  return self;
}
@end

@implementation KMPackageInfo

- (instancetype)initWithBuilder:(KMPackageInfoBuilder *)builder {
  self = [super init];
  if (self) {
    _packageName = builder.packageName;
    _packageVersion = builder.packageVersion;
    _readmeFilename = builder.readmeFilename;
    _graphicFilename = builder.graphicFilename;
    _fileVersion = builder.fileVersion;
    _keymanDeveloperVersion = builder.keymanDeveloperVersion;
    _copyright = builder.copyright;
    _authorName = builder.authorName;
    _authorUrl = builder.authorUrl;
    _website = builder.website;
    _keyboards = builder.keyboards;
    _fonts = builder.fonts;
    _files = builder.files;
  }
  return self;
}

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
                   keyboards:(NSArray*)keyboards
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

@end
