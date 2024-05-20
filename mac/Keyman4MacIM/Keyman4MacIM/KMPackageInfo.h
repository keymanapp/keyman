/*
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KMPackageInfo.h
 * Keyman
 *
 * Created by Shawn Schantz on 2021/11/22.
 *
 * Value object describing a Keyman package.
 */

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMPackageInfoBuilder : NSObject
@property (nonatomic,copy) NSString* packageName;
@property (nonatomic,copy) NSString* packageVersion;
@property (nonatomic,copy) NSString* readmeFilename;
@property (nonatomic,copy) NSString* graphicFilename;
@property (nonatomic,copy) NSString* fileVersion;
@property (nonatomic,copy) NSString* keymanDeveloperVersion;
@property (nonatomic,copy) NSString* copyright;
@property (nonatomic,copy) NSString* authorName;
@property (nonatomic,copy) NSString* authorUrl;
@property (nonatomic,copy) NSString* website;
@property (nonatomic,copy) NSArray* keyboards;
@property (nonatomic,copy) NSArray* files;
@property (nonatomic,copy) NSArray* fonts;

- (instancetype)init;

@end


@interface KMPackageInfo : NSObject
@property (nonatomic,readonly) NSString* packageName;
@property (nonatomic,readonly) NSString* packageVersion;
@property (nonatomic,readonly) NSString* readmeFilename;
@property (nonatomic,readonly) NSString* graphicFilename;
@property (nonatomic,readonly) NSString* fileVersion;
@property (nonatomic,readonly) NSString* keymanDeveloperVersion;
@property (nonatomic,readonly) NSString* copyright;
@property (nonatomic,readonly) NSString* authorName;
@property (nonatomic,readonly) NSString* authorUrl;
@property (nonatomic,readonly) NSString* website;
@property (nonatomic,readonly) NSArray* keyboards;
@property (nonatomic,readonly) NSArray* files;
@property (nonatomic,readonly) NSArray* fonts;

- (instancetype)initWithBuilder:(KMPackageInfoBuilder *)builder;

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
                       files:(NSArray*)files;

@end

NS_ASSUME_NONNULL_END
