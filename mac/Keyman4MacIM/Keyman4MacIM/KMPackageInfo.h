//
//  KMPackageInfo.h
//  Keyman
//
//  Created by Shawn Schantz on 11/22/21.
//  Copyright © 2021 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface KMPackageInfo : NSObject
@property (nonatomic,readonly) NSString* packageName;
@property (nonatomic,readonly) NSString* packageVersion;
@property (nonatomic,readonly) NSString* readMe;
@property (nonatomic,readonly) NSString* fileVersion;
@property (nonatomic,readonly) NSString* keymanDeveloperVersion;
@property (nonatomic,readonly) NSString* copyright;
@property (nonatomic,readonly) NSString* authorName;
@property (nonatomic,readonly) NSString* authorUrl;
@property (nonatomic,readonly) NSString* website;
@property (nonatomic,readonly) NSDictionary* keyboards;
@property (nonatomic,readonly) NSArray* files;

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
                   files:(NSArray*)files;
@end

NS_ASSUME_NONNULL_END
