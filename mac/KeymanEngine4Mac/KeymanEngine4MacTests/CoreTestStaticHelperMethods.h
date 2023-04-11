/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreTestStaticHelperMethods.h
 * CoreWrapperTests
 * 
 * Created by Shawn Schantz on 2023-02-21.
 * 
 * Description...
 */

#import <Foundation/Foundation.h>
#import "CoreHelper.h"

NS_ASSUME_NONNULL_BEGIN

@interface CoreTestStaticHelperMethods : NSObject

+ (CoreHelper *)helper;
+ (NSString *)getKmxFilePathTestMacEngine;
+ (NSString *)getKmxFilePathForPlatformTest;
+ (NSString *)getKmxFilePathForCipherMusicTests;
+ (NSString *)getKmxFilePathForSilIpaTests;
+ (NSString *)getKmxFilePathForIndexOffsetTests;

@end


NS_ASSUME_NONNULL_END
