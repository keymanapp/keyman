/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * CoreTestStaticHelperMethods.m
 * CoreWrapperTests
 * 
 * Created by Shawn Schantz on 2023-02-21.
 * 
 * Description...
 */

#import "CoreTestStaticHelperMethods.h"

@implementation CoreTestStaticHelperMethods

+ (CoreHelper *)helper {
  static CoreHelper *coreHelper = nil;
  if (coreHelper == nil) {
    coreHelper = [[CoreHelper alloc] init];
  }
  return coreHelper;
}

+ (NSString *)getKmxFilePathTestMacEngine {
  NSString *path = [[NSBundle bundleForClass:[CoreTestStaticHelperMethods class]] pathForResource:@"TestMacEngine.kmx" ofType:nil];
  return path;
}

+ (NSString *)getKmxFilePathForPlatformTest {
  NSString *path = [[NSBundle bundleForClass:[CoreTestStaticHelperMethods class]] pathForResource:@"PlatformTest.kmx" ofType:@"NSString"];
  return path;
}

+ (NSString *)getKmxFilePathForCipherMusicTests {
  NSString *path = [[NSBundle bundleForClass:[CoreTestStaticHelperMethods class]] pathForResource:@"CipherMusicUnicode.kmx" ofType:@"NSString"];
  return path;
}

+ (NSString *)getKmxFilePathForSilIpaTests {
  NSString *path = [[NSBundle bundleForClass:[CoreTestStaticHelperMethods class]] pathForResource:@"sil_ipa.kmx" ofType:@"NSString"];
  return path;
}

+ (NSString *)getKmxFilePathForIndexOffsetTests {
  NSString *path = [[NSBundle bundleForClass:[CoreTestStaticHelperMethods class]] pathForResource:@"indexoffset1892.kmx" ofType:@"NSString"];
  return path;
}

+ (NSString *)getKmxFilePathForElNuerTests {
  NSString *path = [[NSBundle bundleForClass:[CoreTestStaticHelperMethods class]] pathForResource:@"el_nuer.kmx" ofType:@"NSString"];
  return path;
}

@end
