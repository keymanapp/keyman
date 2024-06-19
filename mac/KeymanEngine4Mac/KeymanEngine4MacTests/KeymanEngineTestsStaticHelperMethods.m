//
//  KeymanEngineTestsStaticHelperMethods.m
//  KeymanEngine4MacTests
//
//  Created by tom on 1/18/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "KeymanEngineTestsStaticHelperMethods.h"

@implementation KeymanEngineTestsStaticHelperMethods

+ (KMXFile *)getKmxFileTestMacEngine {
  NSString *path = [[NSBundle bundleForClass:[KeymanEngineTestsStaticHelperMethods class]] pathForResource:@"TestMacEngine.kmx" ofType:nil];
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
  return kmxFile;
}

+ (KMXFile *)getKmxFileForPlatformTest {
  NSString *path = [[NSBundle bundleForClass:[KeymanEngineTestsStaticHelperMethods class]] pathForResource:@"PlatformTest.kmx" ofType:nil];
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
  return kmxFile;
}

+ (KMXFile *)getKmxFileForCipherMusicTests {
  NSString *path = [[NSBundle bundleForClass:[KeymanEngineTestsStaticHelperMethods class]] pathForResource:@"CipherMusicUnicode.kmx" ofType:nil];
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
  return kmxFile;
}

+ (KMXFile *)getKmxFileForSilIpaTests {
  NSString *path = [[NSBundle bundleForClass:[KeymanEngineTestsStaticHelperMethods class]] pathForResource:@"sil_ipa.kmx" ofType:nil];
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
  return kmxFile;
}

+ (KMXFile *)getKmxFileForIndexOffsetTests {
  NSString *path = [[NSBundle bundleForClass:[KeymanEngineTestsStaticHelperMethods class]] pathForResource:@"indexoffset1892.kmx" ofType:nil];
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
  return kmxFile;
}


+ (KMXFile *)getKmxFileForElNuerTests {
  NSString *path = [[NSBundle bundleForClass:[KeymanEngineTestsStaticHelperMethods class]] pathForResource:@"el_nuer.kmx" ofType:nil];
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
  return kmxFile;
}

+ (KMXFile *)getKmxFileForArmenianMnemonicTests {
  NSString *path = [[NSBundle bundleForClass:[KeymanEngineTestsStaticHelperMethods class]] pathForResource:@"armenian_mnemonic.kmx" ofType:nil];
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:path];
  return kmxFile;
}

@end
