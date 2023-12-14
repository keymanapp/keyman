//
//  KeymanEngineTestsStaticHelperMethods.h
//  KeymanEngine4Mac
//
//  Created by tom on 1/18/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

#ifndef KeymanEngineTestsStaticHelperMethods_h
#define KeymanEngineTestsStaticHelperMethods_h

#import "KMXFile.h"

// See KMEngine.m - these can be bitwise OR'd
#define LEFT_SHIFT_FLAG  0x20002
#define RIGHT_SHIFT_FLAG 0x20004
#define LEFT_CTRL_FLAG   0x40001
#define RIGHT_CTRL_FLAG  0x42000
#define LEFT_ALT_FLAG    0x80020
#define RIGHT_ALT_FLAG   0x80040

@interface KeymanEngineTestsStaticHelperMethods : NSObject
+ (KMXFile *)getKmxFileTestMacEngine;
+ (KMXFile *)getKmxFileForPlatformTest;
+ (KMXFile *)getKmxFileForCipherMusicTests;
+ (KMXFile *)getKmxFileForSilIpaTests;
+ (KMXFile *)getKmxFileForIndexOffsetTests;
+ (KMXFile *)getKmxFileForElNuerTests;
+ (KMXFile *)getKmxFileForArmenianMnemonicTests;
@end

#endif /* KeymanEngineTestsStaticHelperMethods_h */
