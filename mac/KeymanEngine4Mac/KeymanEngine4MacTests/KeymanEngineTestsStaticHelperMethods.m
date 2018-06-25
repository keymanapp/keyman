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
@end
