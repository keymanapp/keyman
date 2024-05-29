//
//  KeymanEngine4MacTests.m
//  KeymanEngine4MacTests
//
//  Created by Serkan Kurt on 29/01/2015.
//  Copyright (c) 2017 SIL International. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import <XCTest/XCTest.h>
#import "KeymanEngineTestsStaticHelperMethods.h"

@interface KMXFileTests : XCTestCase

@end

@implementation KMXFileTests

- (void)setUp {
  [super setUp];
  // Put setup code here. This method is called before the invocation of each test method in the class.
}

- (void)tearDown {
  // Put teardown code here. This method is called after the invocation of each test method in the class.
  [super tearDown];
}

- (void)testinitWithFilePath_NilPath_ReturnsNil {
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:nil];
  XCTAssert(kmxFile == nil, @"Expected nil file");
}

- (void)testinitWithFilePath_BogusPath_ReturnsNil {
  KMXFile *kmxFile = [[KMXFile alloc] initWithFilePath:@"This is not a valid path to nuthin'"];
  XCTAssert(kmxFile == nil, @"Expected nil file");
}

- (void)testinitWithFilePath_Valid_ReturnsInitializedKmxFile {
  KMXFile *kmxFile = [KeymanEngineTestsStaticHelperMethods getKmxFileTestMacEngine];
  XCTAssert(kmxFile != nil, @"Expected non-nil file");
  XCTAssert([kmxFile isValid], @"Expected valid KMX file.");
}

@end
