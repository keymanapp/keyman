//
//  VersionTests.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2020-02-13.
//  Copyright © 2020 SIL International. All rights reserved.
//

@testable import KeymanEngine
import XCTest

class VersionTests: XCTestCase {
  
  func testStringConstruction() {
    let simpleEarly = Version("11.0")
    
    XCTAssertNotNil(simpleEarly, "Could not construct Version instance from a simple major-minor version string")
    
    let complexEarly = Version("11.0.65.17.8")
    
    XCTAssertNotNil(complexEarly, "Could not process the version string '11.0.65.17.8'")
    
    let broken = Version("apple.orange")
    
    XCTAssertNil(broken, "Erroneously constructed Version instance from character-text strings")
    
    let tagged = Version("14.0.18-alpha-local")
    
    XCTAssertNotNil(tagged, "Could not construct Verison instance from a text-tagged version string")
    XCTAssertTrue(tagged! == Version("14.0.18")!, "Did not produce expected version from text-tagged version string")
    XCTAssertEqual(tagged!.tier, .alpha, "Did not properly store a tagged version's tier information")
  }
  
  func testVersionComparison() {
    // Simple unit testing for version-comparison logic.
    let simpleEarly = Version("11.0")!
    let simpleLate = Version("13.0")!
    
    XCTAssertLessThan(simpleEarly, simpleLate)
    
    let complexEarly1 = Version("11.0.65")!
    let complexEarly2 = Version("11.1.1")!
    
    XCTAssertLessThan(complexEarly1, complexEarly2)
    
    let complexLate1 = Version("13.0.1")!
    
    XCTAssertLessThan(complexEarly2, complexLate1)
    XCTAssertLessThan(simpleLate, complexLate1)
    
    let simpleLater = Version("13.1")!
    
    XCTAssertLessThan(complexLate1, simpleLater)
  }
  
  func testMajorMinor() {
    let simple = Version("12")!
    
    XCTAssertTrue(simple.majorMinor == Version("12.0")!, "Did not append .minor to major-only version")
    
    let complex = Version("11.0.65.17.8")!
    
    XCTAssertTrue(complex.majorMinor == Version("11.0")!, "Did not properly trim off excess version components")
  }
  
  func testEquals() {
    let simple = Version("12.0")!
    let other = Version("12.0")!
    
    XCTAssertTrue(simple == other)
    
    let longer = Version("12.0.0.0")!
    
    XCTAssertTrue(simple == longer)
    
    let unequal_1 = Version("12.0.0.1")!
    
    XCTAssertFalse(simple == unequal_1)
    XCTAssertFalse(longer == unequal_1)
    
    let diffMajor = Version("13.0")!
    
    XCTAssertFalse(simple == diffMajor)
    
    // Tests in "optional" mode.  Was trickier than it would appear!
    XCTAssertEqual(Version("12.0"), Version("12.0"))
    
    // Some tests against major-version zero to ensure the edge case is covered.
    XCTAssertEqual(Version("0"), Version("0"))
    XCTAssertEqual(Version("0"), Version("0.0"))
    XCTAssertNotEqual(Version("0"), Version("0.0.1"))
  }
  
  func testValidCurrentEngineVersion() {
    let version = Version.current
    
    // The key is that it always returns a version value; this is the most likely 'big break' we may see.
    XCTAssertNotNil(version, "Could not construct a valid version from value in bundle's plist file")
    
    // Now that we update KeymanEngine's version too, we can rely on its versioning.
    // This was implemented in very early 14.0 development.
    //
    // So long as we don't change the actual _project's_ version, this test proves that Versions
    // are being updated correctly by the build process.
    let minimumEngineVersion = Version("14.0")!
    XCTAssertGreaterThan(version, minimumEngineVersion, "Build process did not properly update Engine version")
  }
}
