//
//  KeymanEngineTests.swift
//  KeymanEngineTests
//
//  Created by Randy Boring on 3/7/19.
//  Copyright © 2019 SIL International. All rights reserved.
//

import XCTest

//import KeymanPackage
@testable import KeymanEngine

class KeymanEngineTests: XCTestCase {

    override func setUp() {
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
    }

    // AMDD acceptance test for existing KeymanPackage so I'll know what has to keep working despite my changers
    func testExample() {
        let testUrl = URL(fileURLWithPath: "/Users/Shared/testpackage.kmp");
        let outUrl  = URL(fileURLWithPath: "/Users/Shared/outPackageFolder")

        KeymanPackage.extract(fileUrl: testUrl, destination: outUrl, complete: { kmp in
            if let kmp = kmp {
                // extracted ok, test kmp
                XCTAssert(kmp.sourceFolder == outUrl, "the sourceFolder should be outURL")
            } else {
                XCTAssert(false, "KeymanPackage.extract failed")
            }
        })        // Use XCTAssert and related functions to verify your tests produce the correct results.
    }

    func testPerformanceExample() {
        // This is an example of a performance test case.
        self.measure {
            // Put the code you want to measure the time of here.
        }
    }

}
