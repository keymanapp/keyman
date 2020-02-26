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

    override func setUp() {
        // Put setup code here. This method is called before the invocation of each test method in the class.
    }

    override func tearDown() {
        // Put teardown code here. This method is called after the invocation of each test method in the class.
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

}
