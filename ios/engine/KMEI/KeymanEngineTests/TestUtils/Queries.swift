//
//  Queries.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/26/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension TestUtils {
  enum Queries {
    static let query_bundle = TestUtils.findSubBundle(forResource: "Queries", ofType: ".bundle")

    static let package_version_case_1 = query_bundle.url(forResource: "package-version-case-1", withExtension: "json")!
  }
}
