//
//  Updates.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 7/15/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
@testable import KeymanEngine

extension TestUtils {
  enum Updates {
    static let km_base_state = TestUtils.findSubBundle(forResource: "khmer_angkor update-base", ofType: ".bundle")
  }
}
