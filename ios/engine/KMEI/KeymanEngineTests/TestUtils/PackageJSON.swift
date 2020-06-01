//
//  PackageJSON.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 6/1/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation

extension TestUtils {
  enum PackageJSON {
    static let jsonBundle = findSubBundle(forResource: "PackageJSON", ofType: "bundle")

    // place entries here.
    static let language_en = jsonBundle.url(forResource: "language-en", withExtension: "json")!
    static let language_km = jsonBundle.url(forResource: "language-km", withExtension: "json")!
  }
}
