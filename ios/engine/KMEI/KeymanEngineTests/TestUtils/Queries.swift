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
    
    static let package_version_case_mtnt = query_bundle.url(forResource: "package-version-case-mtnt", withExtension: "json")!
    
    static let package_version_km = query_bundle.url(forResource: "package-version-km", withExtension: "json")!
    static let package_version_km_updated = query_bundle.url(forResource: "package-version-km-updated", withExtension: "json")!
    
    static let package_version_post_migration = query_bundle.url(forResource: "package-version-post-migration", withExtension: "json")!
    
    static let model_case_en = query_bundle.url(forResource: "model-case-en", withExtension: "json")!
    static let model_case_km = query_bundle.url(forResource: "model-case-km", withExtension: "json")!
    static let model_case_str = query_bundle.url(forResource: "model-case-str", withExtension: "json")!
    static let model_case_none = query_bundle.url(forResource: "model-case-none", withExtension: "json")!
  }
}
