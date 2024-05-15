//
//  Font.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-24.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Font object for Keyman Cloud API 4.0.
public struct Font: Codable {
  /// Font family that KeymanWeb will provide.
  public let family: String
  
  /// Local filenames for this font.
  public let source: [String]
  
  /// Font size (in CSS dimensions).
  public let size: String
  
  public init(family: String, source: [String], size: String? = nil) {
    self.family = family
    self.source = source
    self.size = size ?? "1em"
  }
  
  public init(filename: String) {
    let family = "font_family_\(filename)"
    let source = [filename]
    self.init(family: family, source: source)
  }
  
  public init(from decoder: Decoder) throws {
    let container = try decoder.container(keyedBy: CodingKeys.self)
    
    let family = try container.decode(String.self, forKey: .family)
    let size = try container.decodeIfPresent(String.self, forKey: .size)
    
    let source: [String]
    if let s = try? container.decode([String].self, forKey: .source) {
      source = s
    } else {
      source = [try container.decode(String.self, forKey: .source)]
    }
    
    self.init(family: family, source: source, size: size)
  }
}
