//
//  INIParser.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-27.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

class INIParser {
  /// - Returns: Dictionary keyed by section name, whose values are a dictionary of properties under the section.
  /// Properties at the top of a file without a section are put under a section whose name is the empty string.
  func parse(_ string: String) throws -> [String: [String: String]] {
    let lines = string.components(separatedBy: .newlines)
      .map { $0.trimmingCharacters(in: .whitespaces) }
      .filter { !$0.hasPrefix(";") && !$0.isEmpty }

    var result: [String: [String: String]] = [:]
    var section = ""

    for line in lines {
      if let newSection = try parseHeader(line) {
        guard result[newSection] == nil else {
          throw INIError.duplicateSection
        }
        result[newSection] = [:]
        section = newSection
        continue
      }

      let (key, value) = try parseProperty(line)
      var sectionProperties = result[section] ?? [:]
      guard sectionProperties[key] == nil else {
        throw INIError.duplicateKey
      }
      sectionProperties[key] = value
      result[section] = sectionProperties
    }

    return result
  }

  private func parseHeader(_ line: String) throws -> String? {
    guard line.hasPrefix("[") else {
      return nil
    }
    guard let closingBracketIndex = line.index(of: "]") else {
      throw INIError.missingSectionClosingBracket
    }
    guard line.index(after: closingBracketIndex) == line.endIndex else {
      throw INIError.charactersAfterSectionHeader
    }
    return String(line[line.index(after: line.startIndex)..<closingBracketIndex])
  }

  private func parseProperty(_ line: String) throws -> (String, String) {
    guard let equalIndex = line.index(of: "=") else {
      throw INIError.missingEqualSignInProperty
    }
    let key = String(line[..<equalIndex])
    let value = String(line[line.index(after: equalIndex)...])
    return (key, value)
  }
}
