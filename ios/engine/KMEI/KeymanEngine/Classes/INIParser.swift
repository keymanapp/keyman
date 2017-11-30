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
          throw INIParsingError.duplicateSection
        }
        result[newSection] = [:]
        section = newSection
        continue
      }

      let (key, value) = try parseProperty(line)
      var sectionProperties = result[section] ?? [:]
      guard sectionProperties[key] == nil else {
        throw INIParsingError.duplicateKey
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
      throw INIParsingError.missingSectionClosingBracket
    }
    guard line.index(after: closingBracketIndex) == line.endIndex else {
      throw INIParsingError.charactersAfterSectionHeader
    }
    return String(line[line.index(after: line.startIndex)..<closingBracketIndex])
  }

  private func parseProperty(_ line: String) throws -> (String, String) {
    guard let equalIndex = line.index(of: "=") else {
      throw INIParsingError.missingEqualSignInProperty
    }
    let key = String(line[..<equalIndex])
    let value = String(line[line.index(after: equalIndex)...])
    return (key, value)
  }

  func unquotedString(_ string: String) throws -> String {
    if string.hasPrefix("\"") {
      let withoutOpeningQuote = string.dropFirst()
      guard let closingQuoteIndex = withoutOpeningQuote.index(of: "\"") else {
        throw INIValueParsingError.unterminatedQuote
      }
      guard withoutOpeningQuote.index(after: closingQuoteIndex) == withoutOpeningQuote.endIndex else {
        throw INIValueParsingError.invalidTextAdjacentToQuote
      }
      return String(withoutOpeningQuote.dropLast())
    }
    guard string.index(of: "\"") == nil else {
      throw INIValueParsingError.invalidTextAdjacentToQuote
    }
    return string
  }

  func components(_ string: String) throws -> [String] {
    var components: [String] = []
    var remaining = Substring(string)
    var currentComponent: String?

    while !remaining.isEmpty {
      if remaining.hasPrefix(",") {
        remaining = remaining.dropFirst()
        components.append(currentComponent ?? "")
        currentComponent = nil
        continue
      }

      guard currentComponent == nil else {
        throw INIValueParsingError.invalidTextAdjacentToQuote
      }

      if remaining.hasPrefix("\"") {
        let withoutOpeningQuote = remaining.dropFirst()
        guard let closingQuoteIndex = withoutOpeningQuote.index(of: "\"") else {
          throw INIValueParsingError.unterminatedQuote
        }
        currentComponent = String(withoutOpeningQuote[..<closingQuoteIndex])
        remaining = withoutOpeningQuote[withoutOpeningQuote.index(after: closingQuoteIndex)...]
      } else {
        let endIndex = remaining.index { $0 == "," || $0 == "\"" } ?? remaining.endIndex
        currentComponent = String(remaining[..<endIndex])
        remaining = remaining[endIndex...]
      }
    }
    components.append(currentComponent ?? "")
    return components
  }
}
