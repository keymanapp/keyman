//
//  Version.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-23.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Dotted-decimal version.
public struct Version: Comparable {
  public static let fallback = Version("1.0")!
  public static let latestFeature = Version("13.0.65")!

  public static var current: Version {
    // TODO:  actually update KMEI's version in the bundle during builds, not just the app's.
    //let engineInfo = Bundle(for: Manager.self).infoDictionary

    // For now, we just rely on the app's version instead.
    let engineInfo = Bundle.main.infoDictionary
    return Version(engineInfo!["CFBundleVersion"] as! String)!
  }

  // The Engine first started tracking the 'last loaded version' in 12.0.
  public static let freshInstall = Version("0.0")!
  public static let firstTracked = Version("12.0")!

  // The Keyman App's file browser was first added in 13.0 alpha;
  // this prompted a mild rework of KMP installation file management,
  // since this exposed byproducts to the users in the Files app.
  public static let fileBrowserImplemented = Version("13.0")!
  public static let defaultsNeedBackup = Version("13.0.65")!

  private let components: [Int]
  public let string: String

  public init?(_ string: String) {
    let tagComponents = string.components(separatedBy: "-")
    let stringComponents = tagComponents[0].components(separatedBy: ".")

    var components: [Int] = []
    for s in stringComponents {
      guard let i = Int(s), i >= 0 else {
        return nil
      }
      components.append(i)
    }

    self.string = string
    self.components = components
  }

  public init?(_ components: [Int]) {
    if components.count == 0 {
      return nil
    }

    var string = ""

    for i in components {
      guard i >= 0 else {
        return nil
      }

      let dot = (string == "" ? "" : ".")
      string = "\(string)\(dot)\(i)"
    }

    self.string = string
    self.components = components  // Swift arrays are value types, not reference types!
  }

  public static func <(lhs: Version, rhs: Version) -> Bool {
    let len = max(lhs.components.count, rhs.components.count)
    for i in 0..<len {
      let leftComponent = lhs.components[safe: i] ?? 0
      let rightComponent = rhs.components[safe: i] ?? 0
      if leftComponent < rightComponent {
        return true
      }
      if leftComponent > rightComponent {
        return false
      }
    }
    return false
  }

  public static func ==(lhs: Version, rhs: Version) -> Bool {
    return lhs.components == rhs.components
  }

  // For nice logging output.
  public var description: String {
    return self.string
  }

  public var majorMinor: Version {
    if(self.components.count >= 2) {
      return Version([self.components[0], self.components[1]])!
    } else {
      // If we somehow have just a major version, append a simple '0'.
      return Version([self.components[0], 0])!
    }
  }
}
