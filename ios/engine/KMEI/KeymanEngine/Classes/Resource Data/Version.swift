//
//  Version.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-11-23.
//  Copyright © 2017 SIL International. All rights reserved.
//

import Foundation

/// Dotted-decimal version.
public class Version: NSObject, Comparable {
  public enum Tier: String {
    case alpha
    case beta
    case stable
    
    // In case we wish to do any tier-based comparisons.
    public static func < (lhs: Version.Tier, rhs: Version.Tier) -> Bool {
      return lhs.rawValue < rhs.rawValue
    }
  }
  
  public static let fallback = Version("1.0")!
  public static let latestFeature = packageBasedFileReorg
  
  public static var current: Version {
    return Version(Version.currentTagged.plainString)!
  }
  
  public static var currentTagged: Version {
    let engineInfo = Bundle(for: Manager.self).infoDictionary
    return Version(engineInfo!["KeymanVersionWithTag"] as! String)!
  }
  
  // The Engine first started tracking the 'last loaded version' in 12.0.
  public static let freshInstall = Version("0.0")!
  public static let firstTracked = Version("12.0")!
  
  // The Keyman App's file browser was first added in 13.0 alpha;
  // this prompted a mild rework of KMP installation file management,
  // since this exposed byproducts to the users in the Files app.
  public static let fileBrowserImplemented = Version("13.0")!
  public static let defaultsNeedBackup = Version("13.0.65")!
  
  // A major shift in where resources are installed occurred during
  // the 14.0 alpha.
  public static let packageBasedFileReorg = Version("14.0")!
  
  private let components: [Int]
  public let plainString: String
  public let fullString: String
  public let tier: Tier?
  public var major: Int {
    get {
      return components[0]
    }
  }
  
  public init?(_ string: String) {
    let tagComponents = string.components(separatedBy: "-")
    
    plainString = tagComponents[0]
    let stringComponents = tagComponents[0].components(separatedBy: ".")
    
    var components: [Int] = []
    for s in stringComponents {
      guard let i = Int(s), i >= 0 else {
        return nil
      }
      components.append(i)
    }
    
    if tagComponents.count > 1 {
      switch tagComponents[1] {
      case "alpha":
        tier = .alpha
      case "beta":
        tier = .beta
      case "stable":
        tier = .stable
      default:
        tier = nil
      }
    } else {
      tier = nil
    }
    
    self.fullString = string
    self.components = components
  }
  
  public init?(_ components: [Int], tier: Tier? = nil) {
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
    
    self.plainString = string
    
    if let tier = tier {
      fullString = "\(plainString)-\(tier)"
    } else {
      fullString = plainString
    }
    
    self.components = components  // Swift arrays are value types, not reference types!
    self.tier = tier
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
  
  /**
   * Checks for version equality, disregarding any metadata tagging.
   *
   * (For example, 12.3.45 beta is considered equal to 12.3.45 stable.)
   */
  public static func ==(lhs: Version, rhs: Version) -> Bool {
    var left = lhs.components
    while(left.last == 0 && left.count > 0) {
      left.removeLast()
    }
    
    var right = rhs.components
    while(right.last == 0 && right.count > 0) {
      right.removeLast()
    }
    
    return left.elementsEqual(right)
  }
  
  public override func isEqual(_ object: Any?) -> Bool {
    if let object = object as? Version {
      return self == object
    } else {
      return false
    }
  }
  
  // For nice logging output & debugger visibility.
  public override var description: String {
    return self.fullString
  }
  
  public var majorMinor: Version {
    if(self.components.count >= 2) {
      return Version([self.components[0], self.components[1]], tier: tier)!
    } else {
      // If we somehow have just a major version, append a simple '0'.
      return Version([self.components[0], 0], tier: tier)!
    }
  }
}
