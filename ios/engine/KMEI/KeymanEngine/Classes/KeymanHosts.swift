//
//  KeymanHosts.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 7/20/20.
//  Copyright © 2020 SIL International. All rights reserved.

import Foundation

/**
 * Defines constant-like definitions for Keyman's websites dependent upon the compile-time version
 * of the KeymanEngine framework.
 *
 * Note that as a result, 'alpha', 'beta', and 'stable' versions of KeymanEngine may point to different
 * sites.
 */
public enum KeymanHosts {
  /**
   * Used to enable '.local' variants of the endpoints for use in local development testing.
   */
  internal static let useLocal = false
  
  // This should never be used outside of the corresponding `static var`...
  // save for use in automated testing.
  internal static func getApiSiteURL(forTier: Version.Tier, useLocal: Bool) -> URL {
    if useLocal {
      return URL.init(string: "http://api.keyman.com.local")!
    } else {
      switch forTier {
      case .alpha:
        fallthrough
      case .beta:
        return URL.init(string: "https://api.keyman.com")! // #7227 disabling: "https://api.keyman-staging.com")!
      case .stable:
        return URL.init(string: "https://api.keyman.com")!
      }
    }
  }
  
  /**
   * Used for package-version and model queries.
   */
  public static var API_KEYMAN_COM: URL {
    return getApiSiteURL(forTier: Version.currentTagged.tier ?? .stable, useLocal: useLocal)
  }
  
  // This should never be used outside of the corresponding `static var`...
  // save for use in automated testing.
  internal static func getHelpSiteURL(forTier: Version.Tier, useLocal: Bool) -> URL {
    if useLocal {
      return URL.init(string: "http://help.keyman.com.local")!
    } else {
      switch forTier {
      case .alpha:
        fallthrough
      case .beta:
        return URL.init(string: "https://help.keyman.com")! // #7227 disabling: "https://help.keyman-staging.com")!
      case .stable:
        return URL.init(string: "https://help.keyman.com")!
      }
    }
  }
  
  /**
   * Used for online help.
   */
  public static var HELP_KEYMAN_COM: URL {
    return getHelpSiteURL(forTier: Version.currentTagged.tier ?? .stable, useLocal: useLocal)
  }
  
  // This should never be used outside of the corresponding `static var`...
  // save for use in automated testing.
  internal static func getMainSiteURL(forTier: Version.Tier, useLocal: Bool) -> URL {
    if useLocal {
      return URL.init(string: "http://keyman.com.local")!
    } else {
      switch forTier {
      case .alpha:
        fallthrough
      case .beta:
        return URL.init(string: "https://keyman.com")! // #7227 disabling: "https://keyman-staging.com")!
      case .stable:
        return URL.init(string: "https://keyman.com")!
      }
    }
  }
  
  /**
   * Used for keyboard searches and resource sharing links.
   */
  public static var KEYMAN_COM: URL {
    return getMainSiteURL(forTier: Version.currentTagged.tier ?? .stable, useLocal: useLocal)
  }
}
