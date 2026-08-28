/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-07-13
 *
 * Used for getting application metadata from the Config application
 */

import Foundation

public struct ConfigAppUtil {
  static public let configBundleId = "com.keyman.config"
  
  /**
   * returns the short version string from the bundle of the Config app
   */
  static public func configAppVersion()  -> String {
    let kConfigTestVersionKey = "testConfigVersion"
    
    if let configTestVersion = UserDefaults.standard.string(forKey: kConfigTestVersionKey) {
      // for testing only, if a test version string is found in the standard UserDefaults of the config app
      // (not in the group container UserDefaults), then use it instead
      return configTestVersion
    } else {
      // get the actual version number from the application bundle
      return Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "unknown"
    }
  }
  
  /**
   * returns whether this app is stable, beta or alpha
   */
  static public func configAppTier() -> String {
    guard let appTier = Bundle.main.object(forInfoDictionaryKey: "KMAppTier") as? String else {
      print("KMAppTier missing from Info.plist")
      return "unknown"
    }
    
    print("appTier: \(appTier)")

    return appTier
  }
}
