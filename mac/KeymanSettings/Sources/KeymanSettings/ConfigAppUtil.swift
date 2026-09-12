/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-07-13
 *
 * Used for getting application metadata from the Config application
 */

import Foundation
import OSLog
import Sentry

extension Logger {
  private static let settingsSubsystem = "com.keyman.settings"
  static let setup = Logger(subsystem: settingsSubsystem, category: "setup")
  static let data = Logger(subsystem: settingsSubsystem, category: "data")
}

public struct ConfigAppUtil {
  static public let configBundleId = "com.keyman.config"
  
  // executes exactly once, the first time any config variable is read
  private static let configMap: [String: String]? = {
    guard let map = Bundle.main.infoDictionary?["Keyman"] as? [String: String] else {
      let message = "Keyman dictionary not found in main app bundle."
      LogUtil.errorBreadcrumb(message, category: .setup)
      fatalError(message)
    }
    return map
  }()
  
  public static let sentryEnvironment: String = {
    return configMap?["SentryEnvironment"] as? String ?? ""
  }()

  public static let appTier: String = {
    return configMap?["Tier"] as? String ?? ""
  }()

  public static let versionTag: String = {
    return configMap?["VersionTag"] as? String ?? ""
  }()

  public static let versionWithTag: String = {
    return configMap?["VersionWithTag"] as? String ?? ""
  }()

  public static let versionGitTag: String = {
    return configMap?["VersionGitTag"] as? String ?? ""
  }()

  public static let versionRelease: String = {
    return configMap?["VersionRelease"] as? String ?? ""
  }()

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
  
  static func captureSentryError(_ error: Error) {
    SentrySDK.capture(error: error)
  }
}
