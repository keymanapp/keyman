/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-07-13
 *
 * Used for getting application metadata from the Config application
 */

import Foundation

public struct ConfigAppUtil {
  /**
   * returns the short version string from the bundle of the Config app
   */
  static public func configAppVersion()  -> String {
    return Bundle.main.infoDictionary?["CFBundleShortVersionString"] as? String ?? "unknown"
  }
  
  
}
