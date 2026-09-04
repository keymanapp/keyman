/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-09-01
 *
 * Convenience messages for logging to Sentry
 */

import Sentry

import Foundation

/**
 * Extend URL to add function that cleans path strings, removing the home directory from the path.
 */
extension URL {
  /**
   * If the path contains the user's home, replace it with ~ as the home directory name
   * may contain the user's name and should not be written to the logs.
   */
  public func cleanUrlPath() -> String {
    guard self.isFileURL else { return self.absoluteString }
    
    let unescapedPath = self.path(percentEncoded: false)
    let homeDirectory = NSHomeDirectory()
    
    if unescapedPath.hasPrefix(homeDirectory) {
      let relativeComponent = unescapedPath.dropFirst(homeDirectory.count)
      return "~\(relativeComponent)"
    }
    
    return unescapedPath
  }
}

public struct LogUtil {
  public enum LogCategory: String {
    case setup  // related to start of app and installation
    case data   // related to settings and reading and writing packages
    case app    // general app functionality and UI
    case download   // downloading keyboard packages
  }
  
  public static func debugBreadcrumb(_ message: String, category: LogCategory) {
    addBreadcrumb(message: message, category: category.rawValue, level: .debug)
  }
  
  public static func infoBreadcrumb(_ message: String, category: LogCategory) {
    addBreadcrumb(message: message, category: category.rawValue, level: .info)
  }

  public static func warningBreadcrumb(_ message: String, category: LogCategory) {
    addBreadcrumb(message: message, category: category.rawValue, level: .warning)
  }

  public static func errorBreadcrumb(_ message: String, category: LogCategory) {
    addBreadcrumb(message: message, category: category.rawValue, level: .error)
  }
  
  // Private helper to talk to Sentry
  private static func addBreadcrumb(message: String, category: String, level: SentryLevel) {
    let crumb = Breadcrumb(level: level, category: category)
    crumb.message = message
    SentrySDK.addBreadcrumb(crumb)
  }
}
