/*
 * Keyman is copyright (C) SIL Global. MIT License.
 *
 * Created by Shawn Schantz on 2026-09-01
 *
 * Convenience messages for logging to Sentry
 */

import Sentry

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
