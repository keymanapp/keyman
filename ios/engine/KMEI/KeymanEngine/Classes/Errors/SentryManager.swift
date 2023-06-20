//
//  SentryManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 3/5/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import XCGLogger
import Sentry

/**
 * This class centralizes the methods used among the KeymanEngine library, Keyman app, and SWKeyboard app-ex for Sentry-based
 * error reporting.
 */
public class SentryManager {
  private static var _started: Bool = false

  public static var hasStarted: Bool {
    return _started
  }

  public static func start(sendingEnabled: Bool = true) {
    // First things first:  enable Sentry for crash reporting.
    #if NO_SENTRY
      // If doing development debugging (and NOT for Sentry code), silence Sentry reporting.
      let allowEnabled = false
      log.debug("Sentry error logging disabled for development mode.")
    #else
      let allowEnabled = true
      log.debug("Sentry error logging enabled.")
    #endif

    let infoDict = Bundle(for: SentryManager.self).infoDictionary
    let versionWithTag = infoDict?["KeymanVersionWithTag"] as? String ?? ""
    let environment = infoDict?["KeymanVersionEnvironment"] as? String ?? ""
    let versionGitTag = "release@\(versionWithTag)"

    let options = Sentry.Options()
    options.dsn = "https://d14d2efb594e4345b8367dbb61ebceaf@o1005580.ingest.sentry.io/5983521"
    options.enabled = allowEnabled && sendingEnabled
    options.environment = environment
    options.releaseName = versionGitTag
    options.beforeSend = { event in
      // This function is called on _every_ event and crash that may occur, giving us a place to
      // capture and filter them.
      if SentryManager.shouldSendEventHandler(event: event) {
        return event
      } else {
        return nil
      }
    }

    SentrySDK.start(options: options)
    _started = true
  }

  public static func altEnabled() -> Bool {
    let userData = Storage.active.userDefaults
    return userData.bool(forKey: Key.optShouldReportErrors)
  }

  public static var enabled: Bool {
    get {
      let userData = Storage.active.userDefaults
      return userData.bool(forKey: Key.optShouldReportErrors)
    }

    set(flag) {
      let userData = Storage.active.userDefaults

      // Save the preference
      userData.set(flag, forKey: Key.optShouldReportErrors)
      userData.synchronize()

      // Ensure that the embedded KeymanWeb engine's crash-reporting state is also updated.
      Manager.shared.inputViewController.setSentryState(enabled: flag)
    }
  }

  // Note:  this function is called from a separate thread!
  // For some reason, does not update when `enabled` is changed unless the app is restarted.
  private static func shouldSendEventHandler(event: Sentry.Event) -> Bool {
    #if NO_SENTRY
      // Prevents Sentry from buffering the event.
      return false
    #else
      return SentryManager.enabled
    #endif
  }

  private static func mapLoggingLevel(_ level: Sentry.SentryLevel) -> XCGLogger.Level {
    switch(level) {
        case .none:
          return XCGLogger.Level.none
        case .debug:
          return .debug
        case .info:
          return .info
        case .warning:
          return .warning
        case .error:
          return .error
        case .fatal:
          return .severe
        default:
          return .info
      }
  }

  /**
   * Captures a Sentry event and copies its message to the engine's logging mechanism.
   * If the logging level is not specified, the Sentry event's log-level will be used as a default.
   *
   * Will safely bypass the Sentry component if not activated by the app, only logging the
   * message in such scenarios.
   */
  public static func captureAndLog(_ event: Sentry.Event, logLevel: XCGLogger.Level? = nil) {
    // Guarded in case a library consumer decides against initializing Sentry.
    if _started {
      SentrySDK.capture(event: event)
    }

    let level = logLevel ?? mapLoggingLevel(event.level)
    log.logln(event.message?.formatted, level: level)
  }

  /**
   * Captures a Sentry event and copies its message to the engine's logging mechanism.
   * If the logging level is not specified, the Sentry event's log-level will be used as a default.
   *
   * Will safely bypass the Sentry component if not activated by the app, only logging the
   * message in such scenarios.
   */
  public static func captureAndLog(_ error: Error, message: String? = nil, sentryLevel: Sentry.SentryLevel = .error, logLevel: XCGLogger.Level? = nil) {
    let event = Sentry.Event(error: error)
    event.level = sentryLevel
    if let message = message {
      event.message = SentryMessage(formatted: message)
    }

    self.captureAndLog(event, logLevel: logLevel)
  }

  /**
   * Constructs a SentryEvent around a message and also passes it to the engine's logging mechanism.
   * If the logging level is not specified, it will default to .error.
   *
   * Will safely bypass the Sentry component if not activated by the app, only logging the
   * message in such scenarios.
   */
  public static func captureAndLog(_ message: String, sentryLevel: Sentry.SentryLevel = .error, logLevel: XCGLogger.Level? = nil) {
    let event = Sentry.Event(level: sentryLevel)
    event.message = SentryMessage(formatted: message)

    self.captureAndLog(event, logLevel: logLevel)
  }

  /**
   * Adds a Sentry breadcrumb and copies its message to the engine's logging mechanism.
   * If the logging level is not specified, the Sentry event's log-level will be used as a default.
   *
   * Will safely bypass the Sentry component if not activated by the app, only logging the
   * message in such scenarios.
   */
  public static func breadcrumbAndLog(crumb: Sentry.Breadcrumb, logLevel: XCGLogger.Level? = nil) {
    // Guarded in case a library consumer decides against initializing Sentry.
    if _started {
      SentrySDK.addBreadcrumb(crumb)
    }

    let level = logLevel ?? mapLoggingLevel(crumb.level)
    log.logln(crumb.message, level: level)
  }

  /**
   * Adds a Sentry breadcrumb and copies its message to the engine's logging mechanism.
   * If the logging level is not specified, the Sentry event's log-level will be used as a default.
   *
   * Will safely bypass the Sentry component if not activated by the app, only logging the
   * message in such scenarios.
   */
  public static func breadcrumbAndLog(_ message: String, category: String? = nil, sentryLevel: Sentry.SentryLevel = .info, logLevel: XCGLogger.Level? = nil) {
    let crumb = Sentry.Breadcrumb()
    crumb.level = sentryLevel
    if let category = category {
      crumb.category = category
    }
    crumb.message = message

    self.breadcrumbAndLog(crumb: crumb, logLevel: logLevel)
  }

  public static func forceError() {
    SentrySDK.addBreadcrumb(Sentry.Breadcrumb(level: .info, category: "Deliberate testing error"))
    SentrySDK.crash()
  }
}
