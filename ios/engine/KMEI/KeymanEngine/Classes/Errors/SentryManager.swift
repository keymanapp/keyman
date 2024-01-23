//
//  SentryManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 3/5/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
import Sentry
import os.log

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
      os_log("Sentry error logging disabled for development mode.", log: KeymanEngineLogger.settings, type: .debug)
    #else
      let allowEnabled = true
      os_log("Sentry error logging enabled.", log: KeymanEngineLogger.settings, type: .debug)
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

  /**
   * Captures a Sentry event  and safely bypass the Sentry component if not activated by the app.
   */
  public static func capture(_ event: Sentry.Event) {
    // Guarded in case a library consumer decides against initializing Sentry.
    if _started {
      SentrySDK.capture(event: event)
    }
  }

  /**
   * Captures a Sentry event for the specified error. If the logging level is not specified, it will default to .error.
   * Will safely bypass the Sentry component if not activated by the app.
   */
  public static func capture(_ error: Error, message: String? = nil, sentryLevel: Sentry.SentryLevel = .error) {
    let event = Sentry.Event(error: error)
    event.level = sentryLevel
    if let message = message {
      event.message = SentryMessage(formatted: message)
    }

    self.capture(event)
  }

  /**
   * Constructs a SentryEvent around a message. If the logging level is not specified, it will default to .error.
   * Will safely bypass the Sentry component if not activated by the app.
   */
  public static func capture(_ message: String, sentryLevel: Sentry.SentryLevel = .error) {
    let event = Sentry.Event(level: sentryLevel)
    event.message = SentryMessage(formatted: message)

    self.capture(event)
  }

  /**
   * Adds a Sentry breadcrumb. Will safely bypass the Sentry component if not activated by the app.
   */
  public static func breadcrumb(crumb: Sentry.Breadcrumb) {
    // Guarded in case a library consumer decides against initializing Sentry.
    if _started {
      SentrySDK.addBreadcrumb(crumb)
    }
  }

  /**
   * Adds a Sentry breadcrumb. Will safely bypass the Sentry component if not activated by the app.
   */
  public static func breadcrumb(_ message: String, category: String? = nil, sentryLevel: Sentry.SentryLevel = .info) {
    let crumb = Sentry.Breadcrumb()
    crumb.level = sentryLevel
    if let category = category {
      crumb.category = category
    }
    crumb.message = message

    self.breadcrumb(crumb: crumb)
  }

  public static func forceError() {
    SentrySDK.addBreadcrumb(Sentry.Breadcrumb(level: .info, category: "Deliberate testing error"))
    SentrySDK.crash()
  }
}
