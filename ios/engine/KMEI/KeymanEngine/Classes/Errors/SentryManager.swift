//
//  SentryManager.swift
//  KeymanEngine
//
//  Created by Joshua Horton on 3/5/20.
//  Copyright © 2020 SIL International. All rights reserved.
//

import Foundation
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

    let infoDict = Bundle.main.infoDictionary
    let versionWithTag = infoDict?["KeymanVersionWithTag"] as? String ?? ""
    let environment = infoDict?["KeymanVersionEnvironment"] as? String ?? ""
    let release = "release-\(versionWithTag)"

    let options = Sentry.Options()
    options.dsn = "https://d14d2efb594e4345b8367dbb61ebceaf@sentry.keyman.com/8"
    options.enabled = allowEnabled && sendingEnabled
    options.environment = environment
    options.releaseName = release
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

  public static func forceError() {
    SentrySDK.addBreadcrumb(crumb: Sentry.Breadcrumb(level: .info, category: "Deliberate testing error"))
    SentrySDK.crash()
  }
}
