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
  private static var _enabled: Bool = true

  public static var hasStarted: Bool {
    return Sentry.Client.shared != nil
  }

  public static func start(sendingEnabled: Bool = true) {
    // First things first:  enable Sentry for crash reporting.
    do {
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

      let options: [String: Any] = [
        "dsn": "https://d14d2efb594e4345b8367dbb61ebceaf@sentry.keyman.com/8",
        "enabled": allowEnabled && sendingEnabled,
        "environment": environment,
        "release": release
      ]
      Sentry.Client.shared = try Sentry.Client(options: options)
      try Sentry.Client.shared?.startCrashHandler()
    } catch let error {
      // Does not throw error if 'net is unavailable.  It's for some sort of error within the Sentry system.
      print("\(error)")
    }

    // This function is called on _every_ event and crash that may occur, giving us a place to
    // capture and filter them.
    Sentry.Client.shared?.shouldSendEvent = { event in
      return SentryManager.shouldSendEventHandler(event: event)
    }
  }

  public static var enabled: Bool {
    get {
      return SentryManager._enabled
    }

    set(flag) {
      SentryManager._enabled = flag

      // Ensure that the embedded KeymanWeb engine's crash-reporting state is also updated.
      Manager.shared.inputViewController.refreshCrashReporting()
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
    Sentry.Client.shared?.breadcrumbs.add(Sentry.Breadcrumb(level: .info, category: "Deliberate testing error"))
    Sentry.Client.shared?.crash()
  }
}
