/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KeymanEngineLogger.swift
 * KeymanEngine
 *
 * Created by Shawn Schantz on 2024-01-16.
 *
 * Basic logging capability provided by wrapping Apple's Unified Logging
 * Framework. Works with iOS 10 and later.
 * Log statements are written with a call the `os_log` API which takes a
 * message, an `OSLog` reference and log level.
 * Several static references to `OSLog` objects are defined, each of which
 * determines the subsystem and category of subsequent log statements.
 * Both subsystem and category are useful for filtering with the Console app.
 * Use the same subsystem name for all `OSLog` objects created here, but specify
 * a unique category to identify related functionality.
 * Add as many static `OSLog` resources as necessary to support new log
 * categories.
 */

import Foundation
import os.log

// bundle for engine is "org.sil.Keyman.ios.Engine"
// but selecting this subsystem name for consistency across apps
private let subsystem = "com.keyman.ios.engine"

public struct KeymanEngineLogger {
  static let engine = OSLog(subsystem: subsystem, category: "Engine")
  static let migration = OSLog(subsystem: subsystem, category: "Migration")
  static let resources = OSLog(subsystem: subsystem, category: "Resources")
  static let settings = OSLog(subsystem: subsystem, category: "Settings")
  static let ui = OSLog(subsystem: subsystem, category: "UI")
}
