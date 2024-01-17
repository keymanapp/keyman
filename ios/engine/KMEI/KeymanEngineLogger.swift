/**
 * Keyman is copyright (C) SIL International. MIT License.
 *
 * KeymanEngineLogger.swift
 * KeymanEngine
 *
 * Created by Shawn Schantz on 2024-01-16.
 *
 * Basic logging capability provided by wrapping Apple's Unified Logging Framework.
 */

import Foundation
import os.log

private let subsystem = "org.sil.KeymanEngine4Mac"

public struct KeymanEngineLogger {
  static let engine = OSLog(subsystem: subsystem, category: "engine")
  static let migration = OSLog(subsystem: subsystem, category: "migration")
  static let sentry = OSLog(subsystem: subsystem, category: "sentry")
  static let settings = OSLog(subsystem: subsystem, category: "settings")
}
