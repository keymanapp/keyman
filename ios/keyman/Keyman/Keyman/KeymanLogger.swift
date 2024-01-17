/**
 * Keyman is copyright (C) SIL International. MIT License.
 * 
 * KeymanLogger.swift
 * Keyman
 * 
 * Created by Shawn Schantz on 2024-01-11.
 * 
 * Basic logging capability provided by wrapping Apple's Unified Logging Framework.
 * Works with iOS 10 and later.
 * Each time we call the `os_log` API
 */

import Foundation
import os.log

private let subsystem = "keyman.inputmethod.Keyman"

public struct KeymanLogger {
  static let ui = OSLog(subsystem: subsystem, category: "ui")
  static let resources = OSLog(subsystem: subsystem, category: "resources")
  static let settings = OSLog(subsystem: subsystem, category: "settings")
}
