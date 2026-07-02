//
//  Logger.swift
//  KeyFig
//
//  Created by Shawn - SIL on 12/9/25.
//

import OSLog

class ConfigLogger {
  //static let shared = ConfigLogger()

  fileprivate let subsystem = KeymanPaths.configBundleId
  fileprivate let testCategory = "test"
  public let testLogger: Logger
  
  fileprivate init() {
    testLogger = Logger(subsystem: subsystem, category: testCategory)

    testLogger.debug("ConfigLogger instance created.")
  }
}
