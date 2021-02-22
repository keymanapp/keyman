//
//  Log.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-12-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import XCGLogger

// From XCGLogger docs:
// Note: This creates the log object lazily, which means it's not created until it's actually needed.
public let log: XCGLogger = {
  // Default:  the 'console', which is read by Xcode but doesn't reach the system logs.
  let mainLog = XCGLogger(identifier: "Keyman", includeDefaultDestinations: false)

  // Ensures our log messages go out to the device's system log as well as the console.
  let systemLogDest = AppleSystemLogDestination(identifier: "")
  systemLogDest.showLogIdentifier = true

  mainLog.add(destination: systemLogDest)

  // Temporary logging level to ensure that app details are reported properly.
  mainLog.outputLevel = .info
  mainLog.logAppDetails()

#if DEBUG
  mainLog.outputLevel = .debug
#else
  mainLog.outputLevel = .warning
#endif

  return mainLog
}()
