//
//  Log.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-12-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import XCGLogger

public let log: XCGLogger = {
  // Default:  the 'console', which is read by Xcode but doesn't reach the system logs.
  let mainLog = XCGLogger(identifier: "KeymanEngine", includeDefaultDestinations: true)

  // Ensures our log messages go out to the device's system log.
  let systemLogDest = AppleSystemLogDestination(identifier: "")
  systemLogDest.showLogIdentifier = true
#if DEBUG
  systemLogDest.outputLevel = .debug
#else
  systemLogDest.outputLevel = .warning
#endif
  mainLog.add(destination: systemLogDest)

  return mainLog
}()
