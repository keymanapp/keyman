//
//  AppDelegate.swift
//  FirstVoices app
//
//  License: MIT
//
//  Copyright © 2019 FirstVoices.
//
//  Created by Serkan Kurt on 17/11/2015.
//  Converted by Marc Durdin on 15/05/2019.
//

import KeymanEngine
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
  var window: UIWindow?

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
    SentryManager.start()
    FVRegionStorage.upgrade()

    #if DEBUG
      KeymanEngine.log.outputLevel = .debug
      KeymanEngine.log.logAppDetails()
    #else
      KeymanEngine.log.outputLevel = .warning
    #endif

    // TODO:  we need a way for the setting to be toggled by users (opt-in).
    // SentryManager.enabled = true;

    // Replace with your application group id
    Manager.applicationGroupIdentifier = FVConstants.groupID
    return true
  }
}

