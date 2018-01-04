//
//  AppDelegate.swift
//  KMSample2
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
  var window: UIWindow?

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]? = nil) -> Bool {
    // Set desired level before release
    KeymanEngine.log.outputLevel = .debug
    KeymanEngine.log.logAppDetails()

    // Replace with your application group id
    Manager.applicationGroupIdentifier = "group.KMSample"
    return true
  }
}
