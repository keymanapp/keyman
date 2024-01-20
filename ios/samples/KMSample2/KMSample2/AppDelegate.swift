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
    // Replace with your application group id
    // Ensure this happens before installing any keyboards or models within the engine
    // whenever using App Group Identifiers.
    Manager.applicationGroupIdentifier = "group.KMSample"
    return true
  }
}
