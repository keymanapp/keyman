//
//  AppDelegate.swift
//  KeymanEngineTests
//
//  Created by Joshua Horton on 2/18/20.
//  Copyright © 2020 SIL International. All rights reserved.
//
// This AppDelegate is extremely minimal and is designed solely as an app that provides a nice
// sandboxed location for file-system-oriented testing.

import Foundation
import KeymanEngine
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
  var window: UIWindow?
  
  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
    return true
  }
  
  func application(_ application: UIApplication, handleOpen url: URL) -> Bool {
    return true
  }
  
  func application(_ application: UIApplication, open url: URL, sourceApplication: String?, annotation: Any) -> Bool {
    return true
  }
  
  func applicationDidEnterBackground(_ application: UIApplication) {
    FontManager.shared.unregisterCustomFonts()
  }
  
  func applicationWillEnterForeground(_ application: UIApplication) {
    FontManager.shared.registerCustomFonts()
  }
}
