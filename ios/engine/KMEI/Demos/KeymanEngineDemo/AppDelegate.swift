//
//  AppDelegate.swift
//  KeymanEngineDemo
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
  var window: UIWindow?
  
  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]? = nil) -> Bool {
    Manager.applicationGroupIdentifier = "group.KMEI"
    Manager.shared.canRemoveDefaultKeyboard = true
    
    window = UIWindow(frame: UIScreen.main.bounds)
    
    let mainViewController = MainViewController(nibName: nil, bundle: nil)
    let navCon = UINavigationController(rootViewController: mainViewController)
    window!.rootViewController = navCon
    
    window!.makeKeyAndVisible()
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
