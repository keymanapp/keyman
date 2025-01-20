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

    FVRegionStorage.upgrade()
    
    // Replace with your application group id
    Manager.applicationGroupIdentifier = FVConstants.groupID
    Manager.shared.spacebarText = .KEYBOARD
    return true
  }
}

