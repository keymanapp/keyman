//
//  AppDelegate.swift
//  FirstVoices
//
//  Created by Serkan Kurt on 17/11/2015.
//  Converted by Marc Durdin on 15/5/19.
//  Copyright © 2019 FirstVoices. All rights reserved.
//
import KeymanEngine
//import FVShared
import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
  var window: UIWindow?

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
    // Set desired level before release
    KeymanEngine.log.outputLevel = .debug
    KeymanEngine.log.logAppDetails()

    // Replace with your application group id
    Manager.applicationGroupIdentifier = "group.FVKeyboards"
    return true
  }
}

