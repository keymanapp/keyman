//
//  AppDelegate.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine
import UIKit
import WebKit
import Sentry

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {

  private var _overlayWindow: UIWindow?
  private var _adhocDirectory: URL?

  var window: UIWindow?
  var viewController: MainViewController!
  var navigationController: UINavigationController?

  func application(_ app: UIApplication, open url: URL,
                   options: [UIApplication.OpenURLOptionsKey: Any] = [:]) -> Bool {
    // We really should validate that it is a .kmp first... but the app doesn't yet
    // process URL links, so it's fine for now.  (Will change with QR code stuff.)

    let rfm = ResourceFileManager.shared
    guard let destinationUrl = rfm.importFile(url) else {
      return false
    }

    if let vc = window?.rootViewController {
      // Force the app to the top-level view.  (Prompts won't display if we're in a submenu!)
      vc.dismiss(animated: true, completion: nil)

      if let package = rfm.prepareKMPInstall(from: destinationUrl, alertHost: vc) {
        // We choose to prompt the user for comfirmation, rather
        // than automatically installing the package.
        rfm.promptPackageInstall(of: package, in: vc, isCustom: true)
      }
    } else {
      log.error("Cannot find app's root UIViewController")
    }

    return true
  }

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
    SentryManager.start()

    #if DEBUG
      KeymanEngine.log.outputLevel = .debug
      log.outputLevel = .debug
      KeymanEngine.log.logAppDetails()
    #else
      KeymanEngine.log.outputLevel = .warning
      log.outputLevel = .warning
    #endif

    Manager.applicationGroupIdentifier = "group.KM4I"
    Manager.shared.openURL = UIApplication.shared.openURL

    // TODO:  Assign a subclassed version of InputViewController that implements the image stuff.
    Manager.shared.inputViewController = KeyboardViewController(forSystem: false)

    window = UIWindow(frame: UIScreen.main.bounds)

    // Initialize overlayWindow
    _ = overlayWindow

    // Override point for customization after application launch.
    viewController = MainViewController()
    let navigationController = UINavigationController(rootViewController: viewController)

    // Navigation bar became translucent by default in iOS7 SDK, however we don't want it to be translucent.
    navigationController.navigationBar.isTranslucent = false

    window!.rootViewController = navigationController
    window!.makeKeyAndVisible()

    //TODO: look in documents directory for .zip / .kmp files
    //TODO: OR browse documents Dir from new keyboard window
    //self.installAdhocKeyboard(filePath: "")

    return true
  }

  func applicationDidEnterBackground(_ application: UIApplication) {
    _overlayWindow = nil
    FontManager.shared.unregisterCustomFonts()
    let userData = AppDelegate.activeUserDefaults()
    // TODO: Have viewController save its data
    userData.set(viewController?.textView?.text, forKey: userTextKey)
    userData.set(viewController?.textSize.description, forKey: userTextSizeKey)
    userData.synchronize()
  }

  func applicationWillEnterForeground(_ application: UIApplication) {
    perform(#selector(self.registerCustomFonts), with: nil, afterDelay: 1.0)
  }

  var overlayWindow: UIWindow {
    if _overlayWindow == nil {
      _overlayWindow = UIWindow(frame: UIScreen.main.bounds)
      _overlayWindow!.rootViewController = UIViewController()
      _overlayWindow!.autoresizingMask = [.flexibleWidth, .flexibleHeight]
      _overlayWindow!.autoresizesSubviews = true
      _overlayWindow!.backgroundColor = UIColor(white: 0.0, alpha: 0.75)
      _overlayWindow!.windowLevel = UIWindow.Level.statusBar + 1
      _overlayWindow!.isUserInteractionEnabled = true
      _overlayWindow!.isHidden = true
    }

    // Workaround to display overlay window above keyboard
    if #available(iOS 9.0, *) {
      let windows = UIApplication.shared.windows
      if let lastWindow = windows.last {
        _overlayWindow!.windowLevel = lastWindow.windowLevel + 1
      }
    }
    return _overlayWindow!
  }

  @objc func registerCustomFonts() {
    FontManager.shared.registerCustomFonts()
  }

  class func activeUserDefaults() -> UserDefaults {
    if let userDefaults = UserDefaults(suiteName: "group.KM4I") {
      return userDefaults
    }
    return UserDefaults.standard
  }

  class func isKeymanEnabledSystemWide() -> Bool {
    guard let keyboards = UserDefaults.standard.dictionaryRepresentation()["AppleKeyboards"] as? [String] else {
      return false
    }
    return keyboards.contains("Tavultesoft.Keyman.SWKeyboard")
  }

  class func statusBarHeight() -> CGFloat {
    return UIApplication.shared.statusBarFrame.height
  }
}
