//
//  AppDelegate.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {

  private var _overlayWindow: UIWindow?

  var window: UIWindow?
  var viewController: MainViewController!
  var navigationController: UINavigationController?

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey : Any]? = nil) -> Bool {
    window = UIWindow(frame: UIScreen.main.bounds)

    // Initialize overlayWindow
    _ = overlayWindow

    // Override point for customization after application launch.
    if UIDevice.current.userInterfaceIdiom == .phone {
      viewController = MainViewController(nibName: "MainViewController_iPhone", bundle: nil)
    } else {
      viewController = MainViewController(nibName: "MainViewController_iPad", bundle: nil)
    }
    let navigationController = UINavigationController(rootViewController: viewController)

    // Navigation bar became translucent by default in iOS7 SDK, however we don't want it to be translucent.
    navigationController.navigationBar.isTranslucent = false

    window!.rootViewController = navigationController
    window!.makeKeyAndVisible()
    return true
  }

  func application(_ application: UIApplication, open url: URL, sourceApplication: String?, annotation: Any) -> Bool {
    NotificationCenter.default.post(name: launchedFromUrlNotification, object: self,
        userInfo: [urlKey: url]
    )
    return true
  }

  func applicationDidEnterBackground(_ application: UIApplication) {
    _overlayWindow = nil
    KMManager.sharedInstance().unregisterCustomFonts()
    let userData = AppDelegate.activeUserDefaults()
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
      _overlayWindow!.windowLevel = UIWindowLevelStatusBar + 1
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

  func registerCustomFonts() {
    KMManager.sharedInstance().registerCustomFonts()
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
