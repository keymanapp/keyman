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
import os.log

// Note:  this needs to be kept separate from AppDelegate; unintended behavior
// may result if one object implements both.  (Two separate instances of the same,
// supposedly-singleton object!)
class SceneDelegate: UIResponder, UISceneDelegate {
  private var _overlayWindow: UIWindow?
  private var _mainScene: UIWindowScene?

  var window: UIWindow?
  var viewController: MainViewController!
  var navigationController: UINavigationController?

  func commonStart() {
    // Override point for customization after application launch.
    viewController = MainViewController()
    let navigationController = UINavigationController(rootViewController: viewController)

    // Navigation bar became translucent by default in iOS7 SDK, however we don't want it to be translucent.
    navigationController.navigationBar.isTranslucent = false

    window!.rootViewController = navigationController
    window!.makeKeyAndVisible()

    // TODO: look in documents directory for .zip / .kmp files
    // TODO: OR browse documents Dir from new keyboard window
    // self.installAdhocKeyboard(filePath: "")
  }

  func scene(
    _ scene: UIScene,
    willConnectTo session: UISceneSession,
    options connectionOptions: UIScene.ConnectionOptions
  ) {
    guard let windowScene = scene as? UIWindowScene else {
      return
    }

    // In iOS 15, navigation bars become transparent by default when the edge
    // of the scrollable content aligns with the edge of the navigation bar.
    // Force the appearance back to the pre-iOS 15 default behavior.
    UINavigationBar.appearance().scrollEdgeAppearance = UINavigationBarAppearance()

    UniversalLinks.externalLinkLauncher = { url in
      UIApplication.shared.open(url)
    }

    Manager.applicationGroupIdentifier = "group.KM4I"

    // TODO:  Assign a subclassed version of InputViewController that implements the image stuff.
    Manager.shared.inputViewController = KeyboardViewController(forSystem: false)

    window = UIWindow(windowScene: windowScene)
    _mainScene = windowScene
    commonStart()
  }

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {

    SentryManager.start()
    return true
  }

  func sceneDidEnterBackground(_ scene: UIScene) {
    _overlayWindow = nil
    FontManager.shared.unregisterCustomFonts()
    viewController?.saveTextSettings()
  }

  func sceneWillEnterForeground(_ scene: UIScene) {
    perform(#selector(self.registerCustomFonts), with: nil, afterDelay: 1.0)
  }

  // Might be from this setup?
  var overlayWindow: UIWindow {
    let mainScene = self._mainScene!

    if _overlayWindow == nil {
      _overlayWindow = UIWindow(windowScene: mainScene)
      _overlayWindow!.rootViewController = UIViewController()
      _overlayWindow!.autoresizingMask = [.flexibleWidth, .flexibleHeight]
      _overlayWindow!.autoresizesSubviews = true
      _overlayWindow!.backgroundColor = UIColor(white: 0.0, alpha: 0.75)
      _overlayWindow!.windowLevel = UIWindow.Level.statusBar + 1
      _overlayWindow!.isUserInteractionEnabled = true
      _overlayWindow!.isHidden = true
    }

    // Workaround to display overlay window above keyboard
    let windows = mainScene.windows
    if let lastWindow = windows.last {
      _overlayWindow!.windowLevel = lastWindow.windowLevel + 1
    }

    return _overlayWindow!
  }

  @objc func registerCustomFonts() {
    FontManager.shared.registerCustomFonts()
  }
}
