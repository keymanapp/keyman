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

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
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
        // First, explicitly hide the keyboard.  Otherwise, the app may try to
        // redisplay it before package installation is fully complete.
        Manager.shared.hideKeyboard()

        // We choose to prompt the user for confirmation, rather
        // than automatically installing the package.
        //
        // Since we're operating at the root, we want to present in a separate UINavigationController.
        let nvc = UINavigationController.init()
        rfm.promptPackageInstall(of: package, in: nvc, isCustom: true)
        vc.present(nvc, animated: true, completion: nil)
      }
    } else {
      let message = "Cannot find app's root UIViewController"
      os_log("%{public}s", log: KeymanLogger.ui, type: .info, message)
      SentryManager.capture(message)
    }

    return true
  }

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]? = nil) -> Bool {
    
    SentryManager.start()
    return true
  }

  // Handles universal links.
  func application(_ application: UIApplication,
                   continue userActivity: NSUserActivity,
                   restorationHandler: @escaping ([UIUserActivityRestoring]?) -> Void) -> Bool {
    if userActivity.activityType == NSUserActivityTypeBrowsingWeb {
      guard let incomingURL = userActivity.webpageURL else {
        return false
      }

      if let parsedLink = UniversalLinks.tryParseKeyboardInstallLink(incomingURL) {
        // We use this mostly to shorten line lengths, b/c lint warnings.
        let downloadManager = ResourceDownloadManager.shared

        // Aha!  We know this link type!
        let downloadLink: URL
        if let langID = parsedLink.lang_id {
          let fullID = FullKeyboardID(keyboardID: parsedLink.keyboard_id, languageID: langID)
          downloadLink = downloadManager.defaultDownloadURL(forPackage: parsedLink.packageKey,
                                                            andResource: fullID,
                                                            asUpdate: false)
        } else {
          downloadLink = downloadManager.defaultDownloadURL(forPackage: parsedLink.packageKey, asUpdate: false)
        }
        downloadManager.downloadPackage(withKey: parsedLink.packageKey,
                                        from: downloadLink) { (package: KeyboardKeymanPackage?, error: Error?) in
          guard error == nil, let package = package else {
            // Maybe add an alert about the package error?
            return
          }
          if let vc = self.window?.rootViewController {
            // Force the app to the top-level view.  (Prompts won't display if we're in a submenu!)
            vc.dismiss(animated: true, completion: nil)

            // We choose to prompt the user for comfirmation, rather
            // than automatically installing the package.
            let nvc = UINavigationController.init()
            ResourceFileManager.shared.promptPackageInstall(of: package, in: nvc, isCustom: true)
            vc.present(nvc, animated: true, completion: nil)
          } else {
            let message = "Cannot find app's root UIViewController"
            os_log("%{public}s", log: KeymanLogger.ui, type: .error, message)
            SentryManager.capture(message)
          }
        }
        return true
      }
    }

    return false
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
