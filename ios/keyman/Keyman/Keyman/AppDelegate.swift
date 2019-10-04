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

@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {

  private var _overlayWindow: UIWindow?
  private var _adhocDirectory: URL?

  var window: UIWindow?
  var viewController: MainViewController!
  var navigationController: UINavigationController?

  func application(_ app: UIApplication, open url: URL,
                   options: [UIApplicationOpenURLOptionsKey: Any] = [:]) -> Bool {
    // .kmp package install, Keyman 10 onwards
    var destinationUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    destinationUrl.appendPathComponent("\(url.lastPathComponent).zip")
    do {
      try FileManager.default.copyItem(at: url, to: destinationUrl)
      installAdhocKeyboard(url: destinationUrl)
      return true
    } catch {
      showKMPError(KMPError.copyFiles)
      return false
    }
  }

  func application(_ application: UIApplication,
                   didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]? = nil) -> Bool {
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

  func application(_ application: UIApplication, open url: URL, sourceApplication: String?, annotation: Any) -> Bool {
    NotificationCenter.default.post(name: launchedFromUrlNotification, object: self,
        userInfo: [urlKey: url]
    )
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

  public func installAdhocKeyboard(url: URL) {
    let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
    var destination =  documentsDirectory
    destination.appendPathComponent("temp/\(url.lastPathComponent)")

    KeymanPackage.extract(fileUrl: url, destination: destination, complete: { kmp in
      if let kmp = kmp {
        self.promptAdHocInstall(kmp)
      } else {
        self.showKMPError(KMPError.invalidPackage)
      }
    })
  }

  public func showKMPError(_ error: KMPError) {
    showSimpleAlert(title: "Error", message: error.rawValue)
  }

  public func showSimpleAlert(title: String, message: String) {
    let alertController = UIAlertController(title: title, message: message,
                                            preferredStyle: UIAlertControllerStyle.alert)
    alertController.addAction(UIAlertAction(title: "OK",
                                            style: UIAlertActionStyle.default,
                                            handler: nil))

    self.window?.rootViewController?.present(alertController, animated: true, completion: nil)
  }

  public func promptAdHocInstall(_ kmp: KeymanPackage) {
    _adhocDirectory = kmp.sourceFolder
    let isKbd = kmp.isKeyboard()

    let vc = UIViewController()
    vc.view.backgroundColor = .red
    let wkWebView = WKWebView.init(frame: vc.view.frame)
    wkWebView.backgroundColor = .white
    vc.view.addSubview(wkWebView)
    let cancelBtn = UIBarButtonItem(title: "Cancel", style: .plain,
                                    target: self,
                                    action: #selector(cancelAdHocBtnHandler))
    let installBtn = UIBarButtonItem(title: "Install", style: .plain,
                                     target: self,
                                     action: (isKbd ? #selector(installAdHocKeyboardBtnHandler) :
                                       #selector(installAdHocLexicalModelBtnHandler)) )
    vc.navigationItem.leftBarButtonItem = cancelBtn
    vc.navigationItem.rightBarButtonItem = installBtn
    let nvc = UINavigationController.init(rootViewController: vc)

    self.window?.rootViewController?.present(nvc, animated: true, completion: {
      wkWebView.loadHTMLString(kmp.infoHtml(), baseURL: nil)
    })
  }

  @objc func installAdHocKeyboardBtnHandler() {
    if let adhocDir = _adhocDirectory {
      self.window?.rootViewController?.dismiss(animated: true, completion: {
        do {
          try Manager.shared.parseKbdKMP(adhocDir)
          self.showSimpleAlert(title: "Success", message: "Installed successfully.")
        } catch {
          self.showKMPError(error as! KMPError)
        }

        //this can fail gracefully and not show errors to users
        do {
          try FileManager.default.removeItem(at: adhocDir)
        } catch {
          log.error("unable to delete temp files")
        }
      })
    }
  }

  @objc func installAdHocLexicalModelBtnHandler() {
    if let adhocDir = _adhocDirectory {
      self.window?.rootViewController?.dismiss(animated: true, completion: {
        do {
          try Manager.parseLMKMP(adhocDir)
          self.showSimpleAlert(title: "Success", message: "Installed successfully.")
        } catch {
          self.showKMPError(error as! KMPError)
        }

        //this can fail gracefully and not show errors to users
        do {
          try FileManager.default.removeItem(at: adhocDir)
        } catch {
          log.error("unable to delete temp files")
        }
      })
    }
  }

  @objc func cancelAdHocBtnHandler() {
    self.window?.rootViewController?.dismiss(animated: true, completion: nil)
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
