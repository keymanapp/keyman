////
////  ResourceManager.swift
////  KeymanEngine
////
////  Created by Joshua Horton on 12/18/19.
////  Copyright © 2019 SIL International. All rights reserved.
////
//
//import Foundation
//
///**
// * This class stores common methods used for installing language resources, regardless of source.
// *
// * It also contains methods for general-purpose installation of language resources from .kmp files.
// */
//public class ResourceFileManager {
//  public static let shared = ResourceFileManager()
//
//  fileprivate init() {
//  }
//
//  /**
//   * Use this function to "install" external KMP files to within the Keyman app's alloted iOS file management domain.
//   * Note that we don't request permissions to support opening/modifying files "in place," so we need to copy .kmps
//   * before unzipping them.
//   *
//   * This implementation does not change how files are managed by the app; only where the file management code
//   * is located.
//   */
//  public func installFile(_ url: URL) {
//      // Once selected, start the standard install process.
//      log.info("Installing KMP at \(url)")
//
//      // Step 1: Copy it to within the app's controlled space, making it a .zip in the process
//      var destinationUrl = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
//      destinationUrl.appendPathComponent("\(url.lastPathComponent).zip")
//
//      do {
//        let fileManager = FileManager.default
//
//        // For now, we'll always allow overwriting.
//        if fileManager.fileExists(atPath: destinationUrl.path) {
//          try fileManager.removeItem(at: destinationUrl)
//        }
//
//        // Throws an error if the destination file already exists, and there's no
//        // built-in override parameter.  Hence, the previous if-block.
//        try fileManager.copyItem(at: url, to: destinationUrl)
//        installAdhocKeyboard(url: destinationUrl)
//        return true
//      } catch {
//        showKMPError(KMPError.copyFiles)
//        log.error(error)
//        return
//      }
//
//      // Now, install it as if we'd just downloaded it.
//      let resourceManager = ResourceDownloadManager.shared
//      // Obviously, a BIG assumption here:  that it's a lexical model, not a keyboard.
//      // Pardon the proof of concept.
//      guard let lexicalModels = resourceManager.installLexicalModelPackage(at: destinationUrl) else {
//        log.info("Could not install KMP at \(url)")
//        return
//      }
//
//      log.info("Attempt success!")
//  }
//
//  private func installAdhocKeyboard(url: URL) {
//    let documentsDirectory = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask)[0]
//    var destination =  documentsDirectory
//    destination.appendPathComponent("temp/\(url.lastPathComponent)")
//
//    KeymanPackage.extract(fileUrl: url, destination: destination, complete: { kmp in
//      if let kmp = kmp {
//        self.promptAdHocInstall(kmp)
//      } else {
//        self.showKMPError(KMPError.invalidPackage)
//      }
//    })
//  }
//
//  public func promptAdHocInstall(_ kmp: KeymanPackage) {
//    _adhocDirectory = kmp.sourceFolder
//    let isKbd = kmp.isKeyboard()
//
//    let vc = UIViewController()
//    vc.view.backgroundColor = .red
//    let wkWebView = WKWebView.init(frame: vc.view.frame)
//    wkWebView.backgroundColor = .white
//    vc.view.addSubview(wkWebView)
//    let cancelBtn = UIBarButtonItem(title: "Cancel", style: .plain,
//                                    target: self,
//                                    action: #selector(cancelAdHocBtnHandler))
//    let installBtn = UIBarButtonItem(title: "Install", style: .plain,
//                                     target: self,
//                                     action: (isKbd ? #selector(installAdHocKeyboardBtnHandler) :
//                                       #selector(installAdHocLexicalModelBtnHandler)) )
//    vc.navigationItem.leftBarButtonItem = cancelBtn
//    vc.navigationItem.rightBarButtonItem = installBtn
//    let nvc = UINavigationController.init(rootViewController: vc)
//
//    self.window?.rootViewController?.present(nvc, animated: true, completion: {
//      wkWebView.loadHTMLString(kmp.infoHtml(), baseURL: nil)
//    })
//  }
//
//  @objc func installAdHocKeyboardBtnHandler() {
//    if let adhocDir = _adhocDirectory {
//      self.window?.rootViewController?.dismiss(animated: true, completion: {
//        do {
//          try Manager.shared.parseKbdKMP(adhocDir)
//          self.showSimpleAlert(title: "Success", message: "Installed successfully.")
//        } catch {
//          self.showKMPError(error as! KMPError)
//        }
//
//        //this can fail gracefully and not show errors to users
//        do {
//          try FileManager.default.removeItem(at: adhocDir)
//        } catch {
//          log.error("unable to delete temp files")
//        }
//      })
//    }
//  }
//
//  @objc func installAdHocLexicalModelBtnHandler() {
//    if let adhocDir = _adhocDirectory {
//      self.window?.rootViewController?.dismiss(animated: true, completion: {
//        do {
//          try Manager.parseLMKMP(adhocDir)
//          self.showSimpleAlert(title: "Success", message: "Installed successfully.")
//        } catch {
//          self.showKMPError(error as! KMPError)
//        }
//
//        //this can fail gracefully and not show errors to users
//        do {
//          try FileManager.default.removeItem(at: adhocDir)
//        } catch {
//          log.error("unable to delete temp files")
//        }
//      })
//    }
//  }
//
//  @objc func cancelAdHocBtnHandler() {
//    self.window?.rootViewController?.dismiss(animated: true, completion: nil)
//  }
//
//  @objc func registerCustomFonts() {
//    FontManager.shared.registerCustomFonts()
//  }
//}
