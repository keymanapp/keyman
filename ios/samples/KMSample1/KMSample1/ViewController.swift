//
//  ViewController.swift
//  KMSample1
//
//  Created by Gabriel Wong on 2017-10-06.
//  Copyright © 2017 SIL International. All rights reserved.
//
import KeymanEngine
import UIKit

class ViewController: UIViewController, TextViewDelegate {
  @IBOutlet var textView: TextView!

  override func viewDidLoad() {
    super.viewDidLoad()
    Manager.shared.isKeymanHelpOn = false

    let kmpFileURL = Bundle.main.url(forResource: "ekwtamil99uni", withExtension: "kmp")!
    let keyboardID = FullKeyboardID(keyboardID: "ekwtamil99uni", languageID: "ta")

    do {
      let resource = try ResourceFileManager.shared.prepareKMPInstall(from: kmpFileURL)
      if let package = resource as? KeyboardKeymanPackage {
        try ResourceFileManager.shared.install(resourceWithID: keyboardID, from: package)
        _ = Manager.shared.setKeyboard(withFullID: keyboardID)
      } else {
        print("Error installing: \(keyboardID) is not a keyboard package")
      }
    } catch {
      print("Error preloading: \(error)")
    }

    textView.becomeFirstResponder()
    textView.setKeymanDelegate(self)
    textView.viewController = self
  }
}
