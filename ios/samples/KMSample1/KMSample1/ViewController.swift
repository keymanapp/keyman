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
    Manager.shared.openURL = UIApplication.shared.openURL
    Manager.shared.isKeymanHelpOn = false

    let kb = InstallableKeyboard(id: "tamil99m",
                                 name: "Tamil 99M",
                                 languageID: "tam",
                                 languageName: "Tamil",
                                 version: "1.1",
                                 isRTL: false,
                                 font: Font(filename: "aava1.ttf"),
                                 oskFont: nil,
                                 isCustom: true)
    let urls = [
      Bundle.main.url(forResource: "tamil99m-1.1", withExtension: "js")!,
      Bundle.main.url(forResource: "aava1", withExtension: "ttf")!
    ]
    do {
      try Manager.shared.preloadFiles(forKeyboardID: kb.id, at: urls, shouldOverwrite: true)
    } catch {
      print("Error preloading: \(error)")
    }
    FontManager.shared.registerCustomFonts()

    Manager.shared.addKeyboard(Defaults.keyboard)
    Manager.shared.addKeyboard(kb)

    textView.setKeymanDelegate(self)
    textView.viewController = self
  }
}
