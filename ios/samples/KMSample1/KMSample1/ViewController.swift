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
    do {
      try Manager.shared.preloadKeyboardFile(at: Bundle.main.url(forResource: "tamil99m-1.1", withExtension: "js")!,
                                             shouldOverwrite: true)
      try Manager.shared.preloadFontFile(at: Bundle.main.url(forResource: "aava1", withExtension: "ttf")!,
                                         shouldOverwrite: true)
    } catch {
      print("Error preloading: \(error)")
    }
    FontManager.shared.registerCustomFonts()

    Manager.shared.addKeyboard(Defaults.keyboard)
    let kb = InstallableKeyboard(id: "tamil99m",
                                 name: "Tamil 99M",
                                 languageID: "tam",
                                 languageName: "Tamil",
                                 version: "1.1",
                                 isRTL: false,
                                 font: Font(filename: "aava1.ttf"),
                                 oskFont: nil,
                                 isCustom: true)
    Manager.shared.addKeyboard(kb)

    textView.setKeymanDelegate(self)
    textView.viewController = self
  }
}
