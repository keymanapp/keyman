//
//  KeyboardPickerBarButtonItem.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-12.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

class KeyboardPickerBarButtonItem: UIBarButtonItem {
  weak var presentingVC: UIViewController?

  // Returns a UIBarButtonItem that will display the keyboard picker when tapped
  //   - since the keyboard picker is modal, a UIViewController must be supplied to display it
  //   - the button has default images for normal and landscape orientations
  //      - these can be overridden with other images or a title
  @objc init(presentingVC: UIViewController) {
    super.init()
    style = .plain
    action = #selector(self.showKeyboardPicker)
    target = self
    self.presentingVC = presentingVC

    guard let bundlePath = Bundle.main.path(forResource: "Keyman", ofType: "bundle") else {
      return
    }
    let keymanBundle = Bundle(path: bundlePath)
    let retinaSuffix = KMManager.retinaScreen() ? "@2x" : ""

    if let imagePath = keymanBundle?.path(forResource: "keyboard_icon\(retinaSuffix)", ofType: "png") {
      image = UIImage(contentsOfFile: imagePath)
    }

    if UIDevice.current.userInterfaceIdiom == .phone {
      if let imagePath = keymanBundle?.path(forResource: "keyboard_icon_landscape\(retinaSuffix)", ofType: "png") {
        landscapeImagePhone = UIImage(contentsOfFile: imagePath)
      }
    }
  }

  required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }

  deinit {
    presentingVC = nil
  }

  @objc func showKeyboardPicker() {
    if let presentingVC = presentingVC {
      KMManager.sharedInstance().showKeyboardPicker(in: presentingVC, shouldAddKeyboard: false)
    }
  }

  override var title: String? {
    get {
      return super.title
    }

    set(title) {
      image = nil
      landscapeImagePhone = nil
      super.title = title
    }
  }
}
