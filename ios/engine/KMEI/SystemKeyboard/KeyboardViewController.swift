//
//  KeyboardViewController.swift
//  SystemKeyboard
//
//  Created by Gabriel Wong on 2017-10-05.
//  Copyright © 2017 SIL International. All rights reserved.
//

import KeymanEngine

class KeyboardViewController: KeymanInputViewController {
  override func viewDidLoad() {
    Manager.setApplicationGroupIdentifier("group.KMEI")
    Manager.shared.isDebugPrintingOn = true
    super.viewDidLoad()
  }
}
