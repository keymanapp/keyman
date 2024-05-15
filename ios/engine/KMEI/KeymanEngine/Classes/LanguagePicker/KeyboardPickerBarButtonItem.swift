//
//  KeyboardPickerBarButtonItem.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-09-12.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

public class KeyboardPickerBarButtonItem: UIBarButtonItem {
  weak var presentingVC: UIViewController?
  
  // Returns a UIBarButtonItem that will display the keyboard picker when tapped
  //   - since the keyboard picker is modal, a UIViewController must be supplied to display it
  //   - the button has default images for normal and landscape orientations
  //      - these can be overridden with other images or a title
  public init(presentingVC: UIViewController) {
    super.init()
    style = .plain
    action = #selector(self.showKeyboardPicker)
    target = self
    self.presentingVC = presentingVC
    
    image = UIImage(named: "keyboard_icon", in: Resources.bundle, compatibleWith: nil)
    
    if UIDevice.current.userInterfaceIdiom == .phone {
      landscapeImagePhone = UIImage(named: "keyboard_icon_landscape",
                                    in: Resources.bundle,
                                    compatibleWith: nil)
    }
  }
  
  public required init?(coder aDecoder: NSCoder) {
    fatalError("init(coder:) has not been implemented")
  }
  
  deinit {
    presentingVC = nil
  }
  
  @objc func showKeyboardPicker() {
    if let presentingVC = presentingVC {
      Manager.shared.showKeyboardPicker(in: presentingVC, shouldAddKeyboard: false)
    }
  }
  
  public override var title: String? {
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
