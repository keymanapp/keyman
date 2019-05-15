//
//  Checkbox.swift
//  FirstVoices
//
//  Created by Marc Durdin on 15/5/19.
//  Copyright © 2019 FirstVoices. All rights reserved.
//
//  https://stackoverflow.com/a/33519147/1836776

import Foundation
import UIKit

class CheckBox: UIButton {
  // Images
  let checkedImage = UIImage(named: "ic_check_box")! as UIImage
  let uncheckedImage = UIImage(named: "ic_check_box_outline_blank")! as UIImage

  // Bool property
  var isChecked: Bool = false {
    didSet{
      if isChecked == true {
        self.setImage(checkedImage, for: UIControl.State.normal)
      } else {
        self.setImage(uncheckedImage, for: UIControl.State.normal)
      }
    }
  }

  override func awakeFromNib() {
    self.addTarget(self, action:#selector(buttonClicked(sender:)), for: UIControl.Event.touchUpInside)
    self.isChecked = false
  }

  @objc func buttonClicked(sender: UIButton) {
    if sender == self {
      isChecked = !isChecked
    }
  }
}
