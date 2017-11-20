//
//  UIButton+Helpers.swift
//  KeymanEngine
//
//  Created by Gabriel Wong on 2017-10-16.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

extension UIButton {
  func setRoundedBorder(withRadius radius: CGFloat, borderWidth: CGFloat, color: UIColor) {
    layer.cornerRadius = radius
    layer.borderWidth = borderWidth
    layer.borderColor = color.cgColor
    layer.masksToBounds = true
  }
}
