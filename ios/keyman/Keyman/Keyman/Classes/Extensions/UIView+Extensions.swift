//
//  UIView+Extensions.swfit
//  Keyman
//
//  Created by Jacob Bullock on 4/26/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import UIKit

extension UIView {
  
  func constrainEqually(to parent: UIView,
                        attribute: NSLayoutConstraint.Attribute,
                        constant: CGFloat = 0) -> NSLayoutConstraint {
    
    let constraint = NSLayoutConstraint(item: self,
                                        attribute: attribute,
                                        relatedBy: .equal,
                                        toItem: parent,
                                        attribute: attribute,
                                        multiplier: 1,
                                        constant: constant)
    return constraint
  }
  
  func constrainEqually(to constant: CGFloat, attribute: NSLayoutConstraint.Attribute) -> NSLayoutConstraint {
    let constraint = NSLayoutConstraint(
      item: self,
      attribute: attribute,
      relatedBy: .equal,
      toItem: nil,
      attribute: .notAnAttribute,
      multiplier: 1.0,
      constant: constant)
    return constraint
  }
  
  func constrainCorners(to parent: UIView, options: [String: CGFloat]) {
    self.translatesAutoresizingMaskIntoConstraints = false
    NSLayoutConstraint(item: self,
                       attribute: .top,
                       relatedBy: .equal,
                       toItem: parent,
                       attribute: .top,
                       multiplier: 1,
                       constant: (options["top.constant"] ?? 0)).isActive = true
    
    NSLayoutConstraint(item: self,
                       attribute: .bottom,
                       relatedBy: .equal,
                       toItem: parent,
                       attribute: .bottom,
                       multiplier: 1,
                       constant: (options["bottom.constant"] ?? 0)).isActive = true
    
    NSLayoutConstraint(item: self,
                       attribute: .leading,
                       relatedBy: .equal,
                       toItem: parent,
                       attribute: .leading,
                       multiplier: 1,
                       constant: (options["leading.constant"] ?? 0)).isActive = true
    
    NSLayoutConstraint(item: self,
                       attribute: .trailing,
                       relatedBy: .equal,
                       toItem: parent,
                       attribute: .trailing,
                       multiplier: 1,
                       constant: (options["trailing.constant"] ?? 0)).isActive = true
  }
}
