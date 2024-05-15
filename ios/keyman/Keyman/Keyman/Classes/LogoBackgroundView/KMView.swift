//
//  KMView.swift
//  Keyman
//
//  Created by Jacob Bullock on 4/29/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import UIKit

class KMView: UIView {
  required public init?(coder: NSCoder) {
    super.init(coder: coder)
    self.commonInit()
  }
  
  public override init(frame: CGRect) {
    super.init(frame: frame)
    self.commonInit()
  }
  
  func commonInit() {
  }
}
