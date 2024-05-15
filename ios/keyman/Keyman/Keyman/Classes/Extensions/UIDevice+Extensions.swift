//
//  UIDevice+Extensions.swift
//  Keyman
//
//  Created by Jacob Bullock on 4/26/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import UIKit

extension UIDevice {
  static var isIphoneX: Bool {
    let iphonexScreenHeight: Int = 812
    let screenRect = UIScreen.main.bounds
    let size = CGFloat.maximum(screenRect.width, screenRect.height)
    return UIDevice.current.userInterfaceIdiom == .phone && size == CGFloat(iphonexScreenHeight)
  }
}
