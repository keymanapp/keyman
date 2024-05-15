//
//  UIImage+Helpers.swift
//  Keyman
//
//  Created by Gabriel Wong on 2017-09-07.
//  Copyright © 2017 SIL International. All rights reserved.
//

import UIKit

extension UIImage {
  func resize(to newSize: CGSize) -> UIImage {
    UIGraphicsBeginImageContextWithOptions(newSize, false, 0)
    draw(in: CGRect(origin: CGPoint(x: 0, y: 0), size: newSize))
    let newImage = UIGraphicsGetImageFromCurrentImageContext()
    UIGraphicsEndImageContext()
    return newImage!
  }
  
  func tintedImage(using tintColor: UIColor) -> UIImage {
    UIGraphicsBeginImageContextWithOptions(size, false, scale)
    let drawRect = CGRect(x: 0, y: 0, width: size.width, height: size.height)
    draw(in: drawRect)
    tintColor.set()
    UIRectFillUsingBlendMode(drawRect, CGBlendMode.sourceAtop)
    let tintedImage = UIGraphicsGetImageFromCurrentImageContext()
    UIGraphicsEndImageContext()
    return tintedImage!
  }
}
