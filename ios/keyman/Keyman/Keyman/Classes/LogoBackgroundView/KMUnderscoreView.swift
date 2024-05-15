//
//  KMUnderscoreView.swift
//  Keyman
//
//  Created by Jacob Bullock on 4/26/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import  UIKit

class KMUnderscoreView: KMView {
  override func draw(_ rect: CGRect) {
    // percentage based widths for the color bar portion of the logo
    let widths: [CGFloat] = [0.6, 0.25, 0.15]
    let colors: [UIColor] = [ UIColor.keymanOrange,
                              UIColor.keymanRed,
                              UIColor.keymanBlue]
    
    var x: CGFloat = 0
    for i in 0..<(widths.count) {
      let w: CGFloat = rect.width * widths[i]
      let drect = CGRect(x: x, y: 0, width: w, height: rect.height)
      let bpath: UIBezierPath = UIBezierPath(rect: drect)
      
      let color = colors[i]
      color.set()
      bpath.fill()
      
      x += w
    }
    
  }
}
