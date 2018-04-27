//
//  KMUnderscoreView.swift
//  Keyman
//
//  Created by Jacob Bullock on 4/26/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import  UIKit

class KMUnderscoreView: UIView  {
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

    override func draw(_ rect: CGRect) {
        let color: UIColor = .yellow

       
        let widths: [CGFloat] = [0.6, 0.25, 0.15]
        let colors: [UIColor] = [ UIColor(red: 244/255, green: 113/255, blue: 32/255, alpha: 1.0),
                                  UIColor(red: 186/255, green: 32/255, blue: 52/255, alpha: 1.0),
                                  UIColor(red: 105/255, green: 183/255, blue: 211/255, alpha: 1.0)]

        var x: CGFloat = 0
        for i in 0..<(widths.count) {
            let w: CGFloat = rect.width * widths[i]
            let drect = CGRect(x: x, y: 0,  width: w, height: rect.height)
            let bpath: UIBezierPath = UIBezierPath(rect: drect)
            
            let c = colors[i]
            c.set()
            bpath.fill()

            x += w
        }

    }
}
