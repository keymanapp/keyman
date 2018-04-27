//
//  KMNavigationBarBackgroundView.swift
//  Keyman
//
//  Created by Jacob Bullock on 4/26/18.
//  Copyright © 2018 SIL International. All rights reserved.
//

import Foundation
import UIKit

class KMNavigationBarBackgroundView: UIView {

    var imageLeadingConstraint: NSLayoutConstraint?

    required public init?(coder: NSCoder) {
        super.init(coder: coder)
        self.commonInit()
    }

    public override init(frame: CGRect) {
        super.init(frame: frame)
        self.commonInit()
    }

    func commonInit() {
        self.backgroundColor = .clear

        setupLogo()
        setupUnderscore()
    }

    func setupUnderscore() {
        let v = KMUnderscoreView(frame: CGRect())
        self.addSubview(v)
        v.translatesAutoresizingMaskIntoConstraints = false

        v.constrainEqually(to: self, attribute: .bottom).isActive  = true
        v.constrainEqually(to: self, attribute: .leading).isActive  = true
        v.constrainEqually(to: self, attribute: .trailing).isActive  = true
        v.constrainEqually(to: 3, attribute: .height).isActive  = true
    }

    func setupLogo() {
        let image = UIImage(named: "keyman_logo")
        let imageView = UIImageView(image: image)
        imageView.contentMode = .scaleAspectFit
        imageView.translatesAutoresizingMaskIntoConstraints = false
        self.addSubview(imageView)

        imageView.constrainEqually(to: self, attribute: .centerY).isActive  = true
        imageView.constrainEqually(to: self, attribute: .centerY).isActive  = true
        imageView.constrainEqually(to: self, attribute: .height, constant: -8).isActive  = true
        imageView.constrainEqually(to: 100, attribute: .width).isActive  = true

        imageLeadingConstraint = imageView.constrainEqually(to: self, attribute: .leading, constant: 10)
        imageLeadingConstraint?.isActive = true
    }

    func addToNavbar(_ navbar: UINavigationBar!) {
        navbar.addSubview(self)
        self.constrainCorners(to: navbar, options: ["top.constant": 0])
    }

    func setOrientation(_ orientation: UIInterfaceOrientation) {
        var constant: CGFloat = 10
        if let constraint = imageLeadingConstraint {
            if orientation.isLandscape && UIDevice.isIphoneX {
                constant = 25
            }
            constraint.constant = constant
            self.setNeedsLayout()
        }
    }
}
